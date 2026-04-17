{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.CanonicalState.Import where

import Cardano.Ledger.BaseTypes (
  EpochNo,
  SlotNo,
  Version,
 )
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  Decoder,
  DecoderError,
  decodeFull,
  decodeFullAnnotator,
  decodeFullDecoder,
 )
import Cardano.Ledger.CanonicalState.Export (
  BlockFailures,
  ExportState (ExportLedgerState),
  TestFixture (..),
  TxFailures,
 )
import Cardano.Ledger.Core (
  EraTx (Tx),
  KeyHash,
  KeyRole (BlockIssuer),
  TopTx,
 )
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, decodeChunkEntry)
import Cardano.SCLS.Internal.Reader (withNamespacedDataHandle)
import Cardano.SCLS.NamespaceCodec (KnownNamespace (NamespaceEntry, NamespaceKey))
import Cardano.Types.Namespace (fromSymbol)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Text as T
import GHC.IO.Handle (Handle)
import GHC.TypeLits (KnownSymbol)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S
import System.FilePath (takeExtension, (</>))

class KnownNamespace ns => ImportCanonicalNamespace era ns where
  importNamespace ::
    Monad m =>
    ExportLedgerState era ->
    Stream (Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) m () ->
    m (ExportLedgerState era)

class ImportCanonicalState era where
  importCanonicalState ::
    FilePath ->
    EpochNo ->
    IO (SlotNo, ExportLedgerState era)

class ImportFailures era where
  decodeTxFailures :: Decoder s (TxFailures era)
  decodeBlockFailures :: Decoder s (BlockFailures era)

data InMemoryTestFixture era = InMemoryTestFixture
  { imtfEpochNo :: EpochNo
  , imtfInitialState :: (SlotNo, ExportLedgerState era)
  , imtfTransactions ::
      TxOrBlock (Tx TopTx era) (KeyHash BlockIssuer, SSeq.StrictSeq (Tx TopTx era))
  , imtfFinalState ::
      Either (TxOrBlock (TxFailures era) (BlockFailures era)) (SlotNo, ExportLedgerState era)
  }

data TxOrBlock tx block
  = OrTx tx
  | OrBlock block
  deriving (Show)

loadInMemoryTestFixture ::
  forall era.
  ( ImportCanonicalState era
  , DecCBOR (Annotator (Tx TopTx era))
  , ImportFailures era
  ) =>
  FilePath ->
  Version ->
  TestFixture ->
  IO (Either DecoderError (InMemoryTestFixture era))
loadInMemoryTestFixture dir protocolVersion TestFixture {..} = do
  imtfInitialState <- importCanonicalState @era (dir </> initialState) epochNo

  txs <- case transactions of
    Left txFile -> do
      bytes <- BSL.readFile (dir </> txFile)
      pure $ OrTx <$> decodeFullAnnotator protocolVersion (T.pack "Tx") decCBOR bytes
    Right (blockIssuerFile, txFiles) -> do
      blockIssuer <-
        decodeFull protocolVersion <$> BSL.readFile (dir </> blockIssuerFile)
      t <-
        forM txFiles $
          fmap
            (decodeFullAnnotator protocolVersion (T.pack "Tx") decCBOR)
            . BSL.readFile
            . (dir </>)
      pure $
        blockIssuer >>= \bi ->
          OrBlock . (bi,) . SSeq.fromList
            <$> foldr
              ( \tt acc -> case (acc, tt) of
                  (Left err, _) -> Left err
                  (Right _, Left err) -> Left err
                  (Right acc', Right ttt) -> Right $ ttt : acc'
              )
              (Right [])
              t

  imtfFinalState' <-
    if takeExtension finalState == ".scls"
      then
        Right . Right
          <$> importCanonicalState @era (dir </> finalState) epochNo
      else do
        bs <- BSL.readFile (dir </> finalState)
        case decodeFullDecoder protocolVersion "TxFailures" (decodeTxFailures @era) bs of
          Left _err ->
            case decodeFullDecoder protocolVersion "BlockFailures" (decodeBlockFailures @era) bs of
              Left err ->
                pure (Left err)
              Right blockFailures ->
                pure (Right (Left (OrBlock blockFailures)))
          Right txFailures ->
            pure (Right (Left (OrTx txFailures)))

  case imtfFinalState' of
    Left err ->
      pure (Left err)
    Right imtfFinalState ->
      pure $
        fmap
          ( \imtfTransactions ->
              InMemoryTestFixture
                { imtfEpochNo = epochNo
                , imtfInitialState
                , imtfTransactions
                , imtfFinalState
                }
          )
          txs

importNamespaceFromHandle ::
  forall era v.
  (KnownSymbol v, ImportCanonicalNamespace era v) =>
  Handle -> Proxy v -> ExportLedgerState era -> IO (ExportLedgerState era)
importNamespaceFromHandle h (p :: Proxy v) nes =
  withNamespacedDataHandle h (fromSymbol p) $ \s ->
    importNamespace @era @v
      nes
      (S.mapMaybe (decodeChunkEntry p) s)
