{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  DecoderError,
  decodeFull,
  decodeFullAnnotator,
 )
import Cardano.Ledger.CanonicalState.Export (ExportState (ExportLedgerState), TestFixture (..))
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
import Data.Sequence.Strict (StrictSeq, fromList)
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

data InMemoryTestFixture era = InMemoryTestFixture
  { imtfEpochNo :: EpochNo
  , imtfInitialState :: (SlotNo, ExportLedgerState era)
  , imtfTransactions :: TxOrBlock (Tx TopTx era)
  , imtfFinalState :: Either () (SlotNo, ExportLedgerState era)
  }

data TxOrBlock tx
  = OrTx tx
  | OrBlock (KeyHash BlockIssuer) (StrictSeq tx)
  deriving (Show)

loadInMemoryTestFixture ::
  forall era.
  ( ImportCanonicalState era
  , DecCBOR (Annotator (Tx TopTx era))
  ) =>
  FilePath ->
  Version ->
  TestFixture ->
  IO (Either DecoderError (InMemoryTestFixture era))
loadInMemoryTestFixture dir protocolVersion TestFixture {..} = do
  imtfInitialState <- importCanonicalState @era (dir </> initialState) epochNo
  imtfFinalState <-
    if takeExtension finalState == ".scls"
      then
        Right <$> importCanonicalState @era (dir </> finalState) epochNo
      else
        pure (Left ())

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
          OrBlock bi . fromList
            <$> foldr
              ( \tt acc -> case (acc, tt) of
                  (Left err, _) -> Left err
                  (Right _, Left err) -> Left err
                  (Right acc', Right ttt) -> Right $ ttt : acc'
              )
              (Right [])
              t
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
