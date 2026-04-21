{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
  Decoder,
  DecoderError,
  decodeFull,
  decodeFullAnnotator,
  decodeFullDecoder,
 )
import Cardano.Ledger.CanonicalState.Export (
  BlockFailures,
  ExportCanonicalState (ExportLedgerState),
  StateTransition (..),
  TxFailures,
  TxOrBlock (..),
  mapTxOrBlockM,
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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), except)
import Data.Bitraversable (bimapM)
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Text as T
import GHC.IO.Handle (Handle)
import GHC.TypeLits (KnownSymbol)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S
import System.FilePath ((</>))

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
      Either (TxOrBlock (TxFailures era) (BlockFailures era)) FilePath
  }

loadInMemoryTestFixture ::
  forall era.
  ( ImportCanonicalState era
  , DecCBOR (Annotator (Tx TopTx era))
  , ImportFailures era
  ) =>
  FilePath ->
  Version ->
  StateTransition ->
  ExceptT DecoderError IO (InMemoryTestFixture era)
loadInMemoryTestFixture dir protocolVersion StateTransition {..} = do
  imtfInitialState <- liftIO $ importCanonicalState @era (dir </> initialState) epochNo
  imtfTransactions <-
    mapTxOrBlockM
      ( \txFile ->
          decodeTx (dir </> txFile)
      )
      ( \(blockIssuerFile, txFiles) -> do
          blockIssuerBytes <- liftIO $ BSL.readFile (dir </> blockIssuerFile)
          blockIssuer <- except $ decodeFull protocolVersion blockIssuerBytes
          t <- forM txFiles (decodeTx . (dir </>))
          pure (blockIssuer, SSeq.fromList t)
      )
      transactions
  imtfFinalState <- loadStateOrFailures
  pure $
    InMemoryTestFixture
      { imtfEpochNo = epochNo
      , imtfInitialState
      , imtfTransactions
      , imtfFinalState
      }
  where
    decodeTx filepath =
      except . decodeFullAnnotator protocolVersion (T.pack "Tx") decCBOR
        =<< liftIO (BSL.readFile filepath)

    loadStateOrFailures ::
      ExceptT DecoderError IO (Either (TxOrBlock (TxFailures era) (BlockFailures era)) FilePath)
    loadStateOrFailures =
      bimapM
        ( \failuresFile -> ExceptT $ do
            bs <- BSL.readFile (dir </> failuresFile)
            pure $
              case decodeFullDecoder protocolVersion "TxFailures" (decodeTxFailures @era) bs of
                Left _ -> OrBlock <$> decodeFullDecoder protocolVersion "BlockFailures" (decodeBlockFailures @era) bs
                Right txFailures -> Right (OrTx txFailures)
        )
        pure
        finalState

importNamespaceFromHandle ::
  forall era v.
  (KnownSymbol v, ImportCanonicalNamespace era v) =>
  Handle -> Proxy v -> ExportLedgerState era -> IO (ExportLedgerState era)
importNamespaceFromHandle h (p :: Proxy v) nes =
  withNamespacedDataHandle h (fromSymbol p) $ \s ->
    importNamespace @era @v
      nes
      (S.mapMaybe (decodeChunkEntry p) s)
