{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.CanonicalState.Import where

import Cardano.Ledger.BaseTypes (
  EpochNo,
  SlotNo,
 )
import Cardano.Ledger.Binary (
  Decoder,
 )
import Cardano.Ledger.CanonicalState.Export (
  BlockFailures,
  ExportCanonicalState (ExportLedgerState),
  TxFailures,
 )
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, decodeChunkEntry)
import Cardano.SCLS.Internal.Reader (withNamespacedDataHandle)
import Cardano.SCLS.NamespaceCodec (KnownNamespace (NamespaceEntry, NamespaceKey))
import Cardano.Types.Namespace (fromSymbol)
import Data.Proxy (Proxy)
import GHC.IO.Handle (Handle)
import GHC.TypeLits (KnownSymbol)
import Streaming.Prelude (Of, Stream)
import qualified Streaming.Prelude as S

class KnownNamespace ns => ImportCanonicalNamespace era ns where
  importNamespace ::
    MonadFail m =>
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

importNamespaceFromHandle ::
  forall era v.
  (KnownSymbol v, ImportCanonicalNamespace era v) =>
  Handle -> Proxy v -> ExportLedgerState era -> IO (ExportLedgerState era)
importNamespaceFromHandle h (p :: Proxy v) nes =
  withNamespacedDataHandle h (fromSymbol p) $ \s ->
    importNamespace @era @v
      nes
      (S.mapM (decodeChunkEntry p) s)
