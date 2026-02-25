{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.CanonicalState.Dump (
  dump,
  dumpTx,
  withScls,
) where

import Cardano.Ledger.BaseTypes (ProtVer (ProtVer), Version)
import Cardano.Ledger.Binary (
  EncCBOR (encCBOR),
  toLazyByteString,
  toPlainEncoding,
 )
import Cardano.Ledger.CanonicalState.Conway ()
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 (UtxoIn (UtxoKeyIn), mkUtxo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.State (CanGetUTxO (utxoG), UTxO (unUTxO))
import Cardano.Ledger.Core (EraPParams (ppProtocolVersionL), Tx, TxLevel (TopTx))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.SCLS.CDDL (knownNamespaceKeySizes)
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry), SomeChunkEntry)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  SerializationPlan,
  addNamespacedChunks,
  defaultSerializationPlan,
 )
import Cardano.SCLS.Internal.Serializer.External.Impl (serialize)
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as BSL
import Data.Data (Proxy (Proxy))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.MemPack.Extra (RawBytes)
import qualified Data.Text as T
import Lens.Micro ((&), (^.))
import qualified Streaming.Prelude as S
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Cardano.Ledger.Common (SpecWith)
import Test.Cardano.Ledger.Conway.ImpTest
import Text.Read (readMaybe)

-- TODO: move somewhere common to all eras?
dumpTx ::
  EncCBOR (Tx TopTx era) =>
  FilePath ->
  String ->
  Version ->
  Tx TopTx era ->
  IO ()
dumpTx dir prefix version tx = do
  createDirectoryIfMissing True dir
  let e = encCBOR tx
  filepath <- getNextFile dir prefix "cbor"
  BSL.writeFile filepath (toLazyByteString (toPlainEncoding version e))

addUtxo ::
  (Monad m, CanGetUTxO t, era ~ ConwayEra) =>
  t era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addUtxo t plan =
  let utxos =
        S.each (Map.toList $ unUTxO $ t ^. utxoG)
          & S.map (\(txIn, txOut) -> ChunkEntry (UtxoKeyIn txIn) (mkUtxo txOut))
      p = Proxy :: Proxy "utxo/v0"
   in addNamespacedChunks p utxos plan

getNextFile :: FilePath -> String -> String -> IO FilePath
getNextFile dir prefix extension = do
  -- dir: "/path/to"
  -- prefix: "dump"
  let prefixT = T.pack prefix
  basenames <-
    map takeBaseName . filter (T.isPrefixOf prefixT . T.pack) <$> listDirectory dir
  -- basenames: ["dump-1", "dump-2", "dump-3", ...]
  -- Extract the numeric suffixes and find the maximum
  let counters = mapMaybe (T.stripPrefix (prefixT <> T.pack "-") . T.pack) basenames
      -- counters: ["1", "2", "3", ...]
      maxCounter = maximum ((0 :: Int) : mapMaybe (readMaybe . T.unpack) counters)
  -- maxCounter: 3 (if the existing files are dump-1.scls, dump-2.scls, dump-3.scls)
  pure (dir </> (T.unpack prefixT <> "-" <> show (maxCounter + 1) <> "." <> extension))

dump :: (CanGetUTxO t, era ~ ConwayEra) => FilePath -> String -> t era -> IO ()
dump dumpDir prefix t = do
  createDirectoryIfMissing True dumpDir
  let plan = defaultSerializationPlan & addUtxo t
  filepath <- getNextFile dumpDir prefix "scls"
  _ <-
    runResourceT $
      serialize
        filepath
        (SlotNo 1)
        knownNamespaceKeySizes
        plan
  pure ()

withScls ::
  FilePath -> SpecWith (ImpInit (LedgerSpec ConwayEra)) -> SpecWith (ImpInit (LedgerSpec ConwayEra))
withScls dir =
  modifyImpInitSclsDumpHook
    ( \nes tx res -> liftIO $ do
        dump dir "initial" nes
        let ProtVer version _ = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
        dumpTx dir "txn" version tx
        case res of
          Left _failures -> do
            -- TODO: dump the failures
            pure ()
          Right (st, _) -> do
            dump dir "final" st
    )
