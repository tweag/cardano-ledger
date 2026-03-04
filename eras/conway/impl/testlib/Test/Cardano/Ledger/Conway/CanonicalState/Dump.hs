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
import Cardano.Ledger.CanonicalState.Conway (mkCanonicalConstitution)
import Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 (BlockIn (BlockIn), BlockOut (BlockOut))
import Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 (
  CanonicalCommitteeState (CanonicalCommitteeState),
  GovCommitteeIn (GovCommitteeIn),
  GovCommitteeOut (GovCommitteeOut),
  mkCanonicalCommitteeAuthorization,
 )
import Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 (
  GovConstitutionIn (GovConstitutionIn),
  GovConstitutionOut (GovConstitutionOut),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 (
  GovPParamsIn (..),
  GovPParamsOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 (UtxoIn (UtxoKeyIn), mkUtxo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (constitutionGovStateL),
 )
import Cardano.Ledger.Conway.State (
  CanGetUTxO (utxoG),
  ConwayEraCertState (certVStateL),
  FuturePParams (..),
  UTxO (unUTxO),
  csCommitteeCredsL,
  vsCommitteeStateL,
 )
import Cardano.Ledger.Core (EraPParams (ppProtocolVersionL), Tx, TxLevel (TopTx))
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState,
  NewEpochState,
  curPParamsEpochStateL,
  esLStateL,
  futurePParamsEpochStateL,
  lsCertStateL,
  nesBcurL,
  nesELL,
  nesEpochStateL,
  nesEsL,
  newEpochStateGovStateL,
  prevPParamsEpochStateL,
 )
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
import Control.Monad.Trans.Resource (ResIO, runResourceT)
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
  (Monad m, era ~ ConwayEra) =>
  LedgerState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addUtxo t =
  addNamespacedChunks (Proxy :: Proxy "utxo/v0") utxos
  where
    utxos =
      S.each (Map.toList $ unUTxO $ t ^. utxoG)
        & S.map (\(txIn, txOut) -> ChunkEntry (UtxoKeyIn txIn) (mkUtxo txOut))

addBlocks ::
  Monad m =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addBlocks nes =
  addNamespacedChunks (Proxy :: Proxy "blocks/v0") blocks
  where
    epochNo = nes ^. nesELL
    blocks =
      S.each (Map.toList $ nes ^. nesBcurL)
        & S.map (\(keyHash, n) -> ChunkEntry (BlockIn keyHash epochNo) (BlockOut n))

addGovCommittee ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addGovCommittee nes =
  addNamespacedChunks
    (Proxy :: Proxy "gov/committee/v0")
    (S.yield (ChunkEntry (GovCommitteeIn epochNo) (GovCommitteeOut committeeState)))
  where
    epochNo = nes ^. nesELL
    committeeState =
      CanonicalCommitteeState $
        Map.map mkCanonicalCommitteeAuthorization $
          nes
            ^. nesEpochStateL
              . esLStateL
              . lsCertStateL
              . certVStateL
              . vsCommitteeStateL
              . csCommitteeCredsL

addGovConstitution ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addGovConstitution nes =
  addNamespacedChunks
    (Proxy :: Proxy "gov/constitution/v0")
    (S.yield (ChunkEntry (GovConstitutionIn epochNo) (GovConstitutionOut canonicalConstitution)))
  where
    constitution = nes ^. newEpochStateGovStateL . constitutionGovStateL
    epochNo = nes ^. nesELL
    canonicalConstitution = mkCanonicalConstitution constitution

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

addPParams ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addPParams nes =
  addNamespacedChunks (Proxy :: Proxy "gov/pparams/v0") (S.each pparams)
  where
    epochState = nes ^. nesEsL
    currPParams = epochState ^. curPParamsEpochStateL
    prevPParams = epochState ^. prevPParamsEpochStateL
    (futurePossiblePParams, futureDefinitePParams) = case epochState ^. futurePParamsEpochStateL of
      NoPParamsUpdate -> ([], [])
      DefinitePParamsUpdate p -> ([], [ChunkEntry GovPParamsInDefiniteFuture (GovPParamsOut p)])
      PotentialPParamsUpdate (Just p) -> ([ChunkEntry GovPParamsInPossibleFuture (GovPParamsOut p)], [])
      PotentialPParamsUpdate Nothing -> ([], [])
    pparams =
      [ ChunkEntry GovPParamsInPrev (GovPParamsOut prevPParams)
      , ChunkEntry GovPParamsInCurr (GovPParamsOut currPParams)
      ]
        ++ futurePossiblePParams
        ++ futureDefinitePParams

dump ::
  FilePath ->
  String ->
  SerializationPlan (SomeChunkEntry RawBytes) ResIO ->
  IO ()
dump dumpDir prefix plan = do
  createDirectoryIfMissing True dumpDir
  filepath <- getNextFile dumpDir prefix "scls"
  _ <-
    runResourceT $
      serialize
        filepath
        (SlotNo 1)
        knownNamespaceKeySizes
        plan
  pure ()

dumpLedgerState ::
  (Monad m, era ~ ConwayEra) =>
  LedgerState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m
dumpLedgerState ls = do
  defaultSerializationPlan
    & addUtxo ls

dumpNewEpochState ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m
dumpNewEpochState nes = do
  defaultSerializationPlan
    & addUtxo (nes ^. nesEsL . esLStateL)
    & addBlocks nes
    & addGovCommittee nes
    & addGovConstitution nes
    & addPParams nes

withScls ::
  FilePath -> SpecWith (ImpInit (LedgerSpec ConwayEra)) -> SpecWith (ImpInit (LedgerSpec ConwayEra))
withScls dir =
  modifyImpInitSclsDumpHook
    ( \nes tx res -> liftIO $ do
        dump dir "initial" $ dumpNewEpochState nes
        let ProtVer version _ = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
        dumpTx dir "txn" version tx
        case res of
          Left _failures -> do
            -- TODO: dump the failures
            pure ()
          Right (st, _) -> do
            dump dir "final" $ dumpLedgerState st -- TODO: this should be dumpNewEpochState, but we don't have the final NewEpochState available here.
    )
