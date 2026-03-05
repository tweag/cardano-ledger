{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.CanonicalState.Conway.Dump (
  dump,
  addUtxo,
  addBlocks,
  addGovCommittee,
  addGovConstitution,
  addPParams,
  dumpLedgerState,
  dumpNewEpochState,
) where

import Cardano.Ledger.CanonicalState.Conway (mkCanonicalConstitution)
import Cardano.Ledger.CanonicalState.Dump (getNextFile)
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
import Control.Monad (void)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Data.Data (Proxy (Proxy))
import qualified Data.Map as Map
import Data.MemPack.Extra (RawBytes)
import Lens.Micro ((&), (^.))
import qualified Streaming.Prelude as S

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
  SerializationPlan (SomeChunkEntry RawBytes) ResIO ->
  IO ()
dump filepath = do
  -- TODO: should we ignore?
  void
    . runResourceT
    . serialize
      filepath
      (SlotNo 1)
      knownNamespaceKeySizes

dumpLedgerState ::
  (Monad m, era ~ ConwayEra) =>
  LedgerState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m
dumpLedgerState ls =
  defaultSerializationPlan
    & addUtxo ls

dumpNewEpochState ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m
dumpNewEpochState nes =
  defaultSerializationPlan
    & addUtxo (nes ^. nesEsL . esLStateL)
    & addBlocks nes
    & addGovCommittee nes
    & addGovConstitution nes
    & addPParams nes
