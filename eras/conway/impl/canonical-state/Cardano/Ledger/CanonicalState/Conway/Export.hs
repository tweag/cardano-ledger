{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Conway.Export () where

import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.CanonicalState.Conway (
  fromGovActionState,
  mkCanonicalConstitution,
 )
import Cardano.Ledger.CanonicalState.Export (ExportState (..))
import Cardano.Ledger.CanonicalState.Namespace.Blocks.V0 (BlockIn (BlockIn), BlockOut (BlockOut))
import Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0 (
  EntitiesAccountsIn (EntitiesAccountsIn),
  EntitiesAccountsOut (EntitiesAccountsOut),
 )
import Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0 (
  CanonicalCommitteeState (..),
  EntitiesCommitteeIn (..),
  EntitiesCommitteeOut (..),
  mkCanonicalCommitteeAuthorization,
 )
import Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 (
  CanonicalCommittee (..),
  GovCommitteeIn (..),
  GovCommitteeOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 (
  GovConstitutionIn (..),
  GovConstitutionOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 (
  GovPParamsIn (..),
  GovPParamsOut (..),
 )
import Cardano.Ledger.CanonicalState.Namespace.UTxO.V0 (UtxoIn (UtxoKeyIn), mkUtxo)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  ConwayEraGov (constitutionGovStateL, drepPulsingStateGovStateL),
  DRepPulser (DRepPulser, dpProposals),
  DRepPulsingState (DRComplete, DRPulsing),
  cgsCommitteeL,
  psProposalsL,
 )
import Cardano.Ledger.Conway.State (
  CanGetUTxO (utxoG),
  CanSetAccounts (accountsL),
  ConwayEraCertState (certVStateL),
  EraAccounts (accountsMapL),
  EraCertState (certDStateL),
  FuturePParams (..),
  UTxO (unUTxO),
  csCommitteeCredsL,
  vsCommitteeStateL,
 )
import Cardano.Ledger.Core (EraPParams (ppProtocolVersionL))
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
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry (ChunkEntry), SomeChunkEntry)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  SerializationPlan,
  addNamespacedChunks,
  defaultSerializationPlan,
 )
import Data.Data (Proxy (Proxy))
import Data.Foldable (Foldable (toList))
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

addEntitiesCommittee ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addEntitiesCommittee nes =
  addNamespacedChunks
    (Proxy :: Proxy "entities/committee/v0")
    (S.yield (ChunkEntry (EntitiesCommitteeIn epochNo) (EntitiesCommitteeOut committeeState)))
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

addGovCommittee ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addGovCommittee nes =
  addNamespacedChunks
    (Proxy :: Proxy "gov/committee/v0")
    (S.yield (ChunkEntry (GovCommitteeIn epochNo) (GovCommitteeOut committee)))
  where
    epochNo = nes ^. nesELL
    committee =
      fmap
        (\Committee {..} -> CanonicalCommittee {committeeMembers, committeeThreshold})
        $ nes
          ^. newEpochStateGovStateL
          . cgsCommitteeL

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

addProposals ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addProposals nes =
  addNamespacedChunks (Proxy :: Proxy "gov/proposals/v0") proposals
  where
    proposals =
      S.each
        [ uncurry ChunkEntry $ fromGovActionState g
        | g <-
            toList
              ( case nes ^. newEpochStateGovStateL . drepPulsingStateGovStateL of
                  DRComplete snap _rs -> snap ^. psProposalsL
                  DRPulsing DRepPulser {..} -> dpProposals
              )
        ]

addAccounts ::
  (Monad m, era ~ ConwayEra) =>
  NewEpochState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addAccounts nes =
  addNamespacedChunks (Proxy :: Proxy "entities/accounts/v0") accounts
  where
    accounts =
      S.map
        (\(cred, accountState) -> ChunkEntry (EntitiesAccountsIn cred) (EntitiesAccountsOut accountState))
        $ S.each
        $ Map.toList
        $ nes
          ^. nesEsL
          . esLStateL
          . lsCertStateL
          . certDStateL
          . accountsL
          . accountsMapL

instance ExportState ConwayEra where
  type ExportLedgerState ConwayEra = LedgerState ConwayEra
  type ExportNewEpochState ConwayEra = NewEpochState ConwayEra
  dumpLedgerState ls =
    defaultSerializationPlan
      & addUtxo ls
  dumpNewEpochState nes =
    defaultSerializationPlan
      & addUtxo (nes ^. nesEsL . esLStateL)
      & addBlocks nes
      & addEntitiesCommittee nes
      & addGovCommittee nes
      & addGovConstitution nes
      & addPParams nes
      & addProposals nes
      & addAccounts nes
  getProtocolVersion nes =
    pvMajor $ nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
