{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Cardano.Ledger.SCLS.Spec (spec) where

import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.SCLS.Arbitrary ()
import Cardano.Ledger.SCLS.BaseTypes
import Cardano.Ledger.SCLS.Common as Common
import qualified Cardano.Ledger.SCLS.Namespace.GovCommittee.V0 as Committee.V0
import qualified Cardano.Ledger.SCLS.Namespace.GovConstitution.V0 as Constitution.V0
import qualified Cardano.Ledger.SCLS.Namespace.GovPParams.V0 as PParams.V0
import qualified Cardano.Ledger.SCLS.Namespace.GovProposals.V0 as Proposals.V0
import qualified Cardano.Ledger.SCLS.Namespace.Nonces.V0 as Nonces.V0
import qualified Cardano.Ledger.SCLS.Namespace.PoolStake.V0 as PoolStake.V0
import qualified Cardano.Ledger.SCLS.Namespace.Pots.V0 as Pots.V0
import qualified Cardano.Ledger.SCLS.Namespace.Snapshots.V0 as Snapshots.V0
import qualified Cardano.Ledger.SCLS.Namespace.UTxO.V0 as UTxO.V0
import qualified Cardano.Ledger.Hashes as H
import Cardano.SCLS.Testlib
import Data.Typeable
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import GHC.TypeLits
import Test.Cardano.Ledger.Common
import Debug.Trace (traceM)

spec :: SpecWith ()
spec = do
    describe "BaseTypes" $ do
        isCanonical @"base" @Anchor
        validateType @"gov/committee/v0" @Anchor "anchor"
        isCanonical @"base" @EpochNo
        validateType @"gov/proposals/v0" @EpochNo "epoch_no"
        isCanonical @"base" @EpochInterval
        validateType @"gov/proposals/v0" @EpochInterval "epoch_interval"
        isCanonical @"base" @NonNegativeInterval
        validateType @"gov/proposals/v0" @NonNegativeInterval "nonnegative_interval"
        isCanonical @"base" @Port
        validateType @"snapshots/v0" @Port "port"
        isCanonical @"base" @ProtVer
        validateType @"gov/pparams/v0" @ProtVer "protocol_version"
        isCanonical @"base" @SlotNo
        validateType @"nonces/v0" @SlotNo "slot_no"
        isCanonical @"base" @UnitInterval
        validateType @"gov/pparams/v0" @UnitInterval "unit_interval"
        isCanonical @"base" @Url
        validateType @"snapshots/v0" @Url "url"
        isCanonical @"base" @DnsName
        validateType @"snapshots/v0" @DnsName "dns_name"
    describe "Common Types" $ do
        isCanonical @"utxo/v0" @Common.CanonicalCoin
        validateType @"gov/proposals/v0" @Common.CanonicalCoin "coin"
        isCanonical @"gov/proposals/v0" @(Common.CanonicalCredential H.Guard)
        validateType @"gov/proposals/v0" @(Common.CanonicalCredential H.Guard) "credential"
        isCanonical @"utxo/v0" @Common.CanonicalRewardAccount
        validateType @"snapshots/v0" @Common.CanonicalRewardAccount "reward_account"
        isCanonical @"snapshots/v0" @(Common.CanonicalVRFVerKeyHash H.StakePoolVRF)
        validateType @"snapshots/v0" @(Common.CanonicalVRFVerKeyHash H.StakePoolVRF) "vrf_keyhash"
        isCanonical @"gov/constitution/v0" @Common.ScriptHash
        validateType @"gov/constitution/v0" @(Common.ScriptHash) "script_hash"
    describe "gov/committee/v0" $ do
        isCanonical @"gov/committee/v0" @Committee.V0.CanonicalCommitteeState
        validateType @"gov/committee/v0" @Committee.V0.CanonicalCommitteeState "committee"
        isCanonical @"gov/committee/v0" @Committee.V0.CanonicalCommitteeAuthorization
        validateType @"gov/committee/v0" @Committee.V0.CanonicalCommitteeAuthorization "committee_authorization"
    describe "gov/constitution/v0" $ do
        isCanonical @"gov/constitution/v0" @Constitution.V0.CanonicalConstitution
        validateType @"gov/constitution/v0" @Constitution.V0.CanonicalConstitution "constitution"
    describe "gov/pparams/v0" $ do
        isCanonical @"gov/pparams/v0" @PParams.V0.CanonicalCostModels
        validateType @"gov/pparams/v0" @PParams.V0.CanonicalCostModels "cost_models"
        isCanonical @"gov/pparams/v0" @PParams.V0.CanonicalPrices
        validateType @"gov/pparams/v0" @PParams.V0.CanonicalPrices "ex_unit_prices"
        isCanonical @"gov/pparams/v0" @PParams.V0.CanonicalDRepVotingThresholds
        validateType @"gov/pparams/v0" @PParams.V0.CanonicalDRepVotingThresholds "drep_voting_thresholds"
        isCanonical @"gov/pparams/v0" @PParams.V0.CanonicalPoolVotingThresholds
        validateType @"gov/pparams/v0" @PParams.V0.CanonicalPoolVotingThresholds "pool_voting_thresholds"
        isCanonical @"gov/pparams/v0" @PParams.V0.CanonicalExUnits
        validateType @"gov/pparams/v0" @PParams.V0.CanonicalExUnits "ex_units"
        isCanonical @"gov/pparams/v0" @PParams.V0.CanonicalPParams
        validateType @"gov/pparams/v0" @PParams.V0.CanonicalPParams "gov_pparams_out"
    describe "gov/proposals/v0" $ do
        isCanonical @"gov/proposals/v0" @Proposals.V0.CanonicalGovActionState
        validateType @"gov/proposals/v0" @Proposals.V0.CanonicalGovActionState "proposal"
        isCanonical @"gov/proposals/v0" @Proposals.V0.CanonicalGovAction
        validateType @"gov/proposals/v0" @Proposals.V0.CanonicalGovAction "gov_action"
        isCanonical @"gov/proposals/v0" @Proposals.V0.CanonicalProposalProcedure
        validateType @"gov/proposals/v0" @Proposals.V0.CanonicalProposalProcedure "proposal_procedure"
        isCanonical @"gov/proposals/v0" @Proposals.V0.CanonicalPParamsUpdate
        validateType @"gov/proposals/v0" @Proposals.V0.CanonicalPParamsUpdate "gov_params_update"
        isCanonical @"gov/proposals/v0" @Proposals.V0.CanonicalPurposeId
        validateType @"gov/proposals/v0" @Proposals.V0.CanonicalPurposeId "gov_action_id"
        isCanonical @"common" @(H.KeyHash H.Guard)
        validateType @"gov/proposals/v0" @(H.KeyHash H.Guard) "addr_keyhash"
    describe "nonces/v0" $ do
        isCanonical @"nonces/v0" @Nonces.V0.NoncesState
        validateType @"nonces/v0" @Nonces.V0.NoncesState "nonces"
        isCanonical @"nonces/v0" @Nonces.V0.CanonicalNonce
        isCanonical @"nonces/v0" @(Nonces.V0.CanonicalWithOrigin SlotNo)
        validateType @"nonces/v0" @Nonces.V0.CanonicalNonce "nonce"
        validateType @"nonces/v0" @(H.Hash H.HASH Nonce) "hash32"
    describe "pool_stake/v0" $ do
        isCanonical @"pool_stake/v0" @PoolStake.V0.PoolStakeOut
        validateType @"pool_stake/v0" @PoolStake.V0.PoolStakeOut "individual_pool_stake"
    describe "pots/v0" $ do
        isCanonical @"pots/v0" @Pots.V0.PotsOut
        validateType @"pots/v0" @Pots.V0.PotsOut "pots_table"
    describe "snapshots/v0" $ do
        isCanonical @"snapshots/v0" @Snapshots.V0.CanonicalStakePoolParams
        validateType @"snapshots/v0" @Snapshots.V0.CanonicalStakePoolParams "pool_params"
        isCanonical @"snapshots/v0" @Snapshots.V0.CanonicalPoolMetadata
        validateType @"snapshots/v0" @Snapshots.V0.CanonicalPoolMetadata "pool_metadata"
        isCanonical @"snapshots/v0" @Snapshots.V0.CanonicalStakePoolRelay
        validateType @"snapshots/v0" @Snapshots.V0.CanonicalStakePoolRelay "relay"
    describe "utxo/v0" $ do
        -- TODO: requires better instance
        -- isCanonical @"utxo/v0" @UTxO.V0.CanonicalScript
        -- validateType @"utxo/v0" @UTxO.V0.CanonicalScript "script"
        -- isCanonical @"utxo/v0" @UTxO.V0.CanonicalPlutusScript
        -- validateType @"utxo/v0" @UTxO.V0.CanonicalPlutusScript "plutus_script"
        -- isCanonical @"utxo/v0" @UTxO.V0.CanonicalNativeScript
        -- validateType @"utxo/v0" @UTxO.V0.CanonicalNativeScript "native_script"
        -- isCanonical @"utxo/v0" @UTxO.V0.CanonicalBabbageTxOut
        -- validateType @"utxo/v0" @UTxO.V0.CanonicalBabbageTxOut "babbage_tx_out"
        isCanonical @"utxo/v0" @UTxO.V0.CanonicalShelleyTxOut
        validateType @"utxo/v0" @UTxO.V0.CanonicalShelleyTxOut "shelley_tx_out"
        isCanonical @"utxo/v0" @UTxO.V0.CanonicalValue
        validateType @"utxo/v0" @UTxO.V0.CanonicalValue "value"
        validateType @"utxo/v0" @UTxO.V0.CompactAddr "address"
    describe "1123" $ do
        traceM $ B.unpack $ debugEncodeType @"nonces/v0"
           $ Nonces.V0.NoncesState
               { Nonces.V0.noncesStateLastSlot = Nonces.V0.CanonicalOrigin
               , Nonces.V0.noncesStateOCertCounters = Map.fromList [(H.KeyHash "fb800826cf81815d3f8af8dda9805e315a150dab078e5008135342da",0)]
               , Nonces.V0.noncesStateEvolvingNonce = Nonces.V0.CanonicalNonce "c98b15d6f695bb83e055a073871281093fff3d968ae075ef5a9a8813cb00c4fd"
               , Nonces.V0.noncesStateCandidateNonce = Nonces.V0.CanonicalNonce "1343c4817c910f428b0288c893adac649ef58f739ac0194853e5116a60af0fb4"
               , Nonces.V0.noncesStateEpochNonce = Nonces.V0.CanonicalNonce "6a7a345cd73b569518f4dacc7e10b0b4c1fac7edb78a57de71927fa5d5dbf735"
               , Nonces.V0.noncesStateLabNonce = Nonces.V0.CanonicalNeutralNonce
               , Nonces.V0.noncesStateLastEpochBlockNonce = Nonces.V0.CanonicalNonce "623d11cc57a846678d18534c8d3cc3166f2f46766b2e614e5a1dad91199f9717"
               }
        traceM $ B.unpack $ debugEncodeType @"nonces/v0"
            $ Nonces.V0.NoncesState
               { Nonces.V0.noncesStateLastSlot = Nonces.V0.CanonicalOrigin
               , Nonces.V0.noncesStateOCertCounters = Map.fromList []
               , Nonces.V0.noncesStateEvolvingNonce = Nonces.V0.CanonicalNonce "1601b5c449aa661eb685519e888a4796c94f240de90421dbf02cdb2cd3b920f6"
               , Nonces.V0.noncesStateCandidateNonce = Nonces.V0.CanonicalNonce "dad6c7af14b6faa0db2949122f15fc76f81d50da67fea33e833c747f51bb9839"
               , Nonces.V0.noncesStateEpochNonce = Nonces.V0.CanonicalNeutralNonce
               , Nonces.V0.noncesStateLabNonce = Nonces.V0.CanonicalNonce "00cc0b0c73c8eb958d4b3617376fcdd3309e8693f3bb1c5af4f8b6fed9b46b10"
               , Nonces.V0.noncesStateLastEpochBlockNonce = Nonces.V0.CanonicalNeutralNonce
               }
        traceM $ show $ debugValidateType @"nonces/v0" "slot_no" $ SlotNo 12345678
        traceM $ show $ debugValidateType @"nonces/v0" "nonces"
            $ Nonces.V0.NoncesState
               { Nonces.V0.noncesStateLastSlot = Nonces.V0.CanonicalAt (SlotNo 123456789)
               , Nonces.V0.noncesStateOCertCounters = Map.fromList [(H.KeyHash "fb800826cf81815d3f8af8dda9805e315a150dab078e5008135342da",0)]
               , Nonces.V0.noncesStateEvolvingNonce = Nonces.V0.CanonicalNonce "c98b15d6f695bb83e055a073871281093fff3d968ae075ef5a9a8813cb00c4fd"
               , Nonces.V0.noncesStateCandidateNonce = Nonces.V0.CanonicalNonce "1343c4817c910f428b0288c893adac649ef58f739ac0194853e5116a60af0fb4"
               , Nonces.V0.noncesStateEpochNonce = Nonces.V0.CanonicalNonce "6a7a345cd73b569518f4dacc7e10b0b4c1fac7edb78a57de71927fa5d5dbf735"
               , Nonces.V0.noncesStateLabNonce = Nonces.V0.CanonicalNeutralNonce
               , Nonces.V0.noncesStateLastEpochBlockNonce = Nonces.V0.CanonicalNonce "623d11cc57a846678d18534c8d3cc3166f2f46766b2e614e5a1dad91199f9717"
               }
        traceM $ show $ debugValidateType @"nonces/v0" "nonces"
            $ Nonces.V0.NoncesState
               { Nonces.V0.noncesStateLastSlot = Nonces.V0.CanonicalAt (SlotNo 0)
               , Nonces.V0.noncesStateOCertCounters = Map.fromList [] -- (H.KeyHash "fb800826cf81815d3f8af8dda9805e315a150dab078e5008135342da",0)]
               , Nonces.V0.noncesStateEvolvingNonce = Nonces.V0.CanonicalNeutralNonce
               , Nonces.V0.noncesStateCandidateNonce = Nonces.V0.CanonicalNeutralNonce
               , Nonces.V0.noncesStateEpochNonce = Nonces.V0.CanonicalNeutralNonce
               , Nonces.V0.noncesStateLabNonce = Nonces.V0.CanonicalNeutralNonce
               , Nonces.V0.noncesStateLastEpochBlockNonce = Nonces.V0.CanonicalNeutralNonce
               }

isCanonical ::
  forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a, Typeable a, Arbitrary a, Show a) => Spec
isCanonical = go
  where
    typ = showsTypeRep (typeRep (Proxy @a))
    go = prop (typ " is canonical") $ propTypeIsCanonical @ns @a