{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wwarn #-}
module Test.Cardano.Ledger.Conway.Scls
  ( spec
  ) where

import Cardano.Ledger.Conway.SCLS
import Cardano.Ledger.Conway.SCLS.Arbitrary ()
import Cardano.SCLS.Testlib
import Test.Cardano.Ledger.Common
import Cardano.Ledger.State (StakePoolParams (..), PoolMetadata (..)) -- , StakePoolRelay (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance -- (ProposalProcedure(..), GovAction(..), GovPurposeId(..), GovActionPurpose(..), Constitution(..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.BaseTypes (UnitInterval, Anchor, ProtVer(..), EpochInterval, NonNegativeInterval{-, StrictMaybe(..)-})
import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.Credential
import Cardano.Ledger.Conway.PParams
-- import Data.Set qualified as Set
-- import Data.Map qualified as Map
-- import Cardano.Ledger.Hashes
import Cardano.Ledger.Core
import Cardano.Ledger.Plutus.CostModels
import Cardano.Ledger.Alonzo.PParams


spec :: Spec
spec = do
  describe "scls" testAllNS
  describe "scls0" $ do
    validateType @"snapshots/v0" @StakePoolParams "pool_params"
    validateType @"snapshots/v0" @(KeyHash StakePool) "pool_keyhash"
    validateType @"snapshots/v0" @(VRFVerKeyHash StakePoolVRF) "vrf_keyhash"
    validateType @"snapshots/v0" @(Coin) "coin"
    validateType @"snapshots/v0" @(UnitInterval) "unit_interval"
    validateType @"snapshots/v0" @(RewardAccount) "reward_account"
    validateType @"snapshots/v0" @(PoolMetadata) "pool_metadata"
    validateType @"gov/proposals/v0" @GovProposalOut "proposal"
    validateType @"gov/proposals/v0" @(ProposalProcedure ConwayEra)  "proposal_procedure"
    validateType @"gov/proposals/v0" @(RewardAccount) "reward_account"
    validateType @"gov/proposals/v0" @(Coin) "coin"
    validateType @"gov/proposals/v0" @(Constitution ConwayEra) "constitution"
    validateType @"gov/proposals/v0" @(Anchor) "anchor"
    validateType @"gov/proposals/v0" @(Credential ColdCommitteeRole) "credential"
    validateType @"gov/proposals/v0" @(ProtVer) "protocol_version"
    validateType @"gov/proposals/v0" @(GovPurposeId 'HardForkPurpose) "gov_action_id"
    validateType @"gov/proposals/v0" @(GovPurposeId 'CommitteePurpose) "gov_action_id"
    validateType @"gov/proposals/v0" @(NonNegativeInterval) "nonnegative_interval"
    validateType @"gov/proposals/v0" @(EpochInterval) "epoch_interval"
    validateType @"gov/proposals/v0" @(CostModels) "cost_models"
    validateType @"gov/proposals/v0" @(OrdExUnits) "ex_units"
    validateType @"gov/proposals/v0" @(DRepVotingThresholds) "drep_voting_thresholds"
    validateType @"gov/proposals/v0" @(PoolVotingThresholds) "pool_voting_thresholds"

    validateType @"gov/proposals/v0" @(PParamsUpdate ConwayEra) "gov_params_update"

    validateType @"gov/proposals/v0" @(GovAction ConwayEra) "gov_action"
    validateType @"gov/proposals/v0" @(PParamsUpdate ConwayEra) "gov_params_update"

