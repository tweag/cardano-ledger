{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.CanonicalState.Arbitrary () where

import Cardano.Ledger.CanonicalState.BasicTypes (
  CanonicalExUnits (..),
  mkCanonicalExUnits,
 )
import Cardano.Ledger.CanonicalState.Conway (
  CanonicalGovActionState,
  fromGovActionState,
  mkCanonicalConstitution,
 )
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0 as EntitiesAccounts.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesCommittee.V0 as EntitiesCommittee.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesDReps.V0 as EntitiesDReps.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.EntitiesStakePools.V0 as EntitiesStakePools.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovCommittee.V0 as GovCommittee.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovConstitution.V0 as GovConstitution.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovPParams.V0 as GovPParams.V0
import qualified Cardano.Ledger.CanonicalState.Namespace.GovProposals.V0 as GovProposals.V0
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (Constitution, GovActionState)
import Generic.Random (genericArbitraryU)
import Test.Cardano.Ledger.CanonicalState.Arbitrary ()
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.QuickCheck (Arbitrary (..))

instance Arbitrary GovConstitution.V0.CanonicalConstitution where
  arbitrary = mkCanonicalConstitution <$> arbitrary @(Constitution ConwayEra)

instance Arbitrary GovConstitution.V0.GovConstitutionOut where
  arbitrary = genericArbitraryU

instance Arbitrary EntitiesCommittee.V0.EntitiesCommitteeOut where
  arbitrary = genericArbitraryU

instance Arbitrary EntitiesCommittee.V0.CanonicalCommitteeState where arbitrary = genericArbitraryU

instance Arbitrary EntitiesCommittee.V0.CanonicalCommitteeAuthorization where
  arbitrary = fmap EntitiesCommittee.V0.mkCanonicalCommitteeAuthorization arbitrary

instance Arbitrary GovCommittee.V0.GovCommitteeOut where
  arbitrary = genericArbitraryU

instance Arbitrary GovCommittee.V0.CanonicalCommittee where arbitrary = genericArbitraryU

instance Arbitrary (GovPParams.V0.GovPParamsOut ConwayEra) where
  arbitrary = genericArbitraryU

instance Arbitrary CanonicalExUnits where
  arbitrary = mkCanonicalExUnits <$> arbitrary

instance Arbitrary (GovProposals.V0.GovProposalOut CanonicalGovActionState) where
  arbitrary = snd . fromGovActionState <$> arbitrary @(GovActionState ConwayEra)

instance Arbitrary (EntitiesAccounts.V0.EntitiesAccountsOut ConwayEra) where
  arbitrary = genericArbitraryU

instance Arbitrary EntitiesDReps.V0.CanonicalDRepState where
  arbitrary = genericArbitraryU

instance Arbitrary EntitiesStakePools.V0.CanonicalStakePoolState where
  arbitrary = genericArbitraryU

instance Arbitrary EntitiesStakePools.V0.CanonicalStakePool where
  arbitrary = genericArbitraryU

instance Arbitrary EntitiesStakePools.V0.EntitiesStakePoolsOut where
  arbitrary = genericArbitraryU
