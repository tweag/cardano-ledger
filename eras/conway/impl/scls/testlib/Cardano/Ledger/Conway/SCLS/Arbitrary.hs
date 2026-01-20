{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Arbitrary (

) where

import Test.Cardano.Ledger.SCLS.Arbitrary()
import Cardano.Ledger.SCLS.Namespace.Blocks.V0
import Cardano.Ledger.SCLS.Namespace.Snapshots.V0
import Cardano.Ledger.Conway.SCLS.Namespace.UTxO
import Generic.Random (genericArbitraryU)
import Test.QuickCheck.Arbitrary


deriving newtype instance Arbitrary BlockOut

instance Arbitrary SnapShotOut where arbitrary = genericArbitraryU

instance Arbitrary UtxoOut where arbitrary = genericArbitraryU