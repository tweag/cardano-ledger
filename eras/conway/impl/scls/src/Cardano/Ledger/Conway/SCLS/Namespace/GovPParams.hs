{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovPParams (
  GovPParamsIn (..),
  GovPParamsOut (..),
) where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits)
import Cardano.Ledger.Babbage.PParams (CoinPerByte)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  DRepVotingThresholds,
  PoolVotingThresholds,
  THKD (..),
 )
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Conway.SCLS.LedgerCBOR
import Cardano.Ledger.Core (PParams (..), PParamsUpdate (..))
import Cardano.Ledger.HKD
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.MemPack
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T

data GovPParamsIn
  = GovPParamsInPrev
  | GovPParamsInCurr
  | GovPParamsInPossibleFuture
  | GovPParamsInDefiniteFuture
  deriving (Eq, Ord, Show)

instance IsKey GovPParamsIn where
  keySize = namespaceKeySize @"gov/pparams/v0"
  packKeyM GovPParamsInPrev =
    packByteStringM "prev"
  packKeyM GovPParamsInCurr = packByteStringM "curr"
  packKeyM GovPParamsInPossibleFuture = packByteStringM "fut0"
  packKeyM GovPParamsInDefiniteFuture = packByteStringM "fut1"
  unpackKeyM = do
    tag :: ByteString <- unpackByteStringM 4
    case tag of
      _
        | tag == "prev" -> return GovPParamsInPrev
        | tag == "curr" -> return GovPParamsInCurr
        | tag == "fut0" -> return GovPParamsInPossibleFuture
        | tag == "fut1" -> return GovPParamsInDefiniteFuture
        | otherwise -> fail "Invalid GovPParamsIn tag"

deriving via (LedgerCBOR v (CostModels)) instance ToCanonicalCBOR v (CostModels)

deriving via (LedgerCBOR v (CostModels)) instance FromCanonicalCBOR v (CostModels)

deriving via (LedgerCBOR v (PoolVotingThresholds)) instance ToCanonicalCBOR v (PoolVotingThresholds)

deriving via
  (LedgerCBOR v (PoolVotingThresholds))
  instance
    FromCanonicalCBOR v (PoolVotingThresholds)

deriving via (LedgerCBOR v (DRepVotingThresholds)) instance ToCanonicalCBOR v (DRepVotingThresholds)

deriving via
  (LedgerCBOR v (DRepVotingThresholds))
  instance
    FromCanonicalCBOR v (DRepVotingThresholds)

deriving via (LedgerCBOR v (CoinPerByte)) instance ToCanonicalCBOR v (CoinPerByte)

deriving via (LedgerCBOR v (CoinPerByte)) instance FromCanonicalCBOR v (CoinPerByte)

deriving via (LedgerCBOR v (OrdExUnits)) instance ToCanonicalCBOR v (OrdExUnits)

deriving via (LedgerCBOR v (OrdExUnits)) instance FromCanonicalCBOR v (OrdExUnits)

newtype GovPParamsOut = GovPParamsOut (PParams ConwayEra)
  deriving (Eq, Show)

deriving newtype instance ToCanonicalCBOR v (GovPParamsOut)

deriving newtype instance FromCanonicalCBOR v (GovPParamsOut)

instance ToCanonicalCBOR v (PParams ConwayEra) where
  toCanonicalCBOR v (PParams ConwayPParams {..}) =
    encodeAsMap
      [ SomeEncodablePair v ("a0" :: Text) (unTHKD cppA0)
      , SomeEncodablePair v ("rho" :: Text) (unTHKD cppRho)
      , SomeEncodablePair v ("tau" :: Text) (unTHKD cppTau)
      , SomeEncodablePair v ("n_opt" :: Text) (unTHKD cppNOpt)
      , SomeEncodablePair v ("prices" :: Text) (unTHKD cppPrices)
      , SomeEncodablePair v ("epoch_max" :: Text) (unTHKD cppEMax)
      , SomeEncodablePair v ("min_fee_a" :: Text) (unTHKD cppMinFeeA)
      , SomeEncodablePair v ("min_fee_b" :: Text) (unTHKD cppMinFeeB)
      , SomeEncodablePair v ("cost_models" :: Text) (unTHKD cppCostModels)
      , SomeEncodablePair v ("key_deposit" :: Text) (unTHKD cppKeyDeposit)
      , SomeEncodablePair v ("max_tx_size" :: Text) (unTHKD cppMaxTxSize)
      , SomeEncodablePair v ("drep_deposit" :: Text) (unTHKD cppDRepDeposit)
      , SomeEncodablePair v ("max_val_size" :: Text) (unTHKD cppMaxValSize)
      , SomeEncodablePair v ("pool_deposit" :: Text) (unTHKD cppPoolDeposit)
      , SomeEncodablePair v ("drep_activity" :: Text) (unTHKD cppDRepActivity)
      , SomeEncodablePair v ("min_pool_cost" :: Text) (unTHKD cppMinPoolCost)
      , SomeEncodablePair v ("max_block_size" :: Text) (unTHKD cppMaxBBSize)
      , SomeEncodablePair v ("max_tx_ex_units" :: Text) (unTHKD cppMaxTxExUnits)
      , SomeEncodablePair v ("protocol_version" :: Text) (cppProtocolVersion)
      , SomeEncodablePair v ("coin_per_utxo_byte" :: Text) (unTHKD cppCoinsPerUTxOByte)
      , SomeEncodablePair v ("gov_action_deposit" :: Text) (unTHKD cppGovActionDeposit)
      , SomeEncodablePair v ("max_block_ex_units" :: Text) (unTHKD cppMaxBlockExUnits)
      , SomeEncodablePair v ("min_committee_size" :: Text) (unTHKD cppCommitteeMinSize)
      , SomeEncodablePair v ("gov_action_lifetime" :: Text) (unTHKD cppGovActionLifetime)
      , SomeEncodablePair v ("committee_term_limit" :: Text) (unTHKD cppCommitteeMaxTermLength)
      , SomeEncodablePair v ("collateral_percentage" :: Text) (unTHKD cppCollateralPercentage)
      , SomeEncodablePair v ("max_block_header_size" :: Text) (unTHKD cppMaxBHSize)
      , SomeEncodablePair v ("max_collateral_inputs" :: Text) (unTHKD cppMaxCollateralInputs)
      , SomeEncodablePair v ("drep_voting_thresholds" :: Text) (unTHKD cppDRepVotingThresholds)
      , SomeEncodablePair v ("pool_voting_thresholds" :: Text) (unTHKD cppPoolVotingThresholds)
      , SomeEncodablePair
          v
          ("min_fee_ref_script_cost_per_byte" :: Text)
          (unTHKD cppMinFeeRefScriptCostPerByte)
      ]

instance FromCanonicalCBOR v (PParams ConwayEra) where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 31
    Versioned (THKD -> cppA0) <- decodeField "a0"
    Versioned (THKD -> cppRho) <- decodeField "rho"
    Versioned (THKD -> cppTau) <- decodeField "tau"
    Versioned (THKD -> cppNOpt) <- decodeField "n_opt"
    Versioned (THKD -> cppPrices) <- decodeField "prices"
    Versioned (THKD -> cppEMax) <- decodeField "epoch_max"
    Versioned (THKD -> cppMinFeeA) <- decodeField "min_fee_a"
    Versioned (THKD -> cppMinFeeB) <- decodeField "min_fee_b"
    Versioned (THKD -> cppCostModels) <- decodeField "cost_models"
    Versioned (THKD -> cppKeyDeposit) <- decodeField "key_deposit"
    Versioned (THKD -> cppMaxTxSize) <- decodeField "max_tx_size"
    Versioned (THKD -> cppDRepDeposit) <- decodeField "drep_deposit"
    Versioned (THKD -> cppMaxValSize) <- decodeField "max_val_size"
    Versioned (THKD -> cppPoolDeposit) <- decodeField "pool_deposit"
    Versioned (THKD -> cppDRepActivity) <- decodeField "drep_activity"
    Versioned (THKD -> cppMinPoolCost) <- decodeField "min_pool_cost"
    Versioned (THKD -> cppMaxBBSize) <- decodeField "max_block_size"
    Versioned (THKD -> cppMaxTxExUnits) <- decodeField "max_tx_ex_units"
    Versioned (cppProtocolVersion) <- decodeField "protocol_version"
    Versioned (THKD -> cppCoinsPerUTxOByte) <- decodeField "coin_per_utxo_byte"
    Versioned (THKD -> cppGovActionDeposit) <- decodeField "gov_action_deposit"
    Versioned (THKD -> cppMaxBlockExUnits) <- decodeField "max_block_ex_units"
    Versioned (THKD -> cppCommitteeMinSize) <- decodeField "min_committee_size"
    Versioned (THKD -> cppGovActionLifetime) <- decodeField "gov_action_lifetime"
    Versioned (THKD -> cppCommitteeMaxTermLength) <- decodeField "committee_term_limit"
    Versioned (THKD -> cppCollateralPercentage) <- decodeField "collateral_percentage"
    Versioned (THKD -> cppMaxBHSize) <- decodeField "max_block_header_size"
    Versioned (THKD -> cppMaxCollateralInputs) <- decodeField "max_collateral_inputs"
    Versioned (THKD -> cppDRepVotingThresholds) <- decodeField "drep_voting_thresholds"
    Versioned (THKD -> cppPoolVotingThresholds) <- decodeField "pool_voting_thresholds"
    Versioned (THKD -> cppMinFeeRefScriptCostPerByte) <- decodeField "min_fee_ref_script_cost_per_byte"
    pure $ Versioned $ PParams ConwayPParams {..}

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

instance ToCanonicalCBOR v (PParamsUpdate ConwayEra) where
  toCanonicalCBOR v (PParamsUpdate ConwayPParams {..}) =
    encodeAsMap
      [ SomeEncodablePair v ("a0" :: Text) (unTHKD cppA0)
      , SomeEncodablePair v ("rho" :: Text) (unTHKD cppRho)
      , SomeEncodablePair v ("tau" :: Text) (unTHKD cppTau)
      , SomeEncodablePair v ("n_opt" :: Text) (unTHKD cppNOpt)
      , SomeEncodablePair v ("prices" :: Text) (unTHKD cppPrices)
      , SomeEncodablePair v ("epoch_max" :: Text) (unTHKD cppEMax)
      , SomeEncodablePair v ("min_fee_a" :: Text) (unTHKD cppMinFeeA)
      , SomeEncodablePair v ("min_fee_b" :: Text) (unTHKD cppMinFeeB)
      , SomeEncodablePair v ("cost_models" :: Text) (unTHKD cppCostModels)
      , SomeEncodablePair v ("key_deposit" :: Text) (unTHKD cppKeyDeposit)
      , SomeEncodablePair v ("max_tx_size" :: Text) (unTHKD cppMaxTxSize)
      , SomeEncodablePair v ("drep_deposit" :: Text) (unTHKD cppDRepDeposit)
      , SomeEncodablePair v ("pool_deposit" :: Text) (unTHKD cppPoolDeposit)
      , SomeEncodablePair v ("max_val_size" :: Text) (unTHKD cppMaxValSize)
      , SomeEncodablePair v ("drep_activity" :: Text) (unTHKD cppDRepActivity)
      , SomeEncodablePair v ("min_pool_cost" :: Text) (unTHKD cppMinPoolCost)
      , SomeEncodablePair v ("max_block_size" :: Text) (unTHKD cppMaxBBSize)
      , SomeEncodablePair v ("max_tx_ex_units" :: Text) (unTHKD cppMaxTxExUnits)
      , SomeEncodablePair v ("coin_per_utxo_byte" :: Text) (unTHKD cppCoinsPerUTxOByte)
      , SomeEncodablePair v ("gov_action_deposit" :: Text) (unTHKD cppGovActionDeposit)
      , SomeEncodablePair v ("max_block_ex_units" :: Text) (unTHKD cppMaxBlockExUnits)
      , SomeEncodablePair v ("min_committee_size" :: Text) (unTHKD cppCommitteeMinSize)
      , SomeEncodablePair v ("committee_term_limit" :: Text) (unTHKD cppCommitteeMaxTermLength)
      , SomeEncodablePair v ("collateral_percentage" :: Text) (unTHKD cppCollateralPercentage)
      , SomeEncodablePair v ("drep_voting_thresholds" :: Text) (unTHKD cppDRepVotingThresholds)
      , SomeEncodablePair v ("gov_action_lifetime" :: Text) (unTHKD cppGovActionLifetime)
      , SomeEncodablePair v ("max_block_header_size" :: Text) (unTHKD cppMaxBHSize)
      , SomeEncodablePair v ("max_collateral_inputs" :: Text) (unTHKD cppMaxCollateralInputs)
      , SomeEncodablePair v ("pool_voting_thresholds" :: Text) (unTHKD cppPoolVotingThresholds)
      , SomeEncodablePair
          v
          ("min_fee_ref_script_cost_per_byte" :: Text)
          (unTHKD cppMinFeeRefScriptCostPerByte)
      ]

instance FromCanonicalCBOR v (PParamsUpdate ConwayEra) where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 30
    Versioned (THKD -> cppA0) <- decodeField "a0"
    Versioned (THKD -> cppRho) <- decodeField "rho"
    Versioned (THKD -> cppTau) <- decodeField "tau"
    Versioned (THKD -> cppNOpt) <- decodeField "n_opt"
    Versioned (THKD -> cppPrices) <- decodeField "prices"
    Versioned (THKD -> cppEMax) <- decodeField "epoch_max"
    Versioned (THKD -> cppMinFeeA) <- decodeField "min_fee_a"
    Versioned (THKD -> cppMinFeeB) <- decodeField "min_fee_b"
    Versioned (THKD -> cppCostModels) <- decodeField "cost_models"
    Versioned (THKD -> cppKeyDeposit) <- decodeField "key_deposit"
    Versioned (THKD -> cppMaxTxSize) <- decodeField "max_tx_size"
    Versioned (THKD -> cppDRepDeposit) <- decodeField "drep_deposit"
    Versioned (THKD -> cppMaxValSize) <- decodeField "max_val_size"
    Versioned (THKD -> cppPoolDeposit) <- decodeField "pool_deposit"
    Versioned (THKD -> cppDRepActivity) <- decodeField "drep_activity"
    Versioned (THKD -> cppMinPoolCost) <- decodeField "min_pool_cost"
    Versioned (THKD -> cppMaxBBSize) <- decodeField "max_block_size"
    Versioned (THKD -> cppMaxTxExUnits) <- decodeField "max_tx_ex_units"
    Versioned (THKD -> cppCoinsPerUTxOByte) <- decodeField "coin_per_utxo_byte"
    Versioned (THKD -> cppGovActionDeposit) <- decodeField "gov_action_deposit"
    Versioned (THKD -> cppMaxBlockExUnits) <- decodeField "max_block_ex_units"
    Versioned (THKD -> cppCommitteeMinSize) <- decodeField "min_committee_size"
    Versioned (THKD -> cppGovActionLifetime) <- decodeField "gov_action_lifetime"
    Versioned (THKD -> cppCommitteeMaxTermLength) <- decodeField "committee_term_limit"
    Versioned (THKD -> cppCollateralPercentage) <- decodeField "collateral_percentage"
    Versioned (THKD -> cppMaxBHSize) <- decodeField "max_block_header_size"
    Versioned (THKD -> cppMaxCollateralInputs) <- decodeField "max_collateral_inputs"
    Versioned (THKD -> cppDRepVotingThresholds) <- decodeField "drep_voting_thresholds"
    Versioned (THKD -> cppPoolVotingThresholds) <- decodeField "pool_voting_thresholds"
    Versioned (THKD -> cppMinFeeRefScriptCostPerByte) <- decodeField "min_fee_ref_script_cost_per_byte"
    let cppProtocolVersion = NoUpdate
    pure $ Versioned $ PParamsUpdate ConwayPParams {..}

type instance NamespaceKeySize "gov/pparams/v0" = 4

instance KnownNamespace "gov/pparams/v0" where
  type NamespaceKey "gov/pparams/v0" = GovPParamsIn
  type NamespaceEntry "gov/pparams/v0" = GovPParamsOut

instance CanonicalCBOREntryEncoder "gov/pparams/v0" GovPParamsOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/pparams/v0") n

instance CanonicalCBOREntryDecoder "gov/pparams/v0" GovPParamsOut where
  decodeEntry = fromCanonicalCBOR
