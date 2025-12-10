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
module Cardano.Ledger.Conway.SCLS.Namespace.GovPParams
    ( GovPParamsIn(..)
    , GovPParamsOut(..)
    ) where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits)
import Cardano.Ledger.Babbage.PParams (CoinPerByte)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.PParams (ConwayPParams (..), THKD(..), PoolVotingThresholds, DRepVotingThresholds)
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Conway.SCLS.LedgerCBOR
import Cardano.Ledger.Core (PParams(..), PParamsUpdate(..))
import Cardano.Ledger.HKD
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding qualified as E
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.MemPack
import Data.Proxy
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
        _ | tag == "prev" -> return GovPParamsInPrev
          | tag == "curr" -> return GovPParamsInCurr
          | tag == "fut0" -> return GovPParamsInPossibleFuture
          | tag == "fut1" -> return GovPParamsInDefiniteFuture
          | otherwise -> fail "Invalid GovPParamsIn tag"


deriving via (LedgerCBOR v (CostModels)) instance ToCanonicalCBOR v (CostModels)
deriving via (LedgerCBOR v (CostModels)) instance FromCanonicalCBOR v (CostModels)
deriving via (LedgerCBOR v (PoolVotingThresholds)) instance ToCanonicalCBOR v (PoolVotingThresholds)
deriving via (LedgerCBOR v (PoolVotingThresholds)) instance FromCanonicalCBOR v (PoolVotingThresholds)
deriving via (LedgerCBOR v (DRepVotingThresholds)) instance ToCanonicalCBOR v (DRepVotingThresholds)
deriving via (LedgerCBOR v (DRepVotingThresholds)) instance FromCanonicalCBOR v (DRepVotingThresholds)
deriving via (LedgerCBOR v (CoinPerByte)) instance ToCanonicalCBOR v (CoinPerByte)
deriving via (LedgerCBOR v (CoinPerByte)) instance FromCanonicalCBOR v (CoinPerByte)
deriving via (LedgerCBOR v (OrdExUnits)) instance ToCanonicalCBOR v (OrdExUnits)
deriving via (LedgerCBOR v (OrdExUnits)) instance FromCanonicalCBOR v (OrdExUnits)


newtype GovPParamsOut = GovPParamsOut (PParams ConwayEra)
  deriving (Eq, Show)


deriving newtype instance ToCanonicalCBOR v (GovPParamsOut)
deriving newtype instance FromCanonicalCBOR v (GovPParamsOut)

instance ToCanonicalCBOR v (PParams ConwayEra) where
  toCanonicalCBOR v (PParams ConwayPParams{..}) =
    E.encodeMapLen 31
      <> E.encodeString "a0" <> toCanonicalCBOR v (unTHKD cppA0)
      <> E.encodeString "rho" <> toCanonicalCBOR v (unTHKD cppRho)
      <> E.encodeString "tau" <> toCanonicalCBOR v (unTHKD cppTau)
      <> E.encodeString "n_opt" <> toCanonicalCBOR v (unTHKD cppNOpt)
      <> E.encodeString "prices" <> toCanonicalCBOR v (unTHKD cppPrices)
      <> E.encodeString "epoch_max" <> toCanonicalCBOR v (unTHKD cppEMax)
      <> E.encodeString "min_fee_a" <> toCanonicalCBOR v (unTHKD cppMinFeeA)
      <> E.encodeString "min_fee_b" <> toCanonicalCBOR v (unTHKD cppMinFeeB)
      <> E.encodeString "cost_models" <> toCanonicalCBOR v (unTHKD cppCostModels)
      <> E.encodeString "key_deposit" <> toCanonicalCBOR v (unTHKD cppKeyDeposit)
      <> E.encodeString "max_tx_size" <> toCanonicalCBOR v (unTHKD cppMaxTxSize)
      <> E.encodeString "drep_deposit" <> toCanonicalCBOR v (unTHKD cppDRepDeposit)
      <> E.encodeString "max_val_size" <> toCanonicalCBOR v (unTHKD cppMaxValSize)
      <> E.encodeString "pool_deposit" <> toCanonicalCBOR v (unTHKD cppPoolDeposit)
      <> E.encodeString "drep_activity" <> toCanonicalCBOR v (unTHKD cppDRepActivity)
      <> E.encodeString "min_pool_cost" <> toCanonicalCBOR v (unTHKD cppMinPoolCost)
      <> E.encodeString "max_block_size" <> toCanonicalCBOR v (unTHKD cppMaxBBSize)
      <> E.encodeString "max_tx_ex_units" <> toCanonicalCBOR v (unTHKD cppMaxTxExUnits)
      <> E.encodeString "protocol_version" <> toCanonicalCBOR v (cppProtocolVersion)
      <> E.encodeString "coin_per_utxo_byte" <> toCanonicalCBOR v (unTHKD cppCoinsPerUTxOByte)
      <> E.encodeString "gov_action_deposit" <> toCanonicalCBOR v (unTHKD cppGovActionDeposit)
      <> E.encodeString "max_block_ex_units" <> toCanonicalCBOR v (unTHKD cppMaxBlockExUnits)
      <> E.encodeString "min_committee_size" <> toCanonicalCBOR v (unTHKD cppCommitteeMinSize)
      <> E.encodeString "gov_action_lifetime" <> toCanonicalCBOR v (unTHKD cppGovActionLifetime)
      <> E.encodeString "committee_term_limit" <> toCanonicalCBOR v (unTHKD cppCommitteeMaxTermLength)
      <> E.encodeString "collateral_percentage" <> toCanonicalCBOR v (unTHKD cppCollateralPercentage)
      <> E.encodeString "max_block_header_size" <> toCanonicalCBOR v (unTHKD cppMaxBHSize)
      <> E.encodeString "max_collateral_inputs" <> toCanonicalCBOR v (unTHKD cppMaxCollateralInputs)
      <> E.encodeString "drep_voting_thresholds" <> toCanonicalCBOR v (unTHKD cppDRepVotingThresholds)
      <> E.encodeString "pool_voting_thresholds" <> toCanonicalCBOR v (unTHKD cppPoolVotingThresholds)
      <> E.encodeString "min_fee_cost_per_byte" <> toCanonicalCBOR v (unTHKD cppMinFeeRefScriptCostPerByte)

instance FromCanonicalCBOR v (PParams ConwayEra) where
  fromCanonicalCBOR = do
    31 <- D.decodeMapLenCanonical
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
    Versioned (THKD -> cppMinFeeRefScriptCostPerByte) <- decodeField "min_fee_cost_per_byte"
    pure $ Versioned $ PParams ConwayPParams{..}

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> D.Decoder s (Versioned v a)
decodeField fieldName = do
  s <- D.decodeStringCanonical
  unless (s == fieldName) $
    fail $ T.unpack $ "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

instance ToCanonicalCBOR v (PParamsUpdate ConwayEra) where
  toCanonicalCBOR v (PParamsUpdate ConwayPParams{..}) =
    E.encodeMapLen 30
      <> E.encodeString "a0" <> toCanonicalCBOR v (unTHKD cppA0)
      <> E.encodeString "rho" <> toCanonicalCBOR v (unTHKD cppRho)
      <> E.encodeString "tau" <> toCanonicalCBOR v (unTHKD cppTau)
      <> E.encodeString "n_opt" <> toCanonicalCBOR v (unTHKD cppNOpt)
      <> E.encodeString "prices" <> toCanonicalCBOR v (unTHKD cppPrices)
      <> E.encodeString "epoch_max" <> toCanonicalCBOR v (unTHKD cppEMax)
      <> E.encodeString "min_fee_a" <> toCanonicalCBOR v (unTHKD cppMinFeeA)
      <> E.encodeString "min_fee_b" <> toCanonicalCBOR v (unTHKD cppMinFeeB)
      <> E.encodeString "cost_models" <> toCanonicalCBOR v (unTHKD cppCostModels)
      <> E.encodeString "key_deposit" <> toCanonicalCBOR v (unTHKD cppKeyDeposit)
      <> E.encodeString "max_tx_size" <> toCanonicalCBOR v (unTHKD cppMaxTxSize)
      <> E.encodeString "drep_deposit" <> toCanonicalCBOR v (unTHKD cppDRepDeposit)
      <> E.encodeString "pool_deposit" <> toCanonicalCBOR v (unTHKD cppPoolDeposit)
      <> E.encodeString "max_val_size" <> toCanonicalCBOR v (unTHKD cppMaxValSize)
      <> E.encodeString "drep_activity" <> toCanonicalCBOR v (unTHKD cppDRepActivity)
      <> E.encodeString "min_pool_cost" <> toCanonicalCBOR v (unTHKD cppMinPoolCost)
      <> E.encodeString "max_block_size" <> toCanonicalCBOR v (unTHKD cppMaxBBSize)
      <> E.encodeString "max_tx_ex_units" <> toCanonicalCBOR v (unTHKD cppMaxTxExUnits)
      <> E.encodeString "coin_per_utxo_byte" <> toCanonicalCBOR v (unTHKD cppCoinsPerUTxOByte)
      <> E.encodeString "gov_action_deposit" <> toCanonicalCBOR v (unTHKD cppGovActionDeposit)
      <> E.encodeString "max_block_ex_units" <> toCanonicalCBOR v (unTHKD cppMaxBlockExUnits)
      <> E.encodeString "min_committee_size" <> toCanonicalCBOR v (unTHKD cppCommitteeMinSize)
      <> E.encodeString "committee_term_limit" <> toCanonicalCBOR v (unTHKD cppCommitteeMaxTermLength)
      <> E.encodeString "collateral_percentage" <> toCanonicalCBOR v (unTHKD cppCollateralPercentage)
      <> E.encodeString "drep_voting_thresholds" <> toCanonicalCBOR v (unTHKD cppDRepVotingThresholds)
      <> E.encodeString "gov_action_lifetime" <> toCanonicalCBOR v (unTHKD cppGovActionLifetime)
      <> E.encodeString "max_block_header_size" <> toCanonicalCBOR v (unTHKD cppMaxBHSize)
      <> E.encodeString "max_collateral_inputs" <> toCanonicalCBOR v (unTHKD cppMaxCollateralInputs)
      <> E.encodeString "pool_voting_thresholds" <> toCanonicalCBOR v (unTHKD cppPoolVotingThresholds)
      <> E.encodeString "min_fee_cost_per_byte" <> toCanonicalCBOR v (unTHKD cppMinFeeRefScriptCostPerByte)

instance FromCanonicalCBOR v (PParamsUpdate ConwayEra) where
  fromCanonicalCBOR = do
    30 <- D.decodeMapLenCanonical
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
    Versioned (THKD -> cppPoolDeposit) <- decodeField "pool_deposit"
    Versioned (THKD -> cppMaxValSize) <- decodeField "max_val_size"
    Versioned (THKD -> cppDRepActivity) <- decodeField "drep_activity"
    Versioned (THKD -> cppMinPoolCost) <- decodeField "min_pool_cost"
    Versioned (THKD -> cppMaxBBSize) <- decodeField "max_block_size"
    Versioned (THKD -> cppMaxTxExUnits) <- decodeField "max_tx_ex_units"
    Versioned (THKD -> cppCoinsPerUTxOByte) <- decodeField "coin_per_utxo_byte"
    Versioned (THKD -> cppGovActionDeposit) <- decodeField "gov_action_deposit"
    Versioned (THKD -> cppMaxBlockExUnits) <- decodeField "max_block_ex_units"
    Versioned (THKD -> cppCommitteeMinSize) <- decodeField "min_committee_size"
    Versioned (THKD -> cppCommitteeMaxTermLength) <- decodeField "committee_term_limit"
    Versioned (THKD -> cppCollateralPercentage) <- decodeField "collateral_percentage"
    Versioned (THKD -> cppDRepVotingThresholds) <- decodeField "drep_voting_thresholds"
    Versioned (THKD -> cppGovActionLifetime) <- decodeField "gov_action_lifetime"
    Versioned (THKD -> cppMaxBHSize) <- decodeField "max_block_header_size"
    Versioned (THKD -> cppMaxCollateralInputs) <- decodeField "max_collateral_inputs"
    Versioned (THKD -> cppPoolVotingThresholds) <- decodeField "pool_voting_thresholds"
    Versioned (THKD -> cppMinFeeRefScriptCostPerByte) <- decodeField "min_fee_cost_per_byte"
    let cppProtocolVersion = NoUpdate
    pure $ Versioned $ PParamsUpdate ConwayPParams{..}


type instance NamespaceKeySize "gov/pparams/v0" = 4

instance KnownNamespace "gov/pparams/v0" where
  type NamespaceKey "gov/pparams/v0" = GovPParamsIn
  type NamespaceEntry "gov/pparams/v0" = GovPParamsOut

instance CanonicalCBOREntryEncoder "gov/pparams/v0" GovPParamsOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/pparams/v0") n

instance CanonicalCBOREntryDecoder "gov/pparams/v0" GovPParamsOut where
  decodeEntry = fromCanonicalCBOR
