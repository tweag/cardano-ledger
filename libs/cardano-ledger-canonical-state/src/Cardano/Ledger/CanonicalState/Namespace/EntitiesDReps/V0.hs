{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.EntitiesDReps.V0 (
  EntitiesDRepsIn (..),
  EntitiesDRepsOut (..),
  CanonicalDRepState (..),
) where

import Cardano.Ledger.BaseTypes (Anchor, EpochNo, StrictMaybe)
import Cardano.Ledger.CanonicalState.BasicTypes (CanonicalCoin, decodeNamespacedField)
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Core (KeyRole (DRepRole), Staking)
import Cardano.Ledger.Credential (Credential)
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), decodeMapLenCanonicalOf)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack (MemPack (packM, unpackM))
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

instance
  ( Era era
  , NamespaceEra "entities/dreps/v0" ~ era
  , ToCanonicalCBOR "entities/dreps/v0" CanonicalDRepState
  , FromCanonicalCBOR "entities/dreps/v0" CanonicalDRepState
  ) =>
  KnownNamespace "entities/dreps/v0"
  where
  type NamespaceKey "entities/dreps/v0" = EntitiesDRepsIn
  type NamespaceEntry "entities/dreps/v0" = EntitiesDRepsOut

instance
  ( Era era
  , NamespaceEra "entities/dreps/v0" ~ era
  , ToCanonicalCBOR "entities/dreps/v0" CanonicalDRepState
  ) =>
  CanonicalCBOREntryEncoder "entities/dreps/v0" EntitiesDRepsOut
  where
  encodeEntry (EntitiesDRepsOut n) = toCanonicalCBOR (Proxy @"entities/dreps/v0") n

instance
  ( Era era
  , NamespaceEra "entities/dreps/v0" ~ era
  , FromCanonicalCBOR "entities/dreps/v0" CanonicalDRepState
  ) =>
  CanonicalCBOREntryDecoder "entities/dreps/v0" EntitiesDRepsOut
  where
  decodeEntry = fmap EntitiesDRepsOut <$> fromCanonicalCBOR

newtype EntitiesDRepsIn = EntitiesDRepsIn (Credential DRepRole)
  deriving (Eq, Ord, Show)

type instance NamespaceKeySize "entities/dreps/v0" = 29

instance IsKey EntitiesDRepsIn where
  keySize = namespaceKeySize @"entities/dreps/v0"
  packKeyM (EntitiesDRepsIn drepCredential) =
    packM drepCredential
  unpackKeyM =
    EntitiesDRepsIn <$> unpackM

newtype EntitiesDRepsOut
  = EntitiesDRepsOut CanonicalDRepState
  deriving (Eq, Show, Generic)

deriving newtype instance
  ToCanonicalCBOR "entities/dreps/v0" CanonicalDRepState =>
  ToCanonicalCBOR "entities/dreps/v0" EntitiesDRepsOut

deriving newtype instance
  FromCanonicalCBOR "entities/dreps/v0" CanonicalDRepState =>
  FromCanonicalCBOR "entities/dreps/v0" EntitiesDRepsOut

data CanonicalDRepState = CanonicalDRepState
  { cdsExpiry :: !EpochNo
  , cdsAnchor :: !(StrictMaybe Anchor)
  , cdsDeposit :: !CanonicalCoin
  , cdsDelegations :: !(Set (Credential Staking))
  }
  deriving (Show, Eq, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalDRepState where
  toCanonicalCBOR v CanonicalDRepState {..} =
    encodeAsMap
      [ mkEncodablePair v ("anchor" :: Text) cdsAnchor
      , mkEncodablePair v ("expiry" :: Text) cdsExpiry
      , mkEncodablePair v ("deposit" :: Text) cdsDeposit
      , mkEncodablePair v ("delegations" :: Text) cdsDelegations
      ]

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalDRepState where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 4
    Versioned cdsAnchor <- decodeNamespacedField @v "anchor"
    Versioned cdsExpiry <- decodeNamespacedField @v "expiry"
    Versioned cdsDeposit <- decodeNamespacedField @v "deposit"
    Versioned cdsDelegations <- decodeNamespacedField @v "delegations"
    pure $ Versioned CanonicalDRepState {..}
