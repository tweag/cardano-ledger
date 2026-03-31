{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.EntitiesAccounts.V0 (
  EntitiesAccountsIn (..),
  EntitiesAccountsOut (..),
) where

import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Core (Staking)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.State (
  EraAccounts (
    AccountState
  ),
 )
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
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
import GHC.Generics (Generic)

instance
  ( Era era
  , NamespaceEra "entities/accounts/v0" ~ era
  , FromCanonicalCBOR "entities/accounts/v0" (AccountState era)
  , ToCanonicalCBOR "entities/accounts/v0" (AccountState era)
  ) =>
  KnownNamespace "entities/accounts/v0"
  where
  type NamespaceKey "entities/accounts/v0" = EntitiesAccountsIn
  type
    NamespaceEntry "entities/accounts/v0" =
      EntitiesAccountsOut (NamespaceEra "entities/accounts/v0")

instance
  ( Era era
  , NamespaceEra "entities/accounts/v0" ~ era
  , ToCanonicalCBOR "entities/accounts/v0" (AccountState era)
  ) =>
  CanonicalCBOREntryEncoder "entities/accounts/v0" (EntitiesAccountsOut era)
  where
  encodeEntry (EntitiesAccountsOut n) = toCanonicalCBOR (Proxy @"entities/accounts/v0") n

instance
  ( Era era
  , NamespaceEra "entities/accounts/v0" ~ era
  , FromCanonicalCBOR "entities/accounts/v0" (AccountState era)
  ) =>
  CanonicalCBOREntryDecoder "entities/accounts/v0" (EntitiesAccountsOut era)
  where
  decodeEntry = fmap EntitiesAccountsOut <$> fromCanonicalCBOR

newtype EntitiesAccountsIn = EntitiesAccountsIn (Credential Staking)
  deriving (Eq, Ord, Show)

type instance NamespaceKeySize "entities/accounts/v0" = 28

instance IsKey EntitiesAccountsIn where
  keySize = namespaceKeySize @"entities/accounts/v0"
  packKeyM (EntitiesAccountsIn accountCredential) =
    packM accountCredential
  unpackKeyM =
    EntitiesAccountsIn <$> unpackM

newtype EntitiesAccountsOut era
  = EntitiesAccountsOut (AccountState era)
  deriving (Generic)

deriving instance Eq (AccountState era) => Eq (EntitiesAccountsOut era)

deriving instance Show (AccountState era) => Show (EntitiesAccountsOut era)

deriving newtype instance
  ToCanonicalCBOR "entities/accounts/v0" (AccountState era) =>
  ToCanonicalCBOR "entities/accounts/v0" (EntitiesAccountsOut era)

deriving newtype instance
  FromCanonicalCBOR "entities/accounts/v0" (AccountState era) =>
  FromCanonicalCBOR "entities/accounts/v0" (EntitiesAccountsOut era)
