{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Export.Namespace.PoolStake
  ( PoolStakeIn(..)
  , PoolStakeOut(..)
  ) where

import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CBOR.Canonical.Decoder
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Decoding as D
import Cardano.Ledger.Keys
import Cardano.Ledger.Export.Common ()
import Cardano.Ledger.Coin (Coin)
import Cardano.SCLS.Internal.Entry.IsKey
import Cardano.SCLS.Internal.Version
import Cardano.SCLS.Internal.NamespaceCodec
import Data.Proxy
import Data.MemPack

newtype PoolStakeIn = PoolStakeIn (KeyHash 'StakePool)
  deriving (Eq, Ord, Show)


instance IsKey PoolStakeIn where
    keySize = namespaceKeySize @"pool_stake/v0"
    packKeyM (PoolStakeIn kh) = packM kh
    unpackKeyM = PoolStakeIn <$> unpackM

data PoolStakeOut = PoolStakeOut
  { total :: !Coin
  , vrf :: (VRFVerKeyHash 'StakePoolVRF)
  }
  deriving (Eq, Show)

instance ToCanonicalCBOR v PoolStakeOut where
    toCanonicalCBOR v (PoolStakeOut total vrf) =
        E.encodeMapLen 2
            <> E.encodeString "vrf" <> toCanonicalCBOR v vrf
            <> E.encodeString "total" <> toCanonicalCBOR v total

instance FromCanonicalCBOR v PoolStakeOut where
    fromCanonicalCBOR = do
        2 <- D.decodeMapLenCanonical
        "vrf" <- D.decodeStringCanonical
        Versioned vrf <- fromCanonicalCBOR
        "total" <- D.decodeStringCanonical
        Versioned total <- fromCanonicalCBOR
        return $ Versioned PoolStakeOut {..}

type instance NamespaceKeySize "pool_stake/v0" = 28

instance KnownNamespace "pool_stake/v0" where
  type NamespaceKey "pool_stake/v0" = PoolStakeIn
  type NamespaceEntry "pool_stake/v0" = PoolStakeOut

instance CanonicalCBOREntryEncoder "pool_stake/v0" PoolStakeOut where
  encodeEntry n = toCanonicalCBOR (Proxy @V1) n

instance CanonicalCBOREntryDecoder "pool_stake/v0" PoolStakeOut where
  decodeEntry = VersionedNS . unVer @V1 <$> fromCanonicalCBOR