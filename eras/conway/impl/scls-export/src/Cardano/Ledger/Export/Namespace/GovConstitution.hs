{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Export.Namespace.GovConstitution
    ( GovConstitutionIn(..)
    , GovConstitutionOut(..)
    ) where

import Cardano.Ledger.Conway.Governance (Constitution(..))
import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Export.Common ()
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.Internal.Version
import Cardano.SCLS.Internal.NamespaceCodec
import Data.Proxy
-- import Cardano.SCLS.Internal.Entry
import Cardano.SCLS.Internal.Entry.IsKey
import Data.MemPack.ByteOrdered


data GovConstitutionIn = GovConstitutionIn EpochNo
  deriving (Eq, Ord, Show)

instance IsKey GovConstitutionIn where
    keySize = namespaceKeySize @"gov/constitution/v0"
    packKeyM (GovConstitutionIn (EpochNo epochNo)) = do
        packWord64beM epochNo
    unpackKeyM = do
        epochNo <- unpackBigEndianM
        return $ GovConstitutionIn (EpochNo epochNo)

newtype GovConstitutionOut = GovConstitutionOut (Constitution ConwayEra)
  deriving (Eq, Show)

deriving newtype instance ToCanonicalCBOR v (GovConstitutionOut)
deriving newtype instance FromCanonicalCBOR v (GovConstitutionOut)

instance ToCanonicalCBOR v (Constitution ConwayEra) where
    toCanonicalCBOR v Constitution{..} =
      toCanonicalCBOR v (constitutionAnchor, constitutionScript)
instance FromCanonicalCBOR v (Constitution ConwayEra) where
    fromCanonicalCBOR = do
      Versioned (anchor, script) <- fromCanonicalCBOR
      -- Versioned script <- fromCanonicalCBOR
      return $ Versioned $ Constitution anchor script

type instance NamespaceKeySize "gov/constitution/v0" = 8

instance KnownNamespace "gov/constitution/v0" where
  type NamespaceKey "gov/constitution/v0" = GovConstitutionIn
  type NamespaceEntry "gov/constitution/v0" = GovConstitutionOut

instance CanonicalCBOREntryEncoder "gov/constitution/v0" GovConstitutionOut where
  encodeEntry n = toCanonicalCBOR (Proxy @V1) n

instance CanonicalCBOREntryDecoder "gov/constitution/v0" GovConstitutionOut where
  decodeEntry = VersionedNS . unVer @V1 <$> fromCanonicalCBOR