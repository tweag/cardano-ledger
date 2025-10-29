{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Export.Namespace.GovCommittee
    ( GovCommitteeIn(..)
    , GovCommitteeOut(..)
    ) where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.CertState
import Cardano.Ledger.Export.Common ()
import qualified Codec.CBOR.Decoding as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.Internal.Version
import Cardano.SCLS.Internal.NamespaceCodec
import Cardano.Ledger.Export.Namespace.GovConstitution ()
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Export.Namespace.GovPParams ()
import Cardano.Ledger.Export.Namespace.Snapshots ()
import Data.Proxy
import Data.Word (Word8)
import Cardano.SCLS.Internal.Entry.IsKey
import Data.MemPack.ByteOrdered


data GovCommitteeIn = GovCommitteeIn EpochNo
  deriving (Eq, Ord, Show)

instance IsKey GovCommitteeIn where
    keySize = namespaceKeySize @"gov/committee/v0"
    packKeyM (GovCommitteeIn (EpochNo no)) = do
        packWord64beM no
    unpackKeyM = do
        no <- unpackBigEndianM
        return $ GovCommitteeIn (EpochNo no)

newtype GovCommitteeOut = GovCommitteeOut (CommitteeState ConwayEra)
  deriving (Eq, Show)

deriving newtype instance ToCanonicalCBOR v GovCommitteeOut
deriving newtype instance FromCanonicalCBOR v GovCommitteeOut

instance ToCanonicalCBOR v (CommitteeState ConwayEra) where
    toCanonicalCBOR v st = toCanonicalCBOR v (csCommitteeCreds st)

instance FromCanonicalCBOR v (CommitteeState ConwayEra) where
    fromCanonicalCBOR = do
        st_ <- fromCanonicalCBOR
        return $ CommitteeState <$> st_

instance ToCanonicalCBOR v CommitteeAuthorization where
    toCanonicalCBOR v (CommitteeHotCredential cred)
      = toCanonicalCBOR v (0::Word8, cred)
    toCanonicalCBOR v (CommitteeMemberResigned ma)
      = toCanonicalCBOR v (1::Word8, ma)

instance FromCanonicalCBOR v CommitteeAuthorization where
    fromCanonicalCBOR = do
        D.decodeListLenCanonicalOf 2
        Versioned (tag :: Word8) <- fromCanonicalCBOR
        case tag of
            0 -> fmap CommitteeHotCredential <$> fromCanonicalCBOR
            1 -> fmap CommitteeMemberResigned <$> fromCanonicalCBOR
            _ -> fail "Invalid CommitteeAuthorization tag"

type instance NamespaceKeySize "gov/committee/v0" = 8

instance KnownNamespace "gov/committee/v0" where
  type NamespaceKey "gov/committee/v0" = GovCommitteeIn
  type NamespaceEntry "gov/committee/v0" = GovCommitteeOut

instance CanonicalCBOREntryEncoder "gov/committee/v0" GovCommitteeOut where
  encodeEntry n = toCanonicalCBOR (Proxy @V1) n

instance CanonicalCBOREntryDecoder "gov/committee/v0" GovCommitteeOut where
  decodeEntry = VersionedNS . unVer @V1 <$> fromCanonicalCBOR