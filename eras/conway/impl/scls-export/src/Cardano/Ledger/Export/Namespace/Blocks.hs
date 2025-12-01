{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Export.Namespace.Blocks
  ( BlockIn(..)
  , BlockOut(..)
  , Version(..)
  , ToCanonicalCBOR(..)
  ) where

import GHC.Num.Natural
import qualified Codec.CBOR.Encoding as E
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.Ledger.Keys
import Cardano.SCLS.Internal.Entry.IsKey
import Cardano.SCLS.Internal.NamespaceCodec
import Cardano.SCLS.Internal.Version
import Cardano.Ledger.BaseTypes (EpochNo (..))
import Data.Proxy
import Data.MemPack
import Data.MemPack.ByteOrdered

newtype BlockIn = BlockIn (KeyHash StakePool, EpochNo)
  deriving (Eq, Ord, Show)

newtype BlockOut = BlockOut Natural
  deriving (Eq, Ord, Show)

instance ToCanonicalCBOR v BlockOut where
  toCanonicalCBOR _ (BlockOut n) = E.encodeInteger (fromIntegral n)

instance FromCanonicalCBOR v BlockOut where
  fromCanonicalCBOR = fmap (BlockOut . fromIntegral @Integer) <$> fromCanonicalCBOR

instance IsKey BlockIn where
    keySize = namespaceKeySize @"blocks/v0"
    packKeyM (BlockIn (kh, EpochNo epochNo)) = do
        packM kh
        packWord64beM epochNo
    unpackKeyM = do
        a <- unpackM
        epochNo <- unpackBigEndianM
        return $ BlockIn  (a, EpochNo epochNo)

type instance NamespaceKeySize "blocks/v0" = 36

instance KnownNamespace "blocks/v0" where
  type NamespaceKey "blocks/v0" = BlockIn
  type NamespaceEntry "blocks/v0" = BlockOut

instance CanonicalCBOREntryEncoder "blocks/v0" BlockOut where
  encodeEntry (BlockOut n) = toCanonicalCBOR (Proxy @V1) (BlockOut n)

instance CanonicalCBOREntryDecoder "blocks/v0" BlockOut where
  decodeEntry = VersionedNS . unVer @V1 <$> fromCanonicalCBOR
