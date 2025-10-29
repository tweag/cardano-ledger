{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Export.Namespace.Pots
    ( PotsIn(..)
    , PotsOut(..)
    ) where

import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Decoding as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CBOR.Canonical.Decoder
-- import Cardano.SCLS.Internal.Entry
-- import Cardano.SCLS.Internal.Entry
import Cardano.SCLS.Internal.Entry.IsKey
import Cardano.SCLS.Internal.Version
import Cardano.SCLS.Internal.NamespaceCodec
import Data.Proxy
import Cardano.Ledger.BaseTypes (EpochNo (..))
import Data.MemPack.ByteOrdered
import Cardano.Ledger.Export.Common ()
import Cardano.Ledger.Export.Namespace.UTxO ()
import Cardano.Ledger.Coin (Coin)


newtype PotsIn = PotsIn EpochNo
  deriving (Eq, Ord, Show)

instance IsKey PotsIn where
    keySize = namespaceKeySize @"pots/v0"
    packKeyM (PotsIn (EpochNo epochNo)) = do
        packWord64beM epochNo
    unpackKeyM = do
        epochNo <- unpackBigEndianM
        return $ PotsIn (EpochNo epochNo)

data PotsOut = PotsOut
  { poFee :: !Coin
  , poDeposit :: !Coin
  , poDonation :: !Coin
  , poReserves :: !Coin
  , poTreasury :: !Coin
  }
  deriving (Eq, Show)

instance ToCanonicalCBOR v PotsOut where
    toCanonicalCBOR v (PotsOut{..}) =
        E.encodeMapLen 5
           <> E.encodeString "fee" <> toCanonicalCBOR v poFee
           <> E.encodeString "deposit" <> toCanonicalCBOR v poDeposit
           <> E.encodeString "donation" <> toCanonicalCBOR v poDonation
           <> E.encodeString "reserves" <> toCanonicalCBOR v poReserves
           <> E.encodeString "treasury" <> toCanonicalCBOR v poTreasury

instance FromCanonicalCBOR V1 PotsOut where
  fromCanonicalCBOR = do
    5 <- D.decodeMapLenCanonical
    "fee" <- D.decodeStringCanonical
    Versioned poFee <- fromCanonicalCBOR
    "deposit" <- D.decodeStringCanonical
    Versioned poDeposit <- fromCanonicalCBOR
    "donation" <- D.decodeStringCanonical
    Versioned poDonation <- fromCanonicalCBOR
    "reserves" <- D.decodeStringCanonical
    Versioned poReserves <- fromCanonicalCBOR
    "treasury" <- D.decodeStringCanonical
    Versioned poTreasury <- fromCanonicalCBOR
    pure (Versioned PotsOut {..})

type instance NamespaceKeySize "pots/v0" = 8

instance KnownNamespace "pots/v0" where
  type NamespaceKey "pots/v0" = PotsIn
  type NamespaceEntry "pots/v0" = PotsOut

instance CanonicalCBOREntryEncoder "pots/v0" PotsOut where
  encodeEntry n = toCanonicalCBOR (Proxy @V1) n

instance CanonicalCBOREntryDecoder "pots/v0" PotsOut where
  decodeEntry = VersionedNS . unVer @V1 <$> fromCanonicalCBOR