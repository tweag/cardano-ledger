{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Conway.SCLS.Namespace.Pots
    ( PotsIn(..)
    , PotsOut(..)
    ) where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Conway.SCLS.Namespace.UTxO ()
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding qualified as E
import Data.MemPack.ByteOrdered
import Data.Proxy
import GHC.Generics (Generic)

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
  deriving (Generic)

instance ToCanonicalCBOR "pots/v0" PotsOut where
  toCanonicalCBOR v (PotsOut{..}) =
    E.encodeMapLen 5
      <> E.encodeString "fee" <> toCanonicalCBOR v poFee
      <> E.encodeString "deposit" <> toCanonicalCBOR v poDeposit
      <> E.encodeString "donation" <> toCanonicalCBOR v poDonation
      <> E.encodeString "reserves" <> toCanonicalCBOR v poReserves
      <> E.encodeString "treasury" <> toCanonicalCBOR v poTreasury

instance FromCanonicalCBOR "pots/v0" PotsOut where
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

instance KnownNamespace "pots/v0" where
  type NamespaceKey "pots/v0" = PotsIn
  type NamespaceEntry "pots/v0" = PotsOut

instance CanonicalCBOREntryEncoder "pots/v0" PotsOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"pots/v0") n

instance CanonicalCBOREntryDecoder "pots/v0" PotsOut where
  decodeEntry = fromCanonicalCBOR
