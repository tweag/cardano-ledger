{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.Dormant where

import Cardano.Ledger.BaseTypes (EpochNo (EpochNo))
import Cardano.Ledger.CanonicalState.BasicTypes ()
import Cardano.SCLS.CBOR.Canonical.Decoder as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CDDL ()
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

data DormantIn = DormantIn
  deriving (Eq, Show, Ord)

instance IsKey DormantIn where
  keySize = namespaceKeySize @"entities/dormant_epochs/v0"
  packKeyM DormantIn = packM (0 :: Word8)
  unpackKeyM = do
    0 :: Word8 <- unpackM
    return DormantIn

newtype DormantOut = DormantOut EpochNo
  deriving (Eq, Show, Generic)

type instance NamespaceKeySize "entities/dormant_epochs/v0" = 1

instance ToCanonicalCBOR v DormantOut where
  toCanonicalCBOR v (DormantOut (EpochNo epochNo)) =
    toCanonicalCBOR v epochNo

instance FromCanonicalCBOR v DormantOut where
  fromCanonicalCBOR = do
    Versioned d <- fromCanonicalCBOR @v
    return $ Versioned $ DormantOut (EpochNo d)

instance CanonicalCBOREntryEncoder "entities/dormant_epochs/v0" DormantOut where
  encodeEntry = toCanonicalCBOR (Proxy @"entities/dormant_epochs/v0")

instance CanonicalCBOREntryDecoder "entities/dormant_epochs/v0" DormantOut where
  decodeEntry = fromCanonicalCBOR
