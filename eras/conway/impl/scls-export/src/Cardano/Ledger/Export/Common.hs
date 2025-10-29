{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
-- | Common namespace utilities and types for SCLS export.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Export.Common () where

import Cardano.Ledger.Coin (Coin, CompactForm)
import qualified Cardano.Ledger.Coin as Coin
import qualified Codec.CBOR.Encoding as E
import Cardano.Slotting.Slot (EpochInterval, SlotNo(..))
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.Hashes
import Cardano.Ledger.Plutus.ExUnits (Prices)
import Data.Maybe.Strict
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Codec.CBOR.Decoding as D
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.Ledger.BaseTypes (NonNegativeInterval, UnitInterval, ProtVer, Anchor, EpochNo)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Cardano.Ledger.Export.LedgerCBOR

instance {-# OVERLAPPING #-} ToCanonicalCBOR v (CompactForm Coin) where
    toCanonicalCBOR v (Coin.CompactCoin ci) = toCanonicalCBOR v ci

instance {-# OVERLAPPING #-} FromCanonicalCBOR v (CompactForm Coin) where
    fromCanonicalCBOR = fmap Coin.CompactCoin <$> fromCanonicalCBOR

instance {-# OVERLAPPING #-} ToCanonicalCBOR v (Coin) where
    toCanonicalCBOR _ (Coin.Coin ci) = E.encodeInteger ci

instance {-# OVERLAPPING #-} FromCanonicalCBOR v (Coin) where
    fromCanonicalCBOR = Versioned . Coin.Coin <$> D.decodeInteger

instance Typeable kr => ToCanonicalCBOR v (Credential kr) where
    toCanonicalCBOR v (ScriptHashObj sh) = toCanonicalCBOR v (0::Word8, sh)
    toCanonicalCBOR v (KeyHashObj kh) = toCanonicalCBOR v (1::Word8, kh)

instance Typeable kr => FromCanonicalCBOR v (Credential kr) where
    fromCanonicalCBOR = do
        2 <- D.decodeListLenCanonical
        Versioned (tag :: Word8) <- fromCanonicalCBOR
        case tag of
            0 -> fmap ScriptHashObj <$> fromCanonicalCBOR
            1 -> fmap KeyHashObj <$> fromCanonicalCBOR
            _ -> fail "Invalid Credential tag"

-- deriving via (LedgerCBOR v (ScriptHash)) instance FromCanonicalCBOR v ScriptHash
deriving via (LedgerCBOR v (ScriptHash)) instance ToCanonicalCBOR v ScriptHash
deriving via (LedgerCBOR v (ScriptHash)) instance FromCanonicalCBOR v ScriptHash
deriving via (LedgerCBOR v (KeyHash kr)) instance Typeable kr => ToCanonicalCBOR v (KeyHash kr)
deriving via (LedgerCBOR v (KeyHash kr)) instance Typeable kr => FromCanonicalCBOR v (KeyHash kr)


instance ToCanonicalCBOR v a => ToCanonicalCBOR v (StrictMaybe a) where
    toCanonicalCBOR _ SNothing = E.encodeNull
    toCanonicalCBOR v (SJust x) = toCanonicalCBOR v x

instance FromCanonicalCBOR v a => FromCanonicalCBOR v (StrictMaybe a) where
    fromCanonicalCBOR = do
        mt <- D.peekTokenType
        case mt of
            D.TypeNull -> D.decodeNull >> return (Versioned SNothing)
            _ -> fmap SJust <$> fromCanonicalCBOR


deriving via (LedgerCBOR v (VRFVerKeyHash kr)) instance Typeable kr => ToCanonicalCBOR v (VRFVerKeyHash kr)
deriving via (LedgerCBOR v (VRFVerKeyHash kr)) instance Typeable kr => FromCanonicalCBOR v (VRFVerKeyHash kr)
deriving via (LedgerCBOR v (NonNegativeInterval)) instance ToCanonicalCBOR v (NonNegativeInterval)
deriving via (LedgerCBOR v (NonNegativeInterval)) instance FromCanonicalCBOR v (NonNegativeInterval)
deriving via (LedgerCBOR v (UnitInterval)) instance ToCanonicalCBOR v (UnitInterval)
deriving via (LedgerCBOR v (UnitInterval)) instance FromCanonicalCBOR v (UnitInterval)
deriving via (LedgerCBOR v (Prices)) instance ToCanonicalCBOR v (Prices)
deriving via (LedgerCBOR v (Prices)) instance FromCanonicalCBOR v (Prices)
deriving via (LedgerCBOR v (EpochInterval)) instance ToCanonicalCBOR v (EpochInterval)
deriving via (LedgerCBOR v (EpochInterval)) instance FromCanonicalCBOR v (EpochInterval)
deriving via (LedgerCBOR v (ProtVer)) instance ToCanonicalCBOR v (ProtVer)
deriving via (LedgerCBOR v (ProtVer)) instance FromCanonicalCBOR v (ProtVer)
deriving via (LedgerCBOR v (Anchor)) instance ToCanonicalCBOR v (Anchor)
deriving via (LedgerCBOR v (Anchor)) instance FromCanonicalCBOR v (Anchor)
deriving via (LedgerCBOR v (EpochNo)) instance ToCanonicalCBOR v (EpochNo)
deriving via (LedgerCBOR v (EpochNo)) instance FromCanonicalCBOR v (EpochNo)
deriving newtype instance ToCanonicalCBOR v (SlotNo)
deriving newtype instance FromCanonicalCBOR v (SlotNo)


-- deriving via (LedgerCBOR v (StrictSeq a)) instance EncCBOR a => ToCanonicalCBOR v (StrictSeq a)

instance FromCanonicalCBOR v a =>  FromCanonicalCBOR v (StrictSeq a) where
    fromCanonicalCBOR = do
        len_or_indef <- D.decodeListLenOrIndef
        case len_or_indef of
          Nothing ->
              Versioned <$>
                D.decodeSequenceLenIndef (\l (Versioned a) -> a:l) [] (StrictSeq.fromList . reverse) (fromCanonicalCBOR @v)
          Just len -> do
              Versioned <$>
                D.decodeSequenceLenN (\l (Versioned a) -> a:l) [] (StrictSeq.fromList . reverse) len (fromCanonicalCBOR @v)