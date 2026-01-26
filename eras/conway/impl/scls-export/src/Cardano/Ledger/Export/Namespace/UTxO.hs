{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | UTxO namespace export.
module Cardano.Ledger.Export.Namespace.UTxO
  ( UtxoKey(..)
  , UtxoOut(..)
  , Version(..)
  , ToCanonicalCBOR(..)
  , FromCanonicalCBOR(..)
  ) where

import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.Ledger.Binary (decodeMemPack, encodeMemPack, EncCBOR(..), DecCBOR(..), toPlainEncoding, shelleyProtVer, toPlainDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Decoding as D
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.TxIn (TxIn(..), TxId(..))
import Cardano.Ledger.Core (TxOut(..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Address
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.Hashes
import Cardano.Ledger.Plutus.Data (Datum(..))
import Cardano.Ledger.Plutus.Data (BinaryData)
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Coin as Coin
import Cardano.SCLS.Internal.Entry
import Cardano.SCLS.Internal.Version
import Data.Typeable (Typeable)
import qualified Cardano.Ledger.Shelley.TxOut as Shelley
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Allegra.Scripts (Timelock(..))
import Data.MemPack
import Data.Word (Word8)
import Cardano.Ledger.Alonzo.TxOut (DataHash32, Addr28Extra, decodeAddress28)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose, AsItem, AlonzoScript(..))

-- | Helper that allows us to deriving instances via internal CBOR representation
newtype LedgerCBOR (v::Version) a = LedgerCBOR { unLedgerCBOR :: a }
    deriving (Eq, Show)

instance EncCBOR a => ToCanonicalCBOR v (LedgerCBOR v a) where
    toCanonicalCBOR _v (LedgerCBOR a) = toPlainEncoding shelleyProtVer (encCBOR a)

instance DecCBOR a => FromCanonicalCBOR v (LedgerCBOR v a) where
    fromCanonicalCBOR = Versioned . LedgerCBOR <$> toPlainDecoder Nothing shelleyProtVer decCBOR

newtype MemPackCBOR a = MemPackCBOR { unMemPackCBOR :: a }
    deriving (Eq, Show)

instance (MemPack a) => ToCanonicalCBOR V1 (MemPackCBOR a) where
    toCanonicalCBOR _v (MemPackCBOR a) = toPlainEncoding shelleyProtVer (encodeMemPack a)

instance (MemPack a) => FromCanonicalCBOR V1 (MemPackCBOR a) where
    fromCanonicalCBOR = Versioned . MemPackCBOR <$> toPlainDecoder Nothing shelleyProtVer decodeMemPack

-- | Input wrapper for the keys that are used in utxo namespace
data UtxoKey
  = UtxoKeyIn TxIn
  deriving (Show)

instance Eq UtxoKey where
    (UtxoKeyIn txIn1) == (UtxoKeyIn txIn2) = txIn1 == txIn2

instance Ord UtxoKey where
    compare (UtxoKeyIn txIn1) (UtxoKeyIn txIn2) = compare txIn1 txIn2

instance IsKey UtxoKey where
    keySize = 34
    packKeyM (UtxoKeyIn (TxIn (TxId a) b)) = do
        packByteStringM (originalBytes a)
        packM b
    unpackKeyM = do
        a <- unpackM -- FIXME read bytestirng and create unsafe hash
        b <- unpackM
        return $ UtxoKeyIn (TxIn a b)

newtype Out = Out (TxOut ConwayEra)
  deriving newtype (ToCanonicalCBOR V1, FromCanonicalCBOR V1)

-- | Output key that is used in utxo namespace
--
-- Here we follow the current spec, but after benchmarks we can decide that this representation
-- is not efficient and we can replace it with the implementation based on the compact values
data UtxoOut
   = UtxoOutShelley (Shelley.ShelleyTxOut ConwayEra)
   | UtxoOutBabbage (Babbage.BabbageTxOut ConwayEra)
   | UtxoValue MaryValue

instance ToCanonicalCBOR V1 UtxoKey where
    toCanonicalCBOR v (UtxoKeyIn txIn) = E.encodeInt 0 <> toCanonicalCBOR v txIn

instance FromCanonicalCBOR V1 UtxoKey where
    fromCanonicalCBOR = do
        tag <- fromCanonicalCBOR
        case unVer tag :: Word8 of
            0 -> fmap UtxoKeyIn <$> fromCanonicalCBOR
            _ -> fail "Unknown UtxoKey tag"

instance ToCanonicalCBOR V1 UtxoOut where
    toCanonicalCBOR v (UtxoOutShelley shelleyOut) = toCanonicalCBOR v (E.encodeInt 0, toCanonicalCBOR v shelleyOut)
    toCanonicalCBOR v (UtxoOutBabbage babbageOut) = toCanonicalCBOR v (E.encodeInt 1, toCanonicalCBOR v babbageOut)
    toCanonicalCBOR v (UtxoValue value) = toCanonicalCBOR v (E.encodeInt 2, toCanonicalCBOR v value)

instance FromCanonicalCBOR V1 UtxoOut where
    fromCanonicalCBOR = do
        tag <- fromCanonicalCBOR
        case unVer tag :: Word8 of
            1 -> fmap UtxoOutShelley <$> fromCanonicalCBOR
            2 -> fmap UtxoOutBabbage <$> fromCanonicalCBOR
            3 -> fmap UtxoValue <$> fromCanonicalCBOR
            t -> fail $ "Unknown UtxoOut tag: " <> show t

instance ToCanonicalCBOR V1 (Babbage.BabbageTxOut ConwayEra) where
    toCanonicalCBOR v (Babbage.TxOutCompact' cAddr form) =
        E.encodeMapLen 2
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v form
    toCanonicalCBOR v (Babbage.TxOutCompactDH' cAddr form datum) =
        E.encodeMapLen 3
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v form
            <> E.encodeInt 2
                <> case datum of
                    hash_ -> toCanonicalCBOR v (0::Int, originalBytes hash_)
    toCanonicalCBOR v (Babbage.TxOutCompactDatum cAddr form inlineDatum) =
        E.encodeMapLen 3
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v form
            <> E.encodeInt 2
                <> case inlineDatum of
                     binaryData -> toCanonicalCBOR v (1::Int, toCanonicalCBOR v (LedgerCBOR @V1 binaryData))
    toCanonicalCBOR v (Babbage.TxOutCompactRefScript cAddr form datum script) =
        let datumEncoding = case datum of
                NoDatum -> (Nothing)
                DatumHash dh -> Just (toCanonicalCBOR v (0::Int, originalBytes dh))
                Datum binaryData -> Just (toCanonicalCBOR v (1:: Int, toCanonicalCBOR v (LedgerCBOR @V1 binaryData)))
        in E.encodeMapLen (3 + (case datumEncoding of Just{} -> 1 ; Nothing -> 0))
                <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
                <> E.encodeInt 1 <> toCanonicalCBOR v form
                <> case datumEncoding of
                     Nothing -> mempty
                     Just enc -> E.encodeInt 2 <> enc
                <> E.encodeInt 3 <> toCanonicalCBOR v (LedgerCBOR @V1 script)
    toCanonicalCBOR v (Babbage.TxOut_AddrHash28_AdaOnly staking hash28 compactForm) =
        let cAddr = unCompactAddr (compactAddr (decodeAddress28 staking hash28))
        in E.encodeMapLen 2
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v compactForm
    toCanonicalCBOR v (Babbage.TxOut_AddrHash28_AdaOnly_DataHash32 staking hash28 compact dataHash) =
        let cAddr = unCompactAddr (compactAddr (decodeAddress28 staking hash28))
        in E.encodeMapLen 3
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v compact
            <> E.encodeInt 2 <> toCanonicalCBOR v (0::Int, dataHash)

instance FromCanonicalCBOR V1 (Babbage.BabbageTxOut ConwayEra) where
    fromCanonicalCBOR = do
        D.decodeTag >>= \case
            0 -> fmap (\(c, f) -> Babbage.TxOutCompact' c f) <$> fromCanonicalCBOR
            1 -> fmap (\(a,b,c) -> Babbage.TxOutCompactDH' a b c) <$> fromCanonicalCBOR
            2 -> fmap (\(a,b,c) -> Babbage.TxOutCompactDatum a b c) <$> fromCanonicalCBOR
            3 -> fmap (\(a,b,c,d) -> Babbage.TxOutCompactRefScript a b c d) <$> fromCanonicalCBOR
            4 -> fmap (\(a,b,c) -> Babbage.TxOut_AddrHash28_AdaOnly a b c) <$> fromCanonicalCBOR
            5 -> fmap (\(a,b,c,d) -> Babbage.TxOut_AddrHash28_AdaOnly_DataHash32 a b c d) <$> fromCanonicalCBOR
            t -> fail $ "Unknown BabbageTxOut tag: " <> show t

instance ToCanonicalCBOR V1 (Credential kr) where
    toCanonicalCBOR v (ScriptHashObj sh) = toCanonicalCBOR v (0::Word8, sh)
    toCanonicalCBOR v (KeyHashObj kh) = toCanonicalCBOR v (1::Word8, kh)

instance Typeable kr => FromCanonicalCBOR V1 (Credential kr) where
    fromCanonicalCBOR = do
        tag <- fromCanonicalCBOR
        case unVer tag :: Word8 of
            0 -> fmap ScriptHashObj <$> fromCanonicalCBOR
            1 -> fmap KeyHashObj <$> fromCanonicalCBOR
            x -> fail $ "Unknown Credential tag: " <> show x

deriving via (LedgerCBOR v (Shelley.ShelleyTxOut ConwayEra)) instance ToCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)
deriving via (LedgerCBOR v (Shelley.ShelleyTxOut ConwayEra)) instance FromCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance ToCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance FromCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (MemPackCBOR (AlonzoScript ConwayEra)) instance ToCanonicalCBOR V1 (AlonzoScript ConwayEra)
deriving via (MemPackCBOR (AlonzoScript ConwayEra)) instance FromCanonicalCBOR V1 (AlonzoScript ConwayEra)
-- deriving via (MemPackCBOR (CompactForm a)) instance {-# OVERLAPPABLE #-} (MemPack (CompactForm a)) => ToCanonicalCBOR V1 (CompactForm a)

deriving via (LedgerCBOR v MaryValue) instance ToCanonicalCBOR v MaryValue
deriving via (LedgerCBOR v MaryValue) instance FromCanonicalCBOR v MaryValue
instance {-# OVERLAPPING #-} ToCanonicalCBOR version (CompactForm MaryValue) where
    toCanonicalCBOR version v = toCanonicalCBOR version (fromCompact v)

instance {-# OVERLAPPING #-} ToCanonicalCBOR v (CompactForm Coin) where
    toCanonicalCBOR v (Coin.CompactCoin ci) = toCanonicalCBOR v ci

deriving via (MemPackCBOR (CompactForm a)) instance (MemPack (CompactForm a)) => FromCanonicalCBOR V1 (CompactForm a)
deriving via (MemPackCBOR CompactAddr) instance FromCanonicalCBOR V1 CompactAddr
deriving via (MemPackCBOR CompactAddr) instance ToCanonicalCBOR V1 CompactAddr
deriving via (MemPackCBOR Addr28Extra) instance FromCanonicalCBOR V1 Addr28Extra
deriving via (MemPackCBOR Addr28Extra) instance ToCanonicalCBOR V1 Addr28Extra
deriving via (LedgerCBOR v TxIn) instance FromCanonicalCBOR v TxIn
deriving via (LedgerCBOR v TxIn) instance ToCanonicalCBOR v TxIn
deriving via (MemPackCBOR DataHash32) instance FromCanonicalCBOR V1 DataHash32
deriving via (MemPackCBOR DataHash32) instance ToCanonicalCBOR V1 DataHash32
deriving via (MemPackCBOR (Timelock ConwayEra)) instance ToCanonicalCBOR V1 (Timelock ConwayEra)
deriving via (MemPackCBOR (Timelock ConwayEra)) instance FromCanonicalCBOR V1 (Timelock ConwayEra)
-- deriving via (LedgerCBOR v MaryValue) instance ToCanonicalCBOR v MaryValue
-- deriving via (LedgerCBOR v MaryValue) instance FromCanonicalCBOR v MaryValue


deriving via (LedgerCBOR v (KeyHash kr)) instance ToCanonicalCBOR v (KeyHash kr)
deriving via (LedgerCBOR v (KeyHash kr)) instance Typeable kr => FromCanonicalCBOR v (KeyHash kr)
deriving via (LedgerCBOR v (ScriptHash)) instance FromCanonicalCBOR v ScriptHash
deriving via (LedgerCBOR v (ScriptHash)) instance ToCanonicalCBOR v ScriptHash
deriving via (LedgerCBOR v (Datum ConwayEra)) instance ToCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (Datum ConwayEra)) instance FromCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance ToCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance FromCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance ToCanonicalCBOR v ((SafeHash EraIndependentData))
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance FromCanonicalCBOR v ((SafeHash EraIndependentData))



