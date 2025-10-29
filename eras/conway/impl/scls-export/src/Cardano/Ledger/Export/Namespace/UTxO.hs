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
  , AddrUtxoIn(..)
  , ScriptUtxoIn(..)
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
import Cardano.Ledger.TxIn (TxIn(..))
import Cardano.Ledger.Core (TxOut(..))
-- import Cardano.Ledger.TxOut (TxOut(..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Address
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.Hashes
import Cardano.Ledger.Plutus.Data (Datum(..))
import Cardano.Ledger.Plutus.Data (BinaryData)
import Cardano.Ledger.Mary (MaryEra, MaryValue)
import Cardano.SCLS.Internal.Entry
import Cardano.SCLS.Internal.Version
import Data.Typeable (Typeable)
import qualified Cardano.Ledger.Shelley.TxOut as Shelley
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Allegra.Scripts (Timelock(..))
import Data.MemPack
import Data.Word (Word8, Word16)
import Cardano.Ledger.Alonzo.TxOut (DataHash32, Addr28Extra)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose, AsItem, AlonzoScript)

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
  -- | UtxoKeyScript ScriptUtxoIn

instance Eq UtxoKey where
    (UtxoKeyIn txIn1) == (UtxoKeyIn txIn2) = txIn1 == txIn2
    -- (UtxoKeyScript script1) == (UtxoKeyScript script2) = undefined script1 script2
    -- _ == _ = False

instance Ord UtxoKey where
    compare (UtxoKeyIn txIn1) (UtxoKeyIn txIn2) = compare txIn1 txIn2
    -- compare (UtxoKeyScript script1) (UtxoKeyScript script2) = undefined script1 script2
    -- compare (UtxoKeyIn _) (UtxoKeyScript _) = LT
    -- compare (UtxoKeyScript _) (UtxoKeyIn _) = GT

instance IsKey UtxoKey where
    keySize = 34
    packKeyM (UtxoKeyIn (TxIn a b)) = do
        packM a
        packM b
    -- packKeyM (UtxoKeyScript (ScriptUtxoIn purpose hash)) = do
    --     undefined purpose hash
    unpackKeyM = do
        a <- unpackM
        b <- unpackM
        return $ UtxoKeyIn (TxIn a b)
    -- toKeyBytes v key = toStrictByteString $ toCanonicalCBOR v key

newtype Out = Out (TxOut ConwayEra)
  deriving newtype (ToCanonicalCBOR V1, FromCanonicalCBOR V1)

data AddrUtxoIn = AddrUtxoIn { addrUtxoInAddress :: DataHash32, addrUtxoInIndex :: Word16 }
data ScriptUtxoIn = ScriptUtxoIn { scriptUtxoInPurpose :: AlonzoPlutusPurpose AsItem MaryEra, scriptUtxoInHash :: ScriptHash }

-- | Output key that is used in utxo namespace
--
-- Here we follow the current spec, but after benchmarks we can decide that this representation
-- is not efficient and we can replace it with the implementation based on the compact values
data UtxoOut
   = UtxoOutShelley (Shelley.ShelleyTxOut MaryEra)
   | UtxoOutBabbage (Babbage.BabbageTxOut MaryEra)
   | UtxoValue MaryValue

instance ToCanonicalCBOR V1 UtxoKey where
    toCanonicalCBOR v (UtxoKeyIn txIn) = E.encodeTag 0 <> toCanonicalCBOR v txIn
    -- toCanonicalCBOR v (UtxoKeyScript script) = E.encodeTag 1 <> toCanonicalCBOR v script


instance ToCanonicalCBOR V1 AddrUtxoIn where
    toCanonicalCBOR v (AddrUtxoIn addr idx) = toCanonicalCBOR v (addr, idx)

instance FromCanonicalCBOR V1 AddrUtxoIn where
    fromCanonicalCBOR = fmap (uncurry AddrUtxoIn) <$> fromCanonicalCBOR

instance ToCanonicalCBOR V1 ScriptUtxoIn where
    toCanonicalCBOR v (ScriptUtxoIn purpose hash) = toCanonicalCBOR v (purpose, hash)

instance FromCanonicalCBOR V1 ScriptUtxoIn where
    fromCanonicalCBOR = fmap (uncurry ScriptUtxoIn) <$> fromCanonicalCBOR

instance FromCanonicalCBOR V1 UtxoKey where
    fromCanonicalCBOR = do
        tag <- fromCanonicalCBOR
        case unVer tag :: Word8 of
            0 -> fmap UtxoKeyIn <$> fromCanonicalCBOR
            -- 1 -> fmap UtxoKeyScript <$> fromCanonicalCBOR
            _ -> fail "Unknown UtxoKey tag"

instance ToCanonicalCBOR V1 UtxoOut where
    toCanonicalCBOR v (UtxoOutShelley shelleyOut) = E.encodeTag 1 <> toCanonicalCBOR v shelleyOut
    toCanonicalCBOR v (UtxoOutBabbage babbageOut) = E.encodeTag 2 <> toCanonicalCBOR v babbageOut
    toCanonicalCBOR v (UtxoValue value) = E.encodeTag 3 <> toCanonicalCBOR v value

instance FromCanonicalCBOR V1 UtxoOut where
    fromCanonicalCBOR = do
        tag <- fromCanonicalCBOR
        case unVer tag :: Word8 of
            1 -> fmap UtxoOutShelley <$> fromCanonicalCBOR
            2 -> fmap UtxoOutBabbage <$> fromCanonicalCBOR
            3 -> fmap UtxoValue <$> fromCanonicalCBOR
            t -> fail $ "Unknown UtxoOut tag: " <> show t

instance ToCanonicalCBOR V1 (Babbage.BabbageTxOut ConwayEra) where
    toCanonicalCBOR v (Babbage.TxOutCompact cAddr form) = E.encodeTag 0 <> toCanonicalCBOR v (cAddr, form)
    toCanonicalCBOR v (Babbage.TxOutCompactDH cAddr form dataHash) = E.encodeTag 1 <> toCanonicalCBOR v (cAddr, form, dataHash)

instance FromCanonicalCBOR V1 (Babbage.BabbageTxOut ConwayEra) where
    fromCanonicalCBOR = do
        D.decodeTag >>= \case
            0 -> fmap (\(c, f) -> Babbage.TxOutCompact c f) <$> fromCanonicalCBOR
            1 -> fmap (\(a,b,c) -> Babbage.TxOutCompactDH a b c) <$> fromCanonicalCBOR
            t -> fail $ "Unknown BabbageTxOut tag: " <> show t

instance ToCanonicalCBOR V1 (Babbage.BabbageTxOut MaryEra) where
    toCanonicalCBOR v (Babbage.TxOutCompact cAddr form) = E.encodeTag 0 <> toCanonicalCBOR v (cAddr, form)
    toCanonicalCBOR v (Babbage.TxOutCompactDH cAddr form dataHash) = E.encodeTag 1 <> toCanonicalCBOR v (cAddr, form, dataHash)

instance FromCanonicalCBOR V1 (Babbage.BabbageTxOut MaryEra) where
    fromCanonicalCBOR = do
        D.decodeTag >>= \case
            0 -> fmap (\(c, f) -> Babbage.TxOutCompact c f) <$> fromCanonicalCBOR
            1 -> fmap (\(a,b,c) -> Babbage.TxOutCompactDH a b c) <$> fromCanonicalCBOR
            t -> fail $ "Unknown BabbageTxOut tag: " <> show t


instance Typeable kr => ToCanonicalCBOR V1 (Credential kr) where
    toCanonicalCBOR v (ScriptHashObj sh) = toCanonicalCBOR v (0::Word8, sh)
    toCanonicalCBOR v (KeyHashObj kh) = toCanonicalCBOR v (1::Word8, kh)

instance Typeable kr => FromCanonicalCBOR V1 (Credential kr) where
    fromCanonicalCBOR = do
        tag <- fromCanonicalCBOR
        case unVer tag :: Word8 of
            0 -> fmap ScriptHashObj <$> fromCanonicalCBOR
            1 -> fmap KeyHashObj <$> fromCanonicalCBOR
            x -> fail $ "Unknown Credential tag: " <> show x

deriving via (LedgerCBOR v (Shelley.ShelleyTxOut MaryEra)) instance ToCanonicalCBOR v (Shelley.ShelleyTxOut MaryEra)
deriving via (LedgerCBOR v (Shelley.ShelleyTxOut MaryEra)) instance FromCanonicalCBOR v (Shelley.ShelleyTxOut MaryEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem MaryEra)) instance ToCanonicalCBOR v (AlonzoPlutusPurpose AsItem MaryEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem MaryEra)) instance FromCanonicalCBOR v (AlonzoPlutusPurpose AsItem MaryEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance ToCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance FromCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (MemPackCBOR (AlonzoScript ConwayEra)) instance ToCanonicalCBOR V1 (AlonzoScript ConwayEra)
deriving via (MemPackCBOR (AlonzoScript ConwayEra)) instance FromCanonicalCBOR V1 (AlonzoScript ConwayEra)
deriving via (MemPackCBOR (CompactForm a)) instance (MemPack (CompactForm a)) => ToCanonicalCBOR V1 (CompactForm a)
deriving via (MemPackCBOR (CompactForm a)) instance (MemPack (CompactForm a)) => FromCanonicalCBOR V1 (CompactForm a)
deriving via (MemPackCBOR CompactAddr) instance FromCanonicalCBOR V1 CompactAddr
deriving via (MemPackCBOR CompactAddr) instance ToCanonicalCBOR V1 CompactAddr
deriving via (MemPackCBOR Addr28Extra) instance FromCanonicalCBOR V1 Addr28Extra
deriving via (MemPackCBOR Addr28Extra) instance ToCanonicalCBOR V1 Addr28Extra
deriving via (LedgerCBOR v TxIn) instance FromCanonicalCBOR v TxIn
deriving via (LedgerCBOR v TxIn) instance ToCanonicalCBOR v TxIn
deriving via (MemPackCBOR DataHash32) instance FromCanonicalCBOR V1 DataHash32
deriving via (MemPackCBOR DataHash32) instance ToCanonicalCBOR V1 DataHash32
deriving via (MemPackCBOR (Timelock MaryEra)) instance ToCanonicalCBOR V1 (Timelock MaryEra)
deriving via (MemPackCBOR (Timelock MaryEra)) instance FromCanonicalCBOR V1 (Timelock MaryEra)
deriving via (LedgerCBOR v MaryValue) instance ToCanonicalCBOR v MaryValue
deriving via (LedgerCBOR v MaryValue) instance FromCanonicalCBOR v MaryValue


deriving via (LedgerCBOR v (KeyHash kr)) instance Typeable kr => ToCanonicalCBOR v (KeyHash kr)
deriving via (LedgerCBOR v (KeyHash kr)) instance Typeable kr => FromCanonicalCBOR v (KeyHash kr)
deriving via (LedgerCBOR v (ScriptHash)) instance FromCanonicalCBOR v ScriptHash
deriving via (LedgerCBOR v (ScriptHash)) instance ToCanonicalCBOR v ScriptHash
deriving via (LedgerCBOR v (Datum MaryEra)) instance ToCanonicalCBOR v (Datum MaryEra)
deriving via (LedgerCBOR v (Datum MaryEra)) instance FromCanonicalCBOR v (Datum MaryEra)
deriving via (LedgerCBOR v (Datum ConwayEra)) instance ToCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (Datum ConwayEra)) instance FromCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (BinaryData MaryEra)) instance ToCanonicalCBOR v (BinaryData MaryEra)
deriving via (LedgerCBOR v (BinaryData MaryEra)) instance FromCanonicalCBOR v (BinaryData MaryEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance ToCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance FromCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance ToCanonicalCBOR v ((SafeHash EraIndependentData))
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance FromCanonicalCBOR v ((SafeHash EraIndependentData))



