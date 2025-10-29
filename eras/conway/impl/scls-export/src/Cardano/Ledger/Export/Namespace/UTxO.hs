{-# LANGUAGE ScopedTypeVariables #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans -Werror #-}
-- | UTxO namespace export.
module Cardano.Ledger.Export.Namespace.UTxO
  ( UtxoKey(..)
  , UtxoOut(..)
  , Version(..)
  , ToCanonicalCBOR(..)
  , FromCanonicalCBOR(..)
  ) where

import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.Ledger.Binary (decodeMemPack, encodeMemPack, toPlainEncoding, shelleyProtVer, toPlainDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Decoding as D
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.TxIn (TxIn(..), TxId(..))
import Cardano.Ledger.Compactible
import Cardano.Ledger.Address
import Cardano.Ledger.Hashes
import Cardano.Ledger.Plutus.Data (Datum(..))
import Cardano.Ledger.Plutus.Data (BinaryData)
import Cardano.Ledger.Mary (MaryValue)
import Cardano.SCLS.Internal.Entry.IsKey
import Cardano.SCLS.Internal.Version
import Cardano.SCLS.Internal.NamespaceCodec
import Data.Proxy
-- import Cardano.SCLS.Internal.Entry
import qualified Cardano.Ledger.Shelley.TxOut as Shelley
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import Cardano.Ledger.Export.Common ()
import Cardano.Ledger.Allegra.Scripts (Timelock(..))
import Data.MemPack
import Cardano.Ledger.Alonzo.TxOut (DataHash32, Addr28Extra)
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose, AsItem, AlonzoScript(..))
import Cardano.Ledger.Export.LedgerCBOR

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
    keySize = namespaceKeySize @"utxo/v0"
    packKeyM (UtxoKeyIn (TxIn (TxId a) b)) = do
        packByteStringM (originalBytes a)
        packM b
    unpackKeyM = do
        a <- unpackM -- FIXME read bytestirng and create unsafe hash
        b <- unpackM
        return $ UtxoKeyIn (TxIn a b)

-- | Output key that is used in utxo namespace
--
-- Here we follow the current spec, but after benchmarks we can decide that this representation
-- is not efficient and we can replace it with the implementation based on the compact values
data UtxoOut
   = UtxoOutShelley (Shelley.ShelleyTxOut ConwayEra)
   | UtxoOutBabbage (Babbage.BabbageTxOut ConwayEra)
   deriving (Show)

instance ToCanonicalCBOR V1 UtxoOut where
    toCanonicalCBOR v (UtxoOutShelley shelleyOut) = toCanonicalCBOR v (E.encodeInt 0, toCanonicalCBOR v shelleyOut)
    toCanonicalCBOR v (UtxoOutBabbage babbageOut) = toCanonicalCBOR v (E.encodeInt 1, toCanonicalCBOR v babbageOut)

instance FromCanonicalCBOR V1 UtxoOut where
    fromCanonicalCBOR = do
        D.decodeListLenCanonicalOf 2
        Versioned (tag :: Int) <- fromCanonicalCBOR
        case tag of
            0 -> fmap UtxoOutShelley <$> fromCanonicalCBOR
            1 -> fmap UtxoOutBabbage <$> fromCanonicalCBOR
            _ -> fail "Invalid UtxoOut tag"

instance ToCanonicalCBOR V1 (Babbage.BabbageTxOut ConwayEra) where
    toCanonicalCBOR v (Babbage.TxOutCompact cAddr form) =
        E.encodeMapLen 2
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v form
    -- toCanonicalCBOR v (Babbage.TxOut_AddrHash28_AdaOnly staking hash28 compactForm) =
    --     let cAddr = unCompactAddr (compactAddr (decodeAddress28 staking hash28))
    --     in E.encodeMapLen 2
    --         <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
    --         <> E.encodeInt 1 <> toCanonicalCBOR v compactForm
    -- toCanonicalCBOR v (Babbage.TxOut_AddrHash28_AdaOnly_DataHash32 staking hash28 compact dataHash) =
    --     let cAddr = unCompactAddr (compactAddr (decodeAddress28 staking hash28))
    --     in E.encodeMapLen 3
    --         <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
    --         <> E.encodeInt 1 <> toCanonicalCBOR v compact
    --         <> E.encodeInt 2 <> toCanonicalCBOR v (0::Int, dataHash)
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
    toCanonicalCBOR v (Babbage.TxOutCompactDatum cAddr form inlineDatum) =
        E.encodeMapLen 3
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v form
            <> E.encodeInt 2
                <> case inlineDatum of
                     binaryData -> toCanonicalCBOR v (1::Int, toCanonicalCBOR v (LedgerCBOR @V1 binaryData))
    toCanonicalCBOR v (Babbage.TxOutCompactDH cAddr form datum) =
        E.encodeMapLen 3
            <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
            <> E.encodeInt 1 <> toCanonicalCBOR v form
            <> E.encodeInt 2
                <> case datum of
                    hash_ -> toCanonicalCBOR v (0::Int, originalBytes hash_)

instance FromCanonicalCBOR V1 (Babbage.BabbageTxOut ConwayEra) where
    fromCanonicalCBOR = do
        l <- D.decodeMapLenCanonical
        D.decodeWordCanonicalOf 0
        Versioned cAddr <- fromCanonicalCBOR @V1
        D.decodeWordCanonicalOf 1
        Versioned form <- fromCanonicalCBOR
        (datumPart, script)
          <- if l == 2
             then return (NoDatum, Nothing)
             else do
                n <- D.decodeIntCanonical
                case n of
                    2 -> do
                        Versioned datum <- fromCanonicalCBOR
                        if l==4
                        then do
                            D.decodeWordCanonicalOf 3
                            Versioned script <- fromCanonicalCBOR @V1
                            return (datum, Just script)
                        else return (datum, Nothing)
                    3 -> do
                        Versioned script <- fromCanonicalCBOR @V1
                        return (NoDatum, Just script)
                    _ -> fail "Invalid Datum tag"
        case (datumPart, script) of
            (NoDatum, Nothing) -> return $ Versioned (Babbage.TxOutCompact cAddr form)
            (NoDatum, Just scr) -> return $ Versioned (Babbage.TxOutCompactRefScript cAddr form NoDatum scr)
            (datum, Just scr) -> return $ Versioned (Babbage.TxOutCompactRefScript cAddr form datum scr)
            (DatumHash hsh, Nothing) -> return $ Versioned (Babbage.TxOutCompactDH cAddr form hsh)
            (Datum binaryData, Nothing) -> return $ Versioned (Babbage.TxOutCompactDatum cAddr form binaryData)


deriving via (LedgerCBOR v (Shelley.ShelleyTxOut ConwayEra)) instance ToCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)
deriving via (LedgerCBOR v (Shelley.ShelleyTxOut ConwayEra)) instance FromCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance ToCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance FromCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (MemPackCBOR (AlonzoScript ConwayEra)) instance ToCanonicalCBOR V1 (AlonzoScript ConwayEra)
deriving via (MemPackCBOR (AlonzoScript ConwayEra)) instance FromCanonicalCBOR V1 (AlonzoScript ConwayEra)

deriving via (LedgerCBOR v MaryValue) instance ToCanonicalCBOR v MaryValue
deriving via (LedgerCBOR v MaryValue) instance FromCanonicalCBOR v MaryValue

instance {-# OVERLAPPING #-} ToCanonicalCBOR version (CompactForm MaryValue) where
    toCanonicalCBOR version v = toCanonicalCBOR version (fromCompact v)
instance {-# OVERLAPPING #-} FromCanonicalCBOR version (CompactForm MaryValue) where
    fromCanonicalCBOR = do
        Versioned v <- fromCanonicalCBOR
        Just v' <- pure (toCompact v)
        pure $ Versioned v'

deriving via (MemPackCBOR CompactAddr) instance ToCanonicalCBOR V1 CompactAddr
deriving via (MemPackCBOR CompactAddr) instance FromCanonicalCBOR V1 CompactAddr
deriving via (MemPackCBOR Addr28Extra) instance FromCanonicalCBOR V1 Addr28Extra
deriving via (MemPackCBOR Addr28Extra) instance ToCanonicalCBOR V1 Addr28Extra
deriving via (LedgerCBOR v TxIn) instance FromCanonicalCBOR v TxIn
deriving via (LedgerCBOR v TxIn) instance ToCanonicalCBOR v TxIn
deriving via (MemPackCBOR DataHash32) instance FromCanonicalCBOR V1 DataHash32
deriving via (MemPackCBOR DataHash32) instance ToCanonicalCBOR V1 DataHash32
deriving via (MemPackCBOR (Timelock ConwayEra)) instance ToCanonicalCBOR V1 (Timelock ConwayEra)
deriving via (MemPackCBOR (Timelock ConwayEra)) instance FromCanonicalCBOR V1 (Timelock ConwayEra)


deriving via (LedgerCBOR v (Datum ConwayEra)) instance ToCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (Datum ConwayEra)) instance FromCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance ToCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance FromCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance ToCanonicalCBOR v ((SafeHash EraIndependentData))
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance FromCanonicalCBOR v ((SafeHash EraIndependentData))


type instance NamespaceKeySize "utxo/v0" = 34

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoKey
  type NamespaceEntry "utxo/v0" = UtxoOut

instance CanonicalCBOREntryEncoder "utxo/v0" UtxoOut where
  encodeEntry n = toCanonicalCBOR (Proxy @V1) n

instance CanonicalCBOREntryDecoder "utxo/v0" UtxoOut where
  decodeEntry = VersionedNS . unVer @V1 <$> fromCanonicalCBOR
