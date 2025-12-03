{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Werror #-}
-- | UTxO namespace export.
module Cardano.Ledger.Conway.SCLS.Namespace.UTxO
  ( UtxoKey(..)
  , UtxoOut(..)
  , Version(..)
  , ToCanonicalCBOR(..)
  , FromCanonicalCBOR(..)
  ) where

import Cardano.Ledger.Address
import Cardano.Ledger.Allegra.Scripts (Timelock(..), TimelockRaw(..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoPlutusPurpose, AsItem, AlonzoScript(..), decodePlutusScript)
import Cardano.Ledger.Alonzo.TxOut (DataHash32, Addr28Extra)
import Cardano.Ledger.Babbage.TxOut qualified as Babbage
import Cardano.Ledger.Binary (decodeMemPack, encodeMemPack, toPlainEncoding, toPlainDecoder, natVersion)
import Cardano.Ledger.Compactible
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.SCLS.Common ()
import Cardano.Ledger.Conway.SCLS.LedgerCBOR
import Cardano.Ledger.Conway.Scripts ()
import Cardano.Ledger.Hashes
import Cardano.Ledger.Mary (MaryValue)
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Plutus.Data (BinaryData)
import Cardano.Ledger.Plutus.Data (Datum(..))
import Cardano.Ledger.Plutus.Language
import Cardano.Ledger.Shelley.TxOut qualified as Shelley
import Cardano.Ledger.TxIn (TxIn(..), TxId(..))
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.Internal.Entry.IsKey
import Cardano.SCLS.Internal.NamespaceCodec
import Cardano.SCLS.Internal.Version
import Codec.CBOR.Decoding qualified as D
import Codec.CBOR.Encoding qualified as E
import Data.Maybe.Strict
import Data.MemPack
import Data.Proxy
import GHC.Generics (Generic)

newtype MemPackCBOR a = MemPackCBOR { unMemPackCBOR :: a }
  deriving (Eq, Show)

instance (MemPack a) => ToCanonicalCBOR "utxo/v0" (MemPackCBOR a) where
  toCanonicalCBOR _v (MemPackCBOR a) = toPlainEncoding (natVersion @9) (encodeMemPack a)

instance (MemPack a) => FromCanonicalCBOR "utxo/v0" (MemPackCBOR a) where
  fromCanonicalCBOR = Versioned . MemPackCBOR <$> toPlainDecoder Nothing (natVersion @9) decodeMemPack

-- | Input wrapper for the keys that are used in utxo namespace
data UtxoKey
  = UtxoKeyIn TxIn
  deriving (Show)
  deriving (Generic)

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
   deriving (Eq)
   deriving (Generic)



instance ToCanonicalCBOR "utxo/v0" UtxoKey where
  toCanonicalCBOR v (UtxoKeyIn txIn) = E.encodeInt 0 <> toCanonicalCBOR v txIn


instance ToCanonicalCBOR "utxo/v0" UtxoOut where
  toCanonicalCBOR v (UtxoOutShelley shelleyOut) = toCanonicalCBOR v (E.encodeInt 0, toCanonicalCBOR v shelleyOut)
  toCanonicalCBOR v (UtxoOutBabbage babbageOut) = toCanonicalCBOR v (E.encodeInt 1, toCanonicalCBOR v babbageOut)

instance FromCanonicalCBOR "utxo/v0" UtxoOut where
  fromCanonicalCBOR = do
    D.decodeListLenCanonicalOf 2
    Versioned (tag :: Int) <- fromCanonicalCBOR
    case tag of
      0 -> fmap UtxoOutShelley <$> fromCanonicalCBOR
      1 -> fmap UtxoOutBabbage <$> fromCanonicalCBOR
      _ -> fail "Invalid UtxoOut tag"

instance ToCanonicalCBOR "utxo/v0" (Babbage.BabbageTxOut ConwayEra) where
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
          Datum binaryData -> Just (toCanonicalCBOR v (1:: Int, toCanonicalCBOR v (LedgerCBOR @"utxo/v0" binaryData)))
    in E.encodeMapLen (3 + (case datumEncoding of Just{} -> 1 ; Nothing -> 0))
          <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
          <> E.encodeInt 1 <> toCanonicalCBOR v form
          <> case datumEncoding of
               Nothing -> mempty
               Just enc -> E.encodeInt 2 <> enc
          <> E.encodeInt 3 <> toCanonicalCBOR v script
  toCanonicalCBOR v (Babbage.TxOutCompactDatum cAddr form inlineDatum) =
    E.encodeMapLen 3
       <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
       <> E.encodeInt 1 <> toCanonicalCBOR v form
       <> E.encodeInt 2
           <> case inlineDatum of
                binaryData -> toCanonicalCBOR v (1::Int, toCanonicalCBOR v (LedgerCBOR @"utxo/v0" binaryData))
  toCanonicalCBOR v (Babbage.TxOutCompactDH cAddr form datum) =
    E.encodeMapLen 3
       <> E.encodeInt 0 <> toCanonicalCBOR v cAddr
       <> E.encodeInt 1 <> toCanonicalCBOR v form
       <> E.encodeInt 2
           <> case datum of
               hash_ -> toCanonicalCBOR v (0::Int, originalBytes hash_)

instance FromCanonicalCBOR "utxo/v0" (Babbage.BabbageTxOut ConwayEra) where
  fromCanonicalCBOR = do
    l <- D.decodeMapLenCanonical
    D.decodeWordCanonicalOf 0
    Versioned cAddr <- fromCanonicalCBOR @"utxo/v0"
    D.decodeWordCanonicalOf 1
    Versioned form <- fromCanonicalCBOR
    (datumPart, script)
      <- if l == 2
         then return (NoDatum, SNothing)
         else do
           n <- D.decodeIntCanonical
           case n of
             2 -> do
               Versioned datum <- fromCanonicalCBOR
               if l==4
               then do
                   D.decodeWordCanonicalOf 3
                   Versioned script <- fromCanonicalCBOR @"utxo/v0"
                   return (datum, SJust script)
               else return (datum, SNothing)
             3 -> do
               Versioned script <- fromCanonicalCBOR @"utxo/v0"
               return (NoDatum, SJust script)
             _ -> fail "Invalid Datum tag"
    case (datumPart, script) of
      (NoDatum, SNothing) -> return $ Versioned (Babbage.TxOutCompact cAddr form)
      (NoDatum, SJust scr) -> return $ Versioned (Babbage.TxOutCompactRefScript cAddr form NoDatum scr)
      (datum, SJust scr) -> return $ Versioned (Babbage.TxOutCompactRefScript cAddr form datum scr)
      (DatumHash hsh, SNothing) -> return $ Versioned (Babbage.TxOutCompactDH cAddr form hsh)
      (Datum binaryData, SNothing) -> return $ Versioned (Babbage.TxOutCompactDatum cAddr form binaryData)


deriving via (LedgerCBOR v (Shelley.ShelleyTxOut ConwayEra)) instance ToCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)
deriving via (LedgerCBOR v (Shelley.ShelleyTxOut ConwayEra)) instance FromCanonicalCBOR v (Shelley.ShelleyTxOut ConwayEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance ToCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (LedgerCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)) instance FromCanonicalCBOR v (AlonzoPlutusPurpose AsItem ConwayEra)
deriving via (LedgerCBOR v (AlonzoScript ConwayEra)) instance ToCanonicalCBOR v (AlonzoScript ConwayEra)

instance FromCanonicalCBOR v (AlonzoScript ConwayEra) where
  fromCanonicalCBOR = do
    D.decodeListLenCanonicalOf 2
    w <- D.decodeWord8Canonical
    case w of
      0 -> fmap (NativeScript) <$> fromCanonicalCBOR
      1 -> Versioned . PlutusScript <$> toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV1)
      2 -> Versioned . PlutusScript <$> toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV2)
      3 -> Versioned . PlutusScript <$> toPlainDecoder Nothing (natVersion @9) (decodePlutusScript @ConwayEra SPlutusV3)
      n -> fail ("Unknown tag: " <> show n)

instance FromCanonicalCBOR v (Timelock ConwayEra) where
  fromCanonicalCBOR = do
    Versioned raw <- fromCanonicalCBOR
    return $ Versioned $ mkMemoizedEra @ConwayEra (raw :: TimelockRaw ConwayEra)

instance FromCanonicalCBOR v (TimelockRaw ConwayEra) where
  fromCanonicalCBOR = do
    k <- D.decodeListLenCanonical
    n <- D.decodeWord8Canonical
    case n of
      0 -> fmap TimelockSignature <$> fromCanonicalCBOR
      1 -> fmap TimelockAllOf <$> fromCanonicalCBOR
      2 -> fmap TimelockAnyOf <$> fromCanonicalCBOR
      3 | k == 3 -> do
          Versioned f <- fromCanonicalCBOR
          Versioned g <- fromCanonicalCBOR
          return $ Versioned $ TimelockMOf f g
      4 -> fmap TimelockTimeStart <$> fromCanonicalCBOR
      5 -> fmap TimelockTimeExpire <$> fromCanonicalCBOR
      m -> fail $ "Invalid tag: " <> show m

deriving via (LedgerCBOR v MaryValue) instance ToCanonicalCBOR v MaryValue
deriving via (LedgerCBOR v MaryValue) instance FromCanonicalCBOR v MaryValue

instance {-# OVERLAPPING #-} ToCanonicalCBOR version (CompactForm MaryValue) where
  toCanonicalCBOR version v = toCanonicalCBOR version (fromCompact v)
instance {-# OVERLAPPING #-} FromCanonicalCBOR version (CompactForm MaryValue) where
  fromCanonicalCBOR = do
    Versioned v <- fromCanonicalCBOR
    Just v' <- pure (toCompact v)
    pure $ Versioned v'

deriving via (MemPackCBOR CompactAddr) instance ToCanonicalCBOR "utxo/v0" CompactAddr
deriving via (MemPackCBOR CompactAddr) instance FromCanonicalCBOR "utxo/v0" CompactAddr
deriving via (MemPackCBOR Addr28Extra) instance FromCanonicalCBOR "utxo/v0" Addr28Extra
deriving via (MemPackCBOR Addr28Extra) instance ToCanonicalCBOR "utxo/v0" Addr28Extra
deriving via (LedgerCBOR v TxIn) instance FromCanonicalCBOR v TxIn
deriving via (LedgerCBOR v TxIn) instance ToCanonicalCBOR v TxIn
deriving via (MemPackCBOR DataHash32) instance FromCanonicalCBOR "utxo/v0" DataHash32
deriving via (MemPackCBOR DataHash32) instance ToCanonicalCBOR "utxo/v0" DataHash32
deriving via (LedgerCBOR v (Timelock ConwayEra)) instance ToCanonicalCBOR v (Timelock ConwayEra)
deriving via (LedgerCBOR v (Datum ConwayEra)) instance ToCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (Datum ConwayEra)) instance FromCanonicalCBOR v (Datum ConwayEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance ToCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (BinaryData ConwayEra)) instance FromCanonicalCBOR v (BinaryData ConwayEra)
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance ToCanonicalCBOR v ((SafeHash EraIndependentData))
deriving via (LedgerCBOR v (SafeHash EraIndependentData)) instance FromCanonicalCBOR v ((SafeHash EraIndependentData))

instance KnownNamespace "utxo/v0" where
  type NamespaceKey "utxo/v0" = UtxoKey
  type NamespaceEntry "utxo/v0" = UtxoOut

instance CanonicalCBOREntryEncoder "utxo/v0" UtxoOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"utxo/v0") n

instance CanonicalCBOREntryDecoder "utxo/v0" UtxoOut where
  decodeEntry = fromCanonicalCBOR
