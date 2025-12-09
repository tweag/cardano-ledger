{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Its a helper module, that is used to write canonical instances that
-- are lucky to match the current ledger implementation. There is no guarantee
-- that current ledger implementation will never diverge from the canonical
-- one. So it's important to run the scls conformance test for such instances.
module Cardano.Ledger.Conway.SCLS.LedgerCBOR (
  LedgerCBOR (..),
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  natVersion,
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..))
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.NamespaceCodec (unsafeToCanonicalDecoder, unsafeToCanonicalEncoding)
import Cardano.SCLS.Versioned
import GHC.TypeLits

-- | Helper that allows us to deriving instances via decodeTermToken CBOR representation
newtype LedgerCBOR (v :: Symbol) a = LedgerCBOR {unLedgerCBOR :: a}
  deriving (Eq, Show)

instance EncCBOR a => ToCanonicalCBOR v (LedgerCBOR v a) where
  toCanonicalCBOR _v (LedgerCBOR a) = unsafeToCanonicalEncoding $ toPlainEncoding (natVersion @9) (encCBOR a)

instance DecCBOR a => FromCanonicalCBOR v (LedgerCBOR v a) where
  fromCanonicalCBOR =
    Versioned . LedgerCBOR
      <$> (unsafeToCanonicalDecoder $ toPlainDecoder Nothing (natVersion @9) decCBOR)

