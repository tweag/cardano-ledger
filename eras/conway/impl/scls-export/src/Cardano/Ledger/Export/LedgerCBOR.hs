{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Cardano.Ledger.Export.LedgerCBOR
  ( LedgerCBOR(..)
  ) where

import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.CBOR.Canonical.Decoder
import Cardano.SCLS.Internal.Version
import Cardano.Ledger.Binary (EncCBOR(..), DecCBOR(..), toPlainEncoding, toPlainDecoder, natVersion)

-- | Helper that allows us to deriving instances via internal CBOR representation
newtype LedgerCBOR (v::Version) a = LedgerCBOR { unLedgerCBOR :: a }
    deriving (Eq, Show)

instance EncCBOR a => ToCanonicalCBOR v (LedgerCBOR v a) where
    toCanonicalCBOR _v (LedgerCBOR a) = toPlainEncoding (natVersion @9) (encCBOR a)

instance DecCBOR a => FromCanonicalCBOR v (LedgerCBOR v a) where
     fromCanonicalCBOR = Versioned . LedgerCBOR <$> toPlainDecoder Nothing (natVersion @9) decCBOR