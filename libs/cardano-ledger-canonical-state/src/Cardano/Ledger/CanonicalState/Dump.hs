{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.CanonicalState.Dump (
  dumpTx,
) where

import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (
  EncCBOR (encCBOR),
  toLazyByteString,
  toPlainEncoding,
 )
import Cardano.Ledger.Core (Tx, TxLevel (TopTx))
import qualified Data.ByteString.Lazy as BSL

dumpTx ::
  EncCBOR (Tx TopTx era) =>
  FilePath ->
  Version ->
  Tx TopTx era ->
  IO ()
dumpTx filepath version tx = do
  let e = encCBOR tx
  BSL.writeFile filepath (toLazyByteString (toPlainEncoding version e))
