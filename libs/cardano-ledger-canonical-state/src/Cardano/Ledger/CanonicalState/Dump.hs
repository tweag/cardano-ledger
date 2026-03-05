{-# LANGUAGE DataKinds #-}
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
  String ->
  Version ->
  Tx TopTx era ->
  IO ()
dumpTx dir prefix version tx = do
  let e = encCBOR tx
  filepath <- getNextFile dir prefix "cbor"
  BSL.writeFile filepath (toLazyByteString (toPlainEncoding version e))
