{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.CanonicalState.Dump (
  dumpTx,
  getNextFile,
) where

import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (
  EncCBOR (encCBOR),
  toLazyByteString,
  toPlainEncoding,
 )
import Cardano.Ledger.Core (Tx, TxLevel (TopTx))
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Read (readMaybe)

dumpTx ::
  EncCBOR (Tx TopTx era) =>
  FilePath ->
  Version ->
  Tx TopTx era ->
  IO ()
dumpTx filepath version tx = do
  let e = encCBOR tx
  BSL.writeFile filepath (toLazyByteString (toPlainEncoding version e))

getNextFile :: FilePath -> String -> String -> IO FilePath
getNextFile dir prefix extension = do
  -- dir: "/path/to"
  -- prefix: "dump"
  let prefixT = T.pack prefix
  basenames <-
    map takeBaseName . filter (T.isPrefixOf prefixT . T.pack) <$> listDirectory dir
  -- basenames: ["dump-1", "dump-2", "dump-3", ...]
  -- Extract the numeric suffixes and find the maximum
  let counters = mapMaybe (T.stripPrefix (prefixT <> T.pack "-") . T.pack) basenames
      -- counters: ["1", "2", "3", ...]
      maxCounter = maximum ((0 :: Int) : mapMaybe (readMaybe . T.unpack) counters)
  -- maxCounter: 3 (if the existing files are dump-1.scls, dump-2.scls, dump-3.scls)
  pure (dir </> (T.unpack prefixT <> "-" <> show (maxCounter + 1) <> "." <> extension))
