{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as Imp

main :: IO ()
main = ledgerTestMain $ do
  describe "Export SCLS" $
    Imp.specWithScls "/home/joaosreis/projects/cardano-ledger/dumps" -- TODO: make this path configurable via an environment variable or command line argument
