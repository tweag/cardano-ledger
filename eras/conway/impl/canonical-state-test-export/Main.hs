{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as Imp

pathVarName :: String
pathVarName = "SCLS_EXPORT_PATH"

main :: IO ()
main =
  -- HSpec doesn't do well with extra CLI arguments, so we read the export path from an environment variable or use a default value
  lookupEnv pathVarName >>= \case
    Just path -> ledgerTestMain $ describe "Export SCLS" $ Imp.specWithScls path
    Nothing -> do
      putStrLn $
        "No export path provided, skipping SCLS export tests. Set the "
          <> pathVarName
          <> " environment variable to run them."
      exitFailure
