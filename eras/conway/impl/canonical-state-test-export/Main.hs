module Main where

import System.Environment (lookupEnv)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as Imp

pathVarName :: String
pathVarName = "SCLS_EXPORT_PATH"

main :: IO ()
main = do
  -- HSpec doesn't do well with extra CLI arguments, so we read the export path from an environment variable or use a default value
  maybePath <- lookupEnv pathVarName
  ledgerTestMain $ do
    describe "Export SCLS" $
      case maybePath of
        Just path ->
          Imp.specWithScls path
        Nothing ->
          runIO $ do
            expectationFailure $
              "No export path provided, skipping SCLS export tests. Set the "
                <> pathVarName
                <> " environment variable to run them."
