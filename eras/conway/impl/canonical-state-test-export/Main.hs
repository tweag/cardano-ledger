{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (eraProtVersions)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.CanonicalState.Dump (withScls)
import Test.Cardano.Ledger.Conway.Imp (conwayEraGenericSpec)
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common (withImpInit)

pathVarName :: String
pathVarName = "SCLS_EXPORT_PATH"

main :: IO ()
main =
  -- HSpec doesn't do well with extra CLI arguments, so we read the export path from an environment variable
  lookupEnv pathVarName >>= \case
    Just path ->
      ledgerTestMain $
        describe "Export SCLS" $ do
          withImpInit @(LedgerSpec ConwayEra) $ do
            forM_ (eraProtVersions @ConwayEra) $ \protVer ->
              describe ("Protocol " <> show protVer) $
                modifyImpInitProtVer protVer $
                  withScls protVer path $
                    conwayEraGenericSpec @ConwayEra
    Nothing -> do
      putStrLn $
        "No export path provided, skipping SCLS export tests. Set the "
          <> pathVarName
          <> " environment variable to run them."
      exitFailure
