{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.CanonicalState.Conway.Export ()
import Cardano.Ledger.CanonicalState.Export (EraTestImp (ConwayEraTestImp), withScls)
import Cardano.Ledger.Conway (ConwayEra)
import Data.Proxy (Proxy (Proxy))
import Lens.Micro ((.~))
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Imp (conwayEraGenericSpec)
import Test.Cardano.Ledger.Conway.ImpTest (
  iteSclsDumpBlockHookL,
  iteSclsDumpTxHookL,
  withImpInitEachEraVersion,
 )

pathVarName :: String
pathVarName = "SCLS_EXPORT_PATH"

main :: IO ()
main =
  -- HSpec doesn't do well with extra CLI arguments, so we read the export path from an environment variable
  lookupEnv pathVarName >>= \case
    Just path ->
      ledgerTestMain $
        describe "Export SCLS" $ do
          withImpInitEachEraVersion (Proxy @ConwayEra)
            $ withScls @ConwayEra
              ConwayEraTestImp
              (iteSclsDumpTxHookL .~)
              (iteSclsDumpBlockHookL .~)
              path
            $ conwayEraGenericSpec (Proxy @ConwayEra)
    Nothing -> do
      putStrLn $
        "No export path provided. Set the "
          <> pathVarName
          <> " environment variable to run them."
      exitFailure
