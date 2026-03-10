{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.CanonicalState.Conway.Export ()
import Cardano.Ledger.CanonicalState.Export (withScls)
import Cardano.Ledger.Conway (ConwayEra)
import GHC.IsList (IsList (toList))
import Lens.Micro ((.~))
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Imp (conwayEraGenericSpec)
import Test.Cardano.Ledger.Conway.ImpTest (
  iteSclsDumpBlockHookL,
  iteSclsDumpTxHookL,
  withEachEraVersion,
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
          withEachEraVersion @ConwayEra
            $ withScls @ConwayEra
              (iteSclsDumpTxHookL .~)
              ( \blockHook ->
                  iteSclsDumpBlockHookL
                    .~ (\slotNo st -> blockHook slotNo st . toList)
              )
              path
            $ conwayEraGenericSpec @ConwayEra
    Nothing -> do
      putStrLn $
        "No export path provided. Set the "
          <> pathVarName
          <> " environment variable to run them."
      exitFailure
