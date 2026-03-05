{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.BaseTypes (ProtVer (ProtVer))
import Cardano.Ledger.CanonicalState.Conway.Dump (dump, dumpLedgerState, dumpNewEpochState)
import Cardano.Ledger.CanonicalState.Dump (dumpTx)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraPParams (ppProtocolVersionL))
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lens.Micro ((^.))
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as Imp
import Test.Cardano.Ledger.Conway.ImpTest (
  ImpInit,
  LedgerSpec,
  modifyImpInitSclsDumpHook,
  withEachEraVersion,
 )

pathVarName :: String
pathVarName = "SCLS_EXPORT_PATH"

withScls ::
  FilePath -> SpecWith (ImpInit (LedgerSpec ConwayEra)) -> SpecWith (ImpInit (LedgerSpec ConwayEra))
withScls dir =
  modifyImpInitSclsDumpHook
    ( \nes tx res -> liftIO $ do
        createDirectoryIfMissing True dir
        dump dir "initial" $ dumpNewEpochState nes
        let ProtVer version _ = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
        dumpTx dir "txn" version tx
        case res of
          Left _failures -> do
            -- TODO: dump the failures
            pure ()
          Right (st, _) -> do
            dump dir "final" $ dumpLedgerState st -- TODO: this should be dumpNewEpochState, but we don't have the final NewEpochState available here.
    )

main :: IO ()
main = do
  -- HSpec doesn't do well with extra CLI arguments, so we read the export path from an environment variable or use a default value
  maybePath <- lookupEnv pathVarName
  ledgerTestMain $ do
    describe "Export SCLS" $
      case maybePath of
        Just path ->
          withEachEraVersion @ConwayEra $ withScls path $ Imp.conwayEraGenericSpec @ConwayEra
        Nothing ->
          runIO $ do
            expectationFailure $
              "No export path provided, skipping SCLS export tests. Set the "
                <> pathVarName
                <> " environment variable to run them."
