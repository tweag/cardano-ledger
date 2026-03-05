{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.BaseTypes (ProtVer (ProtVer), Version)
import Cardano.Ledger.CanonicalState.Conway.Dump (dump, dumpLedgerState, dumpNewEpochState)
import Cardano.Ledger.CanonicalState.Dump (dumpTx)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraPParams (ppProtocolVersionL), eraProtVersions)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Data.Aeson (
  FromJSON,
  ToJSON (toEncoding),
  decodeFileStrict,
  defaultOptions,
  encode,
  genericToEncoding,
 )
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Imp (conwayEraGenericSpec)
import Test.Cardano.Ledger.Conway.ImpTest (
  ImpInit,
  LedgerSpec,
  iteSclsDumpHookL,
  modifyImpInitProtVer,
 )
import Test.Cardano.Ledger.Imp.Common (withImpInit)
import Test.Hspec.Core.Spec (Item (..), mapSpecItem_)
import Test.ImpSpec (ImpInit (impInitEnv))

data Metadata = Metadata
  { era :: String
  , protocolVersion :: Version
  , description :: String
  , stateCount :: Int
  }
  deriving (Generic, Show)

instance ToJSON Metadata where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Metadata

-- withScls ::
--   FilePath -> SpecWith (ImpInit (LedgerSpec ConwayEra)) -> SpecWith (ImpInit (LedgerSpec ConwayEra))
-- withScls dir =
--   modifyImpInitSclsDumpHook
--     ( \nes tx res -> liftIO $ do
--         dump dir "initial" $ dumpNewEpochState nes
--         let ProtVer version _ = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
--         dumpTx dir "txn" version tx
--         case res of
--           Left _failures -> do
--             -- TODO: dump the failures
--             pure ()
--           Right (st, _) -> do
--             dump dir "final" $ dumpLedgerState st -- TODO: this should be dumpNewEpochState, but we don't have the final NewEpochState available here.
--     )

withScls ::
  Version ->
  FilePath ->
  SpecWith (ImpInit (LedgerSpec ConwayEra)) ->
  SpecWith (ImpInit (LedgerSpec ConwayEra))
withScls protocolVersion baseDir =
  mapSpecItem_ $
    \item@Item
       { itemRequirement
       , itemExample = originalItemExample
       } ->
        item
          { itemExample = \p f ->
              originalItemExample p $ \action ->
                f $ \impInit ->
                  action
                    ( impInit
                        { impInitEnv =
                            impInitEnv impInit
                              & iteSclsDumpHookL
                                .~ hook itemRequirement
                        }
                    )
          }
  where
    hook description nes tx res = do
      let dir = baseDir </> ("Protocol " <> show protocolVersion) </> description
      let metadataFile = dir </> "metadata.json"
      ctx@Metadata {stateCount} <-
        doesFileExist metadataFile >>= \metadataExists ->
          if metadataExists
            then
              decodeFileStrict metadataFile >>= \case
                Just ctx -> pure ctx
                Nothing -> do
                  -- TODO: clean up the directory if the metadata file is corrupted, to avoid leaving around junk files?
                  error $ "Failed to decode metadata file: " <> metadataFile
            else pure $ Metadata {era = "Conway", protocolVersion, description, stateCount = 0}
      createDirectoryIfMissing True dir
      dump (dir </> ("initial-" <> show stateCount <> ".scls")) $ dumpNewEpochState nes
      let ProtVer version _ = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
      dumpTx (dir </> ("txn-" <> show stateCount <> ".cbor")) version tx
      case res of
        Left _failures -> do
          -- TODO: dump the failures
          pure ()
        Right (st, _) -> dump (dir </> ("final-" <> show stateCount <> ".scls")) $ dumpLedgerState st -- TODO: this should be dumpNewEpochState, but we don't have the final NewEpochState available here.
      BSL.writeFile metadataFile $ encode $ ctx {stateCount = stateCount + 1}

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
