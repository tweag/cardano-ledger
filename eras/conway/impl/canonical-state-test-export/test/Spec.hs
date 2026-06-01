{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.CanonicalState.Conway.Export ()
import Cardano.Ledger.CanonicalState.Conway.Import ()
import Cardano.Ledger.CanonicalState.Export (
  ExportCanonicalState (dumpLedgerState),
  Metadata (..),
  StateTransition (..),
  dump,
  getTestDirFromMetadata,
 )
import Cardano.Ledger.CanonicalState.Import (ImportCanonicalState (importCanonicalState))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.SCLS.Internal.Reader (withLatestManifestFrame)
import Cardano.SCLS.Internal.Record.Manifest (Manifest (..))
import Data.Aeson (decodeFileStrict)
import Data.Either (rights)
import GHC.IsList (IsList (toList))
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Cardano.Ledger.Common (
  Spec,
  describe,
  forM_,
  hspec,
  it,
  parallel,
  pendingWith,
  shouldBe,
  when,
 )
import Debug.Trace (traceM)

dumpsPathVarName :: String
dumpsPathVarName = "SCLS_EXPORT_PATH"

main :: IO ()
main = do
  mDumpsPath <- lookupEnv dumpsPathVarName
  case mDumpsPath of
    Nothing ->
      hspec $
        describe "SCLS roundtrip import export" $
          it ("requires " ++ dumpsPathVarName ++ " env var") $
            pendingWith (dumpsPathVarName ++ " not set")
    Just dumpsPath -> do
      testCases <- discoverTestCases dumpsPath
      hspec $ parallel $ buildSpec testCases

data SclsTestCase = SclsTestCase
  { stcSclsFile :: FilePath
  , stcEpochNo :: EpochNo
  , stcRelPath :: [String]
  , stcLabel :: String
  }

discoverTestCases :: FilePath -> IO [SclsTestCase]
discoverTestCases dumpsDir =
  decodeFileStrict metadataFile >>= \case
    Nothing -> do
      putStrLn $ "Warning: could not parse " ++ metadataFile
      pure []
    Just (metadata :: [Metadata]) ->
      pure $
        concatMap metadataToTestCases metadata
  where
    metadataFile = dumpsDir </> "metadata.json"
    metadataToTestCases :: Metadata -> [SclsTestCase]
    metadataToTestCases metadata =
      concatMap (\t -> map (mkTestCase t) $ sclsFileNames t) (stateTransitions metadata)
      where
        sclsFileNames transition =
          rights
            [Right (initialState transition), finalState transition]
        mkTestCase transition fileName =
          SclsTestCase
            { stcSclsFile = dumpsDir </> getTestDirFromMetadata metadata </> fileName
            , stcEpochNo = epochNo transition
            , stcRelPath = show (protocolVersion metadata) : path metadata ++ [description metadata]
            , stcLabel = fileName
            }

buildSpec :: [SclsTestCase] -> Spec
buildSpec testCases =
  describe "SCLS roundtrip import export" $
    forM_ testCases $ \tc ->
      foldr
        describe
        ( it (stcLabel tc) $
            roundtripScls tc
        )
        (stcRelPath tc)

roundtripScls :: SclsTestCase -> IO ()
roundtripScls SclsTestCase {..} =
  withSystemTempDirectory "scls-roundtrip" $ \tmpDir -> do
    (s, nes) <- importCanonicalState @ConwayEra stcSclsFile stcEpochNo
    let exportedFile = tmpDir </> stcLabel
    Right () <- dump exportedFile s (dumpLedgerState @ConwayEra nes)
    originalManifest <- withLatestManifestFrame pure stcSclsFile
    exportedManifest <- withLatestManifestFrame pure exportedFile
    when (rootHash exportedManifest /= rootHash originalManifest) $ do
      forM_ (zip (toList $ nsInfo exportedManifest) (toList $ nsInfo originalManifest)) $ uncurry shouldBe
