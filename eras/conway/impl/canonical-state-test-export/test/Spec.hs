{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.BaseTypes (EpochNo)
import Cardano.Ledger.CanonicalState.Conway.Export ()
import Cardano.Ledger.CanonicalState.Conway.Import ()
import Cardano.Ledger.CanonicalState.Export (
  ExportState (dumpLedgerState),
  Metadata (..),
  TestFixture (..),
  dump,
 )
import Cardano.Ledger.CanonicalState.Import (ImportCanonicalState (importCanonicalState))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.SCLS.Internal.Reader (withLatestManifestFrame)
import Cardano.SCLS.Internal.Record.Manifest (Manifest (..))
import Data.Aeson (decodeFileStrict)
import Data.Either (rights)
import GHC.IsList (IsList (toList))
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
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

dumpsPathVarName :: String
dumpsPathVarName = "SCLS_EXPORT_PATH"

main :: IO ()
main = do
  mDumpsPath <- lookupEnv dumpsPathVarName
  case mDumpsPath of
    Nothing ->
      hspec $
        describe "SCLS roundtrip import/export" $
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
discoverTestCases dumpsDir = findMetadataFiles dumpsDir []
  where
    findMetadataFiles :: FilePath -> [String] -> IO [SclsTestCase]
    findMetadataFiles dir pathSegments = do
      entries <- listDirectory dir
      let metadataFile = dir </> "metadata.json"
      casesHere <-
        doesFileExist metadataFile >>= \case
          True -> parseMetadataFile metadataFile dir pathSegments
          False -> pure []
      casesBelow <-
        concat
          <$> mapM
            ( \entry -> do
                let fullPath = dir </> entry
                isDir <- doesDirectoryExist fullPath
                if isDir
                  then findMetadataFiles fullPath (pathSegments ++ [entry])
                  else pure []
            )
            entries
      pure (casesHere ++ casesBelow)

    parseMetadataFile ::
      FilePath -> FilePath -> [String] -> IO [SclsTestCase]
    parseMetadataFile metadataFile dir pathSegments =
      decodeFileStrict metadataFile >>= \case
        Nothing -> do
          putStrLn $ "Warning: could not parse " ++ metadataFile
          pure []
        Just metadata ->
          pure $ concatMap (fixtureToTestCases dir pathSegments) (states metadata)

    fixtureToTestCases ::
      FilePath -> [String] -> TestFixture -> [SclsTestCase]
    fixtureToTestCases dir pathSegments fixture =
      map mkTestCase sclsFileNames
      where
        sclsFileNames =
          rights
            [Right (initialState fixture), finalState fixture]
        mkTestCase fileName =
          SclsTestCase
            { stcSclsFile = dir </> fileName
            , stcEpochNo = epochNo fixture
            , stcRelPath = pathSegments
            , stcLabel = fileName
            }

buildSpec :: [SclsTestCase] -> Spec
buildSpec testCases =
  describe "SCLS roundtrip import/export" $
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
      -- We want to ensure that the exported file is bitwise identical to the original one, but in case it's not, we at least want to check that the important properties are the same
      -- annotate "should have the same slotNo" $
      slotNo exportedManifest `shouldBe` slotNo originalManifest
      -- annotate "should have the same number of entries" $
      totalEntries exportedManifest `shouldBe` totalEntries originalManifest
      -- annotate "should have the same number of chunks" $
      totalChunks exportedManifest `shouldBe` totalChunks originalManifest
      -- annotate "should have the same nsInfo" $
      forM_ (zip (toList $ nsInfo exportedManifest) (toList $ nsInfo originalManifest)) $ uncurry shouldBe
      -- annotate "should have the same root hash" $
      rootHash originalManifest `shouldBe` rootHash exportedManifest
