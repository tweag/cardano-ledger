{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Cardano.Ledger.BaseTypes (
  Globals,
  ProtVer (ProtVer),
  SlotNo,
  TxIx (TxIx),
 )
import Cardano.Ledger.Binary (decodeFull, serialize)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.CanonicalState.Conway.Export ()
import Cardano.Ledger.CanonicalState.Conway.Import ()
import Cardano.Ledger.CanonicalState.Export (
  ExportState (dumpLedgerState),
  Metadata (..),
  TestFixture (..),
  TxOrBlock (..),
  dump,
  toGlobals,
 )
import Cardano.Ledger.CanonicalState.Import (
  InMemoryTestFixture (InMemoryTestFixture, imtfFinalState, imtfInitialState, imtfTransactions),
  loadInMemoryTestFixture,
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.State (CanSetChainAccountState (chainAccountStateL))
import Cardano.Ledger.Core (
  BlockIssuer,
  EraBlockBody (hashBlockBody, mkBasicBlockBody, txSeqBlockBodyL),
  EraPParams (ppProtocolVersionL),
  EraRule,
  EraTx (Tx),
  KeyHash,
  TopTx,
  bBodySize,
  eraProtVerLow,
 )
import Cardano.Ledger.Shelley.API (
  BlockTransitionError (BlockTransitionError),
  applyBlockEither,
 )
import Cardano.Ledger.Shelley.LedgerState (NewEpochState, curPParamsEpochStateL, esLStateL, nesEsL)
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  epochFromSlot,
 )
import Cardano.SCLS.Internal.Reader (withLatestManifestFrame)
import Cardano.SCLS.Internal.Record.Manifest (Manifest (nsInfo, rootHash))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition (
  ApplySTSOpts (..),
  AssertionPolicy (AssertionsAll),
  STS (..),
  SingEP (EPReturn),
  TRC (TRC),
  ValidationPolicy (ValidateAll),
  applySTSOptsEither,
 )
import Data.Aeson (decodeFileStrict)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Function ((&))
import Data.Sequence.Strict (StrictSeq)
import GHC.Base (NonEmpty, when)
import GHC.IsList (IsList (toList))
import Lens.Micro ((.~), (^.))
import System.Directory (
  doesDirectoryExist,
  doesFileExist,
  listDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Cardano.Ledger.BlockHeader (TestBlockHeader (..))
import Test.Cardano.Ledger.Common (
  Spec,
  describe,
  expectationFailure,
  forM_,
  hspec,
  it,
  parallel,
  pendingWith,
  shouldBe,
 )
import Test.Cardano.Slotting.Numeric ()

dumpsPathVarName :: String
dumpsPathVarName = "SCLS_EXPORT_PATH"

main :: IO ()
main = do
  mDumpsPath <- lookupEnv dumpsPathVarName
  case mDumpsPath of
    Nothing ->
      hspec $
        describe "Black-box test runner" $
          it ("requires " ++ dumpsPathVarName ++ " env var") $
            pendingWith (dumpsPathVarName ++ " not set")
    Just dumpsPath -> do
      testCases <- discoverTestCases dumpsPath
      hspec $ parallel $ buildSpec testCases

data SclsTestCase = SclsTestCase
  { stcMetadata :: Metadata
  , stcDir :: FilePath
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
          True -> parseMetadataFile metadataFile dir
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
      FilePath -> FilePath -> IO [SclsTestCase]
    parseMetadataFile metadataFile dir =
      decodeFileStrict metadataFile >>= \case
        Nothing -> do
          putStrLn $ "Warning: could not parse " ++ metadataFile
          pure []
        Just metadata ->
          pure [SclsTestCase {stcMetadata = metadata, stcDir = dir}]

buildSpec :: [SclsTestCase] -> Spec
buildSpec testCases =
  describe "Black-box test runner" $
    forM_ testCases $ \tc@SclsTestCase {stcMetadata = Metadata {..}} ->
      describe ("Era: " <> era <> ", Imp: " <> eraImp <> ", Protocol version: " <> show protocolVersion) $
        foldr
          describe
          (describe description $ runTest tc)
          path

runTest :: SclsTestCase -> Spec
runTest SclsTestCase {stcMetadata, stcDir} =
  forM_ (states stcMetadata) $ \t@TestFixture {..} ->
    it ("apply txn/block to " ++ initialState) $
      withSystemTempDirectory "blackbox-test-runner" $ \tmpDir -> do
        runExceptT (loadInMemoryTestFixture @ConwayEra stcDir (protocolVersion stcMetadata) t) >>= \case
          Left err ->
            expectationFailure $ "Failed to deserialise transactions: " ++ show err
          Right inMemoryTestFixture ->
            applyTestFixture stcMetadata inMemoryTestFixture >>= \computedRes ->
              case (imtfFinalState inMemoryTestFixture, computedRes) of
                (Left (OrBlock expectedFailures), Left (OrBlock computedFailures)) ->
                  decodeFull (protocolVersion stcMetadata) (serialize (protocolVersion stcMetadata) computedFailures)
                    `shouldBe` Right expectedFailures
                (Left (OrBlock _), Left (OrTx _)) ->
                  expectationFailure "Expected block failures, but got an unexpected tx failure"
                (Left (OrTx expectedFailures), Left (OrTx computedFailures)) ->
                  decodeFull (protocolVersion stcMetadata) (serialize (protocolVersion stcMetadata) computedFailures)
                    `shouldBe` Right expectedFailures
                (Left (OrTx _), Left (OrBlock _)) ->
                  expectationFailure "Expected tx failures, but got an unexpected block failure"
                (Right expectedSclsFilePath, Right (computedNes, computedSlotNo)) -> do
                  let exportedFile = tmpDir </> ("computed-" <> expectedSclsFilePath)
                  Right () <- dump exportedFile computedSlotNo (dumpLedgerState @ConwayEra computedNes)
                  flip withLatestManifestFrame (stcDir </> expectedSclsFilePath) $ \originalManifest ->
                    flip withLatestManifestFrame exportedFile $ \exportedManifest -> do
                      when (rootHash exportedManifest /= rootHash originalManifest) $ do
                        forM_ (zip (toList $ nsInfo exportedManifest) (toList $ nsInfo originalManifest)) $
                          \(exportedNsInfo, originalNsInfo) -> do
                            exportedNsInfo `shouldBe` originalNsInfo
                      rootHash exportedManifest `shouldBe` rootHash originalManifest
                (Right _, Left failures) ->
                  expectationFailure $
                    "Expected success, but got the following failures: "
                      ++ show failures
                      ++ "\nBlock applied:\n"
                      ++ show (imtfTransactions inMemoryTestFixture)
                _ -> undefined

applyTestFixture ::
  Metadata ->
  InMemoryTestFixture ConwayEra ->
  IO
    ( Either
        ( TxOrBlock
            (NonEmpty (PredicateFailure (EraRule "LEDGER" ConwayEra)))
            (NonEmpty (PredicateFailure (EraRule "BBODY" ConwayEra)))
        )
        (NewEpochState ConwayEra, SlotNo)
    )
applyTestFixture
  Metadata {..}
  InMemoryTestFixture
    { imtfInitialState = (slotNo, initialNes)
    , imtfTransactions
    } =
    pure $ case imtfTransactions of
      OrBlock (blockIssuer, txs) ->
        bimap OrBlock (,slotNo) $ applyBlock slotNo (toGlobals globals) initialNes blockIssuer txs
      OrTx tx -> bimap OrTx (,slotNo) $ applyTx slotNo (toGlobals globals) initialNes tx

applyTx ::
  SlotNo ->
  Globals ->
  NewEpochState ConwayEra ->
  Tx TopTx ConwayEra ->
  Either (NonEmpty (PredicateFailure (EraRule "LEDGER" ConwayEra))) (NewEpochState ConwayEra)
applyTx slotNo globals nes tx = do
  let epochNo = runReader (epochFromSlot slotNo) globals
  let lEnv =
        LedgerEnv
          { ledgerSlotNo = slotNo
          , ledgerEpochNo = Just epochNo
          , ledgerPp = nes ^. nesEsL . curPParamsEpochStateL
          , ledgerIx = TxIx 0
          , ledgerAccount = nes ^. chainAccountStateL
          }
  let stsState = nes ^. nesEsL . esLStateL
  let trc = TRC (lEnv, stsState, tx)
  let
    assertionPolicy = AssertionsAll
    stsOpts =
      ApplySTSOpts
        { asoValidation = ValidateAll
        , asoEvents = EPReturn
        , asoAssertions = assertionPolicy
        }
    act = applySTSOptsEither @(EraRule "LEDGER" ConwayEra) stsOpts trc
  case runReader act globals of
    Left failures -> Left failures
    Right (ledgerState, _) -> Right $ nes & nesEsL . esLStateL .~ ledgerState

applyBlock ::
  SlotNo ->
  Globals ->
  NewEpochState ConwayEra ->
  KeyHash BlockIssuer ->
  StrictSeq (Tx TopTx ConwayEra) ->
  Either (NonEmpty (PredicateFailure (EraRule "BBODY" ConwayEra))) (NewEpochState ConwayEra)
applyBlock slotNo globals nes blockIssuer txs = do
  let
    blockBody = mkBasicBlockBody @ConwayEra & txSeqBlockBodyL .~ txs
    blockHeader =
      TestBlockHeader
        { tbhIssuer = blockIssuer
        , tbhBSize = fromIntegral $ bBodySize (ProtVer (eraProtVerLow @ConwayEra) 0) blockBody
        , tbhHSize = 0
        , tbhBHash = hashBlockBody blockBody
        , tbhSlot = slotNo
        , tbhPrevNonce = Nothing
        , tbhProtVer = nes ^. nesEsL . curPParamsEpochStateL . ppProtocolVersionL
        }
    block = Block {blockHeader, blockBody}
  case applyBlockEither EPReturn ValidateAll globals nes block of
    Left (BlockTransitionError failures) -> Left failures
    Right (newNes, _) -> Right newNes
