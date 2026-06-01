{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  EpochNo,
  Globals (epochInfo, systemStart),
  ProtVer (ProtVer),
  SlotNo,
  TxIx (TxIx),
 )
import Cardano.Ledger.Binary (
  DecCBOR,
  DecoderError,
  decodeFull,
  decodeFullDecoder,
  serialize,
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.CanonicalState.Conway.Export ()
import Cardano.Ledger.CanonicalState.Conway.Import ()
import Cardano.Ledger.CanonicalState.Export (
  BlockFailures,
  ExportCanonicalState (dumpLedgerState),
  ExportLedgerState,
  Metadata (..),
  StateTransition (..),
  TxFailures,
  TxOrBlock (..),
  dump,
  getTestDirFromMetadata,
  mapTxOrBlockM,
  toGlobals,
 )
import Cardano.Ledger.CanonicalState.Import (
  ImportCanonicalState (importCanonicalState),
  ImportFailures (decodeBlockFailures, decodeTxFailures),
 )
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.State (CanSetChainAccountState (chainAccountStateL))
import Cardano.Ledger.Core (
  BlockIssuer,
  Era,
  EraBlockBody (blockBodySize, hashBlockBody, mkBasicBlockBody, txSeqBlockBodyL),
  EraRule,
  EraTx (Tx),
  KeyHash,
  TopTx,
  eraProtVerHigh,
  eraProtVerLow,
 )
import Cardano.Ledger.Shelley.API (
  ApplyTx (mkStAnnTx),
  BlockTransitionError (BlockTransitionError),
  LedgerState (lsUTxOState),
  UTxOState (utxosUtxo),
  applyBlockEither,
 )
import Cardano.Ledger.Shelley.LedgerState (NewEpochState, curPParamsEpochStateL, esLStateL, nesEsL)
import Cardano.Ledger.Shelley.Rules (
  LedgerEnv (..),
  epochFromSlot,
  ledgerPpL,
 )
import Cardano.SCLS.Internal.Reader (withLatestManifestFrame)
import Cardano.SCLS.Internal.Record.Manifest (Manifest (nsInfo, rootHash))
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
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
import Data.Bitraversable (bimapM)
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as SSeq
import GHC.Base (NonEmpty, when)
import GHC.IsList (IsList (toList))
import Lens.Micro ((.~), (^.))
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
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
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
      hspec $ parallel $ buildSpec dumpsPath testCases

discoverTestCases :: FilePath -> IO [Metadata]
discoverTestCases dumpsDir =
  decodeFileStrict metadataFile >>= \case
    Nothing -> do
      putStrLn $ "Warning: could not parse " ++ metadataFile
      pure []
    Just metadata ->
      pure metadata
  where
    metadataFile = dumpsDir </> "metadata.json"

buildSpec :: FilePath -> [Metadata] -> Spec
buildSpec dumpsDir testCases =
  describe "Black-box test runner" $
    forM_ testCases $ \m@Metadata {..} ->
      describe ("Era: " <> era <> ", Imp: " <> eraImp <> ", Protocol version: " <> show protocolVersion) $
        foldr
          describe
          (describe description $ runTest m)
          path
  where
    version = eraProtVerHigh @ConwayEra
    runTest :: Metadata -> Spec
    runTest m@Metadata {..} = do
      let dir = dumpsDir </> getTestDirFromMetadata m
      forM_ stateTransitions $ \t@StateTransition {initialState} ->
        it ("apply txn/block to " ++ initialState) $
          withSystemTempDirectory "blackbox-test-runner" $ \tmpDir -> do
            runExceptT
              (loadTestFixture @ConwayEra dir t)
              >>= \case
                Left err ->
                  expectationFailure $ "Failed to deserialise transaction: " ++ show err
                Right testFixture ->
                  applyTestFixture m testFixture >>= \computedRes ->
                    case (tfFinalState testFixture, computedRes) of
                      (Left (OrBlock expectedFailures), Left (OrBlock computedFailures)) ->
                        decodeFull version (serialize version computedFailures)
                          `shouldBe` Right expectedFailures
                      (Left (OrBlock _), Left (OrTx _)) ->
                        expectationFailure "Expected block failures, but got an unexpected tx failure"
                      (Left (OrTx expectedFailures), Left (OrTx computedFailures)) ->
                        decodeFull version (serialize version computedFailures)
                          `shouldBe` Right expectedFailures
                      (Left (OrTx _), Left (OrBlock _)) ->
                        expectationFailure "Expected tx failures, but got an unexpected block failure"
                      (Right expectedSclsFilePath, Right (computedNes, computedSlotNo)) -> do
                        let exportedFile = tmpDir </> ("computed-" <> expectedSclsFilePath)
                        Right () <- dump exportedFile computedSlotNo (dumpLedgerState @ConwayEra computedNes)
                        flip withLatestManifestFrame (dir </> expectedSclsFilePath) $ \originalManifest ->
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
                            ++ show (tfTransactions testFixture)
                      (Left (OrTx failures), Right _) ->
                        expectationFailure $
                          "Expected tx failures, but got success. Failures should've been: "
                            ++ show failures
                      (Left (OrBlock failures), Right _) ->
                        expectationFailure $
                          "Expected block failures, but got success. Failures should've been: "
                            ++ show failures

applyTestFixture ::
  Metadata ->
  TestFixture ConwayEra ->
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
  TestFixture
    { tfInitialState = (slotNo, initialNes)
    , tfTransactions
    } =
    pure $ case tfTransactions of
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
      stAnnTx =
        mkStAnnTx
          (epochInfo globals)
          (systemStart globals)
          (lEnv ^. ledgerPpL)
          (utxosUtxo (lsUTxOState stsState))
          tx
      trc = TRC (lEnv, stsState, stAnnTx)
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
        , tbhBSize = fromIntegral $ blockBodySize (ProtVer (eraProtVerLow @ConwayEra) 0) blockBody
        , tbhHSize = 0
        , tbhBHash = hashBlockBody blockBody
        , tbhSlot = slotNo
        , tbhPrevNonce = Nothing
        , tbhProtVer = ProtVer (eraProtVerHigh @ConwayEra) 0
        }
    block = Block {blockHeader, blockBody}
  case applyBlockEither EPReturn ValidateAll globals nes block of
    Left (BlockTransitionError failures) -> Left failures
    Right (newNes, _) -> Right newNes

data TestFixture era = TestFixture
  { tfEpochNo :: EpochNo
  , tfInitialState :: (SlotNo, ExportLedgerState era)
  , tfTransactions ::
      TxOrBlock (Tx TopTx era) (KeyHash BlockIssuer, SSeq.StrictSeq (Tx TopTx era))
  , tfFinalState ::
      Either (TxOrBlock (TxFailures era) (BlockFailures era)) FilePath
  }

loadTestFixture ::
  forall era.
  ( Era era
  , ImportCanonicalState era
  , ImportFailures era
  , DecCBOR (Tx TopTx era)
  ) =>
  FilePath ->
  StateTransition ->
  ExceptT DecoderError IO (TestFixture era)
loadTestFixture
  dir
  StateTransition
    { epochNo
    , initialState
    , transactions
    , finalState
    } = do
    tfInitialState <- liftIO $ importCanonicalState @era (dir </> initialState) epochNo
    tfTransactions <-
      mapTxOrBlockM
        ( \txFile ->
            decodeTx (dir </> txFile)
        )
        ( \(blockIssuerFile, txFiles) -> do
            blockIssuerBytes <- liftIO $ BSL.readFile (dir </> blockIssuerFile)
            blockIssuer <- except $ decodeFull ver blockIssuerBytes
            t <- forM txFiles (decodeTx . (dir </>))
            pure (blockIssuer, SSeq.fromList t)
        )
        transactions
    tfFinalState <- loadStateOrFailures
    pure $
      TestFixture
        { tfEpochNo = epochNo
        , tfInitialState
        , tfTransactions
        , tfFinalState
        }
    where
      ver = eraProtVerHigh @era
      decodeTx filepath =
        except . decodeFull ver
          =<< liftIO (BSL.readFile filepath)

      loadStateOrFailures ::
        ExceptT DecoderError IO (Either (TxOrBlock (TxFailures era) (BlockFailures era)) FilePath)
      loadStateOrFailures =
        bimapM
          ( \failuresFile -> ExceptT $ do
              bs <- BSL.readFile (dir </> failuresFile)
              pure $
                case decodeFullDecoder ver "TxFailures" (decodeTxFailures @era) bs of
                  Left _ -> OrBlock <$> decodeFullDecoder ver "BlockFailures" (decodeBlockFailures @era) bs
                  Right txFailures -> Right (OrTx txFailures)
          )
          pure
          finalState
