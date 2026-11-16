{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Unit tests of the validation of the Peras certificate of a block body
-- ('validatePerasCertInBlock', the core of the Dijkstra BBODY rule check).
--
-- The certificates are forged with the @cardano-crypto-peras@ committee code
-- from inputs built independently of the ledger-to-crypto conversions under
-- test (pool ids, nonce, public keys), so that a mismatch in those conversions
-- makes the valid certificate fail.
module Test.Cardano.Ledger.Dijkstra.PerasCertValidationSpec (spec) where

import Cardano.Binary.FixedSizeCodec (rawEncodeFixedSized)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), createPossessionProofDSIGN)
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256, castHash, hashWith)
import Cardano.Crypto.Peras (
  PerasBlockRef (..),
  PerasBoostedBlock (..),
  PerasRoundNo (..),
  PerasSigningKey,
 )
import Cardano.Crypto.Peras.Cert (PerasCert (..))
import Cardano.Crypto.Peras.Cert.WFALS (toPerasCert)
import Cardano.Crypto.Peras.Committee.Class (
  CryptoSupportsVotingCommittee (..),
  ensureUniqueVotesWithSameTarget,
 )
import qualified Cardano.Crypto.Peras.Committee.Crypto.BLS as BLS
import Cardano.Crypto.Peras.Crypto.BLS (PerasPrivateKey (..))
import qualified Cardano.Crypto.Peras.Validation as Peras
import Cardano.Ledger.BaseTypes (Nonce (..), StrictMaybe (..), boundRational)
import Cardano.Ledger.Coin (CompactForm (..), knownNonZeroCoin)
import Cardano.Ledger.Core (PParams, emptyPParams)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.PParams (
  ppPerasQuorumThresholdSafetyMarginL,
  ppPerasTargetCommitteeSizeL,
 )
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraBbodyPredFailure (..),
  PerasBbodyEnv (..),
  perasStakeDistrFromPoolDistr,
  validatePerasCertInBlock,
 )
import Cardano.Ledger.Hashes (VRFVerKeyHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Shelley.Rules (PerasBlockContext (..), PerasEpochInfo (..))
import Cardano.Ledger.State (BlsKey (..), IndividualPoolStake (..), PoolDistr (..))
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import GHC.Base (Any)
import Lens.Micro ((&), (.~))
import Test.Cardano.Crypto.Peras.Gen (perasSigningKeyFromSeedByte)
import Test.Cardano.Ledger.Common

{-------------------------------------------------------------------------------
  Fixture
-------------------------------------------------------------------------------}

-- | Pools: seed byte of their keys and relative stake. Ten pools with a
-- long-tailed distribution, so that a committee of target size 6 has persistent
-- and non-persistent seats.
testPools :: [(Word8, Rational)]
testPools = zip [1 ..] ((/ 100) <$> [25, 20, 15, 10, 8, 7, 5, 4, 3, 3])

poolSigningKey :: Word8 -> PerasSigningKey
poolSigningKey = perasSigningKeyFromSeedByte

poolKeyHashBytes :: Word8 -> BS.ByteString
poolKeyHashBytes b = BS.pack [b]

-- | Ledger side: the pool distribution, as found in the ledger state. A pool
-- without BLS key (seed 0) is added: it must be left out of the committee.
testPoolDistr :: PoolDistr
testPoolDistr =
  PoolDistr
    { unPoolDistr =
        Map.fromList $
          (keyHash 0, poolStake 0 (1 / 100) SNothing)
            : [ (keyHash b, poolStake b stake (SJust (blsKey b)))
              | (b, stake) <- testPools
              ]
    , pdTotalActiveStake = knownNonZeroCoin @1000
    }
  where
    keyHash b = KeyHash (castHash (hashWith @Blake2b_224 id (poolKeyHashBytes b)))
    poolStake b stake bls =
      IndividualPoolStake
        { individualPoolStake = stake
        , individualTotalPoolStake = CompactCoin 1
        , individualPoolStakeVrf =
            VRFVerKeyHash (castHash (hashWith @Blake2b_256 id (poolKeyHashBytes b)))
        , individualPoolStakeBls = bls
        }
    blsKey b =
      BlsKey
        { blsPubKey = deriveVerKeyDSIGN (poolSigningKey b)
        , blsPossessionProof = createPossessionProofDSIGN (poolSigningKey b)
        }

testNonceHashBytes :: BS.ByteString
testNonceHashBytes = "ledger-peras-cert-validation-test-nonce"

testNonce :: Nonce
testNonce = Nonce (castHash (hashWith @Blake2b_256 id testNonceHashBytes))

testPParams :: PParams DijkstraEra
testPParams =
  emptyPParams
    & ppPerasTargetCommitteeSizeL .~ 6
    & ppPerasQuorumThresholdSafetyMarginL .~ fromMaybe (error "safety margin") (boundRational (1 / 100))

-- | Crypto side, built independently of the ledger conversions: the committee
-- the voters use to forge their votes.
forgingCommittee :: Peras.VotingCommittee Peras.PerasBLSCrypto Peras.WFALS
forgingCommittee =
  Peras.pcvcCommittee . either (error . show) id $
    Peras.mkPerasCertValidationContext
      (Peras.Nonce (castHash (hashWith @Blake2b_256 id testNonceHashBytes)))
      ( Map.fromList
          [ ( Peras.PoolId (Peras.KeyHash (castHash (hashWith @Blake2b_224 id (poolKeyHashBytes b))))
            , ( Peras.LedgerStake stake
              , Peras.perasPublicKeyFromBlsKey
                  (Peras.BlsKey (deriveVerKeyDSIGN (poolSigningKey b)) (createPossessionProofDSIGN (poolSigningKey b)))
              )
            )
          | (b, stake) <- testPools
          ]
      )
      (Peras.TargetCommitteeSize 6)
      (Peras.PerasQuorumThreshold (3 / 4) (1 / 100))

poolPrivateKey :: Word8 -> PerasPrivateKey
poolPrivateKey b =
  PerasPrivateKey
    . fromMaybe (error "invalid private key")
    . BLS.rawDeserialisePrivateKey @Any Peras.perasLedgerKeyScope
    $ rawEncodeFixedSized (poolSigningKey b)

mkBoostedBlock :: Word8 -> PerasBoostedBlock
mkBoostedBlock b =
  PerasBoostedBlock . At $
    PerasBlockRef
      { pbrSlot = SlotNo (1000 + fromIntegral b)
      , pbrHash = SBS.toShort (BS.replicate 32 b)
      }

-- | Forge a certificate for a round from the votes of all the eligible pools.
forgeTestCert :: PerasRoundNo -> PerasBoostedBlock -> Maybe PerasCert
forgeTestCert roundNo block =
  case votes of
    [] -> Nothing
    v : vs -> either (const Nothing) Just $ do
      uniqueVotes <-
        either (const (Left ())) Right $
          ensureUniqueVotesWithSameTarget voteTarget compareVotesById (v :| vs)
      abstractCert <- either (const (Left ())) Right (forgeCert uniqueVotes)
      either (const (Left ())) Right (toPerasCert abstractCert)
  where
    votes =
      [ forgeVote eligibility (poolPrivateKey b) roundNo block
      | (b, _) <- testPools
      , let poolId = Peras.PoolId (Peras.KeyHash (castHash (hashWith @Blake2b_224 id (poolKeyHashBytes b))))
      , Right (Just eligibility) <- [checkShouldVote forgingCommittee poolId (poolPrivateKey b) roundNo]
      ]

-- | A round whose certificate reaches the quorum, and that certificate.
goodRound :: PerasRoundNo
goodCert :: PerasCert
(goodRound, goodCert) =
  fromMaybe (error "no suitable round") $
    find reachesQuorum
      [(r, cert) | r <- [0 .. 200], Just cert <- [forgeTestCert r (mkBoostedBlock 0xaa)]]
  where
    reachesQuorum (_, cert) =
      validate (Just (contextAround (pcRoundNo cert))) cert == Right ()

-- | An epoch, with the test nonce, that contains the given round.
epochAround :: PerasRoundNo -> PerasEpochInfo
epochAround r =
  PerasEpochInfo
    { peiNonce = testNonce
    , peiStartRound = if r >= 5 then r - 5 else 0
    , peiEndRound = r + 5
    }

-- | A block context whose epoch contains the given round, with no previous
-- epoch.
contextAround :: PerasRoundNo -> PerasBlockContext
contextAround r = PerasBlockContext {pbcEpoch = epochAround r, pbcPrevEpoch = SNothing}

-- | A block context whose /previous/ epoch contains the given round. The
-- current epoch starts right after the round and has another nonce.
prevContextAround :: PerasRoundNo -> PerasBlockContext
prevContextAround r =
  PerasBlockContext
    { pbcEpoch = PerasEpochInfo {peiNonce = NeutralNonce, peiStartRound = r + 1, peiEndRound = r + 10}
    , pbcPrevEpoch = SJust (epochAround r) {peiEndRound = r + 1}
    }

-- | Both epochs have the test pools and parameters.
testPerasEnv :: Maybe PerasBlockContext -> PerasBbodyEnv DijkstraEra
testPerasEnv ctx =
  PerasBbodyEnv
    { pbePerasContext = ctx
    , pbePoolDistr = testPoolDistr
    , pbePrevPParams = testPParams
    , pbePrevPoolDistr = testPoolDistr
    }

-- | Only the previous epoch has the test pools and parameters: the current
-- epoch has no pool and a committee size that the test pools cannot fill, so
-- that a certificate only validates against the previous epoch's inputs.
prevOnlyPerasEnv :: Maybe PerasBlockContext -> PerasBbodyEnv DijkstraEra
prevOnlyPerasEnv ctx =
  (testPerasEnv ctx) {pbePoolDistr = PoolDistr Map.empty (knownNonZeroCoin @1000)}

-- | Current epoch's parameters that the test pools cannot satisfy.
otherPParams :: PParams DijkstraEra
otherPParams = testPParams & ppPerasTargetCommitteeSizeL .~ 1000

validate ::
  Maybe PerasBlockContext -> PerasCert -> Either (DijkstraBbodyPredFailure DijkstraEra) ()
validate = validatePerasCertInBlock testPParams . testPerasEnv

shouldFailValidation :: Either (DijkstraBbodyPredFailure DijkstraEra) () -> PerasCert -> Expectation
shouldFailValidation res cert =
  case res of
    Left (PerasCertValidationFailed cert' nonce _) -> do
      cert' `shouldBe` cert
      nonce `shouldBe` testNonce
    other -> expectationFailure ("expected PerasCertValidationFailed, got: " <> show other)

{-------------------------------------------------------------------------------
  Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = describe "Peras certificate validation (BBODY)" $ do
  it "leaves out the pools without a BLS key" $
    Map.size (perasStakeDistrFromPoolDistr testPoolDistr) `shouldBe` length testPools
  it "accepts a valid certificate" $
    validate (Just (contextAround goodRound)) goodCert `shouldBe` Right ()
  it "accepts a certificate from the first and last rounds of the epoch" $ do
    let withEpoch epoch = PerasBlockContext {pbcEpoch = epoch, pbcPrevEpoch = SNothing}
    validate (Just (withEpoch (epochAround goodRound) {peiStartRound = goodRound})) goodCert
      `shouldBe` Right ()
    validate (Just (withEpoch (epochAround goodRound) {peiEndRound = goodRound + 1})) goodCert
      `shouldBe` Right ()
  it "rejects a certificate with a tampered boosted block" $ do
    let cert = goodCert {pcBoostedBlock = mkBoostedBlock 0xbb}
    validate (Just (contextAround goodRound)) cert `shouldFailValidation` cert
  it "rejects a certificate with a tampered signature" $ do
    let other = fromMaybe (error "no cert") (forgeTestCert goodRound (mkBoostedBlock 0xcc))
        cert = goodCert {pcSignature = pcSignature other}
    validate (Just (contextAround goodRound)) cert `shouldFailValidation` cert
  it "rejects a valid certificate under another epoch nonce" $
    case validate (Just (contextAround goodRound) {pbcEpoch = (epochAround goodRound) {peiNonce = NeutralNonce}}) goodCert of
      Left (PerasCertValidationFailed _ NeutralNonce _) -> pure ()
      other -> expectationFailure ("expected PerasCertValidationFailed, got: " <> show other)
  it "rejects a certificate without a block context" $
    validate Nothing goodCert `shouldBe` Left (PerasCertBlockContextMissing goodCert)
  it "rejects a certificate from a round outside the two epochs" $ do
    let ctxAfter = (prevContextAround goodRound) {pbcPrevEpoch = SNothing}
        ctxBefore = (contextAround goodRound) {pbcEpoch = (epochAround goodRound) {peiEndRound = goodRound}}
        ctxTooOld =
          (prevContextAround (goodRound + 3))
            {pbcPrevEpoch = SJust (epochAround goodRound) {peiStartRound = goodRound + 1, peiEndRound = goodRound + 4}}
    validate (Just ctxAfter) goodCert
      `shouldBe` Left (PerasCertRoundOutsideEpochs goodCert (goodRound + 1) (goodRound + 10))
    validate (Just ctxBefore) goodCert
      `shouldBe` Left (PerasCertRoundOutsideEpochs goodCert (peiStartRound (pbcEpoch ctxBefore)) goodRound)
    validate (Just ctxTooOld) goodCert
      `shouldBe` Left (PerasCertRoundOutsideEpochs goodCert (goodRound + 1) (goodRound + 13))
  it "accepts a certificate from the previous epoch against that epoch's committee" $ do
    validatePerasCertInBlock otherPParams (prevOnlyPerasEnv (Just (prevContextAround goodRound))) goodCert
      `shouldBe` Right ()
  it "rejects a previous-epoch certificate under the current epoch's inputs" $
    -- The same rounds, but the certificate's round is now in the current epoch,
    -- whose pools, parameters and nonce do not match.
    case validatePerasCertInBlock
      otherPParams
      (prevOnlyPerasEnv (Just (contextAround goodRound) {pbcEpoch = (epochAround goodRound) {peiNonce = NeutralNonce}}))
      goodCert of
      Left (PerasCertValidationFailed _ NeutralNonce _) -> pure ()
      other -> expectationFailure ("expected PerasCertValidationFailed, got: " <> show other)
  it "rejects a previous-epoch certificate under another previous-epoch nonce" $ do
    let ctx = prevContextAround goodRound
        ctx' = ctx {pbcPrevEpoch = (\epoch -> epoch {peiNonce = NeutralNonce}) <$> pbcPrevEpoch ctx}
    case validatePerasCertInBlock otherPParams (prevOnlyPerasEnv (Just ctx')) goodCert of
      Left (PerasCertValidationFailed _ NeutralNonce _) -> pure ()
      other -> expectationFailure ("expected PerasCertValidationFailed, got: " <> show other)
