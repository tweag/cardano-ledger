{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#if __GLASGOW_HASKELL__ >= 910
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/27342
{-# OPTIONS_GHC -fno-spec-eval #-}
#endif

module Cardano.Ledger.Dijkstra.Rules.Bbody (
  BBODY,
  BbodyEnv (..),
  PerasBbodyEnv (..),
  DijkstraBbodyPredFailure (..),
  conwayToDijkstraBbodyPredFailure,
  validatePerasCertInBlock,
  perasStakeDistrFromPoolDistr,
) where

import Cardano.Crypto.Hash (castHash)
import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Crypto.Peras (PerasRoundNo)
import qualified Cardano.Crypto.Peras.Validation as Peras
import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.PParams (AlonzoEraPParams, ppMaxBlockExUnitsL)
import qualified Cardano.Ledger.Alonzo.Rules as Alonzo
import Cardano.Ledger.Alonzo.Scripts (OrdExUnits)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx)
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody)
import qualified Cardano.Ledger.Babbage.Rules as Babbage
import Cardano.Ledger.BaseTypes (
  Mismatch (..),
  Nonce (..),
  Relation (..),
  ShelleyBase,
  StrictMaybe (..),
  getVersion32,
  pvMajor,
  unboundRational,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Block (
  Block (..),
  BlockHeaderVersionInfo (..),
  EraBlockHeader (..),
  LeiosBbodySignal (..),
  LeiosEraBlockHeader (..),
 )
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.BlockBody (
  DijkstraEraBlockBody (..),
  PerasCert (..),
 )
import Cardano.Ledger.Dijkstra.Era (
  BBODY,
  DijkstraEra,
 )
import Cardano.Ledger.Dijkstra.PParams (
  DijkstraEraPParams,
  ppLeiosAnnouncementPeriodLengthL,
  ppLeiosDiffusionPeriodLengthL,
  ppLeiosQuorumStakeThresholdL,
  ppLeiosVotePeriodLengthL,
  ppPerasQuorumThresholdSafetyMarginL,
  ppPerasTargetCommitteeSizeL,
 )
import Cardano.Ledger.Dijkstra.Rules.Gov (DijkstraGovPredFailure)
import Cardano.Ledger.Dijkstra.Rules.GovCert (DijkstraGovCertPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledger (DijkstraLedgerPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Ledgers ()
import Cardano.Ledger.Dijkstra.Rules.LeiosCert (
  DijkstraLeiosCertPredFailure,
  LEIOSCERT,
  LeiosCertEnv (..),
 )
import Cardano.Ledger.Dijkstra.Rules.Utxo (DijkstraUtxoPredFailure)
import Cardano.Ledger.Dijkstra.Rules.Utxow (DijkstraUtxowPredFailure)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (StakePool))
import Cardano.Ledger.Shelley.API (incrBlocks)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), lsLastEbAnnouncementL)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (slotToEpochBoundary)
import Control.DeepSeq (NFData)
import Control.State.Transition
import Data.Bifunctor (first)
import Data.Functor (void, (<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Sequence (Seq)
import Data.Sequence.Strict (fromStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (absurd)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))

data BbodyEnv era = BbodyEnv
  { bbePParams :: PParams era
  , bbeChainAccountState :: ChainAccountState
  , bbeLeiosCommittee :: LeiosCommittee
  , bbePeras :: PerasBbodyEnv era
  -- ^ What Peras certificate validation needs. Its fields are lazy, so that
  -- blocks without a certificate never compute them.
  }

-- | The inputs of Peras certificate validation in the BBODY rule.
--
-- A certificate is validated against the voting committee of the epoch of its
-- round, which is the epoch of the block or the one before it, as in
-- consensus's two-epoch window of Peras epoch contexts.
data PerasBbodyEnv era = PerasBbodyEnv
  { pbePerasContext :: Maybe Shelley.PerasBlockContext
  -- ^ Block application context, supplied by the caller of the block
  -- application API. 'Nothing' when the caller does not provide one; every
  -- block that carries a Peras certificate is then rejected.
  , pbePoolDistr :: PoolDistr
  -- ^ Stake pool distribution of the epoch of the block (the one used for
  -- leader election, 'Cardano.Ledger.Shelley.LedgerState.nesStakePoolDistrG')
  , pbePrevPParams :: PParams era
  -- ^ Protocol parameters of the previous epoch
  , pbePrevPoolDistr :: PoolDistr
  -- ^ Stake pool distribution of the previous epoch, computed from the @go@
  -- snapshot, which was the leader-election (@set@) snapshot of that epoch
  }

data DijkstraBbodyPredFailure era
  = WrongBlockBodySizeBBODY (Mismatch RelEQ Int)
  | InvalidBodyHashBBODY (Mismatch RelEQ (Hash HASH EraIndependentBlockBody))
  | -- | LEDGERS rule subtransition Failures
    LedgersFailure (PredicateFailure (EraRule "LEDGERS" era))
  | TooManyExUnits (Mismatch RelLTEQ OrdExUnits)
  | BodyRefScriptsSizeTooBig (Mismatch RelLTEQ Int)
  | -- | The Peras certificate of the block body is invalid: either the voting
    -- committee of the epoch of its round could not be built from that epoch's
    -- nonce (given) and stake distribution, or the certificate does not verify
    -- against it. The text describes the underlying @cardano-crypto-peras@
    -- error.
    PerasCertValidationFailed PerasCert Nonce Text
  | HeaderProtVerTooLow (Mismatch RelGTEQ Word32)
  | LeiosCertMissing
  | LeiosCertBlockBodyHasTxs
  | LeiosCertFailure (PredicateFailure (EraRule "LEIOSCERT" era))
  | LeiosCertWithoutEbAnnouncement
  | LeiosCertEbNotApplied
  | -- | The block body has a Peras certificate, but the caller of the ledger
    -- did not supply the 'Shelley.PerasBlockContext' (epoch nonces and Peras
    -- rounds) needed to validate it.
    PerasCertBlockContextMissing PerasCert
  | -- | The round of the Peras certificate is neither a round of the epoch of
    -- the block nor one of the epoch before it (the rounds of both are given
    -- as the half-open range @[start, end)@). The ledger only has the
    -- committees of these two epochs, as consensus does.
    PerasCertRoundOutsideEpochs PerasCert PerasRoundNo PerasRoundNo
  deriving (Generic)

instance
  ( NFData (PredicateFailure (EraRule "LEDGERS" era))
  , NFData (PredicateFailure (EraRule "LEIOSCERT" era))
  ) =>
  NFData (DijkstraBbodyPredFailure era)

deriving instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  , Show (PredicateFailure (EraRule "LEIOSCERT" era))
  ) =>
  Show (DijkstraBbodyPredFailure era)

deriving instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  , Eq (PredicateFailure (EraRule "LEIOSCERT" era))
  ) =>
  Eq (DijkstraBbodyPredFailure era)

deriving instance
  ( Era era
  , Ord (PredicateFailure (EraRule "LEDGERS" era))
  , Ord (PredicateFailure (EraRule "LEIOSCERT" era))
  ) =>
  Ord (DijkstraBbodyPredFailure era)

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGERS" era))
  , EncCBOR (PredicateFailure (EraRule "LEIOSCERT" era))
  ) =>
  EncCBOR (DijkstraBbodyPredFailure era)
  where
  encCBOR =
    encode . \case
      WrongBlockBodySizeBBODY mm -> Sum WrongBlockBodySizeBBODY 0 !> To mm
      InvalidBodyHashBBODY mm -> Sum (InvalidBodyHashBBODY @era) 1 !> To mm
      LedgersFailure x -> Sum (LedgersFailure @era) 2 !> To x
      TooManyExUnits mm -> Sum TooManyExUnits 3 !> To mm
      BodyRefScriptsSizeTooBig mm -> Sum BodyRefScriptsSizeTooBig 4 !> To mm
      PerasCertValidationFailed cert nonce err ->
        Sum PerasCertValidationFailed 5 !> To cert !> To nonce !> To err
      HeaderProtVerTooLow mm -> Sum HeaderProtVerTooLow 6 !> To mm
      LeiosCertMissing -> Sum LeiosCertMissing 7
      LeiosCertBlockBodyHasTxs -> Sum LeiosCertBlockBodyHasTxs 8
      LeiosCertFailure f -> Sum (LeiosCertFailure @era) 9 !> To f
      LeiosCertWithoutEbAnnouncement -> Sum LeiosCertWithoutEbAnnouncement 10
      LeiosCertEbNotApplied -> Sum LeiosCertEbNotApplied 11
      PerasCertBlockContextMissing cert -> Sum PerasCertBlockContextMissing 12 !> To cert
      PerasCertRoundOutsideEpochs cert start end ->
        Sum PerasCertRoundOutsideEpochs 13 !> To cert !> To start !> To end

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  , DecCBOR (PredicateFailure (EraRule "LEIOSCERT" era))
  ) =>
  DecCBOR (DijkstraBbodyPredFailure era)
  where
  decCBOR = decode . Summands "DijkstraBbodyPred" $ \case
    0 -> SumD WrongBlockBodySizeBBODY <! From
    1 -> SumD InvalidBodyHashBBODY <! From
    2 -> SumD LedgersFailure <! From
    3 -> SumD TooManyExUnits <! From
    4 -> SumD BodyRefScriptsSizeTooBig <! From
    5 -> SumD PerasCertValidationFailed <! From <! From <! From
    6 -> SumD HeaderProtVerTooLow <! From
    7 -> SumD LeiosCertMissing
    8 -> SumD LeiosCertBlockBodyHasTxs
    9 -> SumD LeiosCertFailure <! From
    10 -> SumD LeiosCertWithoutEbAnnouncement
    11 -> SumD LeiosCertEbNotApplied
    12 -> SumD PerasCertBlockContextMissing <! From
    13 -> SumD PerasCertRoundOutsideEpochs <! From <! From <! From
    n -> Invalid n

type instance EraRuleFailure "BBODY" DijkstraEra = DijkstraBbodyPredFailure DijkstraEra

type instance EraRuleEvent "BBODY" DijkstraEra = Alonzo.AlonzoBbodyEvent DijkstraEra

instance InjectRuleFailure "BBODY" DijkstraBbodyPredFailure DijkstraEra

instance InjectRuleFailure "BBODY" DijkstraLeiosCertPredFailure DijkstraEra where
  injectFailure = LeiosCertFailure

instance InjectRuleFailure "BBODY" Conway.ConwayBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure . Conway.alonzoToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure DijkstraEra where
  injectFailure = conwayToDijkstraBbodyPredFailure . Conway.shelleyToConwayBbodyPredFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyLedgersPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure . Conway.shelleyToConwayBbodyPredFailure . Shelley.LedgersFailure

instance InjectRuleFailure "BBODY" DijkstraLedgerPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Babbage.BabbageUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxowPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Babbage.BabbageUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Alonzo.AlonzoUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayUtxosPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Allegra.AllegraUtxoPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayCertsPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayCertPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayDelegPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Shelley.ShelleyPoolPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovCertPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" Conway.ConwayGovPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance InjectRuleFailure "BBODY" DijkstraGovPredFailure DijkstraEra where
  injectFailure =
    conwayToDijkstraBbodyPredFailure
      . Conway.shelleyToConwayBbodyPredFailure
      . Shelley.LedgersFailure
      . injectFailure

instance
  ( Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEnv era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , AlonzoEraTxWits era
  , EraBlockBody era
  , AlonzoEraPParams era
  , InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure era
  , InjectRuleFailure "BBODY" Conway.ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure era
  , EraRule "BBODY" era ~ BBODY era
  , AlonzoEraTx era
  , BabbageEraTxBody era
  , DijkstraEraPParams era
  , Embed (EraRule "LEIOSCERT" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEIOSCERT" era) ~ LeiosCertEnv
  , State (EraRule "LEIOSCERT" era) ~ ()
  , Signal (EraRule "LEIOSCERT" era) ~ LeiosCert
  , DijkstraEraBlockBody era
  ) =>
  STS (BBODY era)
  where
  type State (BBODY era) = Shelley.ShelleyBbodyState era

  type Signal (BBODY era) = LeiosBbodySignal era

  type Environment (BBODY era) = BbodyEnv era

  type BaseM (BBODY era) = ShelleyBase

  type PredicateFailure (BBODY era) = DijkstraBbodyPredFailure era

  type Event (BBODY era) = Alonzo.AlonzoBbodyEvent era

  initialRules = []
  transitionRules = [dijkstraBbodyTransition @era]

dijkstraBbodyTransition ::
  forall era.
  ( Signal (EraRule "BBODY" era) ~ LeiosBbodySignal era
  , State (EraRule "BBODY" era) ~ Shelley.ShelleyBbodyState era
  , State (EraRule "LEDGERS" era) ~ LedgerState era
  , Environment (EraRule "LEDGERS" era) ~ Shelley.ShelleyLedgersEnv era
  , Environment (EraRule "BBODY" era) ~ BbodyEnv era
  , InjectRuleFailure "BBODY" DijkstraBbodyPredFailure era
  , DijkstraEraBlockBody era
  , BabbageEraTxBody era
  , InjectRuleFailure "BBODY" Conway.ConwayBbodyPredFailure era
  , InjectRuleFailure "BBODY" Shelley.ShelleyBbodyPredFailure era
  , STS (EraRule "BBODY" era)
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , AlonzoEraTx era
  , InjectRuleFailure "BBODY" Alonzo.AlonzoBbodyPredFailure era
  , Embed (EraRule "LEDGERS" era) (EraRule "BBODY" era)
  , DijkstraEraPParams era
  , Embed (EraRule "LEIOSCERT" era) (EraRule "BBODY" era)
  , Environment (EraRule "LEIOSCERT" era) ~ LeiosCertEnv
  , State (EraRule "LEIOSCERT" era) ~ ()
  , Signal (EraRule "LEIOSCERT" era) ~ LeiosCert
  ) =>
  TransitionRule (EraRule "BBODY" era)
dijkstraBbodyTransition = do
  TRC
    ( BbodyEnv pp account leiosCommittee perasEnv
      , Shelley.BbodyState ls blocksMade
      , LeiosBbodySignal block@Block {blockBody}
      ) <-
    judgmentContext

  Shelley.validateBlockBodySize block (pp ^. ppProtocolVersionL)

  Shelley.validateBlockBodyHash block

  let curProtVerMajor = getVersion32 . pvMajor $ pp ^. ppProtocolVersionL
      BlockHeaderVersionInfo {bhviHighestSupportedMajorVersion} = block ^. versionInfoBlockHeaderL
  bhviHighestSupportedMajorVersion
    >= curProtVerMajor
      ?! injectFailure
        ( HeaderProtVerTooLow $
            Mismatch
              { mismatchSupplied = bhviHighestSupportedMajorVersion
              , mismatchExpected = curProtVerMajor
              }
        )

  let bhSlot = block ^. slotNoBlockHeaderL

  (firstSlot, curEpoch) <- liftSTS $ slotToEpochBoundary bhSlot

  let txs = blockBody ^. txSeqBlockBodyL

  ls' <-
    trans @(EraRule "LEDGERS" era) $
      TRC
        ( Shelley.LedgersEnv bhSlot curEpoch pp account
        , ls
        , fromStrict txs
        )

  Alonzo.validateExUnits @era txs $ pp ^. ppMaxBlockExUnitsL

  Conway.validateBodyRefScriptsSizeTooBig @era pp blockBody (ls ^. utxoL)

  case blockBody ^. perasCertBlockBodyL of
    SNothing -> pure ()
    SJust cert ->
      -- This is a predicate, hence skipped (and the committee not built) when
      -- re-applying blocks without validation.
      validatePerasCertInBlock pp perasEnv cert ?!: injectFailure

  case blockBody ^. leiosCertBlockBodyL of
    SNothing ->
      not (block ^. blockBodyContainsLeiosCertBlockHeaderL) ?! injectFailure LeiosCertMissing
    SJust cert -> do
      null txs ?! injectFailure LeiosCertBlockBodyHasTxs
      whenFailureFree $
        case ls ^. lsLastEbAnnouncementL of
          SNothing -> failBecause $ injectFailure LeiosCertWithoutEbAnnouncement
          SJust EbAnnouncement {ebAnnouncementSlotNo, ebAnnouncedInBlockHeader, ebIsApplied} -> do
            ebIsApplied ?! injectFailure LeiosCertEbNotApplied
            trans @(EraRule "LEIOSCERT" era) $
              TRC
                ( LeiosCertEnv
                    { lseCurrentSlotNo = bhSlot
                    , lseAnnouncementSlotNo = ebAnnouncementSlotNo
                    , lseAnnouncedInBlockHeader = ebAnnouncedInBlockHeader
                    , lseLeiosCommittee = leiosCommittee
                    , lseThreshold = pp ^. ppLeiosQuorumStakeThresholdL
                    , lseAnnouncementPeriodLength = pp ^. ppLeiosAnnouncementPeriodLengthL
                    , lseVotePeriodLength = pp ^. ppLeiosVotePeriodLengthL
                    , lseDiffusionPeriodLength = pp ^. ppLeiosDiffusionPeriodLengthL
                    }
                , ()
                , cert
                )

  -- Record the EB announced in this block's header, if any. A block that announces nothing
  -- removes any prior announcement.
  let ls'' =
        ls'
          & lsLastEbAnnouncementL
            .~ ( (block ^. ebReferencesAnnouncementBlockHeaderL) <&> \ebRefsAnn ->
                   EbAnnouncement
                     { ebAnnouncement = ebRefsAnn
                     , ebAnnouncementSlotNo = bhSlot
                     , ebAnnouncedInBlockHeader = block ^. blockHeaderHashBlockHeaderG
                     , ebIsApplied = False
                     }
               )

  pure $ Shelley.BbodyState ls'' $ incrBlocks block firstSlot (pp ^. ppDG) blocksMade

-- | Validate the Peras certificate of a block body.
--
-- This mirrors consensus's validation of the certificate in a block
-- (@extractAndValidatePerasCertFromBlock@, i.e. @defaultVerifyPerasCert@
-- against the wFA^LS committee of the certificate's epoch). The epoch of the
-- certificate's round is the epoch of the block or the one before it, as in
-- consensus's two-epoch window of Peras epoch contexts; for that epoch:
--
-- * the committee is built from the epoch nonce supplied by the caller, from
--   every pool of the epoch's stake distribution that has a registered BLS key
--   (see 'perasStakeDistrFromPoolDistr'), and from the epoch's target committee
--   size protocol parameter;
-- * the quorum is consensus's: a vote weight of at least @3/4@ plus the epoch's
--   @perasQuorumThresholdSafetyMargin@ protocol parameter.
--
-- PROTOTYPE LIMITATIONS:
--
-- * The round ranges of the epochs are supplied by the caller rather than
--   computed here, since the ledger knows neither the Peras round length nor
--   the first Peras round of the era.
-- * The committee is rebuilt for every block that carries a certificate
--   (consensus builds it once per epoch).
validatePerasCertInBlock ::
  DijkstraEraPParams era =>
  -- | Protocol parameters of the epoch of the block
  PParams era ->
  PerasBbodyEnv era ->
  PerasCert ->
  Either (DijkstraBbodyPredFailure era) ()
validatePerasCertInBlock pp perasEnv cert = do
  Shelley.PerasBlockContext {Shelley.pbcEpoch, Shelley.pbcPrevEpoch} <-
    maybe (Left (PerasCertBlockContextMissing cert)) Right (pbePerasContext perasEnv)
  let roundNo = pcRoundNo cert
      inEpoch epoch = Shelley.peiStartRound epoch <= roundNo && roundNo < Shelley.peiEndRound epoch
      windowStart = Shelley.peiStartRound $ case pbcPrevEpoch of
        SJust prevEpoch -> prevEpoch
        SNothing -> pbcEpoch
  (epoch, epochPp, poolDistr) <-
    if
      | inEpoch pbcEpoch ->
          Right (pbcEpoch, pp, pbePoolDistr perasEnv)
      | SJust prevEpoch <- pbcPrevEpoch
      , inEpoch prevEpoch ->
          Right (prevEpoch, pbePrevPParams perasEnv, pbePrevPoolDistr perasEnv)
      | otherwise ->
          Left (PerasCertRoundOutsideEpochs cert windowStart (Shelley.peiEndRound pbcEpoch))
  let epochNonce = Shelley.peiNonce epoch
      failed :: Show e => e -> DijkstraBbodyPredFailure era
      failed = PerasCertValidationFailed cert epochNonce . T.pack . show
      quorum =
        Peras.PerasQuorumThreshold
          { Peras.pqtQuorumWeightThreshold = 3 % 4
          , Peras.pqtQuorumWeightThresholdSafetyMargin =
              unboundRational (epochPp ^. ppPerasQuorumThresholdSafetyMarginL)
          }
      targetCommitteeSize =
        Peras.TargetCommitteeSize (fromIntegral (epochPp ^. ppPerasTargetCommitteeSizeL))
  validationContext <-
    first failed $
      Peras.mkPerasCertValidationContext
        (toPerasNonce epochNonce)
        (perasStakeDistrFromPoolDistr poolDistr)
        targetCommitteeSize
        quorum
  void . first failed $ Peras.verifyPerasCert validationContext cert

-- | Committee input of the Peras certificate validation: every pool that has a
-- registered BLS key, with its relative stake and its BLS key in the ledger key
-- scope. Pools without a BLS key are left out. Port of consensus's
-- @extractPerasStakeDistrAndPublicKeys@.
perasStakeDistrFromPoolDistr ::
  PoolDistr -> Map Peras.PoolId (Peras.LedgerStake, Peras.PerasPublicKey)
perasStakeDistrFromPoolDistr =
  -- Rewrapping the hashes preserves their order, hence 'Map.mapKeysMonotonic'
  Map.mapKeysMonotonic toPerasPoolId . Map.mapMaybe entry . unPoolDistr
  where
    entry poolStake = case individualPoolStakeBls poolStake of
      SNothing -> Nothing
      SJust blsKey ->
        Just
          ( Peras.LedgerStake (individualPoolStake poolStake)
          , Peras.perasPublicKeyFromBlsKey
              (Peras.BlsKey (blsPubKey blsKey) (blsPossessionProof blsKey))
          )
    toPerasPoolId :: KeyHash StakePool -> Peras.PoolId
    toPerasPoolId (KeyHash h) = Peras.PoolId (Peras.KeyHash h)

toPerasNonce :: Nonce -> Peras.Nonce
toPerasNonce = \case
  NeutralNonce -> Peras.NeutralNonce
  Nonce h -> Peras.Nonce (castHash h)

-- | Convert a Conway BBODY predicate failure into a Dijkstra one.
conwayToDijkstraBbodyPredFailure ::
  forall era. Conway.ConwayBbodyPredFailure era -> DijkstraBbodyPredFailure era
conwayToDijkstraBbodyPredFailure = \case
  Conway.WrongBlockBodySizeBBODY mm -> WrongBlockBodySizeBBODY mm
  Conway.InvalidBodyHashBBODY mm -> InvalidBodyHashBBODY mm
  Conway.LedgersFailure f -> LedgersFailure f
  Conway.TooManyExUnits mm -> TooManyExUnits mm
  Conway.BodyRefScriptsSizeTooBig mm -> BodyRefScriptsSizeTooBig mm
  Conway.HeaderProtVerTooHigh {} -> error "Impossible: HeaderProtVerTooHigh cannot be triggered in Dijkstra era"

instance
  ( BaseM (Babbage.LEDGERS era) ~ ShelleyBase
  , Babbage.LEDGERS era ~ EraRule "LEDGERS" era
  , STS (Babbage.LEDGERS era)
  ) =>
  Embed (Babbage.LEDGERS era) (BBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = Alonzo.ShelleyInAlonzoEvent . Shelley.LedgersEvent

instance
  ( Era era
  , PredicateFailure (EraRule "LEIOSCERT" era) ~ DijkstraLeiosCertPredFailure era
  ) =>
  Embed (LEIOSCERT era) (BBODY era)
  where
  wrapFailed = LeiosCertFailure
  wrapEvent = absurd
