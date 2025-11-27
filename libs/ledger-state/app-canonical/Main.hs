{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main where

import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State.Query
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.BaseTypes (BlocksMade (..))

import Cardano.Ledger.Compactible
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Vars

import Cardano.Ledger.Export.Namespace.Blocks
import Cardano.Ledger.Export.Namespace.UTxO
import Cardano.Ledger.Export.Namespace.Pots
import Cardano.Ledger.Export.Namespace.Snapshots
import Cardano.Ledger.Export.Namespace.PoolStake
import Cardano.Ledger.Export.Namespace.GovCommittee
import Cardano.Ledger.Export.Namespace.GovPParams
import Cardano.Ledger.Export.Namespace.GovProposals as Proposals
import Cardano.Ledger.Export.Namespace.GovConstitution
import Cardano.Ledger.Api.State.Query (queryConstitution,queryProposals, queryCommitteeState)
import Cardano.Ledger.Conway.Governance
import Control.Exception (throwIO)
import Data.Bifunctor (first)
import Control.Monad
import Data.Function ((&))
import qualified Data.Set as Set
import Cardano.SCLS.Internal.Reader
import Cardano.SCLS.Internal.Serializer.Dump.Plan
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Cardano.Ledger.Mary (MaryEra)
-- import Data.Text as T (pack)
import Cardano.SCLS.Internal.Entry.ChunkEntry
import qualified Data.ByteString.Base16.Lazy as Base16
import Cardano.Ledger.Binary.Plain as Plain
import Options.Applicative
import Cardano.Ledger.Api.Era
import Cardano.Ledger.State -- Core (UTxO (..))
-- import Cardano.Ledger.UTxO
-- import Cardano.Ledger.State.UTxO (CurrentEra) -- , readHexUTxO, readNewEpochState)
import Cardano.Chain.UTxO (TxIn, TxId) -- , TxOut)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Foldable (for_, toList)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Data.Word (Word16)
-- import qualified GHC.Exts as GHC
import qualified GHC.Generics as GHC

-- import Cardano.Ledger.Conway.Era

import qualified Cardano.SCLS.Internal.Serializer.External.Impl as External (serialize)
import Cardano.SCLS.Internal.Entry.IsKey
import Data.MemPack.Extra
import Cardano.Types.Network (NetworkId (..))
import Cardano.Types.SlotNo (SlotNo (..))
import qualified Cardano.Ledger.Babbage.TxOut as Babbage
import qualified Cardano.Ledger.Shelley.TxOut as Shelley ()

-- import Cardano.Ledger.State
-- import Cardano.Ledger.State.UTxO (readHexUTxO)
import System.IO

import Debug.Trace (traceM)

-- | Insight into options:
--
-- * `optsNewEpochStateBinaryFile` is for reading a previously serialized
-- * `NewEpochState` produced by cardano-cli` and is used to populate sqlite
-- * database
--
-- * `optsEpochStateBinaryFile` is used for grabbing data from sqlite,
-- * constructing `EpochState` (in a new format) and writing it into the cbor
-- * serialized file
data Opts = Opts
  { optsNewEpochStateBinaryFile :: Maybe FilePath
  -- ^ Path to the CBOR encoded NewEpochState data type, which will be used to
  -- load into sqlite database
  , optsEpochStateBinaryFile :: Maybe FilePath
  -- ^ Path to the CBOR encoded EpochState data type, which will have data
  -- from sqlite database written into it.
  , optsSqliteDbFile :: Maybe FilePath
  -- ^ Path to Sqlite database file.
  }
  deriving (Show)

data Cmd
    = CmdCreateStateFile FilePath FilePath FilePath
    | Cat FilePath
    deriving (Show)

optsParser :: Parser Cmd
optsParser = hsubparser
  ( command "create" (info createStateCommand (progDesc "Create canonical file for ledger state"))
  <> command "cat" (info catCommand (progDesc "Display contents of canonical ledger state file"))
  )
  where
    createStateCommand = CmdCreateStateFile
      <$> argument str (metavar "STATE_BIN_FILE")
      <*> argument str (metavar "UTXO_HEX_FILE")
      <*> argument str (metavar "SCLS_FILE")
    catCommand = Cat <$> argument str (metavar "SCLS_FILE")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cmd <-
    execParser $
      info
        ( optsParser
            <* abortOption
              (ShowHelpText Nothing)
              (long "help" <> short 'h' <> help "Display this message.")
        )
        (header "canonical-state - Tool for working with canonical ledger state representation")
  case cmd of
    Cat fileName -> do
        putStrLn $ "Reading canonical state from " ++ fileName
        withKnownNamespacedData fileName (Proxy @"utxo/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"blocks/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"gov/committee/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"gov/constitution/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"gov/pparams/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"gov/proposals/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"pool_stake/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"pots/v0") $ \stream ->
          S.print stream
        withKnownNamespacedData fileName (Proxy @"snapshots/v0") $ \stream ->
          S.print stream
    CmdCreateStateFile stateFilePath utxoFilePath fileName -> do
        putStrLn "Creating state file..."
        putStrLn $ "Reading State from " ++ stateFilePath
        nes <- readNewEpochState stateFilePath
        UTxO utxo0 <- localReadDecCBORHex utxoFilePath
        let epoch = nesEL nes


        print epoch
        External.serialize
          fileName
          Mainnet
          (SlotNo 1)
          (defaultSerializationPlan
            & addNamespacedChunks (Proxy @"utxo/v0")
              (S.each
                [ ChunkEntry (UtxoKeyIn txin) (UtxoOutBabbage txout)
                | (txin, txout) <- Map.toList utxo0
                ])
            & addNamespacedChunks (Proxy @"blocks/v0")
              (S.each
                [ ChunkEntry (BlockIn (key_hash, pred epoch)) (BlockOut natural)
                | let BlocksMade mpPrev = nesBprev nes
                , (key_hash, natural) <- Map.toList mpPrev
                ]
              <> S.each
                [ ChunkEntry (BlockIn (key_hash, epoch)) (BlockOut natural)
                | let BlocksMade mpCurrent = nesBcur nes
                , (key_hash, natural) <- Map.toList mpCurrent
                ])
            & addNamespacedChunks (Proxy @"pots/v0")
              (S.each [ ChunkEntry (PotsIn epoch) PotsOut
                          { poFee = nes ^. feesL
                          , poDeposit =  nes ^. depositsL
                          , poDonation = nes ^. donationL
                          , poReserves = nes ^. reservesL
                          , poTreasury = nes ^. treasuryL
                          }
                      ])
            & addNamespacedChunks (Proxy @"snapshots/v0")
              ( S.each
                [ ChunkEntry (SnapShotInCred SnapShotStageSet cred SnapShotValueAddress) (SnapShotOutAddress stakeHash)
                | (cred, stakeHash) <- nes ^. setDelegsL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInKey SnapShotStageSet poolHash SnapShotValuePoolParams) (SnapShotOutPoolParams poolParams)
                | (poolHash, poolParams) <- nes ^. setPoolsL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInCred SnapShotStageSet poolHash SnapShotValueCoin) (SnapShotOutCoin coin)
                | (poolHash, coin) <- nes ^. setStakeL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInCred SnapshotStageMark cred SnapShotValueAddress) (SnapShotOutAddress stakeHash)
                | (cred, stakeHash) <- nes ^. markDelegsL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInKey SnapshotStageMark poolHash SnapShotValuePoolParams) (SnapShotOutPoolParams poolParams)
                | (poolHash, poolParams) <- nes ^. markPoolsL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInCred SnapshotStageGo poolHash SnapShotValueCoin) (SnapShotOutCoin coin)
                | (poolHash, coin) <- nes ^. goStakeL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInCred SnapshotStageGo cred SnapShotValueAddress) (SnapShotOutAddress stakeHash)
                | (cred, stakeHash) <- nes ^. goDelegsL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInKey SnapshotStageGo poolHash SnapShotValuePoolParams) (SnapShotOutPoolParams poolParams)
                | (poolHash, poolParams) <- nes ^. goPoolsL . to(Map.toList)
                ]
              <> S.each
                [ ChunkEntry (SnapShotInCred SnapshotStageGo poolHash SnapShotValueCoin) (SnapShotOutCoin coin)
                | (poolHash, coin) <- nes ^. goStakeL . to(Map.toList)
                ])
            & addNamespacedChunks (Proxy @"pool_stake/v0")
              (S.each
                [ ChunkEntry (PoolStakeIn cred) (PoolStakeOut (fromCompact individualTotalPoolStake) individualPoolStakeVrf)
                | (cred, IndividualPoolStake{..}) <- nes ^. markPoolDistrL . to(Map.toList)
                ])
            & addNamespacedChunks (Proxy @"gov/pparams/v0")
              (S.each
                [ ChunkEntry k (GovPParamsOut v)
                | Just (k, v) <-
                    [ Just (GovPParamsInCurr,  nes ^. nesEpochStateL . curPParamsEpochStateL)
                    , Just (GovPParamsInPrev,  nes ^. nesEpochStateL . prevPParamsEpochStateL)
                    , case nes ^. nesEpochStateL . futurePParamsEpochStateL of
                        NoPParamsUpdate -> Nothing
                        DefinitePParamsUpdate pp -> Just (GovPParamsInDefiniteFuture, pp)
                        PotentialPParamsUpdate mp -> (GovPParamsInPossibleFuture,) <$> mp
                    ]
                ]
              )
            & addNamespacedChunks (Proxy @"gov/constitution/v0")
              (S.each
                [ ChunkEntry (GovConstitutionIn epoch) (GovConstitutionOut $ nes & queryConstitution)
                ])
            & addNamespacedChunks (Proxy @"gov/proposals/v0")
              (S.each
                [ ChunkEntry (GovProposalIn gasId) (GovProposalOut $ Proposals.toWire g)
                | let proposals = nes & flip queryProposals Set.empty
                , g@GovActionState {..} <- toList proposals
                ])
            & addNamespacedChunks (Proxy @"gov/committee/v0")
              (S.each
                [ ChunkEntry (GovCommitteeIn epoch) (GovCommitteeOut cms)
                | let cms = nes & queryCommitteeState
                ])
          )

{-
data TxIn' = TxIn' TxId Word16

instance FromCBOR TxIn' where
  fromCBOR = decodeRecordNamed "TxIn"
    (const 2)
    (TxIn' <$> fromCBOR <*> fromCBOR)
-}

localReadDecCBORHex :: FilePath -> IO (UTxO ConwayEra)
localReadDecCBORHex = either throwIO pure . decodeFullHex <=< LBS.readFile
  where
    decodeFullHex =
      Plain.decodeFull
        <=< first (DecoderErrorCustom "Invalid Hex encoding:" . T.pack) . Base16.decode