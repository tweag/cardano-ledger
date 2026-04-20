{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Export (
  withScls,
  EraTestImp (..),
  ExportCanonicalState (..),
  Metadata (..),
  StateTransition (..),
  dump,
  ExportHooks (..),
  toGlobals,
  ExportGlobals,
  ExportCanonicalNamespace (..),
  addNamespaceToPlan,
  TxFailures,
  BlockFailures,
  ExportFailures (..),
  TxOrBlock (..),
  mapTxOrBlock,
  mapTxOrBlockM,
) where

import Cardano.Ledger.BaseTypes (
  EpochNo (EpochNo),
  EpochSize,
  Globals (..),
  SlotNo (SlotNo),
  Version,
  epochInfo,
  epochInfoPure,
 )
import Cardano.Ledger.Binary (
  EncCBOR (encCBOR),
  toLazyByteString,
  toPlainEncoding,
 )
import Cardano.Ledger.Core (
  Era (eraName),
  EraRule,
  EraTx (Tx),
  KeyHash,
  KeyRole (BlockIssuer),
  TopTx,
 )
import Cardano.SCLS.CDDL (knownNamespaceKeySizes)
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, SomeChunkEntry)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  SerializationPlan,
  addNamespacedChunks,
 )
import Cardano.SCLS.Internal.Serializer.External.Impl (serialize)
import Cardano.SCLS.NamespaceCodec (KnownNamespace (..))
import Cardano.Slotting.EpochInfo (epochInfoSize, epochInfoSlotLength, fixedEpochInfo)
import Cardano.Slotting.Time (SlotLength)
import Cardano.Types.Namespace (Namespace)
import qualified Cardano.Types.SlotNo as SSlotNo
import Control.Monad (forM)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Control.State.Transition (PredicateFailure)
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue (explicitToField),
  ToJSON (..),
  decodeFileStrict,
  defaultOptions,
  encodeFile,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
  object,
  pairs,
  withObject,
  (.:),
  (.=),
 )
import Data.Bifunctor (Bifunctor (first))
import Data.Bitraversable (bimapM)
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Functor.Identity (Identity (runIdentity))
import Data.MemPack.Extra (RawBytes)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence.Strict (StrictSeq)
import GHC.Base (NonEmpty)
import GHC.Generics (Generic)
import GHC.IsList (IsList (toList))
import GHC.TypeLits (KnownSymbol)
import Streaming.Prelude (Of, Stream)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath, (</>))
import Test.Hspec.Core.Spec (
  Item (..),
  SpecWith,
  Tree (Leaf, Node, NodeWithCleanup),
  mapSpecForest,
 )
import Test.ImpSpec (ImpInit (ImpInit, impInitEnv), ImpSpec (ImpSpecEnv))

data TxOrBlock tx block
  = OrTx tx
  | OrBlock block
  deriving (Show, Generic)

instance (ToJSON tx, ToJSON block) => ToJSON (TxOrBlock tx block) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON tx, FromJSON block) => FromJSON (TxOrBlock tx block)

mapTxOrBlock :: (tx -> tx') -> (block -> block') -> TxOrBlock tx block -> TxOrBlock tx' block'
mapTxOrBlock f _ (OrTx tx) = OrTx (f tx)
mapTxOrBlock _ g (OrBlock block) = OrBlock (g block)

mapTxOrBlockM ::
  Monad m => (tx -> m tx') -> (block -> m block') -> TxOrBlock tx block -> m (TxOrBlock tx' block')
mapTxOrBlockM f _ (OrTx tx) = OrTx <$> f tx
mapTxOrBlockM _ g (OrBlock block) = OrBlock <$> g block

data StateTransition = StateTransition
  { epochNo :: EpochNo
  , initialState :: FilePath
  , transactions :: TxOrBlock FilePath (FilePath, [FilePath])
  , finalState :: Either FilePath FilePath
  }
  deriving (Generic, Show)

instance ToJSON StateTransition where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON StateTransition

data ExportGlobals = ExportGlobals
  { eFixedEpochSize :: EpochSize
  , eFixedSlotLength :: SlotLength
  , eGlobals :: Globals
  }

toGlobals :: ExportGlobals -> Globals
toGlobals (ExportGlobals {..}) =
  eGlobals
    { epochInfo = fixedEpochInfo eFixedEpochSize eFixedSlotLength
    }

fromGlobals :: Globals -> ExportGlobals
fromGlobals globals =
  ExportGlobals
    { eFixedEpochSize = runIdentity $ epochInfoSize (epochInfoPure globals) (EpochNo 0)
    , eFixedSlotLength = runIdentity $ epochInfoSlotLength (epochInfoPure globals) (SlotNo 0)
    , eGlobals = globals
    }

instance ToJSON ExportGlobals where
  toEncoding (ExportGlobals {eFixedEpochSize, eFixedSlotLength, eGlobals = Globals {..}}) =
    pairs
      ( "fixedEpochSize" .= eFixedEpochSize
          <> explicitToField (genericToEncoding defaultOptions) "fixedSlotLength" eFixedSlotLength
          <> "slotsPerKESPeriod" .= slotsPerKESPeriod
          <> "stabilityWindow" .= stabilityWindow
          <> "randomnessStabilisationWindow" .= randomnessStabilisationWindow
          <> "securityParameter" .= securityParameter
          <> "maxKESEvo" .= maxKESEvo
          <> "quorum" .= quorum
          <> "maxLovelaceSupply" .= maxLovelaceSupply
          <> explicitToField (genericToEncoding defaultOptions) "activeSlotCoeff" activeSlotCoeff
          <> "networkId" .= networkId
          <> "systemStart" .= systemStart
      )
  toJSON (ExportGlobals {eFixedEpochSize, eFixedSlotLength, eGlobals = Globals {..}}) =
    object
      [ "fixedEpochSize" .= eFixedEpochSize
      , explicitToField (genericToJSON defaultOptions) "fixedSlotLength" eFixedSlotLength
      , "slotsPerKESPeriod" .= slotsPerKESPeriod
      , "stabilityWindow" .= stabilityWindow
      , "randomnessStabilisationWindow" .= randomnessStabilisationWindow
      , "securityParameter" .= securityParameter
      , "maxKESEvo" .= maxKESEvo
      , "quorum" .= quorum
      , "maxLovelaceSupply" .= maxLovelaceSupply
      , explicitToField (genericToJSON defaultOptions) "activeSlotCoeff" activeSlotCoeff
      , "networkId" .= networkId
      , "systemStart" .= systemStart
      ]

instance FromJSON ExportGlobals where
  parseJSON = withObject "ExportGlobals" $ \v ->
    ( \eFixedEpochSize eFixedSlotLength slotsPerKESPeriod stabilityWindow randomnessStabilisationWindow securityParameter maxKESEvo quorum maxLovelaceSupply activeSlotCoeff networkId systemStart ->
        ExportGlobals
          { eFixedEpochSize
          , eFixedSlotLength
          , eGlobals =
              Globals
                { epochInfo = fixedEpochInfo eFixedEpochSize eFixedSlotLength
                , slotsPerKESPeriod
                , stabilityWindow
                , randomnessStabilisationWindow
                , securityParameter
                , maxKESEvo
                , quorum
                , maxLovelaceSupply
                , activeSlotCoeff
                , networkId
                , systemStart
                }
          }
    )
      <$> v .: "fixedEpochSize"
      <*> (v .: "fixedSlotLength" >>= genericParseJSON defaultOptions)
      <*> v .: "slotsPerKESPeriod"
      <*> v .: "stabilityWindow"
      <*> v .: "randomnessStabilisationWindow"
      <*> v .: "securityParameter"
      <*> v .: "maxKESEvo"
      <*> v .: "quorum"
      <*> v .: "maxLovelaceSupply"
      <*> (v .: "activeSlotCoeff" >>= genericParseJSON defaultOptions)
      <*> v .: "networkId"
      <*> v .: "systemStart"

data Metadata = Metadata
  { era :: String
  , eraImp :: String
  , protocolVersion :: Version
  , description :: String
  , stateTransitions :: [StateTransition]
  , dir :: FilePath
  , globals :: ExportGlobals
  }
  deriving (Generic)

instance ToJSON Metadata where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Metadata

dump ::
  FilePath ->
  SlotNo ->
  SerializationPlan (SomeChunkEntry RawBytes) ResIO ->
  IO (Either [Namespace] ())
dump filepath (SlotNo slotNo) =
  runResourceT
    . serialize
      filepath
      (SSlotNo.SlotNo slotNo)
      knownNamespaceKeySizes

type TxFailures era = NonEmpty (PredicateFailure (EraRule "LEDGER" era))

type BlockFailures era = NonEmpty (PredicateFailure (EraRule "BBODY" era))

class ExportCanonicalState era where
  type ExportLedgerState era
  dumpLedgerState :: ExportLedgerState era -> SerializationPlan (SomeChunkEntry RawBytes) ResIO
  getProtocolVersion :: ExportLedgerState era -> Version
  getEpochNo :: ExportLedgerState era -> EpochNo

class ExportFailures era where
  serializeTxFailures :: Version -> TxFailures era -> BSL.ByteString
  serializeBlockFailures :: Version -> BlockFailures era -> BSL.ByteString

data EraTestImp
  = ConwayEraTestImp

eraTestImpName :: EraTestImp -> String
eraTestImpName = \case
  ConwayEraTestImp -> "Conway"

appendMetadata :: FilePath -> Metadata -> IO ()
appendMetadata filePath metadata =
  doesFileExist filePath >>= \case
    True ->
      decodeFileStrict filePath >>= \case
        Just m -> encodeFile filePath (metadata : m)
        Nothing -> error $ "Failed to decode existing metadata file: " <> filePath
    False -> encodeFile filePath [metadata]

withScls ::
  forall era a.
  (Era era, EncCBOR (Tx TopTx era), ExportCanonicalState era, ExportFailures era) =>
  EraTestImp ->
  ( ( Globals ->
      SlotNo ->
      ExportLedgerState era ->
      Tx TopTx era ->
      Either (TxFailures era) (ExportLedgerState era) ->
      IO ()
    ) ->
    ImpSpecEnv a ->
    ImpSpecEnv a
  ) ->
  ( ( Globals ->
      SlotNo ->
      ExportLedgerState era ->
      KeyHash BlockIssuer ->
      StrictSeq (Tx TopTx era) ->
      Either (BlockFailures era) (ExportLedgerState era) ->
      IO ()
    ) ->
    ImpSpecEnv a ->
    ImpSpecEnv a
  ) ->
  FilePath ->
  SpecWith (ImpInit a) ->
  SpecWith (ImpInit a)
withScls eraImp setTxHook setBlockHook baseDir =
  mapSpecForest $
    mapForest []
  where
    mapForest path = map $ \case
      Node d forest -> Node d (mapForest (d : path) forest)
      NodeWithCleanup (Just (d, l)) c forest -> NodeWithCleanup (Just (d, l)) c (mapForest (d : path) forest)
      NodeWithCleanup Nothing c forest -> NodeWithCleanup Nothing c (mapForest path forest)
      Leaf item -> Leaf $ mapItem path item
    mapItem
      path
      item@Item
        { itemRequirement = description
        , itemExample = originalItemExample
        } =
        item
          { itemExample = \params hook ->
              originalItemExample params $ \hookAction ->
                hook $ \impInit@ImpInit {impInitEnv} ->
                  hookAction $
                    impInit
                      { impInitEnv =
                          impInitEnv
                            & setTxHook (exportTx (reverse path) description)
                            & setBlockHook (exportBlock (reverse path) description)
                      }
          }
    exportTx path description globals slotNo nes tx res =
      export path description globals slotNo nes (dumpTx tx) (first (flip $ serializeTxFailures @era) res)
    exportBlock path description globals slotNo nes blockIssuer txs res =
      export
        path
        description
        globals
        slotNo
        nes
        (dumpBlock blockIssuer txs)
        (first (flip $ serializeBlockFailures @era) res)
    export path description globals slotNo nes dumpTxOrBlock res = do
      let protocolVersion = getProtocolVersion @era nes
          dirLocalPath = joinPath (["Protocol " <> show protocolVersion] ++ path ++ [description])
          dir = baseDir </> dirLocalPath
          metadataFile = baseDir </> "metadata.json"
          tmpMetadataFile = baseDir </> "metadata.tmp"
      metadata@Metadata {stateTransitions} <- do
        let defaultMetadata =
              Metadata
                { eraImp = eraTestImpName eraImp
                , era = eraName @era
                , protocolVersion
                , description
                , stateTransitions = []
                , globals = fromGlobals globals
                , dir = dirLocalPath
                }
        doesFileExist tmpMetadataFile >>= \case
          True ->
            decodeFileStrict tmpMetadataFile >>= \case
              Just m
                | isSameScenario m defaultMetadata ->
                    pure m
                | otherwise -> do
                    appendMetadata metadataFile m
                    pure defaultMetadata
              Nothing -> do
                -- TODO: clean up the directory if the metadata file is corrupted, to avoid leaving around junk files?
                error $ "Failed to decode metadata file: " <> metadataFile
          False ->
            pure defaultMetadata
      let stateCount = length stateTransitions
      createDirectoryIfMissing True dir
      let initialStateFile = "initial-" <> show stateCount <> ".scls"
      Right () <-
        dump (dir </> initialStateFile) slotNo $
          dumpLedgerState @era nes
      txFiles <- dumpTxOrBlock protocolVersion stateCount dir
      finalStateFile <-
        bimapM
          ( \serializeFailures -> do
              let failuresFile = "failures-" <> show stateCount <> ".cbor"
              BSL.writeFile (dir </> failuresFile) $ serializeFailures protocolVersion
              pure failuresFile
          )
          ( \st -> do
              let finalStateFile = "final-" <> show stateCount <> ".scls"
              Right () <-
                dump (dir </> finalStateFile) slotNo $
                  dumpLedgerState @era st
              pure finalStateFile
          )
          res
      let epochNo = getEpochNo @era nes
      encodeFile tmpMetadataFile $
        metadata
          { stateTransitions =
              ( StateTransition
                  { epochNo
                  , initialState = initialStateFile
                  , finalState = finalStateFile
                  , transactions = txFiles
                  }
              )
                : stateTransitions
          }

isSameScenario :: Metadata -> Metadata -> Bool
isSameScenario m1 m2 =
  era m1 == era m2
    && eraImp m1 == eraImp m2
    && protocolVersion m1 == protocolVersion m2
    && description m1 == description m2

dumpTx ::
  EncCBOR (Tx TopTx era) =>
  Tx TopTx era -> Version -> Int -> FilePath -> IO (TxOrBlock FilePath (FilePath, [FilePath]))
dumpTx tx protocolVersion stateCount dir = do
  let txFile = "txn-" <> show stateCount <> ".cbor"
  BSL.writeFile
    (dir </> txFile)
    (toLazyByteString (toPlainEncoding protocolVersion (encCBOR tx)))
  pure (OrTx txFile)

dumpBlock ::
  EncCBOR (Tx TopTx era) =>
  KeyHash BlockIssuer ->
  StrictSeq (Tx TopTx era) ->
  Version ->
  Int ->
  FilePath ->
  IO (TxOrBlock FilePath (FilePath, [FilePath]))
dumpBlock blockIssuer txs protocolVersion stateCount dir = do
  let blockIssuerFile = "block-" <> show stateCount <> "-issuer.cbor"
  BSL.writeFile
    (dir </> blockIssuerFile)
    (toLazyByteString (toPlainEncoding protocolVersion (encCBOR blockIssuer)))
  fmap (OrBlock . (blockIssuerFile,)) $ forM (zip [0 :: Integer ..] (toList txs)) $ \(i, tx) -> do
    let txFile = "block-" <> show stateCount <> "-tx-" <> show i <> ".cbor"
    BSL.writeFile
      (dir </> txFile)
      (toLazyByteString (toPlainEncoding protocolVersion (encCBOR tx)))
    pure txFile

data ExportHooks era = ExportHooks
  { exportTxHook ::
      Globals ->
      SlotNo ->
      ExportLedgerState era ->
      Tx TopTx era ->
      Either (TxFailures era) (ExportLedgerState era) ->
      IO ()
  , exportBlockHook ::
      Globals ->
      SlotNo ->
      ExportLedgerState era ->
      KeyHash BlockIssuer ->
      StrictSeq (Tx TopTx era) ->
      Either (BlockFailures era) (ExportLedgerState era) ->
      IO ()
  }

class KnownNamespace ns => ExportCanonicalNamespace era ns where
  exportNamespace ::
    Monad m =>
    ExportLedgerState era ->
    Stream (Of (ChunkEntry (NamespaceKey ns) (NamespaceEntry ns))) m ()

addNamespaceToPlan ::
  forall era ns m.
  (Monad m, KnownSymbol ns, ExportCanonicalNamespace era ns) =>
  ExportLedgerState era ->
  SerializationPlan (SomeChunkEntry RawBytes) m ->
  SerializationPlan (SomeChunkEntry RawBytes) m
addNamespaceToPlan s = addNamespacedChunks (Proxy :: Proxy ns) (exportNamespace @era @ns s)
