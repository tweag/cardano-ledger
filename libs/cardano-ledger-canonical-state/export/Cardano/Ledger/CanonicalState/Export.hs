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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Export (
  getTestDirFromMetadata,
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
  EncCBOR,
  serialize,
 )
import Cardano.Ledger.Core (
  Era (eraName),
  EraRule,
  EraTx (Tx),
  KeyHash,
  KeyRole (BlockIssuer),
  TopTx,
  eraProtVerHigh,
 )
import Cardano.SCLS.CDDL (knownNamespaceKeySizes)
import Cardano.SCLS.Internal.Entry.ChunkEntry (ChunkEntry, SomeChunkEntry)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  SerializationPlan,
  addNamespacedChunks,
 )
import qualified Cardano.SCLS.Internal.Serializer.External.Impl as SCLSS
import Cardano.SCLS.NamespaceCodec (KnownNamespace (..))
import Cardano.Slotting.EpochInfo (epochInfoSize, epochInfoSlotLength, fixedEpochInfo)
import Cardano.Slotting.Time (SlotLength)
import Cardano.Types.Namespace (Namespace)
import qualified Cardano.Types.SlotNo as SSlotNo
import Control.Monad (forM)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Control.State.Transition (PredicateFailure)
import Data.Aeson (
  FromJSON (..),
  FromJSON1 (..),
  FromJSON2 (..),
  Options (..),
  ToJSON (..),
  ToJSON1 (..),
  ToJSON2 (..),
  Value (Object),
  camelTo2,
  decodeFileStrict,
  defaultOptions,
  encodeFile,
  explicitToField,
  genericParseJSON,
  genericToEncoding,
  genericToJSON,
  object,
  pairs,
  parseJSON2,
  toEncoding2,
  toJSON2,
  withObject,
  (.:),
  (.=),
  (<?>),
 )
import Data.Aeson.Encoding (pair)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (JSONPathElement (Key))
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
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
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

instance ToJSON2 TxOrBlock where
  liftToJSON2 _ toA _ _ _toB _ (OrTx tx) = Object $ KM.singleton "tx" (toA tx)
  liftToJSON2 _ _toA _ _ toB _ (OrBlock block) = Object $ KM.singleton "block" (toB block)

  liftToEncoding2 _ toA _ _ _toB _ (OrTx tx) = pairs $ pair "tx" $ toA tx
  liftToEncoding2 _ _toA _ _ toB _ (OrBlock block) = pairs $ pair "block" $ toB block

instance ToJSON a => ToJSON1 (TxOrBlock a) where
  liftToJSON = liftToJSON2 omitField toJSON toJSONList
  liftToEncoding = liftToEncoding2 omitField toEncoding toEncodingList

instance (ToJSON a, ToJSON b) => ToJSON (TxOrBlock a b) where
  toJSON = toJSON2
  toEncoding = toEncoding2

instance FromJSON2 TxOrBlock where
  liftParseJSON2 _ pA _ _ pB _ (Object (KM.toList -> [(key, value)]))
    | key == orTx = OrTx <$> pA value <?> Key orTx
    | key == orBlock = OrBlock <$> pB value <?> Key orBlock
    where
      orTx, orBlock :: Key.Key
      orTx = "tx"
      orBlock = "block"
  liftParseJSON2 _ _ _ _ _ _ _ =
    fail $
      "expected an object with a single property "
        ++ "where the property key should be either "
        ++ "\"tx\" or \"block\""

instance FromJSON a => FromJSON1 (TxOrBlock a) where
  liftParseJSON = liftParseJSON2 omittedField parseJSON parseJSONList

instance (FromJSON a, FromJSON b) => FromJSON (TxOrBlock a b) where
  parseJSON = parseJSON2

mapTxOrBlock :: (tx -> tx') -> (block -> block') -> TxOrBlock tx block -> TxOrBlock tx' block'
mapTxOrBlock f _ (OrTx tx) = OrTx (f tx)
mapTxOrBlock _ g (OrBlock block) = OrBlock (g block)

mapTxOrBlockM ::
  Monad m => (tx -> m tx') -> (block -> m block') -> TxOrBlock tx block -> m (TxOrBlock tx' block')
mapTxOrBlockM f _ (OrTx tx) = OrTx <$> f tx
mapTxOrBlockM _ g (OrBlock block) = OrBlock <$> g block

encodingOptions :: Options
encodingOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    , constructorTagModifier = camelTo2 '_'
    }

data StateTransition = StateTransition
  { epochNo :: EpochNo
  , initialState :: FilePath
  , transactions :: TxOrBlock FilePath (FilePath, [FilePath])
  , finalState :: Either FilePath FilePath
  }
  deriving (Generic, Show)

instance ToJSON StateTransition where
  toEncoding (StateTransition {..}) =
    pairs $
      "epoch_no" .= epochNo
        <> "initial_state" .= initialState
        <> "transactions" .= transactions
        <> "final_state"
          .= ( case finalState of
                 Left failuresFile -> Failures failuresFile
                 Right finalStateFile -> FinalState finalStateFile
             )

  toJSON (StateTransition {..}) =
    object
      [ "epoch_no" .= epochNo
      , "initial_state" .= initialState
      , "transactions" .= transactions
      , "final_state"
          .= ( case finalState of
                 Left failuresFile -> Failures failuresFile
                 Right finalStateFile -> FinalState finalStateFile
             )
      ]

instance FromJSON StateTransition where
  parseJSON = withObject "StateTransition" $ \v ->
    StateTransition
      <$> v .: "epoch_no"
      <*> v .: "initial_state"
      <*> v .: "transactions"
      <*> ( v .: "final_state"
              >>= fmap
                ( \case
                    Failures failuresFile -> Left failuresFile
                    FinalState finalStateFile -> Right finalStateFile
                )
                . genericParseJSON encodingOptions
          )

data FinalStateOrFailuresPath
  = FinalState FilePath
  | Failures FilePath
  deriving (Generic, Show)

instance ToJSON FinalStateOrFailuresPath where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON FinalStateOrFailuresPath where
  parseJSON = genericParseJSON encodingOptions

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
      ( "fixed_epoch_size" .= eFixedEpochSize
          <> explicitToField (genericToEncoding encodingOptions) "fixed_slot_length" eFixedSlotLength
          <> "slots_per_kes_period" .= slotsPerKESPeriod
          <> "stability_window" .= stabilityWindow
          <> "randomness_stabilisation_window" .= randomnessStabilisationWindow
          <> "security_parameter" .= securityParameter
          <> "max_kes_evo" .= maxKESEvo
          <> "quorum" .= quorum
          <> "max_lovelace_supply" .= maxLovelaceSupply
          <> explicitToField (genericToEncoding encodingOptions) "active_slot_coeff" activeSlotCoeff
          <> "network_id" .= networkId
          <> "system_start" .= systemStart
      )
  toJSON (ExportGlobals {eFixedEpochSize, eFixedSlotLength, eGlobals = Globals {..}}) =
    object
      [ "fixed_epoch_size" .= eFixedEpochSize
      , explicitToField (genericToJSON encodingOptions) "fixed_slot_length" eFixedSlotLength
      , "slots_per_kes_period" .= slotsPerKESPeriod
      , "stability_window" .= stabilityWindow
      , "randomness_stabilisation_window" .= randomnessStabilisationWindow
      , "security_parameter" .= securityParameter
      , "max_kes_evo" .= maxKESEvo
      , "quorum" .= quorum
      , "max_lovelace_supply" .= maxLovelaceSupply
      , explicitToField (genericToJSON encodingOptions) "active_slot_coeff" activeSlotCoeff
      , "network_id" .= networkId
      , "system_start" .= systemStart
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
      <$> v .: "fixed_epoch_size"
      <*> (v .: "fixed_slot_length" >>= genericParseJSON encodingOptions)
      <*> v .: "slots_per_kes_period"
      <*> v .: "stability_window"
      <*> v .: "randomness_stabilisation_window"
      <*> v .: "security_parameter"
      <*> v .: "max_kes_evo"
      <*> v .: "quorum"
      <*> v .: "max_lovelace_supply"
      <*> (v .: "active_slot_coeff" >>= genericParseJSON encodingOptions)
      <*> v .: "network_id"
      <*> v .: "system_start"

data Metadata = Metadata
  { era :: String
  , eraImp :: String
  , protocolVersion :: Version
  , description :: String
  , stateTransitions :: [StateTransition]
  , path :: [String]
  , globals :: ExportGlobals
  }
  deriving (Generic)

getTestDirFromMetadata :: Metadata -> FilePath
getTestDirFromMetadata Metadata {..} =
  joinPath (["Protocol " <> show protocolVersion] ++ path ++ [description])

instance ToJSON Metadata where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON Metadata where
  parseJSON = genericParseJSON encodingOptions

dump ::
  FilePath ->
  SlotNo ->
  SerializationPlan (SomeChunkEntry RawBytes) ResIO ->
  IO (Either [Namespace] ())
dump filepath (SlotNo slotNo) =
  runResourceT
    . SCLSS.serialize
      filepath
      (SSlotNo.SlotNo slotNo)
      knownNamespaceKeySizes

type TxFailures era = NonEmpty (PredicateFailure (EraRule "LEDGER" era))

type BlockFailures era = NonEmpty (PredicateFailure (EraRule "BBODY" era))

class ExportCanonicalState era where
  type ExportLedgerState era
  dumpLedgerState :: ExportLedgerState era -> SerializationPlan (SomeChunkEntry RawBytes) ResIO
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
      let protocolVersion = eraProtVerHigh @era
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
                , path
                }
        doesFileExist tmpMetadataFile >>= \case
          True ->
            decodeFileStrict tmpMetadataFile >>= \case
              Just m
                | isSameScenario m defaultMetadata ->
                    pure m
                | otherwise -> do
                    appendMetadata metadataFile m
                    removeFile tmpMetadataFile
                    pure defaultMetadata
              Nothing -> do
                -- TODO: clean up the directory if the metadata file is corrupted, to avoid leaving around junk files?
                error $ "Failed to decode metadata file: " <> tmpMetadataFile
          False ->
            pure defaultMetadata
      let stateCountStr = show $ length stateTransitions
          dir = baseDir </> getTestDirFromMetadata metadata
      createDirectoryIfMissing True dir
      let initialStateFile = "initial-" <> stateCountStr <> ".scls"
      Right () <-
        dump (dir </> initialStateFile) slotNo $
          dumpLedgerState @era nes
      txFiles <- dumpTxOrBlock stateCountStr dir
      finalStateFile <-
        bimapM
          ( \serializeFailures -> do
              let failuresFile = "failures-" <> stateCountStr <> ".cbor"
              BSL.writeFile (dir </> failuresFile) $ serializeFailures protocolVersion
              pure failuresFile
          )
          ( \st -> do
              let finalStateFile = "final-" <> stateCountStr <> ".scls"
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
    && path m1 == path m2
    && description m1 == description m2

dumpTx ::
  forall era.
  (Era era, EncCBOR (Tx TopTx era)) =>
  Tx TopTx era ->
  String ->
  FilePath ->
  IO (TxOrBlock FilePath (FilePath, [FilePath]))
dumpTx tx stateCountStr dir = do
  let txFile = "txn-" <> stateCountStr <> ".cbor"
      protocolVersion = eraProtVerHigh @era
  BSL.writeFile
    (dir </> txFile)
    (serialize protocolVersion tx)
  pure (OrTx txFile)

dumpBlock ::
  forall era.
  (Era era, EncCBOR (Tx TopTx era)) =>
  KeyHash BlockIssuer ->
  StrictSeq (Tx TopTx era) ->
  String ->
  FilePath ->
  IO (TxOrBlock FilePath (FilePath, [FilePath]))
dumpBlock blockIssuer txs stateCountStr dir = do
  let blockIssuerFile = "block-" <> stateCountStr <> "-issuer.cbor"
      protocolVersion = eraProtVerHigh @era
  BSL.writeFile
    (dir </> blockIssuerFile)
    (serialize protocolVersion blockIssuer)
  fmap (OrBlock . (blockIssuerFile,)) $ forM (zip [0 :: Integer ..] (toList txs)) $ \(i, tx) -> do
    let txFile = "block-" <> stateCountStr <> "-tx-" <> show i <> ".cbor"
    BSL.writeFile
      (dir </> txFile)
      (serialize protocolVersion tx)
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
