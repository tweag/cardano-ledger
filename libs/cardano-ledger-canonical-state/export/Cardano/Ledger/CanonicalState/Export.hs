{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.CanonicalState.Export where

import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (
  EncCBOR (encCBOR),
  toLazyByteString,
  toPlainEncoding,
 )

import Cardano.SCLS.CDDL (knownNamespaceKeySizes)
import Cardano.SCLS.Internal.Entry.ChunkEntry (SomeChunkEntry)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  SerializationPlan,
 )
import Cardano.SCLS.Internal.Serializer.External.Impl (serialize)
import Cardano.Types.SlotNo (SlotNo (SlotNo))
import Control.Monad (void)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Data.Aeson (
  FromJSON,
  ToJSON (toEncoding),
  decodeFileStrict,
  defaultOptions,
  encode,
  genericToEncoding,
 )
import qualified Data.ByteString.Lazy as BSL
import Data.MemPack.Extra (RawBytes)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath, (</>))
import Test.Hspec.Core.Spec (
  Item (..),
  SpecWith,
  Tree (Leaf, Node, NodeWithCleanup),
  mapSpecForest,
 )
import Test.ImpSpec (ImpInit (impInitEnv), ImpSpec (ImpSpecEnv))

data Metadata = Metadata
  { era :: String
  , protocolVersion :: Version
  , description :: String
  , stateCount :: Int
  , path :: [String]
  }
  deriving (Generic, Show)

instance ToJSON Metadata where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Metadata

dump ::
  FilePath ->
  SlotNo ->
  SerializationPlan (SomeChunkEntry RawBytes) ResIO ->
  IO ()
dump filepath slotNo = do
  -- TODO: should we ignore?
  void
    . runResourceT
    . serialize
      filepath
      slotNo
      knownNamespaceKeySizes

class ExportState era where
  type ExportLedgerState era
  type ExportNewEpochState era
  dumpLedgerState :: ExportLedgerState era -> SerializationPlan (SomeChunkEntry RawBytes) ResIO
  dumpNewEpochState :: ExportNewEpochState era -> SerializationPlan (SomeChunkEntry RawBytes) ResIO
  getProtocolVersion :: ExportNewEpochState era -> Version

withScls ::
  forall era a tx failures event.
  (ExportState era, EncCBOR tx) =>
  ( ImpSpecEnv a ->
    (ExportNewEpochState era -> tx -> Either failures (ExportLedgerState era, event) -> IO ()) ->
    ImpSpecEnv a
  ) ->
  FilePath ->
  SpecWith (ImpInit a) ->
  SpecWith (ImpInit a)
withScls setHook baseDir =
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
        { itemRequirement
        , itemExample = originalItemExample
        } =
        item
          { itemExample = \params hook ->
              originalItemExample params $ \action ->
                hook $ \impInit ->
                  action
                    ( impInit
                        { impInitEnv = setHook (impInitEnv impInit) (export (reverse path) itemRequirement)
                        }
                    )
          }
    export path description nes tx res = do
      let protocolVersion = getProtocolVersion @era nes
      let dir = joinPath ([baseDir, "Protocol " <> show protocolVersion] ++ path ++ [description])
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
            else pure $ Metadata {era = "Conway", protocolVersion, description, stateCount = 0, path}
      createDirectoryIfMissing True dir
      let initialSlotNo = SlotNo 1 -- TODO: use the actual slot number if available
      dump (dir </> ("initial-" <> show stateCount <> ".scls")) initialSlotNo $ dumpNewEpochState @era nes
      -- Dump tx
      BSL.writeFile
        (dir </> ("txn-" <> show stateCount <> ".cbor"))
        (toLazyByteString (toPlainEncoding protocolVersion (encCBOR tx)))
      case res of
        Left _failures -> do
          -- TODO: dump the failures
          pure ()
        Right (st, _) -> do
          let finalSlotNo = SlotNo 1 -- TODO: use the actual slot number if available
          dump (dir </> ("final-" <> show stateCount <> ".scls")) finalSlotNo $ dumpLedgerState @era st -- TODO: this should be dumpNewEpochState, but we don't have the final NewEpochState available here.
      BSL.writeFile metadataFile $ encode $ ctx {stateCount = stateCount + 1}
