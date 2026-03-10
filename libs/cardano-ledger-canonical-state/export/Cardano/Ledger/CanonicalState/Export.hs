{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.CanonicalState.Export where

import Cardano.Ledger.BaseTypes (SlotNo (SlotNo), Version)
import Cardano.Ledger.Binary (
  EncCBOR (encCBOR),
  Encoding,
  encodeList,
  toLazyByteString,
  toPlainEncoding,
 )
import Cardano.Ledger.Core (Era (eraName), EraRule, EraTx (Tx), TopTx)
import Cardano.SCLS.CDDL (knownNamespaceKeySizes)
import Cardano.SCLS.Internal.Entry.ChunkEntry (SomeChunkEntry)
import Cardano.SCLS.Internal.Serializer.Dump.Plan (
  SerializationPlan,
 )
import Cardano.SCLS.Internal.Serializer.External.Impl (serialize)
import qualified Cardano.Types.SlotNo as SSlotNo
import Control.Monad (void)
import Control.Monad.Trans.Resource (ResIO, runResourceT)
import Control.State.Transition (PredicateFailure)
import Data.Aeson (
  FromJSON,
  ToJSON (toEncoding),
  decodeFileStrict,
  defaultOptions,
  encode,
  genericToEncoding,
 )
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.MemPack.Extra (RawBytes)
import GHC.Base (NonEmpty)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath, (</>))
import Test.Hspec.Core.Spec (
  Item (..),
  SpecWith,
  Tree (Leaf, Node, NodeWithCleanup),
  mapSpecForest,
 )
import Test.ImpSpec (ImpInit (ImpInit, impInitEnv), ImpSpec (ImpSpecEnv))

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
dump filepath (SlotNo slotNo) =
  -- TODO: should we ignore?
  void
    . runResourceT
    . serialize
      filepath
      (SSlotNo.SlotNo slotNo)
      knownNamespaceKeySizes

type TxFailures era = NonEmpty (PredicateFailure (EraRule "LEDGER" era))

type BlockFailures era = NonEmpty (PredicateFailure (EraRule "BBODY" era))

class ExportState era where
  type ExportLedgerState era
  dumpLedgerState :: ExportLedgerState era -> SerializationPlan (SomeChunkEntry RawBytes) ResIO
  getProtocolVersion :: ExportLedgerState era -> Version
  encodeTxFailures :: TxFailures era -> Encoding
  encodeBlockFailures :: BlockFailures era -> Encoding

withScls ::
  forall era a.
  (Era era, ExportState era, EncCBOR (Tx TopTx era)) =>
  ( ( SlotNo ->
      ExportLedgerState era ->
      Tx TopTx era ->
      Either (TxFailures era) (ExportLedgerState era) ->
      IO ()
    ) ->
    ImpSpecEnv a ->
    ImpSpecEnv a
  ) ->
  ( ( SlotNo ->
      ExportLedgerState era ->
      [Tx TopTx era] ->
      Either (BlockFailures era) (ExportLedgerState era) ->
      IO ()
    ) ->
    ImpSpecEnv a ->
    ImpSpecEnv a
  ) ->
  FilePath ->
  SpecWith (ImpInit a) ->
  SpecWith (ImpInit a)
withScls setTxHook setBlockHook baseDir =
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
    exportTx path description slotNo nes tx res =
      export path description slotNo nes (encCBOR tx) (first (encodeTxFailures @era) res)
    exportBlock path description slotNo nes txs res =
      -- TODO: review if we want to use `encodeList`
      export path description slotNo nes (encodeList encCBOR txs) (first (encodeBlockFailures @era) res)
    export path description slotNo nes tx res = do
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
            else pure $ Metadata {era = eraName @era, protocolVersion, description, stateCount = 0, path}
      createDirectoryIfMissing True dir
      dump (dir </> ("initial-" <> show stateCount <> ".scls")) slotNo $ dumpLedgerState @era nes
      -- Dump tx
      BSL.writeFile
        (dir </> ("txn-" <> show stateCount <> ".cbor"))
        (toLazyByteString (toPlainEncoding protocolVersion tx))
      case res of
        Left failures -> do
          BSL.writeFile
            (dir </> ("failures-" <> show stateCount <> ".cbor"))
            (toLazyByteString (toPlainEncoding protocolVersion failures))
        Right st ->
          dump (dir </> ("final-" <> show stateCount <> ".scls")) slotNo $ dumpLedgerState @era st
      BSL.writeFile metadataFile $ encode $ ctx {stateCount = stateCount + 1}
