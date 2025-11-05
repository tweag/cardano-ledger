{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main where

-- import Cardano.Ledger.Shelley.LedgerState
-- import Cardano.Ledger.State.Query
-- import Cardano.Ledger.State.UTxO

import Control.Exception (throwIO)
import Data.Bifunctor (first)
import Control.Monad
import Data.Function ((&))
import Cardano.SCLS.Internal.Serializer.Dump.Plan
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Cardano.Ledger.Mary (MaryEra)
-- import Data.Text as T (pack)
import qualified Data.ByteString.Base16.Lazy as Base16
import Cardano.Ledger.Binary.Plain as Plain
import Options.Applicative
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Export.Namespace.UTxO
import Cardano.Ledger.State -- Core (UTxO (..))
-- import Cardano.Ledger.UTxO
-- import Cardano.Ledger.State.UTxO (CurrentEra) -- , readHexUTxO, readNewEpochState)
import Cardano.Chain.UTxO (TxIn, TxId) -- , TxOut)
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import Data.Foldable (for_)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Data.Word (Word16)
-- import qualified GHC.Exts as GHC
import qualified GHC.Generics as GHC

import Cardano.Ledger.Export.Namespace.UTxO
-- import Cardano.Ledger.Conway.Era

import qualified Cardano.SCLS.Internal.Serializer.External.Impl as External (serialize)
import Cardano.SCLS.Internal.Entry
import Cardano.SCLS.Internal.Serializer.MemPack
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
    = CmdCreateFile FilePath FilePath
    deriving (Show)

optsParser :: Parser Cmd
optsParser = hsubparser
  (command "create" (info createCommand (progDesc "Create canonical file"))
  )
  where
    createCommand = CmdCreateFile
      <$> argument str (metavar "UTXO_HEX_FILE")
      <*> argument str (metavar "SCLS_FILE")

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
    CmdCreateFile utxoFilePath _outputFile -> do
        putStrLn "Creating file..."
        putStrLn $ "Reading UTxO from " ++ utxoFilePath
        UTxO utxo <- localReadDecCBORHex utxoFilePath

        let fileName = "scls-utxo.scls"

        External.serialize
          fileName
          Mainnet
          (SlotNo 1)
          (defaultSerializationPlan & addChunks
              (S.each
                [ "utxo/v0" S.:>
                    (S.each
                      [ ChunkEntry
                          (UtxoKeyIn txin)
                          (RawBytes $ toStrictByteString $ toCanonicalCBOR (Proxy :: Proxy V1) $ UtxoOutBabbage txout)
                      | (txin, txout) <- Map.toList utxo
                      ])
                ]))

data TxIn' = TxIn' TxId Word16

instance FromCBOR TxIn' where
  fromCBOR = decodeRecordNamed "TxIn"
    (const 2)
    (TxIn' <$> fromCBOR <*> fromCBOR)

localReadDecCBORHex :: FilePath -> IO (UTxO ConwayEra)
localReadDecCBORHex = either throwIO pure . decodeFullHex <=< LBS.readFile
  where
    decodeFullHex =
      Plain.decodeFull
        <=< first (DecoderErrorCustom "Invalid Hex encoding:" . T.pack) . Base16.decode