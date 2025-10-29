{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Export.Namespace.GovProposals
    ( GovProposalIn(..)
    , GovProposalOut(..)
    , toWire
    ) where

import Control.Monad (unless)
import Cardano.Ledger.Conway.Governance (GovActionState(..), GovActionId(..), GovAction(..), ProposalProcedure(..), GovPurposeId(..), GovActionIx(..), Vote(..))
import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys
import Cardano.Ledger.Export.Common ()
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.Ledger.Export.Namespace.GovConstitution ()
import Cardano.Ledger.Export.Namespace.GovPParams ()
import Cardano.Ledger.Export.Namespace.Snapshots ()
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Decoding as D
import Cardano.SCLS.CBOR.Canonical.Decoder
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Map (Map)
-- import Cardano.SCLS.Internal.Entry
import Cardano.SCLS.Internal.Entry.IsKey
import qualified Data.Set as Set
import Cardano.SCLS.Internal.Version
import Cardano.SCLS.Internal.NamespaceCodec
import Data.Proxy
-- import qualified Data.Map as Map
import Data.MemPack
import Data.MemPack.ByteOrdered
import Cardano.Ledger.Export.LedgerCBOR
import Data.Typeable (Typeable)


data GovProposalIn = GovProposalIn GovActionId
  deriving (Eq, Ord, Show)

instance MemPack GovActionIx where
    packedByteCount _ = 2
    packM (GovActionIx g) = do
        packWord16beM g
    unpackM = do
        g <- unpackBigEndianM
        return (GovActionIx g)

instance IsKey GovProposalIn where
    keySize = namespaceKeySize @"gov/proposals/v0"
    packKeyM (GovProposalIn GovActionId {..}) = do
        packM gaidTxId
        packM gaidGovActionIx
    unpackKeyM = do
        gaidTxId <- unpackM
        gaidGovActionIx <- unpackM
        return $ GovProposalIn GovActionId{..}

newtype GovProposalOut = GovProposalOut (GovActionState')
  deriving (Eq, Show)

deriving newtype instance ToCanonicalCBOR v (GovProposalOut)
deriving newtype instance FromCanonicalCBOR v (GovProposalOut)

instance ToCanonicalCBOR v (GovActionState') where
    toCanonicalCBOR v GovActionState'{..} =
        E.encodeMapLen 6
            <> E.encodeString "drep_votes" <> toCanonicalCBOR v gasDRepVotes
            <> E.encodeString "proposed_in" <> toCanonicalCBOR v gasProposedIn
            <> E.encodeString "expires_after" <> toCanonicalCBOR v gasExpiresAfter
            <> E.encodeString "committee_votes" <> toCanonicalCBOR v gasCommitteeVotes
            <> E.encodeString "stake_pool_votes" <> toCanonicalCBOR v gasStakePoolVotes
            <> E.encodeString "proposal_procedure" <> toCanonicalCBOR v gasProposalProcedure

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> D.Decoder s (Versioned v a)
decodeField fieldName = do
    s <- D.decodeStringCanonical
    unless (s == fieldName) $
      fail $ T.unpack $ "Expected field name " <> fieldName <> " but got " <> s
    fromCanonicalCBOR

toWire :: GovActionState ConwayEra -> GovActionState'
toWire GovActionState{..} = GovActionState' {..}

data GovActionState' = GovActionState'
  { gasCommitteeVotes :: !(Map (Credential 'HotCommitteeRole) Vote)
  , gasDRepVotes :: !(Map (Credential 'DRepRole) Vote)
  , gasStakePoolVotes :: !(Map (KeyHash 'StakePool) Vote)
  , gasProposalProcedure :: !(ProposalProcedure ConwayEra)
  , gasProposedIn :: !EpochNo
  , gasExpiresAfter :: !EpochNo
  }
  deriving (Eq, Show)

instance FromCanonicalCBOR v (GovActionState') where
    fromCanonicalCBOR = do
        6 <- D.decodeMapLenCanonical
        Versioned gasDRepVotes <- decodeField "drep_votes"
        Versioned gasProposedIn <- decodeField "proposed_in"
        Versioned gasExpiresAfter <- decodeField "expires_after"
        Versioned gasCommitteeVotes <- decodeField "committee_votes"
        Versioned gasStakePoolVotes <- decodeField "stake_pool_votes"
        Versioned gasProposalProcedure <- decodeField "proposal_procedure"
        pure $ Versioned GovActionState'{..}

instance ToCanonicalCBOR v (ProposalProcedure ConwayEra) where
    toCanonicalCBOR v ProposalProcedure{..} =
        E.encodeMapLen 4
            <> E.encodeString "anchor" <> toCanonicalCBOR v pProcAnchor
            <> E.encodeString "deposit" <> toCanonicalCBOR v pProcDeposit
            <> E.encodeString "gov_action" <> toCanonicalCBOR v pProcGovAction
            <> E.encodeString "return_address" <> toCanonicalCBOR v pProcReturnAddr

instance FromCanonicalCBOR v (ProposalProcedure ConwayEra) where
    fromCanonicalCBOR = do
        4 <- D.decodeMapLenCanonical
        Versioned pProcAnchor <- decodeField "anchor"
        Versioned pProcDeposit <- decodeField "deposit"
        Versioned pProcGovAction <- decodeField "gov_action"
        Versioned pProcReturnAddr <- decodeField "return_address"
        pure $ Versioned ProposalProcedure{..}

instance ToCanonicalCBOR v (GovAction ConwayEra) where
    toCanonicalCBOR v (ParameterChange purposeId pparamsUpdate mScriptHash) =
        toCanonicalCBOR v (0::Word8, purposeId, pparamsUpdate, mScriptHash)
    toCanonicalCBOR v (HardForkInitiation purposeId protVer) =
        toCanonicalCBOR v (1::Word8, toCanonicalCBOR v purposeId, toCanonicalCBOR v protVer)
    toCanonicalCBOR v (TreasuryWithdrawals withdrawals mScriptHash) =
        toCanonicalCBOR v (2::Word8, withdrawals, mScriptHash)
    toCanonicalCBOR v (NoConfidence purposeId) =
        toCanonicalCBOR v (3::Word8, purposeId)
    toCanonicalCBOR v (UpdateCommittee purposeId removedMembers addedMembers newThreshold) =
        toCanonicalCBOR v (4::Word8, purposeId, Set.toList removedMembers, addedMembers, newThreshold)
    toCanonicalCBOR v (NewConstitution purposeId constitution) =
        toCanonicalCBOR v (5::Word8, purposeId,  constitution)
    toCanonicalCBOR v (InfoAction) =
        toCanonicalCBOR v (6::Word8, E.encodeNull)

instance FromCanonicalCBOR v (GovAction ConwayEra) where
    fromCanonicalCBOR = do
        l <- D.decodeListLenCanonical
        tag <- D.decodeWord8Canonical
        case tag of
            0 | l == 4 -> do
                Versioned purposeId <- fromCanonicalCBOR
                Versioned pparamsUpdate <- fromCanonicalCBOR
                Versioned mScriptHash <- fromCanonicalCBOR
                pure $ Versioned $ ParameterChange purposeId pparamsUpdate mScriptHash
            1 | l == 3 -> do
                Versioned purposeId <- fromCanonicalCBOR
                Versioned protVer <- fromCanonicalCBOR
                pure $ Versioned $ HardForkInitiation purposeId protVer
            2 | l == 3 -> do
                Versioned withdrawals <- fromCanonicalCBOR
                Versioned mScriptHash <- fromCanonicalCBOR
                pure $ Versioned $ TreasuryWithdrawals withdrawals mScriptHash
            3 | l == 2 -> do
                Versioned purposeId <- fromCanonicalCBOR
                pure $ Versioned $ NoConfidence purposeId
            4 | l == 5 -> do
                Versioned purposeId <- fromCanonicalCBOR
                Versioned removedMembersList <- fromCanonicalCBOR
                let removedMembers = Set.fromList removedMembersList
                Versioned addedMembers <- fromCanonicalCBOR
                Versioned newThreshold <- fromCanonicalCBOR
                pure $ Versioned $ UpdateCommittee purposeId removedMembers addedMembers newThreshold
            5 | l == 3 -> do
                Versioned purposeId <- fromCanonicalCBOR
                Versioned constitution <- fromCanonicalCBOR
                pure $ Versioned $ NewConstitution purposeId constitution
            6 | l == 2 -> do
                _ <- D.decodeNull
                pure $ Versioned InfoAction
            _ -> fail $ "Unknown GovAction tag: " ++ show tag


deriving via (LedgerCBOR v (GovPurposeId purpose ConwayEra)) instance Typeable purpose => ToCanonicalCBOR v (GovPurposeId purpose ConwayEra)
deriving via (LedgerCBOR v (GovPurposeId purpose ConwayEra)) instance Typeable purpose => FromCanonicalCBOR v (GovPurposeId purpose ConwayEra)
deriving via (LedgerCBOR v (Vote)) instance ToCanonicalCBOR v (Vote)
deriving via (LedgerCBOR v (Vote)) instance FromCanonicalCBOR v (Vote)

type instance NamespaceKeySize "gov/proposals/v0" = 34

instance KnownNamespace "gov/proposals/v0" where
  type NamespaceKey "gov/proposals/v0" = GovProposalIn
  type NamespaceEntry "gov/proposals/v0" = GovProposalOut

instance CanonicalCBOREntryEncoder "gov/proposals/v0" GovProposalOut where
  encodeEntry n = toCanonicalCBOR (Proxy @V1) n

instance CanonicalCBOREntryDecoder "gov/proposals/v0" GovProposalOut where
  decodeEntry = VersionedNS . unVer @V1 <$> fromCanonicalCBOR