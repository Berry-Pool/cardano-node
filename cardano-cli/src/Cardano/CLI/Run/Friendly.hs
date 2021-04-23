{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBodyBS) where

import           Cardano.Prelude

import           Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Yaml (array)
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)

import           Cardano.Ledger.Shelley.TxBody (MIRPot (ReservesMIR, TreasuryMIR))

import           Cardano.Api as Api
import           Cardano.Api.Shelley (Address (ShelleyAddress), StakeAddress (..),
                   StakeCredential (..), StakePoolParameters (..), fromShelleyPaymentCredential,
                   fromShelleyStakeCredential, fromShelleyStakeReference)

import           Cardano.CLI.Helpers (textShow)

friendlyTxBodyBS :: CardanoEra era -> TxBody era -> ByteString
friendlyTxBodyBS era =
  encodePretty (setConfCompare compare defConfig) . friendlyTxBody era

friendlyTxBody :: CardanoEra era -> TxBody era -> Aeson.Value
friendlyTxBody
  era
  (TxBody
    TxBodyContent
      { txAuxScripts
      , txCertificates
      , txFee
      , txIns
      , txMetadata
      , txMintValue
      , txOuts
      , txUpdateProposal
      , txValidityRange
      , txWithdrawals
      }) =
  object
    [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
    , "certificates" .= friendlyCertificates txCertificates
    , "era" .= era
    , "fee" .= friendlyFee txFee
    , "inputs" .= friendlyInputs txIns
    , "metadata" .= friendlyMetadata txMetadata
    , "mint" .= friendlyMintValue txMintValue
    , "outputs" .= map friendlyTxOut txOuts
    , "update proposal" .= friendlyUpdateProposal txUpdateProposal
    , "validity range" .= friendlyValidityRange era txValidityRange
    , "withdrawals" .= friendlyWithdrawals txWithdrawals
    ]

-- | Special case of validity range:
-- in Shelley, upper bound is TTL, and no lower bound
pattern ShelleyTtl
  :: SlotNo -> (TxValidityLowerBound era, TxValidityUpperBound era)
pattern ShelleyTtl ttl <-
  ( TxValidityNoLowerBound
  , TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
  )

friendlyValidityRange
  :: CardanoEra era
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
  -> Aeson.Value
friendlyValidityRange era = \case
  ShelleyTtl ttl -> object ["time to live" .= ttl]
  (lowerBound, upperBound)
    | isLowerBoundSupported || isUpperBoundSupported ->
        object
          [ "lower bound" .=
                case lowerBound of
                  TxValidityNoLowerBound -> Null
                  TxValidityLowerBound _ s -> toJSON s
          , "upper bound" .=
              case upperBound of
                TxValidityNoUpperBound _ -> Null
                TxValidityUpperBound _ s -> toJSON s
          ]
    | otherwise -> Null
 where
  isLowerBoundSupported = isJust $ validityLowerBoundSupportedInEra era
  isUpperBoundSupported = isJust $ validityUpperBoundSupportedInEra era

friendlyWithdrawals :: TxWithdrawals ViewTx era -> Aeson.Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object $
        "address" .= serialiseAddress addr :
        "amount" .= friendlyLovelace amount :
        friendlyStakeAddress addr
    | (addr, amount, _) <- withdrawals
    ]

friendlyStakeAddress :: StakeAddress -> [(Text, Aeson.Value)]
friendlyStakeAddress (StakeAddress net cred) =
  [ "network" .= net
  , friendlyStakeCredential $ fromShelleyStakeCredential cred
  ]

friendlyTxOut :: TxOut CtxTx era -> Aeson.Value
friendlyTxOut (TxOut addr amount mdatum) =
  object $
    case addr of
      AddressInEra ByronAddressInAnyEra byronAdr ->
        [ "address era" .= String "Byron"
        , "address" .= serialiseAddress byronAdr
        , "amount" .= friendlyTxOutValue amount
        ]
      AddressInEra (ShelleyAddressInEra sbe) saddr@(ShelleyAddress net cred stake) ->
        let preAlonzo :: [Aeson.Pair]
            preAlonzo =
              friendlyPaymentCredential (fromShelleyPaymentCredential cred) :
              [ "address era" .= Aeson.String "Shelley"
              , "network" .= net
              , "address" .= serialiseAddress saddr
              , "amount" .= friendlyTxOutValue amount
              , "stake reference" .=
                  friendlyStakeReference (fromShelleyStakeReference stake)
              ]
            datum :: ShelleyBasedEra era -> [Aeson.Pair]
            datum ShelleyBasedEraShelley = []
            datum ShelleyBasedEraAllegra = []
            datum ShelleyBasedEraMary = []
            datum ShelleyBasedEraAlonzo = ["datum" .= renderDatum mdatum]
        in preAlonzo ++ datum sbe
 where
  renderDatum :: TxOutDatum CtxTx era -> Aeson.Value
  renderDatum TxOutDatumNone = Aeson.Null
  renderDatum (TxOutDatumHash _ h) =
    Aeson.String $ serialiseToRawBytesHexText h
  renderDatum (TxOutDatum _ sData) =
    scriptDataToJson ScriptDataJsonDetailedSchema sData

friendlyStakeReference :: StakeAddressReference -> Aeson.Value
friendlyStakeReference = \case
  NoStakeAddress -> Null
  StakeAddressByPointer ptr -> String (show ptr)
  StakeAddressByValue cred -> object [friendlyStakeCredential cred]

friendlyUpdateProposal :: TxUpdateProposal era -> Aeson.Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ p -> String $ textShow p

friendlyCertificates :: TxCertificates ViewTx era -> Aeson.Value
friendlyCertificates = \case
  TxCertificatesNone -> Null
  TxCertificates _ cs _ -> array $ map friendlyCertificate cs

friendlyCertificate :: Certificate -> Aeson.Value
friendlyCertificate =
  object . (:[]) .
  \case
    -- Stake address certificates
    StakeAddressRegistrationCertificate credential ->
      "stake address registration" .=
        object [friendlyStakeCredential credential]
    StakeAddressDeregistrationCertificate credential ->
      "stake address deregistration" .=
        object [friendlyStakeCredential credential]
    StakeAddressDelegationCertificate credential poolId ->
      "stake address delegation" .=
        object [friendlyStakeCredential credential, "pool" .= poolId]

    -- Stake pool certificates
    StakePoolRegistrationCertificate parameters ->
      "stake pool registration" .= friendlyStakePoolParameters parameters
    StakePoolRetirementCertificate poolId epochNo ->
      "stake pool retirement" .= object ["pool" .= poolId, "epoch" .= epochNo]

    -- Special certificates
    GenesisKeyDelegationCertificate genesisKeyHash delegateKeyHash vrfKeyHash ->
      "genesis key delegation" .=
        object
          [ "genesis key hash" .= serialiseToRawBytesHexText genesisKeyHash
          , "delegate key hash" .= serialiseToRawBytesHexText delegateKeyHash
          , "VRF key hash" .= serialiseToRawBytesHexText vrfKeyHash
          ]
    MIRCertificate pot target ->
      "MIR" .= object ["pot" .= friendlyMirPot pot, friendlyMirTarget target]

friendlyMirTarget :: MIRTarget -> (Text, Aeson.Value)
friendlyMirTarget = \case
  StakeAddressesMIR addresses ->
    "target stake addresses" .=
      [ object
          [ friendlyStakeCredential credential
          , "amount" .= friendlyLovelace lovelace
          ]
      | (credential, lovelace) <- addresses
      ]
  SendToReservesMIR amount -> "send to reserves" .= friendlyLovelace amount
  SendToTreasuryMIR amount -> "send to treasury" .= friendlyLovelace amount

friendlyStakeCredential :: StakeCredential -> (Text, Aeson.Value)
friendlyStakeCredential = \case
  StakeCredentialByKey keyHash ->
    "stake credential key hash" .= serialiseToRawBytesHexText keyHash
  StakeCredentialByScript scriptHash ->
    "stake credential script hash" .= serialiseToRawBytesHexText scriptHash

friendlyPaymentCredential :: PaymentCredential -> (Text, Aeson.Value)
friendlyPaymentCredential = \case
  PaymentCredentialByKey keyHash ->
    "payment credential key hash" .= serialiseToRawBytesHexText keyHash
  PaymentCredentialByScript scriptHash ->
    "payment credential script hash" .= serialiseToRawBytesHexText scriptHash

friendlyMirPot :: MIRPot -> Aeson.Value
friendlyMirPot = \case
  ReservesMIR -> "reserves"
  TreasuryMIR -> "treasury"

friendlyStakePoolParameters :: StakePoolParameters -> Aeson.Value
friendlyStakePoolParameters
  StakePoolParameters
    { stakePoolId
    , stakePoolVRF
    , stakePoolCost
    , stakePoolMargin
    , stakePoolRewardAccount
    , stakePoolPledge
    , stakePoolOwners
    , stakePoolRelays
    , stakePoolMetadata
    } =
  object
    [ "pool" .= stakePoolId
    , "VRF key hash" .= serialiseToRawBytesHexText stakePoolVRF
    , "cost" .= friendlyLovelace stakePoolCost
    , "margin" .= friendlyRational stakePoolMargin
    , "reward account" .= object (friendlyStakeAddress stakePoolRewardAccount)
    , "pledge" .= friendlyLovelace stakePoolPledge
    , "owners (stake key hashes)"
        .= map serialiseToRawBytesHexText stakePoolOwners
    , "relays" .= map textShow stakePoolRelays
    , "metadata" .= fmap textShow stakePoolMetadata
    ]

friendlyRational :: Rational -> Aeson.Value
friendlyRational r =
  String $
    case d of
      1 -> textShow n
      _ -> textShow n <> "/" <> textShow d
 where
  n = numerator r
  d = denominator r

friendlyFee :: TxFee era -> Aeson.Value
friendlyFee = \case
  TxFeeImplicit _ -> "implicit"
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Aeson.Value
friendlyLovelace (Lovelace value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: TxMintValue ViewTx era -> Aeson.Value
friendlyMintValue = \case
  TxMintNone -> Null
  TxMintValue _ v _ -> friendlyValue v

friendlyTxOutValue :: TxOutValue era -> Aeson.Value
friendlyTxOutValue = \case
  TxOutAdaOnly _ lovelace -> friendlyLovelace lovelace
  TxOutValue _ v -> friendlyValue v

friendlyValue :: Api.Value -> Aeson.Value
friendlyValue v =
  object
    [ case bundle of
        ValueNestedBundleAda q -> "lovelace" .= q
        ValueNestedBundle policy assets ->
          friendlyPolicyId policy .= friendlyAssets assets
    | bundle <- bundles
    ]
 where

  ValueNestedRep bundles = valueToNestedRep v

  friendlyPolicyId = ("policy " <>) . serialiseToRawBytesHexText

  friendlyAssets = Map.mapKeys friendlyAssetName

  friendlyAssetName = \case
    "" -> "default asset"
    name@(AssetName nameBS) ->
      "asset " <> serialiseToRawBytesHexText name <> nameAsciiSuffix
     where
      nameAsciiSuffix
        | nameIsAscii = " (" <> nameAscii <> ")"
        | otherwise = ""
      nameIsAscii = BSC.all (\c -> isAscii c && isAlphaNum c) nameBS
      nameAscii = Text.pack $ BSC.unpack nameBS

friendlyMetadata :: TxMetadataInEra era -> Aeson.Value
friendlyMetadata = \case
  TxMetadataNone -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Aeson.Value
friendlyMetadataValue = \case
  TxMetaNumber int -> toJSON int
  TxMetaBytes bytes -> String $ textShow bytes
  TxMetaList lst -> array $ map friendlyMetadataValue lst
  TxMetaMap m ->
    array
      [array [friendlyMetadataValue k, friendlyMetadataValue v] | (k, v) <- m]
  TxMetaText text -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Aeson.Value
friendlyAuxScripts = \case
  TxAuxScriptsNone -> Null
  TxAuxScripts _ scripts -> String $ textShow scripts

friendlyInputs :: [(TxIn, build)] -> Aeson.Value
friendlyInputs = toJSON . map fst
