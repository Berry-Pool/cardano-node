{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Ledger CDDL Serialisation
--
module Cardano.Api.SerialiseLedgerCddl
  (
  ) where

import           Prelude

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)

import           Control.Exception (bracketOnError)
import           Control.Monad (unless)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither)

import           System.Directory (removeFile, renameFile)
import           System.FilePath (splitFileName, (<.>))
import           System.IO (hClose, openTempFile)

import           Cardano.Binary (DecoderError)
import qualified Cardano.Binary as CBOR

import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.Tx
import           Cardano.Api.TxBody


-- Why have we gone this route? The serialization format of `TxBody era`
-- differs from the CDDL. We serialize to an intermediate type in order to simplify
-- the specification of Plutus scripts and to avoid users having to think about
-- and construct redeemer pointers.
-- Modifying the existing TextEnvelope machinery to encompass
-- this would result in a lot of unnecessary changes where the serialization
-- already defaults to the CDDL spec. Because we are only
-- interested in serializing unsigned and signed transactions in the ledger's
-- CDDL specification we have decided to create a type specifically for this situation,

-- TODO: look at HasTextEnvelope (Tx era) for inspiration with respect to teCddlType
-- Which could really be a text field.
data TextEnvelopeCddl = TextEnvelopeCddl
  { teCddlType :: !TextEnvelopeCddlType
  , teCddlDescription :: !Text
  , teCddlRawCBOR :: !ByteString
  } deriving (Eq, Show)

data TextEnvelopeCddlType
  = TextEnvelopeCddlWitnessedTx
  | TextEnvelopeCddlUnwitnessedTx
  deriving (Eq, Show)


data TextEnvelopeCddlError
  = TextEnvelopeCddlErrExpectedUnwitnessed TextEnvelopeCddl
  | TextEnvelopeCddlErrExpectedWitnessed TextEnvelopeCddl
  | TextEnvelopeCddlErrCBORDecodingError DecoderError

-- TODO: We need to check Tx era directly for witnesses and error on them
serialiseWitnessedTxLedgerCddl :: IsCardanoEra era => Tx era -> TextEnvelopeCddl
serialiseWitnessedTxLedgerCddl tx =
  TextEnvelopeCddl
    { teCddlType = TextEnvelopeCddlWitnessedTx
    , teCddlDescription = "Witnessed Ledger Tx in CDDL Format"
    , teCddlRawCBOR = serialiseToCBOR tx
    -- The SerialiseAsCBOR (Tx era) instance serializes to the CDDL format
    }

-- TODO: Likewise here. Makes more sense to check the Tx directly
deserialiseWitnessedTxLedgerCddl
  :: CardanoEra era
  -> TextEnvelopeCddl
  -> Either TextEnvelopeCddlError (Tx era)
deserialiseWitnessedTxLedgerCddl era tec =
  case teCddlType tec of
    TextEnvelopeCddlUnwitnessedTx -> Left $ TextEnvelopeCddlErrExpectedWitnessed tec
    TextEnvelopeCddlWitnessedTx -> first TextEnvelopeCddlErrCBORDecodingError
                                     $ deserialiseTx era $ teCddlRawCBOR tec

-- TODO: Need to clarify that we are talking about no KEY witnesses.
-- consider a function to check this and produce a wrapped TxBody in a newtype.
-- We will potentially have script witnesses
-- in the tx body
serialiseUnwitnessedTxLedgerCddl :: IsCardanoEra era => TxBody era -> TextEnvelopeCddl
serialiseUnwitnessedTxLedgerCddl tBody =
  TextEnvelopeCddl
    { teCddlType = TextEnvelopeCddlUnwitnessedTx
    , teCddlDescription = "Unwitnessed Ledger Tx in CDDL Format"
    , teCddlRawCBOR = serialiseToCBOR $ makeSignedTransaction [] tBody
    -- The SerialiseAsCBOR (Tx era) instance serializes to the CDDL format
    }

deserialiseUnwitnessedTxLedgerCddl
  :: CardanoEra era
  -> TextEnvelopeCddl
  -> Either TextEnvelopeCddlError (TxBody era)
deserialiseUnwitnessedTxLedgerCddl era tec =
  case teCddlType tec of
    TextEnvelopeCddlWitnessedTx ->
      Left $ TextEnvelopeCddlErrExpectedUnwitnessed tec
    TextEnvelopeCddlUnwitnessedTx -> do
      unwitTx <- first TextEnvelopeCddlErrCBORDecodingError
                   $ deserialiseTx era $ teCddlRawCBOR tec
      case getTxBodyAndWitnesses unwitTx of
        -- TODO: Use getTxWitnesses instead
        (bdy, []) -> Right bdy
        (bdy, wits) -> Left $ TextEnvelopeCddlErrExpectedUnwitnessed tec


deserialiseTx
  :: forall era. IsCardanoEra era
  => CardanoEra era
  -> ByteString
  -> Either DecoderError (Tx era)
deserialiseTx era bs =
  case era of
    ByronEra -> ByronTx <$>
                  CBOR.decodeFullAnnotatedBytes
                    "Byron Tx" fromCBOR (LBS.fromStrict bs)
    _ -> deserialiseFromCBOR (AsTx ttoken) bs
 where
  ttoken :: AsType era
  ttoken = proxyToAsType Proxy
