{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Cardano.TxSubmit.Web
  ( runTxSubmitServer,
  )
where

import Cardano.Api
import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Binary (DecoderError (..))
import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Prelude (Bool (..), Word, all, lift)
import Cardano.TxSubmit.CLI.Types (SocketPath (SocketPath))
import Cardano.TxSubmit.Metrics (TxSubmitMetrics (..))
import Cardano.TxSubmit.Rest.Types (WebserverConfig (..), toWarpSettings)
import qualified Cardano.TxSubmit.Rest.Web as Web
import Cardano.TxSubmit.Types
  ( EnvSocketError (..),
    ExUnitCalculation (..),
    RawCborDecodeError (..),
    ScriptType (..),
    TxCmdError (TxCmdAcquireFailure, TxCmdByronEra, TxCmdEraConsensusModeMismatch, TxCmdEraConsensusModeMismatchTxBalance, TxCmdExecutionError, TxCmdTxReadError, TxCmdTxSubmitError, TxCmdTxSubmitErrorEraMismatch, TxCmdUnsupportedMode),
    TxSubmitApi,
    TxSubmitApiRecord (..),
    TxSubmitWebApiError (TxSubmitFail),
    renderTxCmdError,
  )
import Cardano.TxSubmit.Util (logException)
import Control.Applicative (Applicative (pure), (<$>))
import Control.Monad (Functor (fmap), Monad (return), join, (=<<))
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    MonadIO (liftIO),
    runExceptT,
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except.Extra
  ( firstExceptT,
    handleIOExceptT,
    hoistEither,
    hoistMaybe,
    left,
    newExceptT,
  )
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first, second)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import Data.Either (Either (..), partitionEithers)
import Data.Function (($), (.))
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Data.Maybe
import Data.Maybe (listToMaybe, maybe)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup ((<>)))
import qualified Data.Set as Set
import Data.String (String)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Records (HasField (..))
import Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx
import Servant (Application, Handler, ServerError (..), err400, throwError)
import qualified Servant
import Servant.API.Generic (toServant)
import Servant.Server.Generic (AsServerT)
import System.Environment (lookupEnv)
import System.IO (IO)
import qualified System.IO as IO
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import Text.Show (Show (show))

runTxSubmitServer ::
  Trace IO Text ->
  TxSubmitMetrics ->
  WebserverConfig ->
  AnyConsensusModeParams ->
  NetworkId ->
  SocketPath ->
  IO ()
runTxSubmitServer trace metrics webserverConfig protocol networkId socketPath = do
  logException trace "tx-submit-webapi." $
    Web.runSettings trace (toWarpSettings webserverConfig) $ txSubmitApp trace metrics protocol networkId socketPath
  logInfo trace "txSubmitApp: exiting"

txSubmitApp ::
  Trace IO Text ->
  TxSubmitMetrics ->
  AnyConsensusModeParams ->
  NetworkId ->
  SocketPath ->
  Application
txSubmitApp trace metrics connectInfo networkId socketPath =
  Servant.serve (Proxy :: Proxy TxSubmitApi) (toServant handlers)
  where
    handlers :: TxSubmitApiRecord (AsServerT Handler)
    handlers =
      TxSubmitApiRecord
        { _txSubmitPost = txSubmitPost trace metrics connectInfo networkId socketPath
        }

-- | Read the node socket path from the environment.
-- Fails if the environment variable is not set.
readEnvSocketPath :: ExceptT EnvSocketError IO SocketPath
readEnvSocketPath =
  maybe (left $ CliEnvVarLookup (T.pack envName)) (pure . SocketPath)
    =<< liftIO (lookupEnv envName)
  where
    envName :: String
    envName = "CARDANO_NODE_SOCKET_PATH"

deserialiseOne ::
  forall b.
  () =>
  FromSomeType SerialiseAsCBOR b ->
  ByteString ->
  Either DecoderError b
deserialiseOne (FromSomeType ttoken f) bs = f <$> deserialiseFromCBOR ttoken bs

deserialiseAnyOf ::
  forall b.
  () =>
  [FromSomeType SerialiseAsCBOR b] ->
  ByteString ->
  Either RawCborDecodeError b
deserialiseAnyOf ts te = getResult . partitionEithers $ fmap (`deserialiseOne` te) ts
  where
    getResult :: ([DecoderError], [b]) -> Either RawCborDecodeError b
    getResult (dErrors, []) = Left $ RawCborDecodeError dErrors
    getResult (_, result : _) = Right result -- take the first successful decode

readByteStringTx :: ByteString -> ExceptT TxCmdError IO (InAnyCardanoEra Tx)
readByteStringTx =
  firstExceptT TxCmdTxReadError . hoistEither
    . deserialiseAnyOf
      [ FromSomeType (AsTx AsByronEra) (InAnyCardanoEra ByronEra),
        FromSomeType (AsTx AsShelleyEra) (InAnyCardanoEra ShelleyEra),
        FromSomeType (AsTx AsAllegraEra) (InAnyCardanoEra AllegraEra),
        FromSomeType (AsTx AsMaryEra) (InAnyCardanoEra MaryEra),
        FromSomeType (AsTx AsAlonzoEra) (InAnyCardanoEra AlonzoEra)
      ]

txSubmitPost ::
  Trace IO Text ->
  TxSubmitMetrics ->
  AnyConsensusModeParams ->
  NetworkId ->
  SocketPath ->
  ByteString ->
  Handler [ExUnitCalculation]
txSubmitPost trace metrics (AnyConsensusModeParams cModeParams) networkId (SocketPath socketPath) txBytes = handle $ do
  InAnyCardanoEra era tx <- readByteStringTx txBytes

  let txBody = getTxBody tx
      (TxBody (TxBodyContent {txIns = txinputs})) = txBody
      onlyInputs = [input | (input, _) <- txinputs]
      consensusMode = consensusModeOnly cModeParams

  case (consensusMode, cardanoEraStyle era) of
    (CardanoMode, ShelleyBasedEra sbe) -> do
      eraInMode <- case toEraInMode era CardanoMode of
        Just result -> return result
        Nothing ->
          left TxCmdEraConsensusModeMismatchTxBalance
      let localNodeConnInfo =
            LocalNodeConnectInfo
              { localConsensusModeParams = cModeParams,
                localNodeNetworkId = networkId,
                localNodeSocketPath = socketPath
              }

      (utxo, pparams, eraHistory, systemStart) <-
        newExceptT . fmap (join . first TxCmdAcquireFailure) $
          executeLocalStateQueryExpr localNodeConnInfo Nothing $ \_ntcVersion -> runExceptT $ do
            utxo <-
              firstExceptT TxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr $
                QueryInEra eraInMode $
                  QueryInShelleyBasedEra sbe $
                    QueryUTxO (QueryUTxOByTxIn (Set.fromList onlyInputs))

            pparams <-
              firstExceptT TxCmdTxSubmitErrorEraMismatch . newExceptT . queryExpr $
                QueryInEra eraInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters

            eraHistory <- lift . queryExpr $ QueryEraHistory CardanoModeIsMultiEra

            systemStart <- lift $ queryExpr QuerySystemStart

            return (utxo, pparams, eraHistory, systemStart)

      case evaluateTransactionExecutionUnits eraInMode systemStart eraHistory pparams utxo txBody of
        Left _ -> left TxCmdExecutionError
        Right ex -> do
          let rawExUnits = Map.toList ex
          case (all (\(_, res) -> case res of Left _ -> False; Right _ -> True) rawExUnits) of
            True -> return $ collectExUnits rawExUnits
            False -> left TxCmdExecutionError
    (_, LegacyByronEra) -> left TxCmdByronEra
    (wrongMode, _) -> left (TxCmdUnsupportedMode (AnyConsensusMode wrongMode))
  where
    handle :: ExceptT TxCmdError IO [ExUnitCalculation] -> Handler [ExUnitCalculation]
    handle f = do
      result <- liftIO $ runExceptT f
      handleSubmitResult result

    errorResponse :: ToJSON e => e -> Handler a
    errorResponse e = throwError $ err400 {errBody = Aeson.encode e}

    collectExUnits :: [(ScriptWitnessIndex, (Either ScriptExecutionError ExecutionUnits))] -> [ExUnitCalculation]
    collectExUnits [] = []
    collectExUnits ((w, result) : t) = case result of
      Left _ -> collectExUnits t -- ignore, failure checked in step before
      Right units -> ExUnitCalculation {scriptWitness = getScriptType w, index = getScriptWitnessIndex w, executionUnits = units} : collectExUnits t

    getScriptType :: ScriptWitnessIndex -> ScriptType
    getScriptType (ScriptWitnessIndexTxIn _) = TxInput
    getScriptType (ScriptWitnessIndexMint _) = Mint
    getScriptType (ScriptWitnessIndexCertificate _) = Certificate
    getScriptType (ScriptWitnessIndexWithdrawal _) = Withdrawal

    getScriptWitnessIndex :: ScriptWitnessIndex -> Word
    getScriptWitnessIndex (ScriptWitnessIndexTxIn i) = i
    getScriptWitnessIndex (ScriptWitnessIndexMint i) = i
    getScriptWitnessIndex (ScriptWitnessIndexCertificate i) = i
    getScriptWitnessIndex (ScriptWitnessIndexWithdrawal i) = i

    -- Log relevant information, update the metrics, and return the result to
    -- the client.
    handleSubmitResult :: Either TxCmdError [ExUnitCalculation] -> Handler [ExUnitCalculation]
    handleSubmitResult res =
      case res of
        Left err -> do
          liftIO $
            logInfo trace $
              "txCalc: failed to calculate ex units: "
                <> renderTxCmdError err
          errorResponse (TxSubmitFail err)
        Right exUnitsCalc -> do
          liftIO $
            logInfo trace $
              "txCalc: successfully calculated ex units "
          liftIO $ Gauge.inc (tsmCount metrics)
          pure exUnitsCalc
