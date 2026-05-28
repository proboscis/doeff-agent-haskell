{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Doeff.Agentd.Client
  ( AgentdConfig (..),
    AgentdError (..),
    AgentdFinalResult (..),
    AgentdResult (..),
    AgentdRun,
    AgentdRunFailure (..),
    AgentdSnapshot (..),
    AgentdTerminalCause (..),
    AgentdWaitOptions (..),
    ExpectedResultRequest (..),
    LaunchRequest (..),
    SessionLifecycle (..),
    agentdRunSessionId,
    awaitResult,
    cancelRun,
    daemonStatus,
    defaultAgentdConfig,
    defaultAgentdWaitOptions,
    lifecycleText,
    parseLifecycle,
    parseSnapshot,
    pollRunResult,
    request,
    sessionCancel,
    sessionCapture,
    sessionCleanup,
    sessionGet,
    sessionLaunch,
    sessionLaunchAsync,
    sessionList,
    sessionPollResult,
    sessionSend,
    sessionWaitResult,
    tryParseRfc3339,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception, IOException, bracket, try)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (Array, Null, Object),
    decodeStrict,
    encode,
    eitherDecodeStrict,
    object,
    (.:),
    (.:?),
    (.!=),
    (.=),
  )
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.Types (Pair, Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    close,
    connect,
    socket,
  )
import qualified Network.Socket.ByteString as Net
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Timeout as Timeout

data AgentdConfig = AgentdConfig
  { agentdSocketPath :: FilePath,
    agentdReadBufferBytes :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AgentdError
  = AgentdSocketError String
  | AgentdProtocolError String
  | AgentdServerError String
  | AgentdTimeoutError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

data SessionLifecycle
  = LifecycleRunToCompletion
  | LifecycleInteractive
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

lifecycleText :: SessionLifecycle -> Text
lifecycleText LifecycleRunToCompletion = "run_to_completion"
lifecycleText LifecycleInteractive = "interactive"

parseLifecycle :: Text -> SessionLifecycle
parseLifecycle "interactive" = LifecycleInteractive
parseLifecycle _ = LifecycleRunToCompletion

data LaunchRequest = LaunchRequest
  { launchSessionId :: Text,
    launchSessionName :: Text,
    launchAgentType :: Text,
    launchWorkDir :: FilePath,
    launchCommand :: Text,
    launchPrompt :: Text,
    launchModel :: Text,
    launchEffort :: Text,
    launchMcpServers :: Map Text Text,
    launchSkipTrustSetup :: Bool,
    launchLifecycle :: SessionLifecycle,
    launchSessionEnv :: Map Text Text,
    launchExpectedResult :: Maybe ExpectedResultRequest
  }
  deriving stock (Eq, Show)

data ExpectedResultRequest = ExpectedResultRequest
  { erFilePath :: FilePath,
    erSchemaName :: Maybe Text,
    erSchemaVersion :: Maybe Int,
    erRetryPrompt :: Maybe Text,
    erMaxRetries :: Maybe Int
  }
  deriving stock (Eq, Show)

instance FromJSON ExpectedResultRequest where
  parseJSON = Aeson.withObject "ExpectedResultRequest" $ \obj -> do
    erFilePath <- obj .: "file_path"
    erSchemaName <- obj .:? "schema_name"
    erSchemaVersion <- obj .:? "schema_version"
    erRetryPrompt <- obj .:? "retry_prompt"
    erMaxRetries <- obj .:? "max_retries"
    pure ExpectedResultRequest {..}

instance ToJSON ExpectedResultRequest where
  toJSON = expectedResultObject

data AgentdTerminalCause = AgentdTerminalCause
  { terminalCauseCategory :: Text,
    terminalCauseReason :: Maybe Text,
    terminalCauseRetryable :: Maybe Bool,
    terminalCauseRetryAfterSeconds :: Maybe Int,
    terminalCauseBackendErrorCode :: Maybe Text,
    terminalCauseExitCode :: Maybe Int,
    terminalCauseSignal :: Maybe Text,
    terminalCauseObservedAt :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AgentdTerminalCause where
  parseJSON = Aeson.withObject "AgentdTerminalCause" $ \obj -> do
    terminalCauseCategory <- obj .: "category"
    terminalCauseReason <- obj .:? "reason"
    terminalCauseRetryable <- obj .:? "retryable"
    terminalCauseRetryAfterSeconds <- obj .:? "retry_after_seconds"
    terminalCauseBackendErrorCode <- obj .:? "backend_error_code"
    terminalCauseExitCode <- obj .:? "exit_code"
    terminalCauseSignal <- obj .:? "signal"
    terminalCauseObservedAt <- obj .:? "observed_at"
    pure AgentdTerminalCause {..}

instance ToJSON AgentdTerminalCause where
  toJSON AgentdTerminalCause {..} =
    object
      ( concat
          [ ["category" .= terminalCauseCategory],
            maybeField "reason" terminalCauseReason,
            maybeField "retryable" terminalCauseRetryable,
            maybeField "retry_after_seconds" terminalCauseRetryAfterSeconds,
            maybeField "backend_error_code" terminalCauseBackendErrorCode,
            maybeField "exit_code" terminalCauseExitCode,
            maybeField "signal" terminalCauseSignal,
            maybeField "observed_at" terminalCauseObservedAt
          ]
      )

data AgentdSnapshot = AgentdSnapshot
  { snapshotSessionId :: Text,
    snapshotSessionName :: Text,
    snapshotPaneId :: Text,
    snapshotAgentType :: Text,
    snapshotWorkDir :: Text,
    snapshotLifecycle :: Text,
    snapshotStatus :: Text,
    snapshotBackendKind :: Text,
    snapshotBackendRef :: Map Text Text,
    snapshotStartedAt :: Text,
    snapshotLastObservedAt :: Maybe Text,
    snapshotFinishedAt :: Maybe Text,
    snapshotCleanedAt :: Maybe Text,
    snapshotPrUrl :: Maybe Text,
    snapshotOutputSnippet :: Maybe Text,
    snapshotTerminalCause :: Maybe AgentdTerminalCause,
    snapshotExpectedResult :: Maybe ExpectedResultRequest
  }
  deriving stock (Eq, Show, Generic)

data AgentdWaitOptions = AgentdWaitOptions
  { waitPollIntervalMicros :: Int,
    waitTimeoutMicros :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

defaultAgentdWaitOptions :: AgentdWaitOptions
defaultAgentdWaitOptions =
    AgentdWaitOptions
    { waitPollIntervalMicros = 1000000,
      waitTimeoutMicros = Nothing
    }

data AgentdResult = AgentdResult
  { resultSnapshot :: AgentdSnapshot,
    resultPayload :: Maybe Value,
    resultRawText :: Maybe Text,
    resultFilePath :: Maybe FilePath
  }
  deriving stock (Eq, Show, Generic)

data AgentdRunFailure = AgentdRunFailure
  { runFailureSnapshot :: AgentdSnapshot,
    runFailureCause :: Maybe AgentdTerminalCause,
    runFailureMessage :: Text
  }
  deriving stock (Eq, Show, Generic)

data AgentdFinalResult
  = AgentdFinalSucceeded AgentdResult
  | AgentdFinalFailed AgentdRunFailure
  deriving stock (Eq, Show, Generic)

data AgentdRun = AgentdRun
  { runSessionId :: Text,
    runAsync :: Async.Async (Either AgentdError AgentdFinalResult)
  }

agentdRunSessionId :: AgentdRun -> Text
agentdRunSessionId = runSessionId

instance FromJSON AgentdSnapshot where
  parseJSON = snapshotParser

instance ToJSON AgentdSnapshot where
  toJSON AgentdSnapshot {..} =
    object
      ( concat
          [ ["session_id" .= snapshotSessionId],
            ["session_name" .= snapshotSessionName],
            ["pane_id" .= snapshotPaneId],
            ["agent_type" .= snapshotAgentType],
            ["work_dir" .= snapshotWorkDir],
            ["lifecycle" .= snapshotLifecycle],
            ["status" .= snapshotStatus],
            ["backend_kind" .= snapshotBackendKind],
            ["backend_ref" .= snapshotBackendRef],
            ["started_at" .= snapshotStartedAt],
            maybeField "last_observed_at" snapshotLastObservedAt,
            maybeField "finished_at" snapshotFinishedAt,
            maybeField "cleaned_at" snapshotCleanedAt,
            maybeField "pr_url" snapshotPrUrl,
            maybeField "output_snippet" snapshotOutputSnippet,
            maybeField "terminal_cause" snapshotTerminalCause,
            maybeField "expected_result" snapshotExpectedResult
          ]
      )

defaultAgentdConfig :: IO AgentdConfig
defaultAgentdConfig = do
  socketEnv <- lookupEnv "DOEFF_AGENTD_SOCKET"
  runtimeDir <- lookupEnv "XDG_RUNTIME_DIR"
  user <- lookupEnv "USER"
  let path = case socketEnv of
        Just explicit -> explicit
        Nothing -> case runtimeDir of
          Just dir -> dir <> "/doeff/agentd.sock"
          Nothing -> "/tmp/doeff-agentd-" <> resolveUser user <> ".sock"
  pure
    AgentdConfig
      { agentdSocketPath = path,
        agentdReadBufferBytes = 65536
      }
  where
    resolveUser (Just u) = u
    resolveUser Nothing = "unknown"

requestIdRef :: IORef Int
requestIdRef = unsafePerformIO (newIORef 0)
{-# NOINLINE requestIdRef #-}

nextRequestId :: IO Int
nextRequestId = atomicModifyIORef' requestIdRef (\n -> (n + 1, n + 1))

request :: AgentdConfig -> Text -> Value -> IO (Either AgentdError Value)
request AgentdConfig {..} method params = do
  rid <- nextRequestId
  let payload =
        object
          [ "id" .= rid,
            "method" .= method,
            "params" .= params
          ]
      encoded = LBS.toStrict (encode payload) <> "\n"
  socketResult <-
    try $ bracket openSock close $ \sock -> do
      Net.sendAll sock encoded
      readLine sock agentdReadBufferBytes mempty
  case socketResult of
    Left (err :: IOException) ->
      pure (Left (AgentdSocketError (show err)))
    Right rawLine -> pure (decodeResponse rid rawLine)
  where
    openSock = do
      s <- socket AF_UNIX Stream 0
      connect s (SockAddrUnix agentdSocketPath)
      pure s

decodeResponse :: Int -> BS.ByteString -> Either AgentdError Value
decodeResponse expectedId rawLine =
  case decodeStrict rawLine :: Maybe Value of
    Nothing ->
      Left (AgentdProtocolError ("invalid JSON response: " <> BSC.unpack rawLine))
    Just (Object obj) ->
      case parseEither (parseResponse expectedId) (Object obj) of
        Left err -> Left (AgentdProtocolError err)
        Right outcome -> outcome
    Just _ ->
      Left (AgentdProtocolError "agentd returned a non-object response")

parseResponse :: Int -> Value -> Parser (Either AgentdError Value)
parseResponse expectedId = Aeson.withObject "AgentdResponse" $ \obj -> do
  rid <- obj .: "id"
  ok <- obj .: "ok"
  result <- obj .:? "result"
  err <- obj .:? "error"
  if rid /= expectedId
    then pure (Left (AgentdProtocolError "response id did not match request id"))
    else
      if ok
        then case result of
          Just value -> pure (Right value)
          Nothing -> pure (Right Null)
        else
          let message = case (err :: Maybe Text) of
                Just msg | not (T.null msg) -> T.unpack msg
                _ -> "agentd request failed"
           in pure (Left (AgentdServerError message))

readLine :: Socket -> Int -> BS.ByteString -> IO BS.ByteString
readLine sock chunkSize buffer
  | BS.elem 10 buffer =
      pure (BS.takeWhile (/= 10) buffer)
  | otherwise = do
      chunk <- Net.recv sock chunkSize
      if BS.null chunk
        then
          if BS.null buffer
            then ioError (userError "agentd closed connection before responding")
            else pure buffer
        else readLine sock chunkSize (buffer <> chunk)

daemonStatus :: AgentdConfig -> IO (Either AgentdError Value)
daemonStatus cfg = request cfg "daemon.status" (object [])

sessionLaunch :: AgentdConfig -> LaunchRequest -> IO (Either AgentdError AgentdSnapshot)
sessionLaunch cfg LaunchRequest {..} = do
  let baseFields =
        [ "session_id" .= launchSessionId,
          "session_name" .= launchSessionName,
          "agent_type" .= launchAgentType,
          "work_dir" .= launchWorkDir,
          "lifecycle" .= lifecycleText launchLifecycle,
          "session_env" .= launchSessionEnv,
          "mcp_servers" .= launchMcpServers,
          "skip_trust_setup" .= launchSkipTrustSetup
        ]
      maybeFields =
        concat
          [ ["command" .= launchCommand | not (T.null launchCommand)],
            ["prompt" .= launchPrompt | not (T.null launchPrompt)],
            ["model" .= launchModel | not (T.null launchModel)],
            ["effort" .= launchEffort | not (T.null launchEffort)],
            case launchExpectedResult of
              Nothing -> []
              Just spec -> ["expected_result" .= expectedResultObject spec]
          ]
      params = object (baseFields ++ maybeFields)
  fmap (>>= parseSnapshot) (request cfg "session.launch" params)

sessionLaunchAsync ::
  AgentdConfig ->
  AgentdWaitOptions ->
  LaunchRequest ->
  IO (Either AgentdError AgentdRun)
sessionLaunchAsync cfg options launch = do
  launched <- sessionLaunch cfg launch
  case launched of
    Left err -> pure (Left err)
    Right snapshot -> do
      handle <- Async.async (sessionWaitResult cfg options (snapshotSessionId snapshot))
      pure
        ( Right
            AgentdRun
              { runSessionId = snapshotSessionId snapshot,
                runAsync = handle
              }
        )

expectedResultObject :: ExpectedResultRequest -> Value
expectedResultObject ExpectedResultRequest {..} =
  object
    ( concat
        [ ["file_path" .= erFilePath],
          maybeField "schema_name" erSchemaName,
          maybeField "schema_version" erSchemaVersion,
          maybeField "retry_prompt" erRetryPrompt,
          maybeField "max_retries" erMaxRetries
        ]
    )

maybeField :: ToJSON a => AesonKey.Key -> Maybe a -> [Pair]
maybeField _ Nothing = []
maybeField name (Just value) = [name .= value]

sessionGet :: AgentdConfig -> Text -> IO (Either AgentdError (Maybe AgentdSnapshot))
sessionGet cfg sessionId = do
  outcome <- request cfg "session.get" (object ["session_id" .= sessionId])
  pure $ case outcome of
    Left err -> Left err
    Right Null -> Right Nothing
    Right value -> fmap Just (parseSnapshot value)

sessionList :: AgentdConfig -> Map Text Value -> IO (Either AgentdError [AgentdSnapshot])
sessionList cfg query = do
  outcome <- request cfg "session.list" (Object (toAesonObject query))
  pure $ outcome >>= parseSnapshotList

sessionCapture :: AgentdConfig -> Text -> Int -> IO (Either AgentdError Text)
sessionCapture cfg sessionId lines_ = do
  let params =
        object
          [ "session_id" .= sessionId,
            "lines" .= lines_
          ]
  outcome <- request cfg "session.capture" params
  pure $ outcome >>= parseCapture
  where
    parseCapture = \case
      Object obj -> case parseEither (.: "text") obj of
        Left err -> Left (AgentdProtocolError err)
        Right text -> Right text
      _ -> Left (AgentdProtocolError "session.capture returned a non-object result")

sessionSend :: AgentdConfig -> Text -> Text -> Bool -> Bool -> IO (Either AgentdError ())
sessionSend cfg sessionId message enter literal = do
  let params =
        object
          [ "session_id" .= sessionId,
            "message" .= message,
            "enter" .= enter,
            "literal" .= literal
          ]
  outcome <- request cfg "session.send" params
  pure (fmap (const ()) outcome)

sessionCancel :: AgentdConfig -> Text -> IO (Either AgentdError AgentdSnapshot)
sessionCancel cfg sessionId =
  fmap (>>= parseSnapshot)
    (request cfg "session.cancel" (object ["session_id" .= sessionId]))

sessionCleanup :: AgentdConfig -> Text -> IO (Either AgentdError AgentdSnapshot)
sessionCleanup cfg sessionId =
  fmap (>>= parseSnapshot)
    (request cfg "session.cleanup" (object ["session_id" .= sessionId]))

sessionPollResult ::
  AgentdConfig ->
  Text ->
  IO (Either AgentdError (Maybe AgentdFinalResult))
sessionPollResult cfg sessionId = do
  got <- sessionGet cfg sessionId
  case got of
    Left err -> pure (Left err)
    Right Nothing ->
      pure (Left (AgentdServerError ("session not found: " <> T.unpack sessionId)))
    Right (Just snapshot)
      | not (snapshotIsTerminal snapshot) -> pure (Right Nothing)
      | snapshotStatus snapshot == "done" ->
          fmap (fmap (Just . AgentdFinalSucceeded)) (snapshotResult snapshot)
      | otherwise ->
          pure (Right (Just (AgentdFinalFailed (snapshotFailure snapshot))))

sessionWaitResult ::
  AgentdConfig ->
  AgentdWaitOptions ->
  Text ->
  IO (Either AgentdError AgentdFinalResult)
sessionWaitResult cfg options sessionId =
  case waitTimeoutMicros options of
    Nothing -> waitLoop
    Just micros -> do
      result <- Timeout.timeout micros waitLoop
      pure $
        case result of
          Just value -> value
          Nothing ->
            Left
              ( AgentdTimeoutError
                  ( "timed out waiting for agent session result: "
                      <> T.unpack sessionId
                  )
              )
  where
    waitLoop = do
      polled <- sessionPollResult cfg sessionId
      case polled of
        Left err -> pure (Left err)
        Right Nothing -> do
          threadDelay (max 1 (waitPollIntervalMicros options))
          waitLoop
        Right (Just final) -> pure (Right final)

awaitResult :: AgentdRun -> IO (Either AgentdError AgentdFinalResult)
awaitResult =
  Async.wait . runAsync

pollRunResult :: AgentdRun -> IO (Maybe (Either AgentdError AgentdFinalResult))
pollRunResult run = do
  polled <- Async.poll (runAsync run)
  pure $
    case polled of
      Nothing -> Nothing
      Just (Left err) -> Just (Left (AgentdProtocolError (show err)))
      Just (Right result) -> Just result

cancelRun :: AgentdConfig -> AgentdRun -> IO (Either AgentdError AgentdSnapshot)
cancelRun cfg run = do
  Async.cancel (runAsync run)
  sessionCancel cfg (runSessionId run)

snapshotResult :: AgentdSnapshot -> IO (Either AgentdError AgentdResult)
snapshotResult snapshot =
  case snapshotExpectedResult snapshot of
    Nothing ->
      pure
        ( Right
            AgentdResult
              { resultSnapshot = snapshot,
                resultPayload = Nothing,
                resultRawText = Nothing,
                resultFilePath = Nothing
              }
        )
    Just expected -> do
      let path = T.unpack (snapshotWorkDir snapshot) </> erFilePath expected
      exists <- doesFileExist path
      if not exists
        then
          pure
            ( Left
                ( AgentdProtocolError
                    ( "agentd marked session done but result file is missing: "
                        <> path
                    )
                )
            )
        else do
          raw <- TIO.readFile path
          case eitherDecodeStrict (TE.encodeUtf8 raw) of
            Left err ->
              pure
                ( Left
                    ( AgentdProtocolError
                        ( "agentd result file is not valid JSON: "
                            <> path
                            <> ": "
                            <> err
                        )
                    )
                )
            Right payload ->
              pure
                ( Right
                    AgentdResult
                      { resultSnapshot = snapshot,
                        resultPayload = Just payload,
                        resultRawText = Just raw,
                        resultFilePath = Just path
                      }
                )

snapshotFailure :: AgentdSnapshot -> AgentdRunFailure
snapshotFailure snapshot =
  AgentdRunFailure
    { runFailureSnapshot = snapshot,
      runFailureCause = snapshotTerminalCause snapshot,
      runFailureMessage =
        case snapshotTerminalCause snapshot >>= terminalCauseReason of
          Just reason | not (T.null reason) -> reason
          _ -> "agentd terminal status: " <> snapshotStatus snapshot
    }

snapshotIsTerminal :: AgentdSnapshot -> Bool
snapshotIsTerminal snapshot =
  snapshotStatus snapshot
    `elem` [ "done",
             "failed",
             "cancelled",
             "lost",
             "exited",
             "stopped"
           ]

parseSnapshot :: Value -> Either AgentdError AgentdSnapshot
parseSnapshot value =
  case parseEither snapshotParser value of
    Left err -> Left (AgentdProtocolError err)
    Right snapshot -> Right snapshot

snapshotParser :: Value -> Parser AgentdSnapshot
snapshotParser = Aeson.withObject "AgentdSnapshot" $ \obj -> do
  snapshotSessionId <- obj .: "session_id"
  snapshotSessionName <- obj .: "session_name"
  snapshotPaneId <- obj .:? "pane_id" .!= ""
  snapshotAgentType <- obj .: "agent_type"
  snapshotWorkDir <- obj .: "work_dir"
  snapshotLifecycle <- obj .: "lifecycle"
  snapshotStatus <- obj .: "status"
  snapshotBackendKind <- obj .:? "backend_kind" .!= ""
  snapshotBackendRef <- obj .:? "backend_ref" .!= Map.empty
  snapshotStartedAt <- obj .: "started_at"
  snapshotLastObservedAt <- obj .:? "last_observed_at"
  snapshotFinishedAt <- obj .:? "finished_at"
  snapshotCleanedAt <- obj .:? "cleaned_at"
  snapshotPrUrl <- obj .:? "pr_url"
  snapshotOutputSnippet <- obj .:? "output_snippet"
  snapshotTerminalCause <- obj .:? "terminal_cause"
  snapshotExpectedResult <- obj .:? "expected_result"
  pure AgentdSnapshot {..}

parseSnapshotList :: Value -> Either AgentdError [AgentdSnapshot]
parseSnapshotList = \case
  Array items -> traverse parseSnapshot (V.toList items)
  _ -> Left (AgentdProtocolError "session.list returned a non-array result")

toAesonObject :: Map Text Value -> Aeson.Object
toAesonObject = KeyMap.fromList . map (\(k, v) -> (AesonKey.fromText k, v)) . Map.toList

tryParseRfc3339 :: Text -> Maybe Time.UTCTime
tryParseRfc3339 raw =
  let s = T.unpack raw
   in parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Ez" s
        <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s
        <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" s
