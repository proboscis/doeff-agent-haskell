{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Doeff.Agentd.Client
  ( AgentdCallError (..),
    AgentdConfig (..),
    AgentdError (..),
    AgentdFinalResult (..),
    AgentdResult (..),
    AgentdRun,
    AgentdRunFailure (..),
    AgentdSnapshot (..),
    AgentdTerminalCause (..),
    AgentdWaitOptions (..),
    AwaitError (..),
    AwaitedPayload (..),
    AwaitedResult (..),
    ExpectedResultRequest (..),
    LaunchRequest (..),
    ResumeRequest (..),
    SessionLifecycle (..),
    agentdRunSessionId,
    awaitResult,
    cancelRun,
    daemonStatus,
    defaultAgentdConfig,
    defaultAgentdWaitOptions,
    ensureAgentdConfig,
    lifecycleText,
    parseLifecycle,
    parseSnapshot,
    pollRunResult,
    request,
    resolveDoeffAgentsCommand,
    sessionAwaitResult,
    sessionCancel,
    sessionCapture,
    sessionCleanup,
    sessionGet,
    sessionLaunch,
    sessionLaunchAsync,
    sessionList,
    sessionPollResult,
    sessionResume,
    sessionSend,
    sessionWaitResult,
    tryParseRfc3339,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception, IOException, bracket, displayException, try)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value (Array, Null, Object),
    decodeStrict,
    encode,
    eitherDecodeStrict,
    object,
    withObject,
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
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import qualified System.Timeout as Timeout

data AgentdConfig = AgentdConfig
  { agentdSocketPath :: FilePath,
    agentdReadBufferBytes :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AgentdEnsureResponse = AgentdEnsureResponse
  { ensureResponseSocketPath :: FilePath
  }
  deriving stock (Eq, Show)

instance FromJSON AgentdEnsureResponse where
  parseJSON = withObject "AgentdEnsureResponse" $ \obj ->
    AgentdEnsureResponse <$> obj .: "socket_path"

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
    -- | Non-auth overlay environment only (ADR-DOE-AGENTS-004 R7).  Auth
    -- and profile material (CODEX_HOME, CLAUDE_CONFIG_DIR, …) must ride
    -- 'launchBinding'; the session host rejects binding-owned keys here.
    launchSessionEnv :: Map Text Text,
    -- | Typed auth/profile binding — the serialized binding-time
    -- configuration (kind-discriminated: @{"kind": "codex", "codex_home":
    -- …}@ / @{"kind": "claude-code", "config_dir": …}@).  Carried as a raw
    -- 'Value' so this client stays agnostic to the kind schemas; the
    -- session host validates shape at admission.
    launchBinding :: Maybe Value,
    launchExpectedResult :: Maybe ExpectedResultRequest
  }
  deriving stock (Eq, Show)

-- | Wire request for @session.resume@ (ADR-DOE-AGENTS-006 R4).  The source
-- session id names the terminal predecessor row; everything else is
-- optional and falls back to the session host's own derivation (overlay
-- restore from the source row).  'resumeBinding' / 'resumeNewSessionId' /
-- 'resumeExpectedResult' are the cross-binding failover extensions: a
-- different auth binding re-homes the incarnation (the session host owns
-- the transcript transplant), the caller-minted session id keeps the
-- launcher's id conventions, and an explicit result contract takes
-- precedence over the carried unfulfilled one.  Empty 'Text' / empty 'Map'
-- fields are omitted from the wire so the session host's restore-from-source
-- semantics apply (an explicit empty map would read as an override).
data ResumeRequest = ResumeRequest
  { resumeSourceSessionId :: Text,
    resumeNewSessionId :: Text,
    resumePrompt :: Text,
    resumeModel :: Text,
    resumeEffort :: Text,
    resumeMcpServers :: Map Text Text,
    resumeSessionEnv :: Map Text Text,
    resumeBinding :: Maybe Value,
    resumeExpectedResult :: Maybe ExpectedResultRequest
  }
  deriving stock (Eq, Show)

-- | Error surface for calls that must keep the server's error code
-- distinguishable ('request' collapses code + message into one string).
-- @callErrorCode@ carries the agentd-side @error_code@ verbatim (an Int or
-- a Text on the wire, hence 'Value'); 'Nothing' when the server sent none.
data AgentdCallError
  = CallSocketError String
  | CallProtocolError String
  | CallServerError {callErrorCode :: Maybe Value, callErrorMessage :: String}
  deriving stock (Eq, Show, Generic)

-- | The result contract the launcher attaches to a launch.  Mirrors
-- @doeff-agentd@'s @ExpectedResultSpec@: the launcher supplies ONLY the
-- JSON-Schema (a constrained subset agentd understands) the agent's
-- result must satisfy.  agentd owns the rest of the transmission
-- contract — the result file path, the instruction it injects into the
-- agent, validation, and the retry loop.  Carried as a raw 'Value' so
-- this client stays agnostic to any particular result shape.
newtype ExpectedResultRequest = ExpectedResultRequest
  { erPayloadSchema :: Value
  }
  deriving stock (Eq, Show)

instance FromJSON ExpectedResultRequest where
  parseJSON = Aeson.withObject "ExpectedResultRequest" $ \obj -> do
    erPayloadSchema <- obj .: "payload_schema"
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
    snapshotExpectedResult :: Maybe ExpectedResultRequest,
    -- | ADR-DOE-AGENTS-006 conversation lineage, decoded verbatim off the
    -- wire.  'snapshotConversation' is the kind-discriminated durable
    -- conversation identity (claude @{"session_id": …}@ / codex
    -- @{"session_id": …, "rollout_path": …}@) carried as an opaque
    -- 'Value' — the client does not interpret kind schemas.  All four are
    -- optional so snapshots from a pre-lineage session host keep decoding.
    snapshotConversation :: Maybe Value,
    snapshotGeneration :: Maybe Int,
    snapshotResumedFromSessionId :: Maybe Text,
    snapshotForkedFromSessionId :: Maybe Text
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
            maybeField "expected_result" snapshotExpectedResult,
            maybeField "conversation" snapshotConversation,
            maybeField "generation" snapshotGeneration,
            maybeField "resumed_from_session_id" snapshotResumedFromSessionId,
            maybeField "forked_from_session_id" snapshotForkedFromSessionId
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

ensureAgentdConfig :: IO (Either AgentdError AgentdConfig)
ensureAgentdConfig =
  resolveDoeffAgentsCommand >>= \case
    Left err -> pure (Left err)
    Right command -> ensureWith command
  where
    ensureWith command = do
      outcome <- try (readProcessWithExitCode command ["agentd", "ensure", "--json"] "")
      case outcome of
        Left (err :: IOException) ->
          pure
            ( Left
                ( AgentdSocketError
                    ( "failed to run doeff-agents agentd ensure: "
                        <> displayException err
                    )
                )
            )
        Right (ExitSuccess, stdout, _stderr) ->
          case eitherDecodeStrict (TE.encodeUtf8 (T.pack stdout)) of
            Left err ->
              pure
                ( Left
                    ( AgentdProtocolError
                        ( "doeff-agents agentd ensure returned invalid JSON: "
                            <> err
                        )
                    )
                )
            Right AgentdEnsureResponse {..} ->
              pure
                ( Right
                    AgentdConfig
                      { agentdSocketPath = ensureResponseSocketPath,
                        agentdReadBufferBytes = 65536
                      }
                )
        Right (ExitFailure code, stdout, stderr) ->
          pure
            ( Left
                ( AgentdSocketError
                    ( "doeff-agents agentd ensure failed with exit code "
                        <> show code
                        <> ": "
                        <> firstNonEmpty stderr stdout
                    )
                )
            )

    firstNonEmpty stderr stdout =
      case T.unpack (T.strip (T.pack stderr)) of
        "" -> T.unpack (T.strip (T.pack stdout))
        msg -> msg

-- | Resolve the @doeff-agents@ CLI: the explicit @DOEFF_AGENTS_BIN@ seam
-- first, then @PATH@ — and nothing else.  The retired HOME-candidate
-- fallback (@~\/repos\/doeff\/.venv\/bin\/doeff-agents@) silently resolved to
-- a stale main-branch binary under launchd (whose default @PATH@ lacks
-- @~\/.local\/bin@), which both masked every kinds fetch and pointed the
-- ensure\/spawn path at a version-skewed host.  A set-but-missing
-- @DOEFF_AGENTS_BIN@ fails loud rather than falling through to @PATH@:
-- the seam exists to pin the executable identity, and masking a broken
-- pin with a differently-versioned @PATH@ hit is the same silent skew.
resolveDoeffAgentsCommand :: IO (Either AgentdError FilePath)
resolveDoeffAgentsCommand = do
  override <- lookupEnv "DOEFF_AGENTS_BIN"
  case override of
    Just command -> do
      exists <- doesFileExist command
      pure $
        if exists
          then Right command
          else
            Left
              ( AgentdSocketError
                  ("DOEFF_AGENTS_BIN is set but does not exist: " <> command)
              )
    Nothing -> do
      pathCommand <- findExecutable "doeff-agents"
      pure $ case pathCommand of
        Just command -> Right command
        Nothing ->
          Left
            ( AgentdSocketError
                "doeff-agents not found: set DOEFF_AGENTS_BIN or put doeff-agents on PATH (the silent HOME fallback is retired)"
            )

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

-- | Result returned by @session.await_result@.  The session snapshot
-- is always present; 'awaitedResult' is @Nothing@ when no
-- 'ExpectedResult' contract was attached at launch time (or when the
-- agentd validation gave up but still produced a terminal snapshot).
data AwaitedResult = AwaitedResult
  { awaitedSession :: AgentdSnapshot,
    awaitedResult :: Maybe AwaitedPayload,
    awaitedValidationError :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AwaitedResult where
  parseJSON = Aeson.withObject "AwaitedResult" $ \obj -> do
    awaitedSession <- obj .: "session" >>= snapshotFromValue
    awaitedResult <- obj .:? "result" .!= Nothing
    awaitedValidationError <- obj .:? "validation_error"
    pure AwaitedResult {..}
    where
      snapshotFromValue value = case parseSnapshot value of
        Right snap -> pure snap
        Left err -> fail (show err)

instance ToJSON AwaitedResult where
  toJSON AwaitedResult {..} =
    object
      [ "session" .= awaitedSession,
        "result" .= awaitedResult,
        "validation_error" .= awaitedValidationError
      ]

-- | Result payload nested in 'AwaitedResult'.  Mirrors the wire shape
-- returned by @session.await_result@: agentd hands back the validated
-- result file content under @payload@.  There is no envelope — the file
-- content IS the payload — so this carries only the value itself.
newtype AwaitedPayload = AwaitedPayload
  { awaitedPayload :: Value
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON AwaitedPayload where
  parseJSON = Aeson.withObject "AwaitedPayload" $ \obj -> do
    awaitedPayload <- obj .: "payload"
    pure AwaitedPayload {..}

instance ToJSON AwaitedPayload where
  toJSON AwaitedPayload {..} =
    object
      [ "payload" .= awaitedPayload
      ]

-- | Errors specific to 'sessionAwaitResult'.  Distinct from
-- 'AgentdError' so callers can pattern-match on the JSON-RPC error
-- code without re-parsing a free-form message.  @code@ matches the
-- agentd-side error code (-32000 timeout, -32001 no such session,
-- ... other for protocol / server errors).
data AwaitError
  = AwaitSocketError String
  | AwaitProtocolError String
  | AwaitServerError {awaitErrorCode :: Int, awaitErrorMessage :: String}
  deriving stock (Eq, Show, Generic)

-- | Block until the agentd session reaches a terminal state, then
-- return the validated @expected_result@ envelope (if any) along
-- with the final snapshot.
--
-- The transport here intentionally bypasses the generic 'request'
-- helper so the server-side JSON-RPC error @code@ can be surfaced
-- verbatim to the caller — 'request' collapses code + message into
-- a single string and callers need to distinguish -32000 (timeout)
-- from -32001 (no such session).
--
-- Transport timeout: the Unix-socket recv blocks indefinitely, which
-- is correct for a long-poll RPC.  Agentd is responsible for
-- enforcing @timeout_seconds@ and replying with code -32000 when the
-- session has not reached a terminal state by the deadline.
sessionAwaitResult ::
  AgentdConfig ->
  -- | Session id
  Text ->
  -- | Optional timeout in seconds (defaults agentd-side to 600).
  Maybe Double ->
  IO (Either AwaitError AwaitedResult)
sessionAwaitResult AgentdConfig {..} sid timeoutSeconds = do
  rid <- nextRequestId
  let params = object $
        ["session_id" .= sid]
          ++ maybe [] (\t -> ["timeout_seconds" .= t]) timeoutSeconds
      payload =
        object
          [ "id" .= rid,
            "method" .= ("session.await_result" :: Text),
            "params" .= params
          ]
      encoded = LBS.toStrict (encode payload) <> "\n"
  socketResult <-
    try $ bracket openSock close $ \sock -> do
      Net.sendAll sock encoded
      readLine sock agentdReadBufferBytes mempty
  case socketResult of
    Left (err :: IOException) ->
      pure (Left (AwaitSocketError (show err)))
    Right rawLine -> pure (decodeAwaitResponse rid rawLine)
  where
    openSock = do
      s <- socket AF_UNIX Stream 0
      connect s (SockAddrUnix agentdSocketPath)
      pure s

decodeAwaitResponse :: Int -> BS.ByteString -> Either AwaitError AwaitedResult
decodeAwaitResponse expectedId rawLine =
  case decodeStrict rawLine :: Maybe Value of
    Nothing ->
      Left (AwaitProtocolError ("invalid JSON response: " <> BSC.unpack rawLine))
    Just (Object obj) ->
      case parseEither (parseAwaitResponse expectedId) (Object obj) of
        Left err -> Left (AwaitProtocolError err)
        Right outcome -> outcome
    Just _ ->
      Left (AwaitProtocolError "agentd returned a non-object response")

-- | Parse the agentd JSON-RPC envelope, recovering the server-side
-- error @code@.
--
-- Agentd's @RpcResponse@ envelope is intentionally NOT JSON-RPC 2.0:
-- the @error@ field is a flat string, and an optional sibling
-- top-level @error_code@ carries the integer code.  Nesting the code
-- inside the error object would break the existing Python client at
-- @packages/doeff-agents/src/doeff_agents/agentd_client.py@ which
-- reads @error@ as a string.  We mirror that flat shape here so a
-- single Haskell + Python wire spec is preserved.
parseAwaitResponse :: Int -> Value -> Parser (Either AwaitError AwaitedResult)
parseAwaitResponse expectedId = Aeson.withObject "AwaitedResponse" $ \obj -> do
  rid <- obj .: "id"
  ok <- obj .: "ok"
  result <- obj .:? "result"
  errMessage <- obj .:? "error" :: Parser (Maybe Text)
  errorCode <- obj .:? "error_code" :: Parser (Maybe Int)
  if rid /= expectedId
    then pure (Left (AwaitProtocolError "response id did not match request id"))
    else
      if ok
        then case result of
          Just value -> case Aeson.fromJSON value of
            Aeson.Success parsed -> pure (Right parsed)
            Aeson.Error err ->
              pure
                ( Left
                    ( AwaitProtocolError
                        ("session.await_result result failed to parse: " <> err)
                    )
                )
          Nothing ->
            pure
              ( Left
                  ( AwaitProtocolError
                      "session.await_result returned ok=true with no result"
                  )
              )
        else
          let message = case errMessage of
                Just m | not (T.null m) -> T.unpack m
                _ -> "agentd request failed"
              code = case errorCode of
                Just c -> c
                Nothing -> 0
           in pure (Left (AwaitServerError code message))

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
            case launchBinding of
              Nothing -> []
              Just binding -> ["binding" .= binding],
            case launchExpectedResult of
              Nothing -> []
              Just spec -> ["expected_result" .= expectedResultObject spec]
          ]
      params = object (baseFields ++ maybeFields)
  fmap (>>= parseSnapshot) (request cfg "session.launch" params)

-- | @session.resume@: start a new incarnation of a terminal session's
-- conversation (ADR-DOE-AGENTS-006 R4).  Uses the code-preserving
-- transport so callers can classify typed rejects (one-live-incarnation,
-- identity-unknown, transcript-not-discoverable) without substring
-- matching once the session host stamps @error_code@; the verbatim
-- message is always available as the fallback classifier.  The unix
-- socket read blocks without a deadline — resume goes through the same
-- REPL-ready gate as launch, so a short client timeout would disconnect
-- mid-boot.
sessionResume :: AgentdConfig -> ResumeRequest -> IO (Either AgentdCallError AgentdSnapshot)
sessionResume cfg ResumeRequest {..} = do
  let baseFields = ["session_id" .= resumeSourceSessionId]
      maybeFields =
        concat
          [ ["new_session_id" .= resumeNewSessionId | not (T.null resumeNewSessionId)],
            ["prompt" .= resumePrompt | not (T.null resumePrompt)],
            ["model" .= resumeModel | not (T.null resumeModel)],
            ["effort" .= resumeEffort | not (T.null resumeEffort)],
            ["mcp_servers" .= resumeMcpServers | not (Map.null resumeMcpServers)],
            ["session_env" .= resumeSessionEnv | not (Map.null resumeSessionEnv)],
            case resumeBinding of
              Nothing -> []
              Just binding -> ["binding" .= binding],
            case resumeExpectedResult of
              Nothing -> []
              Just spec -> ["expected_result" .= expectedResultObject spec]
          ]
      params = object (baseFields ++ maybeFields)
  outcome <- requestWithCode cfg "session.resume" params
  pure (outcome >>= parseSnapshotWithCode)
  where
    parseSnapshotWithCode value = case parseSnapshot value of
      Left (AgentdProtocolError err) -> Left (CallProtocolError err)
      Left err -> Left (CallProtocolError (show err))
      Right snapshot -> Right snapshot

-- | 'request' variant that keeps the JSON-RPC @error_code@ (the generic
-- helper collapses code + message into one string).
requestWithCode :: AgentdConfig -> Text -> Value -> IO (Either AgentdCallError Value)
requestWithCode AgentdConfig {..} method params = do
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
      pure (Left (CallSocketError (show err)))
    Right rawLine -> pure (decodeResponseWithCode rid rawLine)
  where
    openSock = do
      s <- socket AF_UNIX Stream 0
      connect s (SockAddrUnix agentdSocketPath)
      pure s

decodeResponseWithCode :: Int -> BS.ByteString -> Either AgentdCallError Value
decodeResponseWithCode expectedId rawLine =
  case decodeStrict rawLine :: Maybe Value of
    Nothing ->
      Left (CallProtocolError ("invalid JSON response: " <> BSC.unpack rawLine))
    Just (Object obj) ->
      case parseEither (parseResponseWithCode expectedId) (Object obj) of
        Left err -> Left (CallProtocolError err)
        Right outcome -> outcome
    Just _ ->
      Left (CallProtocolError "agentd returned a non-object response")

parseResponseWithCode :: Int -> Value -> Parser (Either AgentdCallError Value)
parseResponseWithCode expectedId = Aeson.withObject "AgentdResponse" $ \obj -> do
  rid <- obj .: "id"
  ok <- obj .: "ok"
  result <- obj .:? "result"
  err <- obj .:? "error"
  errCode <- obj .:? "error_code"
  if rid /= expectedId
    then pure (Left (CallProtocolError "response id did not match request id"))
    else
      if ok
        then case result of
          Just value -> pure (Right value)
          Nothing -> pure (Right Null)
        else
          let message = case (err :: Maybe Text) of
                Just msg | not (T.null msg) -> T.unpack msg
                _ -> "agentd request failed"
           in pure (Left (CallServerError errCode message))

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
  object ["payload_schema" .= erPayloadSchema]

-- | The result file agentd owns when it injects the result-protocol
-- instruction.  Mirrors @doeff-agentd@'s @DEFAULT_RESULT_FILE@ — the
-- launcher never names a path, so any client-side disk read (the legacy
-- 'snapshotResult' path) uses this shared default.
defaultResultFile :: FilePath
defaultResultFile = ".agentd-result.json"

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
    Just _expected -> do
      let path = T.unpack (snapshotWorkDir snapshot) </> defaultResultFile
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
  snapshotConversation <- obj .:? "conversation"
  snapshotGeneration <- obj .:? "generation"
  snapshotResumedFromSessionId <- obj .:? "resumed_from_session_id"
  snapshotForkedFromSessionId <- obj .:? "forked_from_session_id"
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
