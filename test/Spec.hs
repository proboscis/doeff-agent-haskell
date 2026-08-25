{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pins for @resolveDoeffAgentsCommand@ (issue #7 in agent-control-plane):
-- resolution is @DOEFF_AGENTS_BIN@ → @PATH@ → loud fail, with NO further
-- fallback.  The retired HOME candidate silently resolved to a stale
-- main-branch binary under launchd and masked a never-succeeding fetch
-- while keeping ensure/spawn on a version-skewed host.
--
-- Also pins the pure wire-params builders ('launchRequestParams' /
-- 'resumeRequestParams'): an optional field the caller did not set stays
-- OFF the wire, so a session host predating the field never sees it —
-- the additive-evolution contract every @Maybe@ field relies on.
module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (Value (Object), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Doeff.Agentd.Client
  ( AgentdError (..),
    LaunchRequest (..),
    ResumeRequest (..),
    SessionLifecycle (LifecycleRunToCompletion),
    launchRequestParams,
    resolveDoeffAgentsCommand,
    resumeRequestParams,
  )
import System.Directory
  ( createDirectory,
    getPermissions,
    setOwnerExecutable,
    setPermissions,
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

main :: IO ()
main = hspec spec

-- | Run an action with the given environment overrides ('Nothing' unsets),
-- restoring the original values afterwards.  The resolver reads the real
-- process environment, so these specs serialise through it; hspec runs
-- items sequentially, which keeps that sound.
withEnv :: [(String, Maybe String)] -> IO a -> IO a
withEnv overrides action =
  bracket save restore (const (applyAll overrides >> action))
  where
    save = mapM (\(name, _) -> (,) name <$> lookupEnv name) overrides
    restore = mapM_ (uncurry applyOne)
    applyAll = mapM_ (uncurry applyOne)
    applyOne name = \case
      Just value -> setEnv name value
      Nothing -> unsetEnv name

-- | Drop a fake executable @doeff-agents@ into @dir@ so 'findExecutable'
-- can resolve it when @dir@ is on PATH.
plantFakeAgents :: FilePath -> IO FilePath
plantFakeAgents dir = do
  let path = dir </> "doeff-agents"
  writeFile path "#!/bin/sh\nexit 0\n"
  perms <- getPermissions path
  setPermissions path (setOwnerExecutable True perms)
  pure path

-- | Minimal launch request: every optional field unset.  What this
-- produces on the wire is exactly what a pre-extension host receives.
minimalLaunchRequest :: LaunchRequest
minimalLaunchRequest =
  LaunchRequest
    { launchSessionId = "sid-1",
      launchSessionName = "name-1",
      launchAgentType = "claude",
      launchWorkDir = "/tmp/w",
      launchCommand = "",
      launchPrompt = "",
      launchModel = "",
      launchEffort = "",
      launchMcpServers = Map.empty,
      launchSkipTrustSetup = False,
      launchLifecycle = LifecycleRunToCompletion,
      launchSessionEnv = Map.empty,
      launchBinding = Nothing,
      launchExpectedResult = Nothing,
      launchContextFile = Nothing,
      launchWorkspaceSeed = Nothing,
      launchAttribution = Nothing
    }

-- | Minimal resume request, same idea.
minimalResumeRequest :: ResumeRequest
minimalResumeRequest =
  ResumeRequest
    { resumeSourceSessionId = "sid-src",
      resumeNewSessionId = "",
      resumePrompt = "",
      resumeModel = "",
      resumeEffort = "",
      resumeMcpServers = Map.empty,
      resumeSessionEnv = Map.empty,
      resumeBinding = Nothing,
      resumeWorkDir = "",
      resumeExpectedResult = Nothing,
      resumeContextFile = Nothing,
      resumeAttribution = Nothing
    }

-- | The example attribution object the ACP scheduler sends: opaque to
-- this client and to the session host — carried verbatim.
attributionFixture :: Value
attributionFixture =
  object
    [ "work_item_id" .= ("wi_attr" :: Text),
      "invocation_id" .= ("inv_wi_attr_a1" :: Text),
      "action_id" .= ("argus-sensor-run" :: Text),
      "resource_key" .= ("default:agent-responsibility:argus-loop" :: Text),
      "namespace" .= ("default" :: Text)
    ]

paramsFields :: Value -> KeyMap.KeyMap Value
paramsFields = \case
  Object fields -> fields
  other -> error ("wire params must be a JSON object, got: " <> show other)

spec :: Spec
spec = do
  wireParamsSpec
  resolverSpec

wireParamsSpec :: Spec
wireParamsSpec = do
  describe "launchRequestParams (session.launch wire params)" $ do
    it "keeps launch_attribution OFF the wire when the caller did not set it" $
      -- The additive contract: a host predating the field must receive
      -- byte-identical params from an unchanged caller.
      KeyMap.member "launch_attribution" (paramsFields (launchRequestParams minimalLaunchRequest))
        `shouldBe` False

    it "carries launch_attribution verbatim when set — the client never interprets it" $ do
      let params =
            launchRequestParams
              minimalLaunchRequest {launchAttribution = Just attributionFixture}
      KeyMap.lookup "launch_attribution" (paramsFields params)
        `shouldBe` Just attributionFixture

  describe "resumeRequestParams (session.resume wire params)" $ do
    it "keeps launch_attribution OFF the wire when the caller did not set it" $
      KeyMap.member "launch_attribution" (paramsFields (resumeRequestParams minimalResumeRequest))
        `shouldBe` False

    it "carries launch_attribution verbatim on the resume face too — one law, both faces" $ do
      -- A resumed incarnation hosts a fresh invocation; dropping the
      -- attribution on this verb would break the spend→function join
      -- exactly on the failover lane.
      let params =
            resumeRequestParams
              minimalResumeRequest {resumeAttribution = Just attributionFixture}
      KeyMap.lookup "launch_attribution" (paramsFields params)
        `shouldBe` Just attributionFixture

resolverSpec :: Spec
resolverSpec = describe "resolveDoeffAgentsCommand" $ do
  it "resolves an existing DOEFF_AGENTS_BIN verbatim" $
    withSystemTempDirectory "doeff-agents-test" $ \dir -> do
      fake <- plantFakeAgents dir
      resolved <-
        withEnv [("DOEFF_AGENTS_BIN", Just fake)] resolveDoeffAgentsCommand
      resolved `shouldBe` Right fake

  it "fails loud on a set-but-missing DOEFF_AGENTS_BIN even when PATH could resolve" $
    withSystemTempDirectory "doeff-agents-test" $ \dir -> do
      -- A PATH hit here would silently swap the pinned executable for a
      -- differently-versioned one — exactly the skew the seam prevents.
      _ <- plantFakeAgents dir
      let missing = dir </> "not-here" </> "doeff-agents"
      resolved <-
        withEnv
          [ ("DOEFF_AGENTS_BIN", Just missing),
            ("PATH", Just dir)
          ]
          resolveDoeffAgentsCommand
      case resolved of
        Left (AgentdSocketError message) ->
          message `shouldContain` "DOEFF_AGENTS_BIN is set but does not exist"
        other ->
          expectationFailure
            ("expected a loud AgentdSocketError, got: " <> show other)

  it "falls back to PATH when DOEFF_AGENTS_BIN is unset" $
    withSystemTempDirectory "doeff-agents-test" $ \dir -> do
      fake <- plantFakeAgents dir
      resolved <-
        withEnv
          [ ("DOEFF_AGENTS_BIN", Nothing),
            ("PATH", Just dir)
          ]
          resolveDoeffAgentsCommand
      resolved `shouldBe` Right fake

  it "fails loud when neither DOEFF_AGENTS_BIN nor PATH resolves (no HOME fallback)" $
    withSystemTempDirectory "doeff-agents-test" $ \dir -> do
      -- An empty dir on PATH plus a HOME primed with the retired candidate
      -- location: the resolver must NOT look there any more.
      let home = dir </> "home"
          venvBin = home </> "repos" </> "doeff" </> ".venv" </> "bin"
      mapM_
        createDirectory
        [ home,
          home </> "repos",
          home </> "repos" </> "doeff",
          home </> "repos" </> "doeff" </> ".venv",
          venvBin
        ]
      _ <- plantFakeAgents venvBin
      resolved <-
        withEnv
          [ ("DOEFF_AGENTS_BIN", Nothing),
            ("PATH", Just dir),
            ("HOME", Just home)
          ]
          resolveDoeffAgentsCommand
      case resolved of
        Left (AgentdSocketError message) ->
          message `shouldContain` "doeff-agents not found"
        other ->
          expectationFailure
            ("expected a loud AgentdSocketError, got: " <> show other)
