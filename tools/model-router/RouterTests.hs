{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, Key, Value(..), eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Grace.Decode (decode)
import Grace.Input (Input(..))
import qualified Grace.Interpret as Interpret
import qualified Grace.Monad as Grace
import NativeCodex (HostPrompt(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

data Case = Case { name :: Text.Text, actual :: Value, expected :: Value }
    deriving stock (Generic, Show)
instance FromJSON Case

main :: IO ()
main = do
    args <- getArgs
    (root, policyPath, workflowPath, workflowCasesPath) <- case args of
        [root] -> pure (root, root </> "policy.ffg", root </> "workflow.ffg", root </> "fixtures/WorkflowCases.ffg")
        [root, policyPath, workflowPath] -> pure (root, policyPath, workflowPath, root </> "fixtures/WorkflowCases.ffg")
        [root, policyPath, workflowPath, workflowCasesPath] -> pure (root, policyPath, workflowPath, workflowCasesPath)
        _ -> fail "Usage: RouterTests.hs ROOT [POLICY WORKFLOW [WORKFLOW_CASES]]"
    policy <- Text.IO.readFile policyPath
    cases <- Text.IO.readFile (root </> "fixtures/PolicyCases.ffg")
    let code = "let policy = (" <> policy <> ") let cases = (" <> cases <> ") in show (cases policy)"
    (_, value) <- Interpret.interpret (Code "router-behavior" code)
    json <- either (fail . show) (pure :: Text.Text -> IO Text.Text) (decode value)
    results <- either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 json) :: Either String [Case])
    let failures = filter (\Case{actual = observed, expected = wanted} -> observed /= wanted) results
    forM_ failures (\Case{name = label, actual = observed, expected = wanted} -> putStrLn (Text.unpack label <> ": expected " <> show wanted <> ", got " <> show observed))
    putStrLn (show (length results - length failures) <> "/" <> show (length results) <> " policy cases passed")
    workflowCases <- Text.IO.readFile workflowCasesPath
    requests <- newIORef []
    let input = Code "router-workflow" ("let workflow = " <> Text.pack workflowPath <> " let cases = (" <> workflowCases <> ") in show (cases workflow)")
        respond HostPrompt{model, effort, text} _schema = do
            unless (model == "fixture-expert" && effort == "high") (fail "workflow changed the configured baseline")
            payload <- either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 text) :: Either String Value)
            let role = field "role" payload
            modifyIORef' requests (<> [payload])
            pure $ if role == Just (String "verification")
                then object [ "proposedProfile" .= ("S" :: Text.Text), "rationale" .= ("Model self-certification" :: Text.Text)
                            , "adoptionAllowed" .= True, "verifierPassed" .= True ]
                else object [ "reply" .= (maybe "missing" id (asText role) <> " reply")
                            , "selectedSourceIds" .= (if field "goal" payload == Just (String "Empty source")
                                || (field "task" payload >>= field "goal") == Just (String "Empty source") then [] else [if field "goal" payload == Just (String "Unknown selection")
                                || (field "task" payload >>= field "goal") == Just (String "Unknown selection")
                                then "missing" else "relevant"] :: [Text.Text]) ]
    (_, result) <- Grace.evalGrace input Grace.Status{Grace.count = 0, Grace.context = []}
        (Grace.withPrompt respond (Interpret.interpretWith [] Nothing))
    workflowJSON <- either (fail . show) (pure :: Text.Text -> IO Text.Text) (decode result)
    document <- either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 workflowJSON) :: Either String Value)
    observed <- readIORef requests
    let completed = field "completed" document
        refused = field "refused" document
        unknownSelection = field "unknownSelection" document
        missingDependency = field "missingDependency" document
        emptySource = field "emptySource" document
        get key nested = nested >>= field key
        expectedRequests =
            [ (Just (String "planning"), Just (String ""))
            , (Just (String "orchestration"), Just (String "planning reply"))
            , (Just (String "execution"), Just (String "orchestration reply"))
            , (Just (String "verification"), Just (String "execution reply"))
            , (Just (String "planning"), Just (String ""))
            , (Just (String "planning"), Just (String ""))
            , (Just (String "orchestration"), Just (String "planning reply"))
            , (Just (String "execution"), Just (String "orchestration reply"))
            , (Just (String "verification"), Just (String "execution reply")) ]
        downstream = take 3 (drop 1 observed)
        sourceIds payload = case field "sources" payload of
            Just (Array sources) -> map (field "id") (Vector.toList sources)
            _ -> []
        expectedSources = Array (Vector.fromList
            [ object [ "id" .= ("relevant" :: Text.Text), "content" .= ("Relevant raw fact" :: Text.Text) ]
            , object [ "id" .= ("required" :: Text.Text), "content" .= ("Required raw dependency" :: Text.Text) ] ])
        checks =
            [ ("four sequential roles share one trusted table", map (\payload -> (field "role" payload, field "previous" payload)) observed == expectedRequests)
            , ("verifier proposal is held", get "adoptionAllowed" completed == Just (Bool False)
                && (get "proposal" completed >>= field "proposedProfile") == Just (String "S"))
            , ("empty evidence remains baseline for all roles", case get "stages" completed of
                    Just (Array stages) -> Vector.length stages == 4
                        && all (\stage -> field "size" stage == Just (String "L") && field "source" stage == Just (String "baseline")) stages
                    _ -> False)
            , ("unsupported configuration refuses without extra dispatch", get "status" refused == Just (String "refused")
                && get "adoptionAllowed" refused == Just (Bool False))
            , ("required omitted selection is retained as raw source", length downstream == 3 && all ((== Just expectedSources) . field "sources") downstream)
            , ("irrelevant packet is absent downstream", length downstream == 3 && all (\payload -> field "task" payload == Nothing
                && Just (String "irrelevant") `notElem` sourceIds payload) downstream)
            , ("plan acceptance and failed evidence survive handoff", length downstream == 3 && all (\payload ->
                field "plan" payload == Just (String "planning reply")
                && field "acceptanceCriteria" payload == Just (toJSONTextList ["Preserve source facts"])
                && field "negativeEvidence" payload == Just (toJSONTextList ["Prior trial failed null boundary"])) downstream)
            , ("verifier sees actual execution with original goal", case drop 3 observed of
                    payload : _ -> field "execution" payload == Just (String "execution reply")
                        && field "goal" payload == Just (String "Synthetic source-linked task")
                    _ -> False)
            , ("unknown selection refuses before downstream prompts", get "status" unknownSelection == Just (String "refused"))
            , ("missing dependency refuses before any dispatch", get "status" missingDependency == Just (String "refused") && length observed == 9)
            , ("empty source task remains callable", get "status" emptySource == Just (String "candidate")
                && length (drop 5 observed) == 4 && all ((== Just (Array Vector.empty)) . field "sources") (drop 5 observed))
            , ("live baseline unknown revision stays null", case get "stages" completed of
                    Just (Array stages) -> Vector.length stages == 4 && all ((== Just Null) . field "modelVersion") stages
                    _ -> False) ]
    forM_ checks (\(label, ok) -> putStrLn (label <> ": " <> if ok then "PASS" else "FAIL"))
    unless (null failures && all snd checks) exitFailure

field :: Key -> Value -> Maybe Value
field key (Object fields) = KM.lookup key fields
field _ _ = Nothing
asText :: Maybe Value -> Maybe Text.Text
asText (Just (String text)) = Just text
asText _ = Nothing

toJSONTextList :: [Text.Text] -> Value
toJSONTextList = Array . Vector.fromList . map String
