{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- Fail-first behavioral test for TieredWorkflow.ffg + TieredSettings.ffg.
-- Mirrors RouterTests.hs's pattern: a fake prompt handler intercepts every
-- HostPrompt call, asserts the exact fixed (model,effort) tier requested,
-- and returns canned JSON keyed off the embedded role so planning,
-- orchestration and execution can be told apart even though planning and
-- orchestration share the identical "XS" tier.

import Control.Monad (forM_, unless)
import Data.Aeson (Key, Value(..), eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import Grace.Decode (decode)
import Grace.Input (Input(..))
import qualified Grace.Interpret as Interpret
import qualified Grace.Monad as Grace
import NativeCodex (HostPrompt(..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

field :: Key -> Value -> Maybe Value
field key (Object fields) = KM.lookup key fields
field _ _ = Nothing

run :: FilePath -> Text -> IO Value
run root executionTierReply = do
    task <- Text.IO.readFile (root </> "fixtures/TieredTask.ffg")
    runWithTask root executionTierReply task

runWithTask :: FilePath -> Text -> Text -> IO Value
runWithTask root executionTierReply task = do
    settings <- Text.IO.readFile (root </> "TieredSettings.ffg")
    workflow <- Text.IO.readFile (root </> "TieredWorkflow.ffg")
    let code = "let settings = (" <> settings <> ") "
            <> "let catalog = "
            <> "[ { model: \"gpt-6-luna\", modelVersion: null : Optional Text, efforts: [ \"low\", \"medium\", \"high\", \"xhigh\", \"max\" ] }"
            <> ", { model: \"gpt-6.1-sol\", modelVersion: null : Optional Text, efforts: [ \"low\", \"medium\", \"high\", \"xhigh\", \"max\", \"ultra\" ] } ] "
            <> "let configuration = "
            <> "{ baseline: settings.configuration.baseline, profiles: settings.configuration.profiles"
            <> ", catalog, observations: settings.configuration.observations } "
            <> "let workflow = (" <> workflow <> ") "
            <> "let task = (" <> task <> ") "
            <> "in show (workflow configuration task)"
        input = Code "tiered-workflow-behavior" code
        respond HostPrompt{model, effort, text} _schema = do
            payload <- either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 text) :: Either String Value)
            let role = field "role" payload
            case role of
                Just (String "planning") -> do
                    unless (model == "gpt-6-luna" && effort == "low") (fail "planning left the XS tier")
                    pure (object ["plan" .= ("synthetic plan" :: Text)])
                Just (String "orchestration") -> do
                    unless (model == "gpt-6-luna" && effort == "low") (fail "orchestration left the XS tier")
                    pure (object ["executionTier" .= executionTierReply])
                Just (String "execution") -> pure (object ["reply" .= ("execution reply" :: Text)])
                _ -> fail "unexpected role in prompt text"
    (_, result) <- Grace.evalGrace input Grace.Status{Grace.count = 0, Grace.context = []}
        (Grace.withPrompt respond (Interpret.interpretWith [] Nothing))
    document <- either (fail . show) (pure :: Text -> IO Text) (decode result)
    either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 document))

main :: IO ()
main = do
    args <- getArgs
    root <- case args of
        [r] -> pure r
        _ -> fail "Usage: TieredWorkflowTests ROOT"

    xs <- run root "XS"
    s <- run root "S"
    l <- run root "L"
    bogus <- run root "nonexistent-tier"
    duplicatePacket <- runWithTask root "XS"
        ("{ taskCohort: \"unclassified\", workflowCohort: \"tiered-candidate\""
        <> ", contextProtocol: \"packet-v1\", qualityProtocol: \"objective-v1\", trialEpoch: null : Optional Text"
        <> ", goal: \"Duplicate packet\", acceptanceCriteria: [ ], negativeEvidence: [ ]"
        <> ", sourcePackets: [ { id: \"dup\", content: \"a\" }, { id: \"dup\", content: \"b\" } ]"
        <> ", requiredSourceIds: [ ] }")
    unknownRequired <- runWithTask root "XS"
        ("{ taskCohort: \"unclassified\", workflowCohort: \"tiered-candidate\""
        <> ", contextProtocol: \"packet-v1\", qualityProtocol: \"objective-v1\", trialEpoch: null : Optional Text"
        <> ", goal: \"Unknown required ID\", acceptanceCriteria: [ ], negativeEvidence: [ ]"
        <> ", sourcePackets: [ { id: \"present\", content: \"a\" } ]"
        <> ", requiredSourceIds: [ \"missing\" ] }")

    let stagesOf doc = case field "stages" doc of
            Just (Array stages) -> Vector.toList stages
            _ -> []
        stageModelEffort doc expectedRole = case [ (field "model" stage, field "effort" stage)
                | stage <- stagesOf doc, field "role" stage == Just (String expectedRole) ] of
            [pair] -> Just pair
            _ -> Nothing
        checks =
            [ ("planning stage always runs at XS (gpt-6-luna/low)"
              , stageModelEffort xs "planning" == Just (Just (String "gpt-6-luna"), Just (String "low")))
            , ("orchestration stage always runs at XS (gpt-6-luna/low)"
              , stageModelEffort xs "orchestration" == Just (Just (String "gpt-6-luna"), Just (String "low")))
            , ("orchestration choosing XS routes execution to gpt-6-luna/low"
              , stageModelEffort xs "execution" == Just (Just (String "gpt-6-luna"), Just (String "low")))
            , ("orchestration choosing S routes execution to gpt-6.1-sol/low"
              , stageModelEffort s "execution" == Just (Just (String "gpt-6.1-sol"), Just (String "low")))
            , ("orchestration choosing L routes execution to gpt-6.1-sol/high"
              , stageModelEffort l "execution" == Just (Just (String "gpt-6.1-sol"), Just (String "high")))
            , ("exactly three stages in order on a routed task"
              , map (field "role") (stagesOf xs) == [Just (String "planning"), Just (String "orchestration"), Just (String "execution")])
            , ("adoption is never allowed from this candidate"
              , field "adoptionAllowed" xs == Just (Bool False) && field "proposal" xs == Just Null)
            , ("an unrecognized model-authored tier fails closed, never silently"
              , field "status" bogus == Just (String "refused")
                && field "adoptionAllowed" bogus == Just (Bool False)
                && stagesOf bogus == [])
            , ("duplicate packet IDs refuse before any prompt call"
              , field "status" duplicatePacket == Just (String "refused")
                && field "adoptionAllowed" duplicatePacket == Just (Bool False)
                && stagesOf duplicatePacket == [])
            , ("unknown required source ID refuses before any prompt call"
              , field "status" unknownRequired == Just (String "refused")
                && field "adoptionAllowed" unknownRequired == Just (Bool False)
                && stagesOf unknownRequired == [])
            ]
    forM_ checks (\(label, ok) -> putStrLn (label <> ": " <> if ok then "PASS" else "FAIL"))
    unless (all snd checks) exitFailure
