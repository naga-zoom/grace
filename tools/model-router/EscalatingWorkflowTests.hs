{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- Fail-first behavioral test for EscalatingWorkflow.ffg's CPS handoff.
-- The fake prompt handler plays the role of the model; the fake host-action
-- executor plays the role of the host tool runner. The test driver plays
-- the role of WorkflowHost's loop: it inspects pendingValidation, executes
-- the (fake) action, and re-invokes the same Grace program with history
-- extended -- all within one process, as the real host must do.

import Control.Monad (forM_, unless)
import Data.Aeson (Key, Value(..), eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
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

textOf :: Value -> Maybe Text
textOf (String t) = Just t
textOf _ = Nothing

-- A scripted sequence of host-action verdicts, consumed one per pendingValidation round.
run :: FilePath -> [Bool] -> IO [Value]
run root verdicts = do
    workflowSrc <- Text.IO.readFile (root </> "EscalatingWorkflow.ffg")
    taskSrc <- Text.IO.readFile (root </> "fixtures/TieredTask.ffg")
    verdictsRef <- newIORef verdicts
    roundsRef <- newIORef ([] :: [Value])
    let configurationCode =
            "{ baseline: \"XS\""
            <> ", profiles: [ { size: \"XS\", model: \"gpt-6-luna\", modelVersion: null : Optional Text, effort: \"low\" }"
            <> ", { size: \"S\", model: \"gpt-6.1-sol\", modelVersion: null : Optional Text, effort: \"low\" }"
            <> ", { size: \"L\", model: \"gpt-6.1-sol\", modelVersion: null : Optional Text, effort: \"high\" } ]"
            <> ", catalog: [ { model: \"gpt-6-luna\", modelVersion: null : Optional Text, efforts: [ \"low\", \"medium\", \"high\", \"xhigh\", \"max\" ] }"
            <> ", { model: \"gpt-6.1-sol\", modelVersion: null : Optional Text, efforts: [ \"low\", \"medium\", \"high\", \"xhigh\", \"max\", \"ultra\" ] } ]"
            <> ", observations: [ ] }"
        emptyHistoryType =
            "[ ] : List { tier: Text, size: Text, model: Text, modelVersion: Optional Text, effort: Text, source: Text"
            <> ", reply: Text, action: Text, ok: Bool, diagnostics: List Text }"
        respond HostPrompt{model, effort, text} _schema = do
            payload <- either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 text) :: Either String Value)
            case field "role" payload of
                Just (String "planning") -> pure (object ["plan" .= ("synthetic plan" :: Text)])
                Just (String "orchestration") -> do
                    unless (model == "gpt-6-luna" && effort == "low") (fail "orchestration left the XS tier")
                    pure (object ["executionTier" .= ("XS" :: Text)])
                Just (String "execution") -> pure (object ["reply" .= ("execution reply for " <> model <> "/" <> effort)])
                _ -> fail "unexpected role in prompt text"
        -- Need plan threaded too; rebuild invoke with explicit plan arg.
        invokeWithPlan :: Maybe Text -> Text -> IO Value
        invokeWithPlan planText historyCode = do
            let planCode = maybe "null : Optional Text" (\p -> "some \"" <> p <> "\"") planText
                code = "let workflow = (" <> workflowSrc <> ") "
                    <> "let configuration = (" <> Text.pack configurationCode <> ") "
                    <> "let task = (" <> taskSrc <> ") "
                    <> "in show (workflow configuration \"haskell-compile\" (" <> planCode <> ") (" <> historyCode <> ") task)"
                input = Code "escalating-workflow-behavior" code
            (_, result) <- Grace.evalGrace input Grace.Status{Grace.count = 0, Grace.context = []}
                (Grace.withPrompt respond (Interpret.interpretWith [] Nothing))
            document <- either (fail . show) (pure :: Text -> IO Text) (decode result)
            either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 document))
        renderHistory :: [Value] -> Text
        renderHistory [] = Text.pack emptyHistoryType
        renderHistory entries = "[ " <> Text.intercalate ", " (map renderEntry entries) <> " ]"
        renderEntry entry =
            let get k = maybe "" renderValue (field k entry)
            in "{ tier: " <> get "tier" <> ", size: " <> get "size" <> ", model: " <> get "model"
                <> ", modelVersion: null : Optional Text, effort: " <> get "effort" <> ", source: " <> get "source"
                <> ", reply: " <> get "reply" <> ", action: " <> get "action" <> ", ok: " <> get "ok"
                <> ", diagnostics: [ ] }"
        renderValue (String t) = "\"" <> t <> "\""
        renderValue (Bool True) = "true"
        renderValue (Bool False) = "false"
        renderValue _ = "\"\""
        loop :: Maybe Text -> [Value] -> IO [Value]
        loop planText entries = do
            result <- invokeWithPlan planText (renderHistory entries)
            modifyIORef' roundsRef (<> [result])
            case field "status" result of
                Just (String "pendingValidation") -> do
                    verdictQueue <- readIORef verdictsRef
                    (ok, rest) <- case verdictQueue of
                        [] -> fail "ran out of scripted host-action verdicts"
                        (v : vs) -> pure (v, vs)
                    modifyIORef' verdictsRef (const rest)
                    let Just attempt = field "pendingAttempt" result
                        Just action = field "pendingAction" result >>= textOf
                        newEntry = case attempt of
                            Object fields -> Object (KM.insert "action" (String action)
                                (KM.insert "ok" (Bool ok) (KM.insert "diagnostics" (Array Vector.empty) fields)))
                            other -> other
                        newPlan = field "plan" result >>= textOf
                    loop newPlan (entries <> [newEntry]) >>= \_ -> pure ()
                _ -> pure ()
            readIORef roundsRef
    loop Nothing []

main :: IO ()
main = do
    args <- getArgs
    root <- case args of
        [r] -> pure r
        _ -> fail "Usage: EscalatingWorkflowTests ROOT"

    passFirstTry <- run root [True]
    escalateThenPass <- run root [False, True]
    exhausted <- run root [False, False, False]

    let statuses rs = map (field "status") rs
        tiersUsed rs = [ t | r <- rs, Just attempt <- [field "pendingAttempt" r]
                       , Just t <- [field "tier" attempt] ]
        checks =
            [ ("first attempt passing validation finalizes immediately as candidate"
              , statuses passFirstTry == [Just (String "pendingValidation"), Just (String "candidate")])
            , ("passing on the first try never re-runs planning (same plan echoed, one round of prompts)"
              , length passFirstTry == 2)
            , ("escalation after one failure moves exactly one fixed tier forward"
              , statuses escalateThenPass == [Just (String "pendingValidation"), Just (String "pendingValidation"), Just (String "candidate")]
                && tiersUsed escalateThenPass == [String "XS", String "S"])
            , ("exhausting every fixed tier refuses instead of escalating past L"
              , statuses exhausted ==
                  [ Just (String "pendingValidation"), Just (String "pendingValidation")
                  , Just (String "pendingValidation"), Just (String "refused") ]
                && tiersUsed exhausted == [String "XS", String "S", String "L"])
            , ("a refusal from exhaustion never carries adoptionAllowed=true"
              , case exhausted of
                  rs -> field "adoptionAllowed" (last rs) == Just (Bool False)
                      && field "refusal" (last rs) /= Just Null)
            , ("fixed-tier invariant: every stage across every run uses exactly one of the three fixed (model,effort) pairs"
              , all stagePairIsFixed (concatMap stagesOf (passFirstTry <> escalateThenPass <> exhausted)))
            ]
    forM_ checks (\(label, ok) -> putStrLn (label <> ": " <> if ok then "PASS" else "FAIL"))
    unless (all snd checks) exitFailure
  where
    stagesOf r = case field "stages" r of
        Just (Array stages) -> Vector.toList stages
        _ -> []
    stagePairIsFixed stage = case (field "model" stage, field "effort" stage) of
        (Just (String "gpt-6-luna"), Just (String "low")) -> True
        (Just (String "gpt-6.1-sol"), Just (String "low")) -> True
        (Just (String "gpt-6.1-sol"), Just (String "high")) -> True
        _ -> False
