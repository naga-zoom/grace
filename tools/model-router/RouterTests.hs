{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, Key, Value(..), eitherDecodeStrict', object, (.=))
import qualified Data.Aeson.Key as Key
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
                                || (field "task" payload >>= field "goal") == Just (String "Empty source") then [] else [if field "goal" payload == Just (String "Untrusted selection")
                                || (field "task" payload >>= field "goal") == Just (String "Untrusted selection")
                                then "missing" else "relevant"] :: [Text.Text]) ]
    (_, result) <- Grace.evalGrace input Grace.Status{Grace.count = 0, Grace.context = []}
        (Grace.withPrompt respond (Interpret.interpretWith [] Nothing))
    workflowJSON <- either (fail . show) (pure :: Text.Text -> IO Text.Text) (decode result)
    document <- either fail pure (eitherDecodeStrict' (Encoding.encodeUtf8 workflowJSON) :: Either String Value)
    observed <- readIORef requests
    let completed = field "completed" document
        emptySource = field "emptySource" document
        get key nested = nested >>= field key
        forGoal goal = filter ((== Just (String goal)) . field "goal") observed
        completedRequests = forGoal "Synthetic source-linked task"
        expectedSources = Array (Vector.fromList
            [ object [ "id" .= ("relevant" :: Text.Text), "content" .= ("Relevant raw fact" :: Text.Text) ]
            , object [ "id" .= ("required" :: Text.Text), "content" .= ("Required raw dependency" :: Text.Text) ]
            , object [ "id" .= ("irrelevant" :: Text.Text), "content" .= ("Irrelevant large raw packet" :: Text.Text) ] ])
        refusedCases = [ ("refused", "Unsupported mapping"), ("missingDependency", "Missing dependency")
            , ("duplicatePacket", "Duplicate packet"), ("emptyPacketId", "Empty packet ID")
            , ("duplicateRequired", "Duplicate required ID"), ("unknownRequired", "Unknown required ID")
            , ("emptyRequired", "Empty required ID") ]
        checks =
            [ ("one execution prompt per accepted task", length observed == 3
                && map (field "role") observed == replicate 3 (Just (String "execution"))
                && length completedRequests == 1)
            , ("model suggestions do not authorize adoption", get "adoptionAllowed" completed == Just (Bool False)
                && get "proposal" completed == Just Null)
            , ("one typed execution stage keeps configured baseline", case get "stages" completed of
                    Just (Array stages) -> Vector.length stages == 1
                        && all (\stage -> field "role" stage == Just (String "execution")
                            && field "size" stage == Just (String "L") && field "source" stage == Just (String "baseline")
                            && field "reply" stage == Just (String "execution reply")) stages
                    _ -> False)
            , ("required IDs are a floor; every raw packet survives", length completedRequests == 1
                && all ((== Just expectedSources) . field "sources") completedRequests)
            , ("goal acceptance and negative evidence survive exactly", length completedRequests == 1 && all (\payload ->
                field "goal" payload == Just (String "Synthetic source-linked task")
                && field "acceptanceCriteria" payload == Just (toJSONTextList ["Preserve source facts"])
                && field "negativeEvidence" payload == Just (toJSONTextList ["Prior trial failed null boundary"])
                && field "requiredSourceIds" payload == Just (toJSONTextList ["required"])) completedRequests)
            , ("untrusted source suggestions cannot prune facts", get "status" (field "unknownSelection" document) == Just (String "candidate")
                && map (field "sources") (forGoal "Untrusted selection") == [Just expectedSources])
            , ("empty source task makes one execution call", get "status" emptySource == Just (String "candidate")
                && map (field "sources") (forGoal "Empty source") == [Just (Array Vector.empty)])
            , ("live baseline unknown revision stays null", case get "stages" completed of
                    Just (Array stages) -> Vector.length stages == 1 && all ((== Just Null) . field "modelVersion") stages
                    _ -> False) ]
            <> [(Text.unpack (Key.toText key) <> " refuses before inference", get "status" (field key document) == Just (String "refused")
                && get "adoptionAllowed" (field key document) == Just (Bool False)
                && null (forGoal goal))
                | (key,goal) <- refusedCases]
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
