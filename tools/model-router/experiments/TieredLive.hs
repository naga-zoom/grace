{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

-- Live, real-money paired cost measurement: one-call baseline (a single
-- direct prompt at the cheapest fixed tier) versus the tiered candidate
-- (TieredWorkflow.ffg's planning -> orchestration -> execution chain), on
-- one trivial, deliberately cheap fixture task, against the real installed
-- native Codex app-server for this account. Not a policy-qualifying
-- evaluation: one task, one order, no retries. Dollar costs are computed
-- from the pricing table confirmed this session (per-model official pages),
-- not estimated.

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as Text.IO
import GHC.Clock (getMonotonicTimeNSec)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Grace.Decode as Decode
import Grace.Decode (FromGrace, ToGraceType)
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import NativeCodex
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Timeout (timeout)

data DirectReply = DirectReply { reply :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)

jsonText :: Value -> Text
jsonText = Encoding.decodeUtf8 . BL.toStrict . encode

-- Per-million-token rates confirmed from official per-model pages this
-- session (see /memories .../model-router-design.md). Price is independent
-- of reasoning effort; effort only changes how many reasoning tokens a call
-- generates, which is a usage-volume effect already captured by Usage.
rates :: Text -> Maybe (Double, Double, Double) -- (input, cachedInput, output), $ per 1e6 tokens
rates "gpt-6-luna" = Just (0.10, 0.01, 0.50)
rates "gpt-6.1-sol" = Just (2.00, 0.10, 10.00)
rates _ = Nothing

dollarCost :: Observation -> Maybe Double
dollarCost Observation{requestedModel, nativeUsage = Just Usage{inputTokens, cachedInputTokens, outputTokens}} = do
    (inRate, cachedRate, outRate) <- rates requestedModel
    let nonCached = fromIntegral (inputTokens - cachedInputTokens) :: Double
    pure ((nonCached * inRate + fromIntegral cachedInputTokens * cachedRate + fromIntegral outputTokens * outRate) / 1000000)
dollarCost _ = Nothing

task :: Value
task = object
    [ "taskCohort" .= ("unclassified" :: Text), "workflowCohort" .= ("tiered-live-1" :: Text)
    , "contextProtocol" .= ("packet-v1" :: Text), "qualityProtocol" .= ("objective-v1" :: Text), "trialEpoch" .= Null
    , "goal" .= ("Reply with exactly the four words: tiered live smoke ok" :: Text)
    , "acceptanceCriteria" .= (["Reply must contain the exact phrase tiered live smoke ok and nothing else"] :: [Text])
    , "negativeEvidence" .= ([] :: [Text])
    , "sourcePackets" .= [object ["id" .= ("note" :: Text), "content" .= ("Live cost-measurement smoke test only. Keep every reply extremely short." :: Text)]]
    , "requiredSourceIds" .= (["note" :: Text])
    ]

configurationWith :: Value -> Value
configurationWith catalog_ = object
    [ "baseline" .= ("XS" :: Text)
    , "profiles" .=
        [ object ["size" .= ("XS" :: Text), "model" .= ("gpt-6-luna" :: Text), "modelVersion" .= Null, "effort" .= ("low" :: Text)]
        , object ["size" .= ("S" :: Text), "model" .= ("gpt-6.1-sol" :: Text), "modelVersion" .= Null, "effort" .= ("low" :: Text)]
        , object ["size" .= ("L" :: Text), "model" .= ("gpt-6.1-sol" :: Text), "modelVersion" .= Null, "effort" .= ("high" :: Text)]
        ]
    , "catalog" .= catalog_
    , "observations" .= ([] :: [Value])
    ]

fieldOr :: Key -> Value -> Value
fieldOr key value = maybe Null id (field key value)

summarize :: Text -> [Observation] -> Word64 -> Value
summarize label observed elapsedNs = object
    [ "label" .= label
    , "elapsedSeconds" .= (fromIntegral elapsedNs / 1000000000 :: Double)
    , "nativeTurns" .= length observed
    , "observations" .= observed
    , "totalTokens" .= sum [totalTokens u | o <- observed, Just u <- [nativeUsage o]]
    , "totalDollarCost" .= sum [c | o <- observed, Just c <- [dollarCost o]]
    , "perCallDollarCost" .= [dollarCost o | o <- observed]
    ]

main :: IO ()
main = do
    args <- getArgs
    (root, provider, cwd, destination) <- case args of
        [r, p, c, d] -> pure (r, Text.pack p, c, d)
        _ -> fail "Usage: TieredLive ROOT PROVIDER CWD DESTINATION"
    createDirectoryIfMissing True destination
    result <- withCodex cwd provider \client -> do
        let catalog_ = catalogFacts client
            directSchema = fmap (const Unknown) (Decode.expected @DirectReply)

        before1 <- length <$> observations client
        started1 <- getMonotonicTimeNSec
        _ <- timeout 120000000 (prompt client (HostPrompt "gpt-6-luna" "low" (jsonText (object
            [ "role" .= ("execution" :: Text), "goal" .= field "goal" task, "acceptanceCriteria" .= field "acceptanceCriteria" task
            , "sources" .= field "sourcePackets" task, "requiredSourceIds" .= field "requiredSourceIds" task ]))) directSchema)
            >>= maybe (fail "baseline call timed out") (const (pure ()))
        ended1 <- getMonotonicTimeNSec
        baselineObservations <- drop before1 <$> observations client

        before2 <- length <$> observations client
        started2 <- getMonotonicTimeNSec
        _ <- timeout 120000000 (runProgram client (Path (root </> "TieredWorkflow.ffg") AsCode)
            (object ["configuration" .= configurationWith catalog_, "task" .= task]))
            >>= maybe (fail "tiered call timed out") (const (pure ()))
        ended2 <- getMonotonicTimeNSec
        tieredObservations <- drop before2 <$> observations client

        pure (object
            [ "status" .= ("Live single-order paired cost measurement; one trivial task, no retries; not a policy-qualifying evaluation" :: Text)
            , "provider" .= provider
            , "baseline" .= summarize "manual-direct-XS" baselineObservations (ended1 - started1)
            , "tiered" .= summarize "tiered-workflow" tieredObservations (ended2 - started2)
            ])
    BL.writeFile (destination </> "counters.json") (encode result)
    Text.IO.writeFile (destination </> "report.ffg") ("show (read " <> jsonText (String (jsonText result)) <> " : JSON)\n")
    putStrLn ("Saved live tiered cost measurement to " <> destination)
