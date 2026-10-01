{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Control.Exception
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.IO as Text.IO
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import Data.Void (absurd)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, ToGraceType)
import qualified Grace.Decode as Decode
import qualified Grace.Infer as Infer
import qualified Grace.Context as Context
import qualified Grace.Value as GraceValue
import qualified Grace.Interpret as Interpret
import qualified Grace.Monad as Grace
import qualified Grace.Prompt as GracePrompt
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import qualified Control.Monad.State as State
import NativeCodex
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Timeout (timeout)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit
import System.Environment (withArgs)

data Profile = Profile { model :: Text, effort :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType, ToJSON)

targetRows :: [(Text,Text,Bool)]
targetRows = [("alpha","blue",True),("disabled","must-not-appear",False),("zulu","green",True)]
expected :: Value
expected = toJSON [object ["key" .= key,"value" .= value] | (key,value,True) <- targetRows]
fixture :: Value -> Bool -> Value
fixture catalog_ selected = object ["catalog" .= catalog_,"selected" .= selected,"task" .= object
    ["goal" .= ("Extract enabled entries only from packet registry-active. Execution reply must be a JSON array of objects with exactly key and value, ordered by key. Preserve all supplied facts; no tools or files." :: Text)
    ,"acceptanceCriteria" .= (["No disabled or invented entries; exact values and key order; no commentary inside execution reply."] :: [Text])
    ,"negativeEvidence" .= (["The disabled registry entry must never appear in execution output."] :: [Text])
    ,"sourcePackets" .= (object ["id" .= ("registry-active" :: Text),"content" .= jsonText (toJSON
        [object ["key" .= key,"value" .= value,"enabled" .= enabled] | (key,value,enabled) <- targetRows])] :
        [object ["id" .= ("archive-" <> Text.pack (show n)),"content" .= Text.replicate 60
            "Archived unrelated registry: inactive entries are historical only and do not define the active registry. " ] | n <- [1..5::Int]])]]
jsonText :: Value -> Text
jsonText = Encoding.decodeUtf8 . BL.toStrict . encode
oracle :: Text -> Bool
oracle answer = case eitherDecodeStrict (Encoding.encodeUtf8 answer) of Right value -> value == expected; Left _ -> False

evalWorkflow :: FilePath -> (HostPrompt -> Type Location -> IO Value) -> Value -> IO Value
evalWorkflow root handler json = do
    let input = Path (root </> "experiments/Benchmark.ffg") AsCode
    (_, value) <- Grace.evalGrace input Grace.Status{Grace.count=0,Grace.context=[]}
        (Grace.withPrompt handler do
            let binding = fmap (const Unknown) (Infer.inferJSON json)
            (type_,_) <- Infer.infer (fmap absurd (GraceValue.quote binding))
            status <- State.get
            Interpret.interpretWith [("input",Context.complete (Grace.context status) type_,binding)] Nothing)
    either throwIO pure (Decode.decode value)

fake :: IORef [Value] -> HostPrompt -> Type Location -> IO Value
fake captured HostPrompt{text} _ = do
    payload <- either fail pure (eitherDecodeStrict (Encoding.encodeUtf8 text))
    modifyIORef' captured (<> [payload])
    pure if field "role" payload == Just (String "planning")
        then object ["reply" .= ("Extract active entries." :: Text),"selectedSourceIds" .= ["registry-active" :: Text]]
        else if field "role" payload == Just (String "verification")
        then object ["proposedProfile" .= ("XS" :: Text),"rationale" .= ("Candidate only." :: Text)]
        else object ["reply" .= jsonText expected]

tests :: FilePath -> IO ()
tests root = do
    profile <- loadProfile root
    let catalog_ = toJSON [object ["model" .= profile.model,"modelVersion" .= Null,"efforts" .= [profile.effort]]]
    withArgs [] (defaultMain (testGroup "Bounded paired experiment"
        [testCase "independent oracle rejects missing, reordered, disabled, invented and extra-field output" do
            assertBool "correct output" (oracle (jsonText expected))
            let rows = case expected of Array xs -> Vector.toList xs; _ -> []
            mapM_ (\bad -> assertBool "false positive oracle" (not (oracle (jsonText bad))))
                [toJSON ([] :: [Value]),toJSON (reverse rows),toJSON (rows <> [object ["key" .= ("disabled" :: Text),"value" .= ("must-not-appear" :: Text)]])
                ,toJSON (rows <> [object ["key" .= ("invented" :: Text),"value" .= ("guess" :: Text)]])
                ,toJSON [object ["key" .= ("alpha" :: Text),"value" .= ("blue" :: Text),"extra" .= True],last rows]]
        ,testCase "budget blocks call 17, threshold, and unknown accounting before another turn" do
            let observed amount = Observation "fixture" "low" Nothing Nothing (Just (Usage amount 0 0 0 amount)) Nothing
                rejected count observations_ = do
                    outcome <- try (guardBudget count observations_) :: IO (Either SomeException ())
                    case outcome of Left _ -> pure (); Right _ -> assertFailure "Budget admitted another turn"
            guardBudget 15 [observed 499999]
            rejected 16 []
            rejected 15 [observed 500000]
            rejected 1 [Observation "fixture" "low" Nothing Nothing Nothing Nothing]
        ,testCase "required-source floor never prunes the one-call context" do
            a <- newIORef []; b <- newIORef []
            resultA <- evalWorkflow root (fake a) (fixture catalog_ False)
            resultB <- evalWorkflow root (fake b) (fixture catalog_ True)
            left <- readIORef a; right <- readIORef b
            length left @?= 1; length right @?= 1
            let withoutRequired (Object xs) = Object (KM.delete "requiredSourceIds" xs); withoutRequired x = x
                sources x = case field "sources" x of Just (Array xs) -> Vector.toList xs; _ -> []
            map withoutRequired left @?= map withoutRequired right
            map (map (field "id") . sources) left @?= [map (Just . String) ["registry-active","archive-1","archive-2","archive-3","archive-4","archive-5"]]
            map (field "role") left @?= [Just (String "execution")]
            let executionReply result = case field "stages" result of
                    Just (Array stages) -> case Vector.toList stages of [stage] -> field "reply" stage; _ -> Nothing
                    _ -> Nothing
            executionReply resultA @?= Just (String (jsonText expected))
            executionReply resultB @?= Just (String (jsonText expected))
        ]))


data BenchmarkError = LimitReached | UnknownAccounting | PriorAttemptFailed | ExperimentDeadline | WrongBenchmarkProfile | ProfileUnavailable
    deriving stock (Show)
instance Exception BenchmarkError

guardBudget :: Int -> [Observation] -> IO ()
guardBudget count observed = do
    when (count >= 16) (throwIO LimitReached)
    when (any ((== Nothing) . nativeUsage) observed) (throwIO UnknownAccounting)
    when (any ((/= Nothing) . failure) observed) (throwIO PriorAttemptFailed)
    when (sum [totalTokens usage_ | observation <- observed, Just usage_ <- [nativeUsage observation]] >= 500000)
        (throwIO LimitReached)

data Trial = Trial
    { arm :: Text, elapsedSeconds :: Double, objectivePassed :: Bool
    , completed :: Bool, errorClass :: Maybe Text, output :: Maybe Value
    , attempts :: [Observation], requests :: [Value] }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

workflowTokens :: Trial -> Maybe Integer
workflowTokens trial = sum <$> traverse (fmap totalTokens . nativeUsage) (attempts trial)
experimentError :: SomeException -> Text
experimentError err = case fromException err :: Maybe NativeError of
    Just native -> Text.pack (displayException native)
    Nothing -> case fromException err :: Maybe BenchmarkError of
        Just limit -> Text.pack (show limit)
        Nothing -> "GraceBenchmarkError"
loadProfile :: FilePath -> IO Profile
loadProfile root = do
    (_,value) <- Grace.evalGrace (Path (root </> "experiments/Profile.ffg") AsCode) Grace.Status{Grace.count=0,Grace.context=[]}
        (Grace.withPrompt (\(_ :: HostPrompt) _ -> throwIO WrongBenchmarkProfile) (Interpret.interpretWith [] Nothing))
    either throwIO pure (Decode.decode value)

-- Use the declared transport result type, rather than an unsolved inferred type.
data DirectReply = DirectReply { reply :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)

directSchema :: Type Location
directSchema = fmap (const Unknown) (Decode.expected @DirectReply)

-- One direct native call versus the one-call Grace workflow; historical counters stay unchanged.
-- This is a rejection screen, not a calibration or production activation gate.
runQuick :: FilePath -> Text -> FilePath -> FilePath -> IO ()
runQuick root provider cwd destination = do
    createDirectoryIfMissing True destination
    profile <- loadProfile root
    result <- withCodex cwd provider \client -> do
        let catalog_ = catalogFacts client
            task = fixture catalog_ True
            directPayload = object
                [ "role" .= ("execution" :: Text), "task" .= field "task" task
                , "instruction" .= ("Perform the task directly. Return an object with reply containing the requested JSON array as text. No tools or files." :: Text) ]
        let schema = directSchema
        let measure name action = do
                before <- length <$> observations client
                started <- getMonotonicTimeNSec
                output_ <- try (timeout 120000000 action >>= maybe (throwIO ExperimentDeadline) pure)
                ended <- getMonotonicTimeNSec
                observed <- drop before <$> observations client
                let answer_ = either (const Nothing) Just (output_ :: Either SomeException Value)
                    replies = if name == ("manual-direct" :: Text)
                        then [reply_ | Just (String reply_) <- [answer_ >>= field "reply"]]
                        else case answer_ >>= field "stages" of
                            Just (Array stages) -> [reply_ | stage <- Vector.toList stages, field "role" stage == Just (String "execution"), Just (String reply_) <- [field "reply" stage]]
                            _ -> []
                    correct = case replies of [reply_] -> oracle reply_; _ -> False
                    complete = either (const False) (const True) output_
                        && length observed == 1
                        && all (\o -> nativeUsage o /= Nothing && failure o == Nothing) observed
                pure (Trial name (fromIntegral (ended-started)/1000000000) correct complete
                    (either (Just . experimentError) (const Nothing) output_) answer_ observed [])
        direct <- measure "manual-direct" (prompt client (HostPrompt profile.model profile.effort (jsonText directPayload)) schema)
        let handler request@HostPrompt{model=chosenModel,effort=chosenEffort} resultType = do
                observed <- observations client
                guardBudget (length observed) observed
                when (length observed >= 5) (throwIO LimitReached)
                when (sum [totalTokens usage_ | o <- observed, Just usage_ <- [nativeUsage o]] >= 120000) (throwIO LimitReached)
                unless (chosenModel == profile.model && chosenEffort == profile.effort) (throwIO WrongBenchmarkProfile)
                prompt client request resultType
        routed <- if completed direct && objectivePassed direct
            then Just <$> measure "grace-one-call" (evalWorkflow root handler task)
            else pure Nothing
        let useful trial = completed trial && objectivePassed trial
                && elapsedSeconds trial < elapsedSeconds direct
                && case (workflowTokens direct,workflowTokens trial) of (Just a,Just b) -> b < a; _ -> False
        pure (object
            [ "status" .= ("Single paired rejection screen; no adoption or general quality claim" :: Text)
            , "model" .= profile.model, "effort" .= profile.effort, "modelVersion" .= Null
            , "order" .= (["manual-direct", "grace-one-call"] :: [Text])
            , "sameObjective" .= True, "manualIncludesAllRawPackets" .= True
            , "attemptLimit" .= (5 :: Int), "tokenStopThreshold" .= (120000 :: Int)
            , "preparationAndReviewCost" .= Null, "dollarCost" .= Null
            , "adoptionAllowed" .= False, "usefulSavingsCriteriaPassed" .= maybe False useful routed
            , "manual" .= direct, "grace" .= routed
            , "manualTokens" .= workflowTokens direct, "graceTokens" .= (routed >>= workflowTokens)
            , "limitations" .= ("One synthetic extraction task, one order; no Haskell capability calibration. Cache differences and preparation/review cost prevent a total-cost claim. New threads use the same host/model/effort. No retries." :: Text) ])
    BL.writeFile (destination </> "counters.json") (encode result)
    Text.IO.writeFile (destination </> "report.ffg") ("show (read " <> jsonText (String (jsonText result)) <> " : JSON)\n")
    putStrLn ("Saved quick comparison to " <> destination)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--test",root] -> tests root
        ["--run",_,_,_,_] -> fail "HistoricalFourStageExperimentUnavailable: the current one-call workflow retains all packets; old AB/BA arms no longer differ. Historical counters remain unchanged."
        ["--quick",root,provider,cwd,destination] -> runQuick root (Text.pack provider) cwd destination
        ["--schema-check"] -> either (const (fail "UnsupportedDirectOutputSchema")) (const (putStrLn "DirectOutputSchemaSupported")) (GracePrompt.toJSONSchema directSchema)
        _ -> fail "Usage: benchmark --test ROOT | --quick ROOT EXACT_PROVIDER NEUTRAL_CWD OUTPUT_DIR (--run retired)"
