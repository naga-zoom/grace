{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where
import Control.Exception
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
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
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Type (Type)
import qualified Control.Monad.State as State
import NativeCodex
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)
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
    ["goal" .= ("Extract enabled entries only from packet registry-active. Execution reply must be a JSON array of objects with exactly key and value, ordered by key. Planning selects only relevant source IDs; other roles preserve this task. Verification checks actual execution; no tools or files." :: Text)
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
        ,testCase "planning identical; only downstream raw context differs" do
            a <- newIORef []; b <- newIORef []
            _ <- evalWorkflow root (fake a) (fixture catalog_ False)
            _ <- evalWorkflow root (fake b) (fixture catalog_ True)
            left <- readIORef a; right <- readIORef b
            length left @?= 4; length right @?= 4
            take 1 left @?= take 1 right
            let withoutSources (Object xs) = Object (KM.delete "sources" xs); withoutSources x = x
                sources x = case field "sources" x of Just (Array xs) -> Vector.toList xs; _ -> []
            map withoutSources left @?= map withoutSources right
            map (length . sources) (drop 1 left) @?= [6,6,6]
            map (length . sources) (drop 1 right) @?= [1,1,1]
            map (map (field "id") . sources) (drop 1 right) @?= replicate 3 [Just (String "registry-active")]
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
payloadBytes :: Trial -> Int
payloadBytes trial = sum [round number | request <- requests trial, Just (Number number) <- [field "payloadBytes" request]]
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

runBenchmark :: FilePath -> Text -> FilePath -> FilePath -> IO ()
runBenchmark root provider cwd destination = do
    createDirectoryIfMissing True destination
    profile <- loadProfile root
    calls <- newIORef (0 :: Int)
    trialLog <- newIORef []
    requestLog <- newIORef []
    clientRef <- newIORef Nothing
    catalogRef <- newIORef Null
    start <- getMonotonicTimeNSec
    hostBuild <- Text.strip . Text.pack <$> readProcess "codex" ["--version"] ""
    outcome <- try (withCodex cwd provider \client -> do
        writeIORef clientRef (Just client)
        let actualCatalog = catalogFacts client
            supports entry = field "model" entry == Just (String (profile.model))
                && case field "efforts" entry of Just (Array efforts) -> String (profile.effort) `elem` Vector.toList efforts; _ -> False
        writeIORef catalogRef actualCatalog
        unless (case actualCatalog of Array entries -> any supports (Vector.toList entries); _ -> False) (throwIO ProfileUnavailable)
        putStrLn "Verified exact native benchmark model and effort; no fallback."
        let handler request@HostPrompt{model=chosenModel,effort=chosenEffort,text} schema = do
                unless (chosenModel == profile.model && chosenEffort == profile.effort) (throwIO WrongBenchmarkProfile)
                now <- getMonotonicTimeNSec
                when (now - start >= 2400 * 1000000000) (throwIO ExperimentDeadline)
                count <- readIORef calls
                observed <- observations client
                guardBudget count observed
                payload <- either (const (throwIO WrongBenchmarkProfile)) pure (eitherDecodeStrict (Encoding.encodeUtf8 text) :: Either String Value)
                writeIORef calls (count+1)
                modifyIORef' requestLog (<> [object ["index" .= (count+1), "role" .= field "role" payload
                    ,"payloadBytes" .= BS.length (Encoding.encodeUtf8 text)
                    ,"sourceIds" .= case field "sources" payload of
                        Just (Array sources) -> toJSON [field "id" source | source <- Vector.toList sources]
                        _ -> Null]])
                answer_ <- prompt client request schema
                current <- observations client
                unless (all ((/= Nothing) . nativeUsage) current) (throwIO UnknownAccounting)
                putStrLn ("Native attempt " <> show (count+1) <> " complete; cumulative total tokens "
                    <> show (sum [totalTokens usage_ | observation <- current, Just usage_ <- [nativeUsage observation]]))
                pure answer_
            runArms [] = pure ()
            runArms (selected:rest) = do
                beforeAttempts <- length <$> observations client
                beforeRequests <- length <$> readIORef requestLog
                begun <- getMonotonicTimeNSec
                result <- try do
                    timed <- timeout 510000000 (evalWorkflow root handler (fixture actualCatalog selected))
                    maybe (throwIO ExperimentDeadline) pure timed
                ended <- getMonotonicTimeNSec
                observed <- drop beforeAttempts <$> observations client
                sent <- drop beforeRequests <$> readIORef requestLog
                let output_ = either (const Nothing) Just (result :: Either SomeException Value)
                    correct = case output_ >>= field "stages" of
                        Just (Array stages) -> case [reply_ | stage <- Vector.toList stages, field "role" stage == Just (String "execution"), Just (String reply_) <- [field "reply" stage]] of
                            [reply_] -> oracle reply_
                            _ -> False
                        _ -> False
                    finished = either (const False) (const True) result && length observed == 4
                        && all (\observation -> nativeUsage observation /= Nothing && failure observation == Nothing) observed
                    trial = Trial (if selected then "B-selected" else "A-full")
                        (fromIntegral (ended-begun)/1000000000) correct finished
                        (either (Just . experimentError) (const Nothing) result) output_ observed sent
                modifyIORef' trialLog (<> [trial])
                putStrLn (Text.unpack (arm trial) <> ": objective=" <> show correct <> ", complete=" <> show finished)
                when (finished && correct) (runArms rest)
        runArms [False,True,True,False]) :: IO (Either SomeException ())
    ended <- getMonotonicTimeNSec
    trials <- readIORef trialLog
    client <- readIORef clientRef
    recorded <- maybe (pure []) observations client
    catalog_ <- readIORef catalogRef
    count <- readIORef calls
    let matchedPairs = case trials of [a,b,b2,a2] -> [(a,b),(a2,b2)]; _ -> []
        pairPass (a,b) = completed a && completed b && objectivePassed a && objectivePassed b
            && payloadBytes b < payloadBytes a && elapsedSeconds b <= elapsedSeconds a
            && case (workflowTokens a,workflowTokens b) of (Just x,Just y) -> y*10 <= x*9; _ -> False
        report = object
            ["status" .= ("bounded experimental measurement; unadopted" :: Text),"adoptionAllowed" .= False
            ,"model" .= profile.model,"effort" .= profile.effort,"modelVersion" .= Null
            ,"nativeHostBuild" .= hostBuild,"catalog" .= catalog_,"provider" .= provider
            ,"cwdKind" .= ("fresh neutral temporary directory" :: Text)
            ,"nativeInputIncludesHarnessAndPayload" .= True,"commonHarnessTokensSeparatelyIdentified" .= False
            ,"dollarPricing" .= Null,"humanReviewCost" .= Null
            ,"attemptLimit" .= (16 :: Int),"tokenStopThreshold" .= (500000 :: Int)
            ,"requestedPromptCount" .= count,"nativeTurnIdsObserved" .= length [observation | observation <- recorded, nativeTurnId observation /= Nothing],"nativeAttempts" .= recorded
            ,"experimentWallSeconds" .= (fromIntegral (ended-start)/1000000000 :: Double)
            ,"runnerFailure" .= either (Just . experimentError) (const (Nothing :: Maybe Text)) outcome
            ,"allFourObjectivePassed" .= (length trials == 4 && all objectivePassed trials && all completed trials)
            ,"usefulSavingsCriteriaPassed" .= (length matchedPairs == 2 && all pairPass matchedPairs)
            ,"pairedTotals" .= [object ["A_tokens" .= workflowTokens a,"B_tokens" .= workflowTokens b
                ,"A_seconds" .= elapsedSeconds a,"B_seconds" .= elapsedSeconds b
                ,"A_payloadBytes" .= payloadBytes a,"B_payloadBytes" .= payloadBytes b,"passed" .= pairPass (a,b)] | (a,b) <- matchedPairs]
            ,"trials" .= trials]
    BL.writeFile (destination </> "counters.json") (encode report)
    Text.IO.writeFile (destination </> "report.ffg") ("show (read " <> jsonText (String (jsonText report)) <> " : JSON)\n")
    putStrLn ("Saved bounded counters and Grace report to " <> destination)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--test",root] -> tests root
        ["--run",root,provider,cwd,destination] -> runBenchmark root (Text.pack provider) cwd destination
        _ -> fail "Usage: benchmark --test ROOT | --run ROOT EXACT_PROVIDER NEUTRAL_CWD OUTPUT_DIR"
