{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
module Main where
import Control.Exception (SomeException, try, throwIO)
import Data.IORef
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, ToGraceType)
import qualified Grace.Decode as Decode
import Grace.Input (Input(..))
import Grace.Location (Location(..))
import qualified Grace.Monad as Grace
import qualified Grace.Interpret as Interpret
import qualified Grace.Prompt as Prompt
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import Data.Aeson
import NativeCodex
import Test.Tasty
import Test.Tasty.HUnit

counter :: Integer -> Integer -> Integer -> Value
counter input cached output = object
    [ "inputTokens" .= input, "cachedInputTokens" .= cached, "outputTokens" .= output
    , "reasoningOutputTokens" .= (0 :: Int), "totalTokens" .= (input + output) ]

notification :: String -> Value -> Value
notification method params = object ["method" .= method, "params" .= params]
usageEvent :: Value -> Value
usageEvent total = notification "thread/tokenUsage/updated" (object
    [ "threadId" .= ("thread-a" :: String), "turnId" .= ("turn-a" :: String)
    , "tokenUsage" .= object ["last" .= total, "total" .= total] ])
message :: String -> String -> String -> Value
message identity phase text = notification "item/completed" (object
    [ "threadId" .= ("thread-a" :: String), "turnId" .= ("turn-a" :: String)
    , "completedAtMs" .= (1 :: Int)
    , "item" .= object ["id" .= identity, "type" .= ("agentMessage" :: String), "phase" .= phase, "text" .= text] ])
completed :: String -> Value
completed status = notification "turn/completed" (object
    [ "threadId" .= ("thread-a" :: String)
    , "turn" .= object ["id" .= ("turn-a" :: String), "status" .= status, "items" .= ([] :: [Value]), "error" .= Null] ])
delta :: String -> Value
delta text = notification "item/agentMessage/delta" (object
    ["threadId" .= ("thread-a" :: String), "turnId" .= ("turn-a" :: String), "delta" .= text])
run :: [Value] -> Either NativeError Completion
run = collectTurn "thread-a" "turn-a"
good :: Value
good = object ["ok" .= True]
want :: Either NativeError Completion
want = Right (Completion good (Usage 20 5 2 0 22))
final :: Value
final = message "answer" "final_answer" "{\"ok\":true}"
counts :: Value
counts = usageEvent (counter 20 5 2)

main :: IO ()
main = defaultMain (testGroup "Native protocol"
    [ testCase "completed item is authoritative without deltas" $
        run [final, counts, completed "completed"] @?= want
    , testCase "commentary and streamed partial JSON are excluded" $
        run [message "comment" "commentary" "not JSON", delta "broken prefix", final, counts, completed "completed"] @?= want
    , testCase "unrelated thread and turn completion are ignored" $ do
        let wrong = notification "turn/completed" (object
                ["threadId" .= ("other" :: String), "turn" .= object ["id" .= ("other-turn" :: String), "status" .= ("failed" :: String)]])
        run [delta "{\"ok\":true}", wrong, final, counts, completed "completed"] @?= want
    , testCase "another turn's token counters cannot replace this turn" $ do
        let wrong = notification "thread/tokenUsage/updated" (object
                ["threadId" .= ("thread-a" :: String), "turnId" .= ("other-turn" :: String), "tokenUsage" .= object ["total" .= counter 99 0 1]])
        run [delta "{\"ok\":true}", final, counts, wrong, completed "completed"] @?= want
    , testCase "duplicate items and usage snapshots are idempotent" $
        run [final, final, counts, counts, completed "completed"] @?= want
    , testCase "latest cumulative counter replaces interim snapshots" $
        run [final, usageEvent (counter 10 0 1), counts, completed "completed"] @?= want
    , testCase "a missing counter is an explicit error" $
        run [delta "{\"ok\":true}", final, completed "completed"] @?= Left MissingUsage
    , testCase "interrupted turn is not successful output" $
        run [final, counts, completed "interrupted"] @?= Left TurnInterrupted
    , testCase "failed turn remains a native failure" $ case run [final, counts, completed "failed"] of
        Left (TurnFailed _) -> pure ()
        other -> assertFailure (show other)
    , testCase "invalid final JSON is rejected even when deltas look valid" $
        run [delta "{\"ok\":true}", message "answer" "final_answer" "not JSON", counts, completed "completed"] @?= Left (MalformedOutput "not JSON")
    , testCase "two distinct final messages are ambiguous" $
        run [delta "{\"ok\":true}", final, message "second" "final_answer" "{\"other\":true}", counts, completed "completed"] @?= Left AmbiguousOutput
    , testCase "missing authoritative final item is explicit" $
        run [delta "{\"ok\":true}", counts, completed "completed"] @?= Left MissingOutput
    , testCase "negative counters are not accepted" $ case run [delta "{\"ok\":true}", final, usageEvent (counter (-1) 0 2), completed "completed"] of
        Left (ProtocolError _) -> pure ()
        other -> assertFailure (show other)
    , testCase "cached input is a subset rather than additional usage" $
        run [final, counts, completed "completed"] @?= want
    , testCase "RPC result is correlated by its request ID" $ do
        let wrong = object ["id" .= (7 :: Int), "result" .= ("wrong" :: String)]
        let correct = object ["id" .= (8 :: Int), "result" .= ("right" :: String)]
        awaitResult 8 [wrong, correct] @?= Right (String "right", [])
    , testCase "native RPC errors remain typed errors" $ do
        let err = object ["code" .= (-1 :: Int), "message" .= ("refused" :: String)]
        awaitResult 8 [object ["id" .= (8 :: Int), "error" .= err]] @?= Left (RpcError err)
    , testGroup "Concrete typed transport" transportTests
    ])

-- Full interpreter/transport fixtures exercise emitted native requests as well
-- as decoding; no process or provider is contacted here.

data Reply = Reply { reply :: Text }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromGrace, ToGraceType)

schema :: Type.Type Location
schema = fmap (const Unknown) (Decode.expected @Reply)
request :: HostPrompt
request = HostPrompt "catalog-model" "low" "source packet"
resultFrame :: Int -> Value -> Value
resultFrame identity value = object ["id" .= (identity :: Int), "result" .= value]
startup :: [Value]
startup =
    [resultFrame 1 (object []), resultFrame 2 (object
        ["data" .= [object ["model" .= ("catalog-model" :: Text), "supportedReasoningEfforts" .=
            [object ["reasoningEffort" .= ("low" :: Text)]]]], "nextCursor" .= Null])]
threadReply :: Value
threadReply = resultFrame 3 (object ["thread" .= object ["id" .= ("thread-a" :: Text)]
    , "model" .= ("catalog-model" :: Text), "modelProvider" .= ("exact-provider" :: Text)
    , "approvalPolicy" .= ("never" :: Text), "sandbox" .= object ["type" .= ("readOnly" :: Text)]])
turnReply :: Value
turnReply = resultFrame 4 (object ["turn" .= object ["id" .= ("turn-a" :: Text)]])
replyFinal :: Value
replyFinal = message "answer" "final_answer" "{\"reply\":\"typed reply\"}"

fixture :: [Value] -> (Client -> IORef [Value] -> IO a) -> IO a
fixture frames action = do
    remaining <- newIORef (startup <> frames)
    sent <- newIORef []
    let wire = Transport {send = \value -> modifyIORef' sent (<> [value]), receive = do
            next <- atomicModifyIORef' remaining (\values -> case values of
                [] -> ([], Nothing); value : rest -> (rest, Just value))
            maybe (throwIO (ProtocolError "Fixture exhausted")) pure next}
    client <- initialize "/tmp/grace-native-no-provider" "exact-provider" wire
    value <- action client sent
    pure value

transportTests :: [TestTree]
transportTests =
    [ testCase "typed injected input and unselected branch produce zero native requests" $
        fixture [] $ \client sent -> do
            value <- runProgram client (Code "input fixture"
                "if input.enabled then ((prompt {model: \"catalog-model\", effort: \"low\", text: input.note}) : {reply: Text}) else {reply: input.note}")
                (object ["enabled" .= False, "note" .= ("typed source" :: Text)])
            value @?= object ["reply" .= ("typed source" :: Text)]
            requests <- readIORef sent
            length requests @?= 3
            logged <- observations client
            logged @?= []
    , testCase "unselected branch still rejects a wrong prompt argument before transport" $
        fixture [] $ \client sent -> do
            result <- try (runProgram client (Code "input bad argument"
                "if input.enabled then ((prompt {model: \"catalog-model\", effort: \"low\", text: 7}) : {reply: Text}) else {reply: input.note}")
                (object ["enabled" .= False, "note" .= ("typed source" :: Text)])) :: IO (Either SomeException Value)
            case result of Left _ -> pure (); Right _ -> assertFailure "Unchecked branch argument"
            requests <- readIORef sent
            length requests @?= 3
            logged <- observations client
            logged @?= []
    , testCase "completed events before turn/start response are preserved" $
        fixture [threadReply, replyFinal, counts, completed "completed", turnReply] $ \client _ -> do
            value <- NativeCodex.prompt client request schema
            value @?= object ["reply" .= ("typed reply" :: Text)]
            logged <- observations client
            map nativeUsage logged @?= [Just (Usage 20 5 2 0 22)]
    , testCase "exact catalog rejects unavailable effort before a thread is started" $
        fixture [] $ \client sent -> do
            result <- try (NativeCodex.prompt client (HostPrompt "catalog-model" "high" "source") schema)
                :: IO (Either NativeError Value)
            result @?= Left (UnsupportedProfile "catalog-model" "high")
            requests <- readIORef sent
            length requests @?= 3
            logged <- observations client
            map failure logged @?= [Just "NativeUnsupportedProfile"]
            map nativeUsage logged @?= [Nothing]
    , testCase "thread policy and real Grace output schema are sent explicitly" $
        fixture [threadReply, turnReply, replyFinal, counts, completed "completed"] $ \client sent -> do
            _ <- NativeCodex.prompt client request schema
            requests <- readIORef sent
            threadParams <- paramsFor "thread/start" requests
            field "ephemeral" threadParams @?= Just (Bool True)
            field "sandbox" threadParams @?= Just (String "read-only")
            field "approvalPolicy" threadParams @?= Just (String "never")
            field "modelProvider" threadParams @?= Just (String "exact-provider")
            turnParams <- paramsFor "turn/start" requests
            expectedSchema <- either throwIO pure (Prompt.toJSONSchema schema)
            field "outputSchema" turnParams @?= Just expectedSchema
            expectedSchema @?= object ["type" .= ("object" :: Text)
                , "properties" .= object ["reply" .= object ["type" .= ("string" :: Text)]]
                , "required" .= ["reply" :: Text], "additionalProperties" .= False]
    , testCase "Grace rejects valid JSON that violates the checked result type" $
        fixture [threadReply, turnReply, final, counts, completed "completed"] $ \client _ -> do
            let code = "(prompt {model: \"catalog-model\", effort: \"low\", text: \"packet\"}) : {reply: Text}"
            result <- try (Grace.evalGrace (Code "typed fixture" code) Grace.Status{Grace.count=0, Grace.context=[]}
                (Grace.withPrompt (NativeCodex.prompt client) (Interpret.interpretWith [] Nothing)))
                :: IO (Either SomeException (Type.Type Location, Value.Value Location))
            case result of Left _ -> pure (); Right _ -> assertFailure "Invalid native output escaped Grace type checking"
            logged <- observations client
            map nativeUsage logged @?= [Just (Usage 20 5 2 0 22)]
    , testCase "raw native error payloads do not escape failure observations" $
        fixture [threadReply, object ["id" .= (4 :: Int), "error" .= object ["message" .= ("secret provider error" :: Text)]]] $ \client _ -> do
            _ <- try (NativeCodex.prompt client request schema) :: IO (Either SomeException Value)
            logged <- observations client
            map failure logged @?= [Just "NativeRpcError"]
            map nativeUsage logged @?= [Nothing]
    , testCase "malformed final output records its error class and native usage" $
        fixture [threadReply, turnReply, message "answer" "final_answer" "secret model text", counts, completed "completed"] $ \client _ -> do
            result <- try (NativeCodex.prompt client request schema) :: IO (Either NativeError Value)
            result @?= Left (MalformedOutput "secret model text")
            logged <- observations client
            map failure logged @?= [Just "NativeMalformedOutput"]
            map nativeUsage logged @?= [Just (Usage 20 5 2 0 22)]
    , testCase "malformed final counters never fall back to valid interim usage" $
        fixture [threadReply, turnReply, replyFinal, counts, usageEvent (object ["inputTokens" .= (99 :: Int)]), completed "failed"] $ \client _ -> do
            result <- try (NativeCodex.prompt client request schema) :: IO (Either NativeError Value)
            case result of Left (ProtocolError _) -> pure (); other -> assertFailure (show other)
            logged <- observations client
            map failure logged @?= [Just "NativeProtocolError"]
            map nativeUsage logged @?= [Nothing]
    , testCase "native provider substitution is rejected before inference" $
        fixture [resultFrame 3 (object ["model" .= ("catalog-model" :: Text), "modelProvider" .= ("other-provider" :: Text)])] $ \client sent -> do
            result <- try (NativeCodex.prompt client request schema) :: IO (Either NativeError Value)
            case result of Left (ProtocolError _) -> pure (); other -> assertFailure (show other)
            requests <- readIORef sent
            length requests @?= 4
            logged <- observations client
            map nativeUsage logged @?= [Nothing]
    , testGroup "native terminal outcomes"
        [testCase status $ fixture [threadReply, turnReply, replyFinal, counts, completed status] $ \client _ -> do
            result <- try (NativeCodex.prompt client request schema) :: IO (Either NativeError Value)
            case (status,result) of
                ("failed", Left (TurnFailed _)) -> pure ()
                ("interrupted", Left TurnInterrupted) -> pure ()
                _ -> assertFailure (show result)
            logged <- observations client
            map failure logged @?= [Just errorClass]
            map nativeUsage logged @?= [Just (Usage 20 5 2 0 22)]
        | (status,errorClass) <- [("failed","NativeTurnFailed"),("interrupted","NativeTurnInterrupted")]]
    ]

paramsFor :: Text -> [Value] -> IO Value
paramsFor method requests = case [p | r <- requests, field "method" r == Just (String method), Just p <- [field "params" r]] of
    [params] -> pure params
    other -> assertFailure ("Expected one native request: " <> show method <> ", got " <> show (length other)) >> pure Null
