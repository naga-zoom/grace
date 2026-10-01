{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import MCPAdapter
import NativeCodex
import Network.MCP.Server
import qualified Network.MCP.Transport.Types as Wire
import System.Environment (getArgs, withArgs)
import System.Directory
import System.FilePath ((</>))
import System.IO (openTempFile, hClose)
import Test.Tasty
import Test.Tasty.HUnit
import WorkflowHost

task :: Value
task = object ["goal" .= ("Fixture task" :: Text), "acceptanceCriteria" .= ["Preserve facts" :: Text]
    , "negativeEvidence" .= ["Prior failure" :: Text], "sourcePackets" .= [object ["id" .= ("packet" :: Text), "content" .= ("Source fact" :: Text)]]
    , "requiredSourceIds" .= ["packet" :: Text]]

-- Deterministic native app-server: protocol only, no routing policy here.
fakeClient :: Bool -> IO (Client, IORef [Value])
fakeClient = fakeCatalog "fixture-expert"

fakeCatalog :: Text -> Bool -> IO (Client, IORef [Value])
fakeCatalog catalogModel malformed = do
    queue <- newIORef []
    sent <- newIORef []
    index <- newIORef (0 :: Int)
    let push = modifyIORef' queue . (<>)
        reply identity result = object ["id" .= identity, "result" .= result]
        wire = Transport {send = \request -> do
            modifyIORef' sent (<> [request])
            let identity = maybe Null id (field "id" request)
                params = maybe Null id (field "params" request)
            case field "method" request of
                Just (String "initialize") -> push [reply identity (object [])]
                Just (String "model/list") -> push [reply identity (object ["data" .=
                    [object ["model" .= catalogModel, "supportedReasoningEfforts" .=
                        [object ["reasoningEffort" .= ("high" :: Text)]]]], "nextCursor" .= Null])]
                Just (String "thread/start") -> do
                    n <- atomicModifyIORef' index (\x -> (x+1,x+1))
                    push [reply identity (object ["thread" .= object ["id" .= show n], "model" .= ("fixture-expert" :: Text)
                        , "modelProvider" .= ("exact-provider" :: Text), "approvalPolicy" .= ("never" :: Text)
                        , "sandbox" .= object ["type" .= ("readOnly" :: Text)]])]
                Just (String "turn/start") -> do
                    n <- readIORef index
                    let thread = maybe Null id (field "threadId" params)
                        turn = String ("turn-" <> Text.pack (show n))
                        notification method p = object ["method" .= (method :: Text), "params" .= p]
                        base extra = object (["threadId" .= thread, "turnId" .= turn] <> extra)
                        output = if n == 1 then object ["reply" .= ("planning reply" :: Text), "selectedSourceIds" .= ["packet" :: Text]]
                            else if n == 4 then object ["proposedProfile" .= ("S" :: Text), "rationale" .= ("Candidate only" :: Text)]
                            else object ["reply" .= ("stage reply" :: Text)]
                        goodCounters = object ["inputTokens" .= (20 :: Int), "cachedInputTokens" .= (5 :: Int), "outputTokens" .= (2 :: Int)
                            , "reasoningOutputTokens" .= (0 :: Int), "totalTokens" .= (22 :: Int)]
                        usageEvent counters = notification "thread/tokenUsage/updated" (base ["tokenUsage" .= object ["total" .= counters]])
                        broken = malformed && n == 1
                        terminal = notification "turn/completed" (object ["threadId" .= thread, "turn" .= object
                            ["id" .= turn, "status" .= (if broken then "failed" else "completed" :: Text), "items" .= ([] :: [Value])]])
                    push ([reply identity (object ["turn" .= object ["id" .= turn]])
                        , notification "item/completed" (base ["item" .= object ["id" .= ("answer" :: Text), "type" .= ("agentMessage" :: Text)
                            , "phase" .= ("final_answer" :: Text), "text" .= Encoding.decodeUtf8 (BL.toStrict (encode output))]])
                        , usageEvent goodCounters] <> [usageEvent (object ["inputTokens" .= (99 :: Int)]) | broken] <> [terminal])
                _ -> pure ()
            , receive = do
                value <- atomicModifyIORef' queue (\xs -> case xs of [] -> ([],Nothing); x:rest -> (rest,Just x))
                maybe (throwIO (ProtocolError "Fixture exhausted")) pure value}
    client <- initialize "/tmp/native-mcp-fixture" "exact-provider" wire
    pure (client,sent)

call :: Server -> Text -> Value -> IO Value
call server method params = do
    let request = Wire.Request (Wire.JSONRPC "2.0") (String "request") method (Just params)
    result <- handleMessage server Nothing (Wire.RequestMessage request)
    case result of
        Just (Wire.ResponseMessage response) -> case response.responseResult of
            Just value -> pure value
            _ -> assertFailure (show response) >> pure Null
        _ -> assertFailure "No SDK response" >> pure Null

main :: IO ()
main = do
    args <- getArgs
    root <- case args of [path] -> pure path; _ -> fail "Usage: MCPTests ROOT"
    bracket (do
        temp <- getTemporaryDirectory
        (bundle, tempHandle) <- openTempFile temp "grace-bundled-settings-"
        hClose tempHandle
        removeFile bundle
        createDirectory bundle
        mapM_ (\name -> copyFile (root </> name) (bundle </> name)) ["Entry.ffg", "workflow.ffg", "policy.ffg"]
        copyFile (root </> "fixtures/Settings.ffg") (bundle </> "Settings.ffg")
        pure bundle) removePathForcibly $ \bundle ->
      withArgs [] (defaultMain (testGroup "Grace MCP boundary"
        [ testCase "SDK discovery and report reading perform no execution" do
            invoked <- newIORef (0 :: Int)
            server <- createMCPServer (\_ -> modifyIORef' invoked (+1) >> pure (Right Null)) (pure "{\"status\":\"unadopted\"}")
            initialized <- call server "initialize" (object ["protocolVersion" .= ("2025-06-18" :: Text)
                , "capabilities" .= object [], "clientInfo" .= object ["name" .= ("fixture" :: Text), "version" .= ("1" :: Text)]])
            case field "instructions" initialized of
                Just (String instructions) -> assertBool "Missing initialization guidance" (not (Text.null instructions))
                _ -> assertFailure "Missing initialization guidance"
            tools <- call server "tools/list" (object [])
            case field "tools" tools of
                Just (Array entries) -> do
                    Vector.length entries @?= 1
                    field "name" (Vector.head entries) @?= Just (String "run_workflow")
                    (field "annotations" (Vector.head entries) >>= field "openWorldHint") @?= Just (Bool True)
                    (field "annotations" (Vector.head entries) >>= field "idempotentHint") @?= Just (Bool False)
                _ -> assertFailure "Missing tool discovery"
            resources <- call server "resources/list" (object [])
            case field "resources" resources of Just (Array entries) -> Vector.length entries @?= 1; _ -> assertFailure "Missing report resource"
            body <- call server "resources/read" (object ["uri" .= ("grace-router://report" :: Text)])
            case field "contents" body of
                Just (Array entries) -> field "text" (Vector.head entries) @?= Just (String "{\"status\":\"unadopted\"}")
                _ -> assertFailure "Missing report body"
            readIORef invoked >>= (@?= 0)
        , testCase "CLI and MCP share typed null-version workflow output and native usage" do
            (clientA,_) <- fakeClient False
            direct <- runWorkflow bundle (\action -> action clientA) task
            (clientB,sent) <- fakeClient False
            server <- createMCPServer (runWorkflow bundle (\action -> action clientB)) (pure "{}")
            result <- call server "tools/call" (object ["name" .= ("run_workflow" :: Text), "arguments" .= object ["task" .= task]])
            field "isError" result @?= Just (Bool False)
            field "structuredContent" result @?= either Just Just direct
            let envelope = maybe Null id (field "structuredContent" result)
            let workflow = maybe Null id (field "result" envelope)
            field "status" workflow @?= Just (String "candidate")
            field "adoptionAllowed" workflow @?= Just (Bool False)
            case field "stages" workflow of
                Just (Array stages) -> do
                    Vector.length stages @?= 1
                    map (field "modelVersion") (Vector.toList stages) @?= replicate 1 (Just Null)
                    map (field "size") (Vector.toList stages) @?= replicate 1 (Just (String "L"))
                _ -> assertFailure "Typed baseline stages missing"
            requests <- readIORef sent
            length [r | r <- requests, field "method" r == Just (String "turn/start")] @?= 1
        , testCase "bundled mapping refuses actual catalog mismatch without a turn" do
            (client,sent) <- fakeCatalog "fixture-other" False
            result <- runWorkflow bundle (\action -> action client) task
            case result of
                Left _ -> assertFailure "Expected Grace policy refusal, not a transport error"
                Right envelope -> do
                    (field "result" envelope >>= field "status") @?= Just (String "refused")
                    field "nativeAttempts" envelope @?= Just (toJSON ([] :: [Value]))
            requests <- readIORef sent
            length [r | r <- requests, field "method" r == Just (String "turn/start")] @?= 0
        , testCase "MCP execution failure retains its one attempt and unknown final usage" do
            (client,_) <- fakeClient True
            server <- createMCPServer (runWorkflow bundle (\action -> action client)) (pure "{}")
            result <- call server "tools/call" (object ["name" .= ("run_workflow" :: Text), "arguments" .= object ["task" .= task]])
            field "isError" result @?= Just (Bool True)
            let envelope = maybe Null id (field "structuredContent" result)
            field "error" envelope @?= Just (String "NativeProtocolError")
            case field "nativeAttempts" envelope of
                Just (Array attempts) -> do
                    Vector.length attempts @?= 1
                    field "nativeUsage" (Vector.last attempts) @?= Just Null
                _ -> assertFailure "Lost failure attempts"
        , testCase "client teardown failure preserves completed attempt accounting" do
            (client,_) <- fakeClient False
            result <- runWorkflow bundle (\action -> action client >> throwIO (userError "secret teardown diagnostic")) task
            case result of
                Right _ -> assertFailure "Teardown failure was hidden"
                Left envelope -> do
                    field "error" envelope @?= Just (String "GraceInterpretationError")
                    case field "nativeAttempts" envelope of
                        Just (Array attempts) -> Vector.length attempts @?= 1
                        _ -> assertFailure "Teardown lost measured usage"
        , testCase "invalid task data and authority overrides are rejected before native initialization" do
            opened <- newIORef (0 :: Int)
            let backend action = modifyIORef' opened (+1) >> fakeClient False >>= \(client,_) -> action client
            server <- createMCPServer (runWorkflow bundle backend) (pure "{}")
            mapM_ (\arguments -> do
                result <- call server "tools/call" (object ["name" .= ("run_workflow" :: Text), "arguments" .= arguments])
                field "isError" result @?= Just (Bool True))
                [Null, object [], object ["task" .= task, "configuration" .= object []], object ["task" .= task, "scope" .= object []], object ["task" .= task, "catalog" .= ([] :: [Value])], object ["task" .= object ["goal" .= (7 :: Int)]], object ["task" .= case task of Object fields -> Object (KM.insert "observations" (toJSON ([] :: [Value])) fields); _ -> Null]]
            readIORef opened >>= (@?= 0)
        ]))
