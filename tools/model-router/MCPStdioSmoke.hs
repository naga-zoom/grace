{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless, void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>))
import System.IO
import System.Posix.Signals (sigKILL, sigTERM, signalProcessGroup)
import System.Posix.Types (ProcessID)
import System.Process
import System.Timeout (timeout)

-- Test-only client. The SDK client does not currently newline-frame requests
-- or consume initialize before its next call; the production SDK server stays
-- under test. Each request here is synchronous and checks its response ID.
data SmokeFailure = SmokeFailure String deriving (Show)
instance Exception SmokeFailure

expect :: String -> Bool -> IO ()
expect label condition = unless condition (throwIO (SmokeFailure label))

field :: Key -> Value -> Maybe Value
field key (Object fields) = KM.lookup key fields
field _ _ = Nothing

decodeValue :: BL.ByteString -> IO Value
decodeValue = either (const (throwIO (SmokeFailure "MalformedJSON"))) pure . eitherDecode

data Child = Child Handle Handle Handle ProcessHandle ProcessID ThreadId (MVar (Either SomeException BS.ByteString))

-- Each child and its native descendants get a separate process group. Pipe
-- drains prevent stderr backpressure; cleanup never prints captured contents.
startChild :: FilePath -> [String] -> FilePath -> IO Child
startChild executable arguments neutralCwd = mask $ \restore -> do
    (input, output, errors, process) <- createProcess (proc executable arguments)
        { cwd = Just neutralCwd, std_in = CreatePipe, std_out = CreatePipe
        , std_err = CreatePipe, create_group = True }
    let abortStartup = do
            identity <- getPid process
            maybe (ignoreFailure (terminateProcess process))
                (\pid -> ignoreFailure (signalProcessGroup sigKILL pid)) identity
            void (timeout 2000000 (waitForProcess process))
            mapM_ (mapM_ (ignoreFailure . hClose)) [input, output, errors]
    case (input, output, errors) of
        (Just writer, Just reader, Just diagnostics) -> do
            identity <- (getPid process >>= maybe (throwIO (SmokeFailure "MissingProcessIdentity")) pure)
                `onException` abortStartup
            restore (mapM_ (\pipe -> hSetBinaryMode pipe True) [writer, reader, diagnostics])
                `onException` abortStartup
            captured <- newEmptyMVar
            drain <- forkIO (try (BS.hGetContents diagnostics >>= \bytes -> evaluate (BS.length bytes) >> pure bytes) >>= putMVar captured)
            pure (Child writer reader diagnostics process identity drain captured)
        _ -> do
            abortStartup
            throwIO (SmokeFailure "MissingProcessPipes")

ignoreFailure :: IO () -> IO ()
ignoreFailure action = action `catch` \(_ :: SomeException) -> pure ()

stopChild :: Child -> IO ()
stopChild (Child writer reader diagnostics process identity drain _) = do
    ignoreFailure (hClose writer)
    ignoreFailure (signalProcessGroup sigTERM identity)
    _ <- timeout 2000000 (waitForProcess process)
    -- Also stop descendants that outlived the parent, without an unbounded wait.
    ignoreFailure (signalProcessGroup sigKILL identity)
    _ <- timeout 2000000 (waitForProcess process)
    killThread drain
    mapM_ (ignoreFailure . hClose) [reader, diagnostics]

sendFrame :: Child -> Value -> IO ()
sendFrame (Child writer _ _ _ _ _ _) value = BL.hPut writer (encode value <> "\n") >> hFlush writer

request :: Child -> Int -> Text -> Value -> IO Value
request child@(Child _ reader _ _ _ _ _) identity method params = do
    sendFrame child (object ["jsonrpc" .= ("2.0" :: Text), "id" .= identity, "method" .= method, "params" .= params])
    response <- BS8.hGetLine reader >>= decodeValue . BL.fromStrict
    expect "MCPResponseIdentityMismatch" (field "id" response == Just (toJSON identity))
    expect "MCPProtocolError" (field "error" response == Nothing)
    maybe (throwIO (SmokeFailure "MissingMCPResult")) pure (field "result" response)

captureStderr :: Child -> IO BS.ByteString
captureStderr (Child _ _ _ _ _ _ captured) = readMVar captured >>= either (const (throwIO (SmokeFailure "StderrCaptureFailed"))) pure

contentsText :: Key -> Value -> IO Text
contentsText key value = case field key value of
    Just (Array entries) | Vector.length entries == 1 -> case field "text" (Vector.head entries) of
        Just (String body) -> pure body
        _ -> throwIO (SmokeFailure "MissingTextContent")
    _ -> throwIO (SmokeFailure "UnexpectedContentCount")

task :: Value
task = object ["goal" .= ("Zero-turn dependency refusal" :: Text)
    , "acceptanceCriteria" .= ([] :: [Value]), "negativeEvidence" .= ([] :: [Value])
    , "sourcePackets" .= ([] :: [Value]), "requiredSourceIds" .= ["missing" :: Text]]

withNeutralDirectory :: (FilePath -> IO a) -> IO a
withNeutralDirectory = bracket create removePathForcibly
  where
    create = do
        temporary <- getTemporaryDirectory
        (path, temporaryHandle) <- openTempFile temporary "grace-mcp-zero-turn-"
        hClose temporaryHandle
        removeFile path
        createDirectory path
        pure path

-- Optional controls corrupt observed boundary results only. They demonstrate
-- the acceptance checks detect parity/usage regressions after real execution;
-- they neither alter production executables nor launch an extra protocol server.
smoke :: FilePath -> FilePath -> String -> Maybe String -> IO ()
smoke root report provider control = withNeutralDirectory $ \neutral -> do
    let taskPath = neutral </> "task.json"
    BL.writeFile taskPath (encode task)
    bracket (startChild (root </> "build/grace-router-mcp") [root, report, provider, neutral] neutral) stopChild $ \server -> do
        initialized <- request server 1 "initialize" (object
            ["protocolVersion" .= ("2025-06-18" :: Text), "capabilities" .= object []
            , "clientInfo" .= object ["name" .= ("grace-zero-turn-smoke" :: Text), "version" .= ("1" :: Text)]])
        expect "MissingInitializationGuidance" (case field "instructions" initialized of Just (String body) -> not (body == ""); _ -> False)
        sendFrame server (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)])
        tools <- request server 2 "tools/list" (object [])
        case field "tools" tools of
            Just (Array entries) | Vector.length entries == 1 -> do
                let tool = Vector.head entries
                expect "UnexpectedToolCatalog" (field "name" tool == Just (String "run_workflow"))
                expect "MissingOpenWorldAnnotation" ((field "annotations" tool >>= field "openWorldHint") == Just (Bool True))
                expect "IncorrectIdempotenceAnnotation" ((field "annotations" tool >>= field "idempotentHint") == Just (Bool False))
            _ -> throwIO (SmokeFailure "UnexpectedToolCatalog")
        resources <- request server 3 "resources/list" (object [])
        expect "UnexpectedReportCatalog" (case field "resources" resources of
            Just (Array entries) | Vector.length entries == 1 -> field "uri" (Vector.head entries) == Just (String "grace-router://report")
            _ -> False)
        reportResult <- request server 4 "resources/read" (object ["uri" .= ("grace-router://report" :: Text)])
        reportBody <- contentsText "contents" reportResult >>= decodeValue . BL.fromStrict . Encoding.encodeUtf8
        expect "MissingReportStatus" (case field "status" reportBody of Just (String _) -> True; _ -> False)
        response <- request server 5 "tools/call" (object ["name" .= ("run_workflow" :: Text), "arguments" .= object ["task" .= task]])
        expect "WorkflowTransportError" (field "isError" response == Just (Bool False))
        envelope <- maybe (throwIO (SmokeFailure "MissingStructuredContent")) pure (field "structuredContent" response)
        let observed = case (control, envelope) of
                (Just "--control-native-attempts", Object values) -> Object (KM.insert "nativeAttempts" (toJSON [object ["unexpectedAttempt" .= True]]) values)
                _ -> envelope
        expect "UnexpectedNativeAttempts" (field "nativeAttempts" observed == Just (toJSON ([] :: [Value])))
        expect "MissingSourceWasNotRefused" ((field "result" observed >>= field "status") == Just (String "refused"))
        expect "AdoptionWasAllowed" ((field "result" observed >>= field "adoptionAllowed") == Just (Bool False))
        textual <- contentsText "content" response >>= decodeValue . BL.fromStrict . Encoding.encodeUtf8
        expect "TextStructuredContentMismatch" (textual == observed)
        bracket (startChild (root </> "build/grace-native") [root, taskPath, provider, neutral] neutral) stopChild $ \cli@(Child writer reader _ process _ _ _) -> do
            hClose writer
            output <- BS.hGetContents reader
            evaluate (BS.length output) >>= \_ -> pure ()
            exit <- waitForProcess process
            expect "CLIExecutionFailed" (exit == ExitSuccess)
            cliOutput <- decodeValue (BL.fromStrict output)
            let compared = case (control, cliOutput) of
                    (Just "--control-cli-mismatch", Object values) -> Object (KM.insert "smokeDivergence" (Bool True) values)
                    _ -> cliOutput
            expect "CLIMCPMismatch" (compared == observed)
            captureStderr cli >>= expect "UnexpectedCLIStderr" . BS.null
        -- Close stdin and let the server exit before checking the complete drain.
        let Child writer _ _ process _ _ _ = server
        hClose writer
        exit <- waitForProcess process
        expect "MCPExitFailed" (exit == ExitSuccess)
        captureStderr server >>= expect "UnexpectedMCPStderr" . BS.null
        BL.putStr (encode (object ["status" .= ("passed" :: Text), "sdkRequests" .= (5 :: Int)
            , "nativeAttempts" .= (0 :: Int), "cliMcpEqual" .= True, "adoptionAllowed" .= False]) <> "\n")

main :: IO ()
main = do
    outcome <- try $ do
        arguments <- getArgs
        (root, report, provider, control) <- case arguments of
            [r, f, p] -> pure (r, f, p, Nothing)
            [r, f, p, c] | c `elem` ["--control-cli-mismatch", "--control-native-attempts"] -> pure (r, f, p, Just c)
            _ -> throwIO (SmokeFailure "Usage: MCPStdioSmoke ROOT REPORT.ffg EXACT_PROVIDER [--control-cli-mismatch|--control-native-attempts]")
        expect "EmptyProvider" (not (null provider))
        absoluteRoot <- makeAbsolute root
        absoluteReport <- makeAbsolute report
        completed <- timeout 45000000 (smoke absoluteRoot absoluteReport provider control)
        expect "MCPStdioSmokeTimeout" (completed == Just ())
    case outcome of
        Right () -> pure ()
        Left (err :: SomeException) -> do
            -- Never disclose provider stderr, task/report contents or raw errors.
            hPutStrLn stderr (case fromException err of Just (SmokeFailure label) -> label; Nothing -> "MCPStdioSmokeFailed")
            exitFailure
