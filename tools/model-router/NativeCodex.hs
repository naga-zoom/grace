{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NativeCodex
    ( HostPrompt(..), Usage(..), NativeError(..), Completion(..), Observation(..)
    , Transport(..), Client, initialize, withCodex, prompt, observations, catalogFacts, runProgram
    , collectTurn, awaitResult, field
    ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, ToGraceType)
import qualified Grace.Prompt as Grace
import Grace.Type (Type)
import System.IO
import System.Process hiding (cwd)
import qualified System.Process as Process
import System.Exit (ExitCode(..))
import Data.Char (isAlphaNum)
import Data.Void (absurd)
import qualified Control.Monad.State as State
import qualified Grace.Context as Context
import qualified Grace.Decode as Decode
import qualified Grace.Infer as Infer
import qualified Grace.Interpret as Interpret
import qualified Grace.Monad as Interpreter
import qualified Grace.Value as GraceValue
import Grace.Input (Input)
import Grace.Location (Location(..))
import System.Timeout (timeout)

-- Exactly the arguments consumed by the native interpreter, without credentials.
data HostPrompt = HostPrompt { model :: Text, effort :: Text, text :: Text }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromGrace, ToGraceType)

-- Cached input and reasoning output are subsets; never add them to total usage.
data Usage = Usage
    { inputTokens :: Integer, cachedInputTokens :: Integer
    , outputTokens :: Integer, reasoningOutputTokens :: Integer
    , totalTokens :: Integer }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data NativeError = ProtocolError Text | RpcError Value | TurnFailed Value
    | TurnInterrupted | MalformedOutput Text | MissingUsage
    | UnsupportedProfile Text Text | MissingOutput | AmbiguousOutput | NativeTimeout
    deriving stock (Eq, Show)
instance Exception NativeError where
    displayException err = case err of
        ProtocolError _ -> "NativeProtocolError"
        RpcError _ -> "NativeRpcError"
        TurnFailed _ -> "NativeTurnFailed"
        TurnInterrupted -> "NativeTurnInterrupted"
        MalformedOutput _ -> "NativeMalformedOutput"
        MissingUsage -> "NativeMissingUsage"
        UnsupportedProfile _ _ -> "NativeUnsupportedProfile"
        MissingOutput -> "NativeMissingOutput"
        AmbiguousOutput -> "NativeAmbiguousOutput"
        NativeTimeout -> "NativeTimeout"

data Completion = Completion { answer :: Value, usage :: Usage }
    deriving stock (Eq, Show)

data Observation = Observation
    { requestedModel :: Text, requestedEffort :: Text
    , nativeThreadId :: Maybe Text, nativeTurnId :: Maybe Text
    , nativeUsage :: Maybe Usage, failure :: Maybe Text }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- The concrete stdio boundary also permits deterministic protocol fixtures.
data Transport = Transport { send :: Value -> IO (), receive :: IO Value }
data Client = Client
    { wire :: Transport, nextId :: IORef Integer
    , catalog :: Map.Map Text [Text], provider :: Text, cwd :: FilePath
    , recorded :: IORef [Observation], serial :: MVar (), pending :: IORef [Value] }

field :: Key -> Value -> Maybe Value
field key (Object value) = KM.lookup key value
field _ _ = Nothing

requiredText :: Key -> Value -> Either NativeError Text
requiredText key value = case field key value of
    Just (String result) -> Right result
    _ -> Left (ProtocolError ("Missing text field: " <> toText key))
  where
    toText = Text.pack . show

readUsage :: Value -> Either NativeError Usage
readUsage value = case fromJSON value of
    Error _ -> Left (ProtocolError "Incomplete or non-integer native token counters")
    Success counters@Usage{ inputTokens, cachedInputTokens, outputTokens, reasoningOutputTokens, totalTokens }
        | any (< 0) [inputTokens, cachedInputTokens, outputTokens, reasoningOutputTokens, totalTokens]
          || cachedInputTokens > inputTokens || reasoningOutputTokens > outputTokens
          || totalTokens /= inputTokens + outputTokens -> Left (ProtocolError "Inconsistent native token counters")
        | otherwise -> Right counters

matching :: Text -> Text -> Value -> Bool
matching thread turn params = field "threadId" params == Just (String thread)
    && field "turnId" params == Just (String turn)

turnCompleted :: Text -> Text -> Value -> Bool
turnCompleted thread turn frame = field "method" frame == Just (String "turn/completed")
    && case field "params" frame of
        Just params -> field "threadId" params == Just (String thread)
            && (field "turn" params >>= field "id") == Just (String turn)
        Nothing -> False

-- Completed agent items, rather than streamed deltas, define the final answer.
collectTurn :: Text -> Text -> [Value] -> Either NativeError Completion
collectTurn thread turn = loop Map.empty Nothing
  where
    loop _ _ [] = Left (ProtocolError "Stream ended before this turn completed")
    loop items counters (frame : rest) = case (field "method" frame, field "params" frame) of
        (Just (String "thread/tokenUsage/updated"), Just params) | matching thread turn params -> do
            total <- maybe (Left MissingUsage) Right (field "tokenUsage" params >>= field "total")
            updated <- readUsage total
            loop items (Just updated) rest
        (Just (String "item/completed"), Just params) | matching thread turn params -> do
            item <- maybe (Left (ProtocolError "Completed item missing")) Right (field "item" params)
            updated <- addItem items item
            loop updated counters rest
        (Just (String "turn/completed"), Just params) | turnCompleted thread turn frame -> do
            nativeTurn <- maybe (Left (ProtocolError "Completed turn missing")) Right (field "turn" params)
            status <- requiredText "status" nativeTurn
            case status of
                "failed" -> Left (TurnFailed nativeTurn)
                "interrupted" -> Left TurnInterrupted
                "completed" -> do
                    let embedded = case field "items" nativeTurn of
                            Just (Array values) -> Vector.toList values
                            _ -> []
                    updated <- foldl (\acc item -> acc >>= (`addItem` item)) (Right items) embedded
                    finalText <- case [body | (phase, body) <- Map.elems updated, phase /= Just "commentary"] of
                        [] -> Left MissingOutput
                        [body] -> Right body
                        _ -> Left AmbiguousOutput
                    result <- either (const (Left (MalformedOutput finalText))) Right
                        (eitherDecode (BL.fromStrict (Encoding.encodeUtf8 finalText)))
                    authoritative <- maybe (Left MissingUsage) Right counters
                    Right (Completion result authoritative)
                _ -> Left (ProtocolError "Nonterminal status in turn/completed")
        _ -> loop items counters rest
    addItem items item = case field "type" item of
        Just (String "agentMessage") -> do
            identity <- requiredText "id" item
            body <- requiredText "text" item
            phase <- case field "phase" item of
                Nothing -> Right Nothing
                Just Null -> Right Nothing
                Just (String p) | p `elem` ["commentary", "final_answer"] -> Right (Just p)
                _ -> Left (ProtocolError "Invalid agent-message phase")
            case Map.lookup identity items of
                Just previous | previous /= (phase, body) -> Left (ProtocolError "Conflicting duplicate final item")
                _ -> Right (Map.insert identity (phase, body) items)
        Just (String kind) | kind `elem` ["commandExecution", "fileChange", "mcpToolCall", "dynamicToolCall", "collabToolCall"] ->
            Left (ProtocolError "Unexpected tool execution in a typed prompt")
        _ -> Right items

awaitResult :: Integer -> [Value] -> Either NativeError (Value, [Value])
awaitResult _ [] = Left (ProtocolError "Response stream ended")
awaitResult wanted (frame : rest)
    | field "id" frame == Just (Number (fromInteger wanted)) = case (field "result" frame, field "error" frame) of
        (Just result, Nothing) -> Right (result, rest)
        (Nothing, Just err) -> Left (RpcError err)
        _ -> Left (ProtocolError "Malformed matching RPC response")
    | otherwise = awaitResult wanted rest

rpc :: Transport -> IORef Integer -> IORef [Value] -> Text -> Value -> IO Value
rpc Transport{send,receive} next pending method params = do
    identity <- atomicModifyIORef' next (\previous -> (previous + 1, previous + 1))
    send (object ["id" .= identity, "method" .= method, "params" .= params])
    let wait = do
            frame <- receive
            case (field "method" frame, field "id" frame) of
                (Just _, Just _) -> throwIO (ProtocolError "Unsolicited server request")
                _ -> if field "id" frame == Just (Number (fromInteger identity))
                    then either throwIO (pure . fst) (awaitResult identity [frame])
                    else do
                        when (field "method" frame /= Nothing) (modifyIORef' pending (<> [frame]))
                        wait
    finished <- timeout 30000000 wait
    maybe (throwIO NativeTimeout) pure finished

initialize :: FilePath -> Text -> Transport -> IO Client
initialize cwd provider wire@Transport{send} = do
    nextId <- newIORef 0
    pending <- newIORef []
    _ <- rpc wire nextId pending "initialize" (object ["clientInfo" .= object
        ["name" .= ("grace_native_router_candidate" :: Text), "title" .= ("Grace native candidate" :: Text), "version" .= ("0.1.0" :: Text)]])
    send (object ["method" .= ("initialized" :: Text), "params" .= object []])
    let page cursor = do
            result <- rpc wire nextId pending "model/list" (object
                (["limit" .= (100 :: Int), "includeHidden" .= False] <> maybe [] (\value -> ["cursor" .= value]) cursor))
            entries <- case field "data" result of
                Just (Array values) -> traverse catalogEntry (Vector.toList values)
                _ -> throwIO (ProtocolError "Model catalog missing")
            more <- case field "nextCursor" result of
                Nothing -> pure []
                Just Null -> pure []
                Just (String next) -> page (Just next)
                _ -> throwIO (ProtocolError "Invalid catalog cursor")
            pure (entries <> more)
        catalogEntry entry = do
            name <- either throwIO pure (requiredText "model" entry)
            efforts <- case field "supportedReasoningEfforts" entry of
                Just (Array values) -> traverse (either throwIO pure . requiredText "reasoningEffort") (Vector.toList values)
                _ -> throwIO (ProtocolError "Model reasoning-effort catalog missing")
            pure (name, efforts)
    entries <- page Nothing
    recorded <- newIORef []
    serial <- newMVar ()
    pure Client{wire, nextId, catalog = Map.fromList entries, provider, cwd, recorded, serial, pending}

observations :: Client -> IO [Observation]
observations = readIORef . recorded

-- Availability facts from this initialized native host; aliases are not revisions.
catalogFacts :: Client -> Value
catalogFacts client = toJSON
    [object ["model" .= name, "modelVersion" .= Null, "efforts" .= efforts]
    | (name, efforts) <- Map.toList (catalog client)]

-- Infer the concrete JSON input through Grace's existing inference, rather
-- than label every record JSON and erase the shape needed by a typed workflow.
runProgram :: Client -> Input -> Value -> IO Value
runProgram client input json = do
    (_, value) <- Interpreter.evalGrace input Interpreter.Status{Interpreter.count=0, Interpreter.context=[]}
        (Interpreter.withPrompt (prompt client) do
            let binding = fmap (const Unknown) (Infer.inferJSON json)
            (type_, _) <- Infer.infer (fmap absurd (GraceValue.quote binding))
            status <- State.get
            let inputType = Context.complete (Interpreter.context status) type_
            Interpret.interpretWith [("input", inputType, binding)] Nothing)
    either throwIO pure (Decode.decode value)

prompt :: Client -> HostPrompt -> Type Location -> IO Value
prompt Client{wire, nextId, catalog, provider, cwd, recorded, serial, pending} HostPrompt{model,effort,text} schema =
    withMVar serial \_ -> do
        threadRef <- newIORef Nothing
        turnRef <- newIORef Nothing
        framesRef <- newIORef []
        result <- try do
            outcome <- timeout 120000000 do
                unless (maybe False (effort `elem`) (Map.lookup model catalog)) (throwIO (UnsupportedProfile model effort))
                outputSchema <- either throwIO pure (Grace.toJSONSchema schema)
                started <- rpc wire nextId pending "thread/start" (object
                    [ "model" .= model, "modelProvider" .= provider, "cwd" .= cwd
                    , "ephemeral" .= True, "sandbox" .= ("read-only" :: Text), "approvalPolicy" .= ("never" :: Text)
                    , "baseInstructions" .= ("Return only JSON matching outputSchema. Use no tools and inspect no files." :: Text) ])
                unless (field "model" started == Just (String model) && field "modelProvider" started == Just (String provider)
                    && field "approvalPolicy" started == Just (String "never")
                    && (field "sandbox" started >>= field "type") == Just (String "readOnly"))
                    (throwIO (ProtocolError "Native host changed the selected profile or permission policy"))
                thread <- either throwIO pure (maybe (Left (ProtocolError "Thread missing")) (requiredText "id") (field "thread" started))
                writeIORef threadRef (Just thread)
                begin <- rpc wire nextId pending "turn/start" (object
                    ["threadId" .= thread, "model" .= model, "effort" .= effort
                    , "input" .= [object ["type" .= ("text" :: Text), "text" .= text]], "outputSchema" .= outputSchema])
                turn <- either throwIO pure (maybe (Left (ProtocolError "Turn missing")) (requiredText "id") (field "turn" begin))
                writeIORef turnRef (Just turn)
                let events = do
                        queued <- atomicModifyIORef' pending (\values -> case values of
                            [] -> ([], Nothing); value : rest -> (rest, Just value))
                        frame <- maybe (receive wire) pure queued
                        modifyIORef' framesRef (frame :)
                        when (field "method" frame /= Nothing && field "id" frame /= Nothing)
                            (throwIO (ProtocolError "Unsolicited server request during inference"))
                        if turnCompleted thread turn frame then pure () else events
                events
                frames <- reverse <$> readIORef framesRef
                either throwIO pure (collectTurn thread turn frames)
            maybe (throwIO NativeTimeout) pure outcome
        thread <- readIORef threadRef
        turn <- readIORef turnRef
        frames <- reverse <$> readIORef framesRef
        let knownUsage = do
                threadId <- thread
                turnId <- turn
                if any (turnCompleted threadId turnId) frames then Just () else Nothing
                let snapshots = [params | frame <- frames, field "method" frame == Just (String "thread/tokenUsage/updated")
                        , Just params <- [field "params" frame], matching threadId turnId params]
                case reverse snapshots of
                    finalSnapshot : _ -> do
                        raw <- field "tokenUsage" finalSnapshot >>= field "total"
                        either (const Nothing) Just (readUsage raw)
                    [] -> Nothing
        case result of
            Right Completion{answer,usage} -> do
                modifyIORef' recorded (<> [Observation model effort thread turn (Just usage) Nothing])
                pure answer
            Left (err :: SomeException) -> do
                modifyIORef' recorded (<> [Observation model effort thread turn knownUsage (Just (case fromException err :: Maybe NativeError of Just native -> Text.pack (displayException native); Nothing -> "NativeTransportError"))])
                throwIO err

-- No model-supplied executable or arguments: the installed native host only.
withCodex :: FilePath -> Text -> (Client -> IO a) -> IO a
withCodex cwd provider action = do
    let flags = ["-c", "features.shell_tool=false", "-c", "features.unified_exec=false"
            , "-c", "features.multi_agent=false", "-c", "features.plugins=false"
            , "-c", "features.multi_agent_v2=false", "-c", "features.tool_suggest=false"
            , "-c", "features.apps=false", "-c", "features.image_generation=false"
            , "-c", "features.code_mode_host=false", "-c", "web_search=\"disabled\""]
        discover overrides = do
            result <- timeout 15000000 (readCreateProcessWithExitCode
                ((proc "codex" (flags <> overrides <> ["mcp", "list", "--json"]))
                    {Process.cwd = Just cwd}) "")
            case result of
                Just (ExitSuccess, output, _) -> case eitherDecodeStrict (Encoding.encodeUtf8 (Text.pack output)) of
                    Right (Array values) -> traverse enabledServer (Vector.toList values)
                    _ -> throwIO (ProtocolError "Invalid native MCP configuration catalog")
                _ -> throwIO (ProtocolError "Native MCP configuration discovery failed")
        enabledServer value = do
            name <- either throwIO pure (requiredText "name" value)
            unless (not (Text.null name) && Text.all (\c -> isAlphaNum c || c `elem` ['_', '-']) name)
                (throwIO (ProtocolError "Unsupported native MCP server key"))
            case field "enabled" value of
                Just (Bool enabled) -> pure (name, enabled)
                _ -> throwIO (ProtocolError "Native MCP server enabled state missing")
    servers <- discover []
    let disabled = concat [["-c", "mcp_servers." <> Text.unpack name <> ".enabled=false"]
            | (name, True) <- servers]
    verified <- discover disabled
    unless (all (not . snd) verified) (throwIO (ProtocolError "Native MCP server remains enabled"))
    withCreateProcess
        ((proc "codex" (["app-server", "--listen", "stdio://"] <> flags <> disabled))
            {std_in = CreatePipe, std_out = CreatePipe, std_err = NoStream, Process.cwd = Just cwd})
        \input output _ _ -> case (input, output) of
            (Just writer, Just reader) -> do
                let send value = BL.hPutStrLn writer (encode value) >> hFlush writer
                let receive = do
                        line <- BS.hGetLine reader
                        either (\_ -> throwIO (ProtocolError "Malformed native JSON-RPC frame")) pure (eitherDecodeStrict line)
                initialized <- timeout 15000000 (initialize cwd provider Transport{send,receive})
                maybe (throwIO NativeTimeout) action initialized
            _ -> throwIO (ProtocolError "Native host stdio pipes unavailable")
