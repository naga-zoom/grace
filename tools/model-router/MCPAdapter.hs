{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MCPAdapter (createMCPServer) where
import Control.Exception
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import Network.MCP.Server
import Network.MCP.Types
import WorkflowHost (taskSchema)

createMCPServer :: (Value -> IO (Either Value Value)) -> IO Text -> IO Server
createMCPServer execute report = do
    schema <- taskSchema
    server <- createServer (Implementation "grace-native-router-candidate" "0.1.0")
        (ServerCapabilities (Just (ResourcesCapability False)) (Just (ToolsCapability False)) Nothing)
        "Candidate/unadopted Grace workflow. run_workflow may invoke paid native provider turns; results are plans/proposals, never adopted policy. Startup owns configuration and trial scope. Read grace-router://report for evidence. nativeAttempts carries provider counters separately from model results."
    registerTools server [Tool "run_workflow"
        (Just "Run the candidate/unadopted Grace workflow on task data. May make native provider calls and consume tokens; repeated calls can differ and incur further usage. Startup policy and trial scope cannot be overridden. See also: grace-router://report resource.")
        (object ["type" .= ("object" :: Text), "properties" .= object ["task" .= schema]
            , "required" .= ["task" :: Text], "additionalProperties" .= False])
        (Just (ToolAnnotations (Just "Run candidate Grace workflow") (Just False) (Just False) (Just False) (Just True)))]
    registerToolCallHandler server $ \_ request ->
        if request.callToolName /= "run_workflow"
        then throwIO (userError "UnknownTool")
        else do
            result <- case request.callToolArguments of
                Object fields | KM.keys fields == ["task"], Just task <- KM.lookup "task" fields -> do
                    outcome <- try (execute task)
                    pure case outcome of
                        Right value -> value
                        Left (_ :: SomeException) -> Left (object ["error" .= ("WorkflowExecutionError" :: Text), "nativeAttempts" .= ([] :: [Value])])
                _ -> pure (Left (object ["error" .= ("InvalidToolArguments" :: Text), "nativeAttempts" .= ([] :: [Value])]))
            let (isError, value) = either (\v -> (True,v)) (\v -> (False,v)) result
            pure (CallToolResult [ToolContent TextualContent (Just (Encoding.decodeUtf8 (BL.toStrict (encode value)))) Nothing]
                isError (Just value))
    registerResources server [Resource "grace-router://report" "Grace router candidate evidence"
        (Just "Read-only candidate/unadopted report evaluated from the configured Grace source.") (Just "application/json") Nothing]
    registerResourceReadHandler server $ \request ->
        if request.resourceReadUri /= "grace-router://report"
        then throwIO (userError "UnknownResource")
        else do
            outcome <- try report
            case outcome of
                Left (_ :: SomeException) -> throwIO (userError "ReportEvaluationError")
                Right body -> pure (ReadResourceResult [ResourceContent "grace-router://report" (Just "application/json") (Just body) Nothing])
    pure server
