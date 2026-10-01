{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WorkflowHost (runWorkflow, taskSchema, readReport) where
import Control.Exception
import Control.Monad (unless)
import Data.Aeson
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding
import GHC.Generics (Generic)
import qualified Grace.Decode as Decode
import Grace.Decode (FromGrace, ToGraceType)
import Grace.Input (Input(..), Mode(..))
import qualified Grace.Infer as Infer
import Grace.Location (Location(..))
import qualified Grace.Monad as Grace
import qualified Grace.Prompt as Prompt
import qualified Grace.Interpret as Interpret
import Grace.Type (Type)
import NativeCodex
import System.FilePath ((</>))

-- Transport DTOs describe only public task inputs, not routing policy.
data SourcePacket = SourcePacket { id :: Text, content :: Text }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType, ToJSON)
data TaskPayload = TaskPayload
    { goal :: Text, acceptanceCriteria :: [Text], negativeEvidence :: [Text]
    , sourcePackets :: [SourcePacket], requiredSourceIds :: [Text] }
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType, ToJSON)

schemaType :: Type Location
schemaType = fmap (const Unknown) (Decode.expected @TaskPayload)
taskSchema :: IO Value
taskSchema = either throwIO pure (Prompt.toJSONSchema schemaType)

-- Validate before even initializing a native client. checkJSON canonically
-- decodes only this task shape. Comparing its canonical encoding also rejects
-- additional properties, including nested authority fields, before effects.
prepare :: Value -> IO TaskPayload
prepare input = do
    value <- Grace.evalGrace (Code "MCP task input" "") Grace.Status{Grace.count=0, Grace.context=[]}
        (Infer.checkJSON schemaType input)
    task <- either throwIO pure (Decode.decode (fmap (const Unknown) value))
    unless (input == toJSON task) (throwIO (userError "InvalidTaskShape"))
    pure task

runWorkflow :: FilePath
    -> ((Client -> IO (Either Value Value)) -> IO (Either Value Value))
    -> Value -> IO (Either Value Value)
runWorkflow root withClient input = do
    clientRef <- newIORef Nothing
    outcome <- try do
        task <- prepare input
        withClient \client -> do
            writeIORef clientRef (Just client)
            result <- try (runProgram client (Path (root </> "Entry.ffg") AsCode)
                (object ["catalog" .= catalogFacts client, "task" .= toJSON task]))
            attempts <- observations client
            pure case result of
                Right value -> Right (object ["result" .= value, "nativeAttempts" .= attempts])
                Left (err :: SomeException) -> Left (failureEnvelope err attempts)
    case outcome of
        Right value -> pure value
        Left (err :: SomeException) -> do
            client <- readIORef clientRef
            attempts <- maybe (pure []) observations client
            pure (Left (failureEnvelope err attempts))
  where
    failureEnvelope :: SomeException -> [Observation] -> Value
    failureEnvelope err attempts = object
        ["error" .= errorClass err, "nativeAttempts" .= attempts]
    errorClass err = case fromException err :: Maybe NativeError of
        Just native -> displayException native
        Nothing -> "GraceInterpretationError"

-- Report evaluation uses a denying host handler and never starts Codex.
readReport :: FilePath -> IO Text
readReport path = do
    (_, value) <- Grace.evalGrace (Path path AsCode) Grace.Status{Grace.count=0, Grace.context=[]}
        (Grace.withPrompt (\(_ :: HostPrompt) _ -> throwIO (ProtocolError "Report attempted provider effect"))
            (Interpret.interpretWith [] Nothing))
    body <- either throwIO pure (Decode.decode value)
    case eitherDecodeStrict (Encoding.encodeUtf8 body) :: Either String Value of
        Right _ -> pure body
        Left _ -> throwIO (userError "InvalidReportJSON")
