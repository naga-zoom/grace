{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import NativeCodex
import System.Environment (getArgs)
import System.Exit (exitFailure)
import WorkflowHost

-- CLI and MCP evaluate the same fixed Entry.ffg with trusted bundled Grace settings.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [root, taskPath, provider, cwd] -> do
            task <- readInput taskPath
            result <- runWorkflow root (withCodex cwd (Text.pack provider)) task
            case result of
                Right value -> BL.putStr (encode value) >> putStrLn ""
                Left value -> BL.putStr (encode value) >> putStrLn "" >> exitFailure
        _ -> fail "Usage: grace-native ROOT TASK.json EXACT_PROVIDER CWD"
  where
    readInput path = eitherDecodeFileStrict path >>= either (const (fail "InvalidStartupJSON")) pure
