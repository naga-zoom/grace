{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text as Text
import MCPAdapter
import NativeCodex
import Network.MCP.Server.StdIO (runServerWithSTDIO)
import System.Environment (getArgs)
import WorkflowHost

main :: IO ()
main = do
    args <- getArgs
    case args of
        [root, reportPath, provider, cwd] -> do
            server <- createMCPServer
                (runWorkflow root (withCodex cwd (Text.pack provider)))
                (readReport reportPath)
            runServerWithSTDIO server
        _ -> fail "Usage: grace-router-mcp ROOT REPORT.ffg EXACT_PROVIDER CWD"
