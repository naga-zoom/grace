{-# LANGUAGE OverloadedStrings #-}
import WorkflowHost (readReport)
import Data.Aeson
import qualified Data.Text.Encoding as Encoding
import Control.Monad (unless)
import System.Environment (getArgs)
main :: IO ()
main = do
    [reportPath,countersPath] <- getArgs
    body <- readReport reportPath
    actual <- either fail pure (eitherDecodeStrict (Encoding.encodeUtf8 body) :: Either String Value)
    expected <- eitherDecodeFileStrict countersPath >>= either fail pure
    unless (actual == expected) (fail "GraceReportCountersMismatch")
    putStrLn "Denying-handler Grace report evaluates and exactly matches counters.json"
