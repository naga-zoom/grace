{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as Text
import NativeCodex (withCodex, catalogFacts)
import System.Environment (getArgs)

-- One-off live-catalog probe: confirms which reasoning efforts the running
-- native host's model/list RPC actually reports for the three fixed tiers.
-- Not part of the router's runtime; a verification tool only.
main :: IO ()
main = do
    args <- getArgs
    cwd <- case args of
        [dir] -> pure dir
        _ -> pure "."
    withCodex cwd "openai" \client ->
        BL.putStrLn (encode (catalogFacts client))
