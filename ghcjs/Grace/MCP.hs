{-| This module performs the actual MCP (Model Context Protocol) interaction
    that powers the @mcp@ keyword, for GHCJS (@trygrace.dev@).

    There is no browser-side MCP transport (hs-mcp's Streamable HTTP client is
    built on "Network.Socket", which does not exist in a browser), so this
    module exists only so that cross-platform code (`Grace.Normalize`) can
    import @Grace.MCP@ unconditionally the same way it already does for
    @Grace.HTTP@. Parsing and type-checking an @mcp@ expression still works
    identically on the website; only evaluating one fails, with a clear
    message rather than a missing-module build break.
-}
module Grace.MCP
    ( MCP(..)
    , McpException(..)
    , mcp
    , importResultText
    , renderError
    ) where

import Control.Exception.Safe (Exception(..))
import Data.Text (Text)
import Grace.MCP.Type (MCP(..))

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

-- | 'mcp' on GHCJS always fails with this; there is no other failure mode
newtype McpException = Unsupported Text
    deriving stock (Show)

instance Exception McpException where
    displayException = Text.unpack . renderError

-- | Render an 'McpException' as human-readable `Data.Text.Text`
renderError :: McpException -> Text
renderError (Unsupported url) =
    "MCP is not supported in the browser\n\
    \\n\
    \" <> url <> "\n\
    \\n\
    \hs-mcp's Streamable HTTP client requires a real socket connection, which\n\
    \is unavailable from GHCJS/trygrace.dev. Run this program with the native\n\
    \`grace` interpreter instead."

-- | Always throws 'Unsupported'; see the module Haddock
mcp :: Bool -> MCP -> IO Aeson.Value
mcp _import_ MCP{ url } = Exception.throwIO (Unsupported url)

-- | Always throws 'Unsupported' -- @import mcp@ never gets this far on
--   GHCJS since 'mcp' already throws
importResultText :: Aeson.Value -> IO Text
importResultText _ = Exception.throwIO (Unsupported "")
