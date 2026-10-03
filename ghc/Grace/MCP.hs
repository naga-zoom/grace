{-| This module performs the actual MCP (Model Context Protocol) interaction
    that powers the @mcp@ keyword, backed by @hs-mcp@'s Streamable HTTP
    client.

    There is no persistent connection or session cache here, matching every
    other Grace I\/O keyword (@prompt@\/@http@\/@github@ are already per-call
    I\/O): each call to 'mcp' does the full MCP @initialize@ handshake, sends
    one JSON-RPC request, and lets the connection go. A caller that loops
    pays the handshake cost on every iteration -- documented technical debt,
    not a bug (see the model-router plan for why a memoized client is not
    being added yet).

    Only plaintext @http:\/\/@ endpoints are supported: @hs-mcp@'s Streamable
    HTTP client is deliberately built on raw "Network.Socket" (matching its
    server half) rather than pulling in a TLS-capable HTTP library, so an
    @https:\/\/@ URL is rejected up front with a clear error instead of
    silently sending plaintext to a TLS port.
-}
module Grace.MCP
    ( MCP(..)
    , McpException(..)
    , mcp
    , importResultText
    , renderError
    ) where

import Control.Exception (IOException)
import Control.Exception.Safe (Exception(..), Handler(..))
import Data.Text (Text)
import Grace.MCP.Type (MCP(..))
import Network.MCP.Client.Types (McpClientError(..))
import Network.MCP.Transport.HTTP
    ( BearerToken(..)
    , connectHttpClient
    , mkHttpClient
    , sendHttpClientRequest
    )

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read
import qualified Grace.Decode as Decode

-- | Any failure while parsing an @mcp@ endpoint URL or talking to it
data McpException
    = InvalidURL Text
      -- ^ The @url@ field did not parse as @http://host[:port][/path]@
    | UnsupportedScheme Text
      -- ^ Any scheme other than @http@ (most notably @https@: hs-mcp's
      --   client has no TLS support, so this is rejected rather than
      --   silently sent in plaintext)
    | ClientError McpClientError
      -- ^ Connection, protocol, or JSON-RPC-level server error, forwarded
      --   from @hs-mcp@
    | TransportFailure Text
      -- ^ A lower-level socket failure (connection refused, host
      --   unreachable, …) below @hs-mcp@'s own error vocabulary
    | NonTextImportResult Aeson.Value
      -- ^ @import mcp { … }@ requires the JSON-RPC @result@ to be a JSON
      --   string of Grace source, same as @import http@\/@import read@
      --   treat their response body as source text; the result was some
      --   other JSON shape instead
    deriving stock (Show)

instance Exception McpException where
    displayException = Text.unpack . renderError

-- | Render an 'McpException' as human-readable `Data.Text.Text`
renderError :: McpException -> Text
renderError (InvalidURL url) =
    "Invalid MCP server URL\n\
    \\n\
    \" <> url
renderError (UnsupportedScheme scheme) =
    "Unsupported MCP server URL scheme: " <> scheme <> "\n\
    \\n\
    \Only plaintext http:// endpoints are supported."
renderError (ClientError (ConnectionError message)) =
    "MCP connection failure\n\
    \\n\
    \" <> message
renderError (ClientError (ProtocolError message)) =
    "MCP protocol failure\n\
    \\n\
    \" <> message
renderError (ClientError ServerError{ serverErrorCode, serverErrorMessage }) =
    "MCP server error" <> code <> "\n\
    \\n\
    \" <> serverErrorMessage
  where
    code = case serverErrorCode of
        Nothing -> ""
        Just n  -> " (code " <> Text.pack (show n) <> ")"
renderError (TransportFailure message) =
    "MCP connection failure\n\
    \\n\
    \" <> message
renderError (NonTextImportResult _) =
    "Invalid MCP import result\n\
    \\n\
    \`import mcp` requires the JSON-RPC result to be a JSON string of Grace source"

{-| Parse an @http://host[:port][/path]@ URL into @(host, port, path)@.

    This is intentionally minimal rather than a general URI parser: hs-mcp's
    Streamable HTTP client only ever talks plaintext HTTP to a host and port,
    so there is nothing else here to parse.
-}
parseHttpUrl :: Text -> Either McpException (String, Int, String)
parseHttpUrl url = case Text.stripPrefix "http://" url of
    Nothing
        | Just _ <- Text.stripPrefix "https://" url ->
            Left (UnsupportedScheme "https")
        | Just (scheme, _) <- splitScheme url ->
            Left (UnsupportedScheme scheme)
        | otherwise ->
            Left (InvalidURL url)
    Just rest
        | Text.null hostPort ->
            Left (InvalidURL url)
        | otherwise ->
            Right (Text.unpack host, port, path)
      where
        (hostPort, rawPath) = Text.breakOn "/" rest

        path = if Text.null rawPath then "/" else Text.unpack rawPath

        (host, rawPort) = Text.breakOn ":" hostPort

        port = case Text.stripPrefix ":" rawPort of
            Nothing -> 80
            Just digits -> case Text.Read.decimal digits of
                Right (n, "") -> n
                _             -> 80
  where
    splitScheme text = case Text.breakOn "://" text of
        (scheme, rest) | not (Text.null rest) -> Just (scheme, Text.drop 3 rest)
        _ -> Nothing

{-| Make one MCP call: parse @url@, connect over Streamable HTTP (full
    @initialize@ handshake), send @{ method, params }@ as a single JSON-RPC
    request, and return the unwrapped JSON-RPC @result@.

    The @Bool@ argument mirrors @http@\/@github@\/@read@'s @import_@
    parameter but is unused here for now: both the @mcp@ and @import mcp@
    forms call this the same way, and the caller (`Grace.Normalize`)
    decides whether to treat the result as JSON data or as Grace source,
    exactly like it already does for @http@.
-}
mcp :: Bool -> MCP -> IO Aeson.Value
mcp _import_ MCP{ url, key, method, params } = do
    (host, port, path) <- either Exception.throwIO pure (parseHttpUrl url)

    let bearer = case key of
            Nothing            -> Nothing
            Just (Decode.Key k) -> Just (BearerToken (Text.strip k))

    let client = mkHttpClient host port path bearer

    Exception.catches
        (do
            connectHttpClient client
            sendHttpClientRequest client method params
        )
        [ Handler (\clientError -> Exception.throwIO (ClientError clientError))
        , Handler (\ioException -> Exception.throwIO (TransportFailure (Text.pack (show (ioException :: IOException)))))
        ]

{-| Extract the Grace source text from an @import mcp@ JSON-RPC result, or
    throw 'NonTextImportResult' if the result wasn't a JSON string -- same
    discipline as @import http@\/@import read@, which both treat their
    response body as source text.
-}
importResultText :: Aeson.Value -> IO Text
importResultText (Aeson.String t) = pure t
importResultText value = Exception.throwIO (NonTextImportResult value)
