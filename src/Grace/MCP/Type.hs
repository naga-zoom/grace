-- | This module contains types shared between the GHC and (future) GHCJS
--   implementations of the @mcp@ keyword
module Grace.MCP.Type where

import Data.Aeson (Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, Key, ToGraceType)
import Grace.Encode (ToGrace)

{-| Arguments to the @mcp@ keyword

    This is a flat record, not a tagged union, matching the majority of
    Grace's I/O keywords (@prompt@, @github@, @read@) rather than @http@'s
    @GET@\/@POST@ union: the MCP transport (stdio vs. Streamable HTTP) is
    plumbing, not a per-call caller choice, so there is nothing here for a
    tag to usefully distinguish.

    @method@ is any JSON-RPC method (@tools\/call@, @resources\/read@,
    @tools\/list@, …) and @params@ is that method's opaque JSON params
    object -- the same role @request@ plays for @http POST@. This keeps
    @mcp@ a single primitive over the full JSON-RPC envelope rather than a
    hardcoded wrapper around one method (e.g. @tools\/call@ via @name@\/
    @arguments@ fields): callers needing @callTool@\/@readResource@-style
    convenience build it as a derived function in the Prelude style, the
    same layering Grace already uses for @prelude\/text\/concatSep.ffg@
    over builtins.
-}
data MCP = MCP
    { url :: Text
    , key :: Maybe Key
    , method :: Text
    , params :: Maybe Value
    } deriving stock (Generic)
      deriving anyclass (FromGrace, ToGrace, ToGraceType)
