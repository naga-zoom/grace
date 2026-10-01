{-# LANGUAGE ExistentialQuantification #-}

{-| This module contains the `Grace` `Monad` shared by type inference and
    evaluation
-}
module Grace.Monad
    ( -- * Monad
      Status(..)
    , Grace(..)
    , runGrace
    , evalGrace
    , execGrace

      -- * Scoped prompt interpretation
    , withPrompt
    , promptArgumentsType
    , promptJSON
    , UnsupportedPromptImport(..)
    , MissingSchema(..)
    ) where

import Control.Exception.Safe (Exception(..))
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT)
import Control.Monad.State (MonadState, StateT)
import Grace.Context (Context)
import Grace.Decode (FromGrace(..), ToGraceType(..))
import Grace.Input (Input)
import Grace.Location (Location)
import Grace.Parallelizable (Parallelizable)
import Grace.Prompt.Types (Prompt)
import Grace.Type (Type)
import Grace.Value (Value)

import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Grace.Parallelizable as Parallelizable

-- | Interpretation state
data Status = Status
    { count :: !Int
      -- ^ Used to generate fresh unsolved variables (e.g. α̂, β̂ from the
      --   original paper)

    , context :: Context Location
      -- ^ The type-checking context (e.g. Γ, Δ, Θ)
    }

-- The decoder and argument type share the existential witness.
data PromptHandler = forall p. FromGrace p =>
    PromptHandler (p -> Type Location -> IO Aeson.Value)

data Runtime = Runtime Input (Maybe PromptHandler)

-- | The shared `Monad` threaded throughout all phases of interpretation
newtype Grace a = Grace{ parallelizable :: ReaderT Runtime (Parallelizable (StateT Status IO)) a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadCatch
        , MonadIO
        , MonadState Status
        , MonadThrow
        )

instance MonadReader Input Grace where
    ask = Grace (Reader.asks (\(Runtime input _) -> input))
    local f Grace{ parallelizable } = Grace
        (Reader.local (\(Runtime input handler) -> Runtime (f input) handler) parallelizable)

{-| Interpret `prompt` using a typed host callback within this lexical scope.
    Arguments are checked against @p@ before evaluation.  The returned JSON is
    checked against the prompt's inferred output type by the evaluator.

    Apply Grace functions inside this scope and return data: decoding a Grace
    function to a Haskell callback starts a new default interpreter and does
    not capture this handler.  Generated-code @prompt import@ is unsupported.
-}
withPrompt
    :: FromGrace p
    => (p -> Type Location -> IO Aeson.Value) -> Grace a -> Grace a
withPrompt handler Grace{ parallelizable } = Grace
    (Reader.local (\(Runtime input _) -> Runtime input (Just (PromptHandler handler))) parallelizable)

-- | The argument type of the active prompt interpreter.
promptArgumentsType :: Bool -> Grace (Type ())
promptArgumentsType import_ = Grace do
    Runtime _ handler <- Reader.ask
    case handler of
        Nothing -> pure (expected @Prompt)
        Just (PromptHandler (_ :: p -> Type Location -> IO Aeson.Value))
            | import_ -> Exception.throwIO UnsupportedPromptImport
            | otherwise -> pure (expected @p)

-- | Invoke the scoped interpreter, or leave the default API path active.
promptJSON :: Value Location -> Maybe (Type Location) -> Grace (Maybe (Type Location, Aeson.Value))
promptJSON arguments maybeSchema = Grace do
    Runtime _ handler <- Reader.ask
    case handler of
        Nothing -> pure Nothing
        Just (PromptHandler callback) -> do
            schema <- case maybeSchema of
                Nothing -> Exception.throwIO MissingSchema
                Just outputType -> pure outputType
            request <- case decode arguments of
                Left err -> Exception.throwIO err
                Right value -> pure value
            json <- liftIO (callback request schema)
            pure (Just (schema, json))

-- | A JSON prompt interpreter cannot execute generated Grace code.
data UnsupportedPromptImport = UnsupportedPromptImport deriving stock (Show)

instance Exception UnsupportedPromptImport where
    displayException _ = "Scoped prompt interpreters return JSON; prompt import is unsupported"

-- | Elaboration didn't infer a schema
data MissingSchema = MissingSchema
    deriving stock (Show)

instance Exception MissingSchema where
    displayException MissingSchema =
        "Internal error - Elaboration failed to infer schema"

-- | Run the `Grace` `Monad`, preserving the result and final `Status`
runGrace :: MonadIO io => Input -> Status -> Grace a -> io (a, Status)
runGrace input status Grace{ parallelizable } =
    liftIO (State.runStateT (Parallelizable.serialize (Reader.runReaderT parallelizable (Runtime input Nothing))) status)

-- | Run the `Grace` `Monad`, discarding the final `Status`
evalGrace :: MonadIO io => Input -> Status -> Grace a -> io a
evalGrace input status Grace{ parallelizable } =
    liftIO (State.evalStateT (Parallelizable.serialize (Reader.runReaderT parallelizable (Runtime input Nothing))) status)

-- | Run the `Grace` `Monad`, discarding the result
execGrace :: MonadIO io => Input -> Status -> Grace a -> io Status
execGrace input status Grace{ parallelizable } =
    liftIO (State.execStateT (Parallelizable.serialize (Reader.runReaderT parallelizable (Runtime input Nothing))) status)
