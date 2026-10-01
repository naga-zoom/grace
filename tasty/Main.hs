{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import Control.Exception.Safe (Exception, SomeException)
import Data.Aeson (Value)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Grace.Decode (FromGrace, Key, ToGraceType)
import Grace.Input (Input(..), Mode(..))
import Grace.Location (Location(..))
import Grace.Pretty (Pretty(..))
import Grace.Type (Type(..))
import Numeric.Natural (Natural)
import System.FilePath ((</>))
import Test.Tasty (TestTree)

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Control.Monad.Reader as Reader
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as Vector
import qualified Grace.Decode as Decode
import qualified Grace.Aeson as Grace.Aeson
import qualified Grace.Infer as Infer
import qualified Grace.Interpret as Interpret
import qualified Grace.Monad as Grace
import qualified Grace.Normalize as Normalize
import qualified Grace.Monotype as Monotype
import qualified Grace.Pretty
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as Type
import qualified Grace.Value as Value
import qualified Grace.Width as Width
import qualified Prettyprinter as Pretty
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Timeout as Timeout
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit
import qualified Test.Tasty.Silver as Silver

pretty_ :: Pretty a => a -> Text
pretty_ x =
    Grace.Pretty.renderStrict False Width.defaultWidth
        (pretty x <> Pretty.hardline)

interpret :: Input -> IO (Either SomeException (Type Location, Value.Value ()))
interpret input =
    fmap (fmap (fmap (fmap (\_ -> ())))) (Exception.try (Interpret.interpret input))

throws :: Exception e => IO (Either e a) -> IO a
throws io = do
    result <- io

    case result of
        Left  e -> Exception.throw e
        Right a -> return a

fileToTestTree :: FilePath -> IO TestTree
fileToTestTree prefix = do
    let input              = prefix <> "-input.ffg"
    let expectedTypeFile   = prefix <> "-type.ffg"
    let expectedOutputFile = prefix <> "-output.ffg"
    let expectedStderrFile = prefix <> "-stderr.txt"

    let name = FilePath.takeBaseName input

    result <- Timeout.timeout 10000000 (interpret (Path input AsCode))

    case result of
        Nothing -> do
            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - timeout")
                        expectedStderrFile
                        (return "timeout")
                        id
                    ]
                )
        Just (Left e) -> do
            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - error")
                        expectedStderrFile
                        (return (Text.pack (Exception.displayException e)))
                        id
                    ]
                )

        Just (Right (inferred, value)) -> do
            let generateTypeFile = return (pretty_ inferred)

            let generateOutputFile = return (pretty_ value)

            return
                (Tasty.testGroup name
                    [ Silver.goldenVsAction
                        (name <> " - type")
                        expectedTypeFile
                        generateTypeFile
                        id
                    , Silver.goldenVsAction
                        (name <> " - output")
                        expectedOutputFile
                        generateOutputFile
                        id
                    ]
                )

inputFileToPrefix :: FilePath -> Maybe FilePath
inputFileToPrefix inputFile =
    fmap Text.unpack (Text.stripSuffix "-input.ffg" (Text.pack inputFile))

directoryToTestTree :: FilePath -> IO TestTree
directoryToTestTree directory = do
    let name = FilePath.takeBaseName directory

    children <- Directory.listDirectory directory

    let process child = do
            let childPath = directory </> child

            isDirectory <- Directory.doesDirectoryExist childPath

            if isDirectory
                then do
                    testTree <- directoryToTestTree childPath

                    return [ testTree ]

                else do
                    case inputFileToPrefix childPath of
                        Just prefix -> do
                            testTree <- fileToTestTree prefix

                            return [ testTree ]

                        Nothing -> do
                            return [ ]

    testTreess <- traverse process children

    return (Tasty.testGroup name (concat testTreess))

data T0
    = C0
    | C1{ foo :: Text }
    | C2{ bar :: Natural, baz :: Maybe Bool }
    | C3{ a :: Maybe Int, b :: Maybe Int, c :: Maybe Int }
    | C4{ a :: Maybe Int, b :: Maybe Int, c :: Maybe Int, d :: Maybe Int }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromGrace, ToGraceType)

data T1
    deriving stock (Generic)
    deriving anyclass (FromGrace, ToGraceType)

main :: IO ()
main = do
    autogeneratedTestTree <- directoryToTestTree "tasty/data"

    let manualTestTree =
            Tasty.testGroup "Manual tests"
                [ interpretCode
                , interpretCodeWithEnvURI
                , interpretCodeWithFileURI
                , interpretCodeWithImport
                , decodeWithTypeError
                , decodeWithRangeError
                , loadSuccessfully
                , conditionalEvaluation
                , hostedPrompts
                , load "()" "{ }" ()
                , load "(Bool, Bool)" "{ \"0\": false, \"1\": true }" (False, True)
                , load "(Bool, Bool)" "{ \"0\": false, \"1\": true }" (False, True)
                , load "Either Int Bool" "Left 2" (Left 2 :: Either Int Bool)
                , load "Either Int Bool" "Right true" (Right True :: Either Int Bool)
                , load "Either Int Bool" "Right true" (Right True :: Either Int Bool)
                , load "Int" "-2" (-2 :: Int)
                , load "Int8" "-2" (-2 :: Int8)
                , load "Int16" "-2" (-2 :: Int16)
                , load "Int32" "-2" (-2 :: Int32)
                , load "Int64" "-2" (-2 :: Int64)
                , load "Word" "2" (2 :: Word)
                , load "Word8" "2" (2 :: Word8)
                , load "Word16" "2" (2 :: Word16)
                , load "Word32" "2" (2 :: Word32)
                , load "Word64" "2" (2 :: Word64)
                , load "Natural" "2" (2 :: Natural)
                , load "Integer" "2" (2 :: Integer)
                , load "Integer" "+2" (2 :: Integer)
                , load "Scientific" "2" (2.0 :: Scientific)
                , load "Scientific" "+2" (2.0 :: Scientific)
                , load "Scientific" "2.5" (2.5 :: Scientific)
                , load "Double" "2.5" (2.5 :: Double)
                , load "Float" "2.5" (2.5 :: Float)
                , load "Text" "\"abc\"" ("abc" :: Text)
                , load "Lazy Text" "\"abc\"" ("abc" :: Text.Lazy.Text)
                , load "String" "\"abc\"" ("abc" :: String)
                , load "Key" "\"abc\"" ("abc" :: Key)
                , load "Value" "null" Aeson.Null
                , load "Seq Bool" "[ false, true ]" (Seq.fromList [ False, True ])
                , load "Vector Bool" "[ false, true ]" (Vector.fromList [ False, True ])
                , load "T0" "C0{ }" C0{ }
                , load "T0" "C1{ foo: \"abc\" }" C1{ foo = "abc" }
                , load "T0" "C2{ bar: 2 }" C2{ bar = 2, baz = Nothing }
                , load "T0" "C2{ bar: 3, baz: true }" C2{ bar = 3, baz = Just True }
                , load "T0" "C3{ }" C3{ a = Nothing, b = Nothing, c = Nothing }
                , load "T0" "C4{ }" C4{ a = Nothing, b = Nothing, c = Nothing, d = Nothing }
                ]

    let tests = Tasty.testGroup "Tests" [ autogeneratedTestTree, manualTestTree ]

    Tasty.defaultMain tests

interpretCode :: TestTree
interpretCode = Tasty.HUnit.testCase "interpret code" do
    actualValue <- throws (interpret (Code "(input)" "2 + 2"))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Natural }, Value.Scalar () (Syntax.Natural 4))
          where
            location = Location{ name = "(input)", code = "2 + 2", offset = 2 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithImport :: TestTree
interpretCodeWithImport = Tasty.HUnit.testCase "interpret code with import from file" do
    actualValue <- throws (interpret (Code "(input)" "./tasty/data/unit/plus-input.ffg"))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Natural }, Value.Scalar () (Syntax.Natural 5))
          where
            location = Location{ name = "./tasty/data/unit/plus-input.ffg", code = "2 + 3\n", offset = 2 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithEnvURI :: TestTree
interpretCodeWithEnvURI = Tasty.HUnit.testCase "interpret code with env: import" do
    let key = "GRACE_TEST_VAR"

    let name = "env:" <> key

    let open = do
            m <- Environment.lookupEnv key

            Environment.setEnv key "true"

            return m

    let close  Nothing  = Environment.unsetEnv key
        close (Just v ) = Environment.setEnv key v

    actualValue <- Exception.bracket open close \_ -> do
        throws (interpret (Code "(input)" (Text.pack name)))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Bool }, Value.Scalar () (Syntax.Bool True))
          where
            location = Location{ name, code = "true", offset = 0 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

interpretCodeWithFileURI :: TestTree
interpretCodeWithFileURI = Tasty.HUnit.testCase "interpret code with file:// import" do
    absolute <- Directory.makeAbsolute "./tasty/data/true.ffg"

    let uri = "file://" <> absolute

    actualValue <- throws (interpret (Code "(input)" (Text.pack uri)))

    let expectedValue =
            (Type.Scalar{ location, scalar = Monotype.Bool }, Value.Scalar () (Syntax.Bool True))
          where
            location = Location{ name = absolute, code = "true\n", offset = 0 }

    Tasty.HUnit.assertEqual "" expectedValue actualValue

loadSuccessfully :: TestTree
loadSuccessfully = Tasty.HUnit.testCase "load code" do
    let actual :: Either DecodingError Natural
        actual = decode (Value.Scalar () (Syntax.Natural 2))

    Tasty.HUnit.assertEqual "" (Right 2) actual

load :: (Eq a, FromGrace a, Show a) => String -> Text -> a -> TestTree
load name code expected = Tasty.HUnit.testCase ("load " <> name) do
    actual <- Interpret.load (Code "(input)" code)

    Tasty.HUnit.assertEqual "" expected actual

conditionalEvaluation :: TestTree
conditionalEvaluation = Tasty.testGroup "Conditional evaluation"
    [ load "true skips invalid JSON in else"
        "if true then 1 else (read \"{\" : Natural)" (1 :: Natural)
    , load "false skips invalid JSON in then"
        "if false then (read \"{\" : Natural) else 2" (2 :: Natural)
    , load "nested selected branch reads JSON"
        "if true then (if false then (read \"{\" : Natural) else (read \"3\" : Natural)) else (read \"{\" : Natural)"
        (3 :: Natural)
    , Tasty.HUnit.testCase "selected invalid JSON still fails" do
        result <- Exception.try
            (Interpret.load (Code "(input)" "if true then (read \"{\" : Natural) else 1"))
            :: IO (Either Grace.Aeson.JSONDecodingFailed Natural)
        case result of
            Left Grace.Aeson.JSONDecodingFailed{ text } ->
                Tasty.HUnit.assertEqual "selected read input" "{" text
            Right value -> Tasty.HUnit.assertFailure ("Unexpected success: " <> show value)
    , Tasty.HUnit.testCase "unselected ill-typed branch is rejected" do
        result <- Exception.try
            (Interpret.load (Code "(input)" "if true then 1 else (true : Natural)"))
            :: IO (Either Infer.TypeInferenceError Natural)
        case result of
            Left (Infer.NotSubtype actual expected) -> do
                Tasty.HUnit.assertEqual "actual branch type" (Monotype.Bool)
                    (Type.scalar actual)
                Tasty.HUnit.assertEqual "required branch type" (Monotype.Natural)
                    (Type.scalar expected)
            Left err -> Tasty.HUnit.assertFailure ("Unexpected type error: " <> show err)
            Right value -> Tasty.HUnit.assertFailure ("Unexpected success: " <> show value)
    ]

data HostPrompt = HostPrompt
    { model :: Text, effort :: Text, text :: Text }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromGrace, ToGraceType)

runHosted
    :: forall p a. (FromGrace p, FromGrace a)
    => (p -> Type Location -> IO Aeson.Value)
    -> [(Text, Type Location, Value.Value Location)] -> Text -> IO a
runHosted handler bindings code = do
    let input = Code "(hosted test)" code
    let status = Grace.Status{ count = 0, context = [] }
    let annotation = fmap (\_ -> Unknown) (Decode.expected @a)
    (_, value) <- Grace.evalGrace input status
        (Grace.withPrompt handler (Interpret.interpretWith bindings (Just annotation)))
    case Decode.decode value of
        Left err -> Exception.throwIO err
        Right result -> pure result

hostedPrompts :: TestTree
hostedPrompts = Tasty.testGroup "Hosted prompts"
    [ Tasty.HUnit.testCase "keyless typed prompt uses the supplied interpreter" do
        calls <- IORef.newIORef []
        let handler request schema = do
                IORef.modifyIORef' calls (<> [(request, fmap (\_ -> ()) schema)])
                pure (Aeson.Number 7)
        actual <- Exception.try
            (runHosted handler [] "prompt{ model: \"chosen\", effort: \"low\", text: \"question\" } : Natural")
            :: IO (Either SomeException Natural)
        case actual of
            Left err -> Tasty.HUnit.assertFailure (Exception.displayException err)
            Right result -> Tasty.HUnit.assertEqual "checked answer" 7 result
        requests <- IORef.readIORef calls
        Tasty.HUnit.assertEqual "typed request and output schema"
            [(HostPrompt "chosen" "low" "question", Type.Scalar () Monotype.Natural)] requests
    , Tasty.HUnit.testCase "conditional lambda chains two calls within the scope" do
        calls <- IORef.newIORef []
        let handler request _ = do
                IORef.modifyIORef' calls (<> [request])
                pure case request of
                    HostPrompt "writer" "low" "evidence" -> Aeson.String "proposal"
                    HostPrompt "reviewer" "high" "proposal" -> Aeson.Bool True
                    other -> error ("Unexpected host request: " <> show other)
        let program = Text.concat
                [ "(\\input -> "
                , "let context = input.context "
                , "let proposal = if input.unresolved then prompt{ model: \"writer\", effort: \"low\", text: context } : Text else input.known "
                , "let review = if input.unresolved then prompt{ model: \"reviewer\", effort: \"high\", text: proposal } : Bool else true "
                , "in if review then proposal else input.known) "
                , "{ context: \"evidence\", known: \"known\", unresolved: "
                , "unresolved }"
                ]
        known <- runHosted handler ["unresolved" Interpret.<~ False] program :: IO Text
        Tasty.HUnit.assertEqual "deterministic answer" "known" known
        Tasty.HUnit.assertEqual "zero calls on deterministic path" [] =<< IORef.readIORef calls
        actual <- runHosted handler ["unresolved" Interpret.<~ True] program :: IO Text
        Tasty.HUnit.assertEqual "reviewed proposal" "proposal" actual
        Tasty.HUnit.assertEqual "chained arguments and chosen profiles"
            [HostPrompt "writer" "low" "evidence", HostPrompt "reviewer" "high" "proposal"]
            =<< IORef.readIORef calls
    , Tasty.HUnit.testCase "both branches check before any host call" do
        calls <- IORef.newIORef (0 :: Int)
        let handler (_ :: HostPrompt) _ = do
                IORef.modifyIORef' calls (+ 1)
                pure (Aeson.Number 1)
        result <- Exception.try (runHosted handler []
            "if true then (prompt{ model: \"m\", effort: \"low\", text: \"x\" } : Natural) else (prompt{ model: true, effort: \"low\", text: \"x\" } : Natural)")
            :: IO (Either Infer.TypeInferenceError Natural)
        case result of
            Left (Infer.NotSubtype actual expected) -> do
                Tasty.HUnit.assertEqual "bad argument" Monotype.Bool (Type.scalar actual)
                Tasty.HUnit.assertEqual "required argument" Monotype.Text (Type.scalar expected)
            Left err -> Tasty.HUnit.assertFailure (show err)
            Right value -> Tasty.HUnit.assertFailure ("Unexpected success: " <> show value)
        Tasty.HUnit.assertEqual "no effects before typechecking" 0 =<< IORef.readIORef calls
    , Tasty.HUnit.testCase "host JSON must satisfy the output type" do
        let handler (_ :: HostPrompt) _ = pure (Aeson.String "wrong")
        result <- Exception.try (runHosted handler []
            "prompt{ model: \"m\", effort: \"low\", text: \"x\" } : Natural")
            :: IO (Either Infer.TypeInferenceError Natural)
        case result of
            Left (Infer.NotSubtype actual expected) -> do
                Tasty.HUnit.assertEqual "actual JSON type" Monotype.Text (Type.scalar actual)
                Tasty.HUnit.assertEqual "requested type" Monotype.Natural (Type.scalar expected)
            Left err -> Tasty.HUnit.assertFailure (show err)
            Right value -> Tasty.HUnit.assertFailure ("Unexpected success: " <> show value)
    , Tasty.HUnit.testCase "nested scopes restore handlers with different argument types" do
        let outer (_ :: HostPrompt) _ = pure (Aeson.Number 1)
        let inner (_ :: Text) _ = pure (Aeson.Number 2)
        let input = Code "(nested scope)" "prompt{ model: \"m\", effort: \"low\", text: \"x\" } : Natural"
        let status = Grace.Status{ count = 0, context = [] }
        values <- Grace.evalGrace input status (Grace.withPrompt outer do
            (_, first) <- Interpret.interpretWith [] Nothing
            second <- Grace.withPrompt inner (Reader.local
                (\_ -> Code "(inner scope)" "prompt \"inner\" : Natural")
                (snd <$> Interpret.interpretWith [] Nothing))
            (_, third) <- Interpret.interpretWith [] Nothing
            pure [Decode.decode first, Decode.decode second, Decode.decode third])
        Tasty.HUnit.assertEqual "lexical scopes" [Right 1, Right 2, Right 1]
            (values :: [Either Decode.DecodingError Natural])
    , Tasty.HUnit.testCase "concurrent scopes do not share handlers" do
        ready <- MVar.newEmptyMVar
        gate <- MVar.newEmptyMVar
        done <- MVar.newEmptyMVar
        let handler value (_ :: HostPrompt) _ = do
                MVar.putMVar ready ()
                MVar.takeMVar gate
                pure (Aeson.Number value)
        let start value = Concurrent.forkIO do
                result <- Exception.try (runHosted (handler value) []
                    "prompt{ model: \"m\", effort: \"low\", text: \"x\" } : Natural")
                    :: IO (Either SomeException Natural)
                MVar.putMVar done result
        first <- start 1
        second <- start 2
        outcome <- Exception.finally
            (Timeout.timeout 5000000 do
                MVar.takeMVar ready
                MVar.takeMVar ready
                MVar.putMVar gate ()
                MVar.putMVar gate ()
                results <- sequence [MVar.takeMVar done, MVar.takeMVar done]
                pure (traverse (either (Left . Exception.displayException) Right) results))
            (mapM_ Concurrent.killThread [first, second])
        case outcome of
            Just (Right values) -> Tasty.HUnit.assertEqual "independent answers" [1, 2] (List.sort values)
            other -> Tasty.HUnit.assertFailure (show other)
    , Tasty.HUnit.testCase "host interpreter refuses generated Grace code before calling" do
        calls <- IORef.newIORef (0 :: Int)
        let handler (_ :: Text) _ = do
                IORef.modifyIORef' calls (+ 1)
                pure (Aeson.Number 1)
        result <- Exception.try (runHosted handler [] "import prompt \"generate code\" : Natural")
            :: IO (Either Grace.UnsupportedPromptImport Natural)
        case result of
            Left Grace.UnsupportedPromptImport -> pure ()
            Right _ -> Tasty.HUnit.assertFailure "Unexpected generated-code execution"
        Tasty.HUnit.assertEqual "no call for generated code" 0 =<< IORef.readIORef calls
    , Tasty.HUnit.testCase "missing output schema never falls back to the API interpreter" do
        calls <- IORef.newIORef (0 :: Int)
        let handler (_ :: Natural) _ = do
                IORef.modifyIORef' calls (+ 1)
                pure (Aeson.Number 1)
        let input = Code "(missing schema)" ""
        let status = Grace.Status{ count = 0, context = [] }
        let expression = Syntax.Prompt
                { location = Unknown, import_ = False, schema = Nothing
                , arguments = Syntax.Scalar{ location = Unknown, scalar = Syntax.Natural 1 }
                }
        result <- Exception.try (Grace.evalGrace input status
            (Grace.withPrompt handler (Normalize.evaluate [] expression)))
            :: IO (Either Normalize.MissingSchema (Value.Value Location))
        case result of
            Left Normalize.MissingSchema -> pure ()
            Right _ -> Tasty.HUnit.assertFailure "Unexpected success without a schema"
        Tasty.HUnit.assertEqual "no call without a schema" 0 =<< IORef.readIORef calls
    , Tasty.HUnit.testCase "default interpreter still requires an API key" do
        result <- Exception.try (Interpret.load
            (Code "(default prompt)" "prompt{ model: \"m\", text: \"x\" } : Natural"))
            :: IO (Either Infer.TypeInferenceError Natural)
        case result of
            Left (Infer.RecordTypeMismatch _ _ fields) -> Tasty.HUnit.assertEqual "missing credential" ["key"] fields
            Left err -> Tasty.HUnit.assertFailure (show err)
            Right value -> Tasty.HUnit.assertFailure ("Unexpected success: " <> show value)
    ]

data DecodingError = TypeError | RangeError deriving stock (Eq, Show)

decode :: FromGrace a => Value.Value () -> Either DecodingError a
decode value = case Decode.decode (fmap (\_ -> Unknown) value) of
    Left  Decode.TypeError{ }  -> Left TypeError
    Left  Decode.RangeError{ } -> Left RangeError
    Right a                    -> Right a

decodeWithTypeError :: TestTree
decodeWithTypeError = Tasty.HUnit.testCase "load code with type error" do
    let actual₀ :: Either DecodingError Bool
        actual₀ = decode (Value.Scalar () (Syntax.Natural 2))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₀

    let actual₁ :: Either DecodingError T0
        actual₁ = decode (Value.Alternative () "C1" (Value.Record () mempty))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₁

    let actual₂ :: Either DecodingError Natural
        actual₂ = decode (Value.Scalar () (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₂

    let actual₃ :: Either DecodingError Integer
        actual₃ = decode (Value.Scalar () (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₃

    let actual₄ :: Either DecodingError Text
        actual₄ = decode (Value.Scalar () (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₄

    let actual₅ :: Either DecodingError Key
        actual₅ = decode (Value.Scalar () (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₅

    let actual₆ :: Either DecodingError Value
        actual₆ = decode (Value.Lambda () [] (Value.Name () "x" Nothing) Syntax.Variable{ location = (), name = "x" })

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₆

    let actual₇ :: Either DecodingError (Seq Bool)
        actual₇ = decode (Value.Scalar () (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₇

    let actual₈ :: Either DecodingError Scientific
        actual₈ = decode (Value.Scalar () (Syntax.Bool False))

    Tasty.HUnit.assertEqual "" (Left TypeError) actual₈

decodeWithRangeError :: TestTree
decodeWithRangeError = Tasty.HUnit.testCase "load code with range error" do
    let actual₀ :: Either DecodingError Word8
        actual₀ = decode (Value.Scalar () (Syntax.Natural 256))

    Tasty.HUnit.assertEqual "" (Left RangeError) actual₀
