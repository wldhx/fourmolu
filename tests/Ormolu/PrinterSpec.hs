{-# LANGUAGE TemplateHaskell #-}

module Ormolu.PrinterSpec (spec) where

import Control.Exception
import Control.Monad
import Data.List (isSuffixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Ormolu
import Ormolu.CLI.TH
import Ormolu.Config
import Ormolu.Utils.IO
import Path
import Path.IO
import System.Environment (lookupEnv)
import qualified System.FilePath as F
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

bundles :: [(String, PrinterOptsTotal)]
bundles =
  [ ( "ormolu",
      PrinterOpts
        { poIndentation = pure 2,
          poCommaStyle = pure Trailing,
          poIndentWheres = pure True,
          poOneLevelIfs = pure False,
          poLetNewline = pure False,
          poRecordBraceSpace = pure True,
          poDiffFriendlyImportExport = pure False,
          poRespectful = pure False,
          poHaddockStyle = pure HaddockSingleLine,
          poNewlinesBetweenDecls = pure 1,
          poAddSpaceBetweenImportedTypeAndConstructor = pure False,
          poRecordConstructorsHanging = pure False
        }
    ),
    ("fourmolu", defaultPrinterOpts)
  ]

singleOpts :: [PrinterOptsTotal]
singleOpts =
  [ defaultPrinterOpts {poIndentation = pure 2},
    defaultPrinterOpts {poCommaStyle = pure Trailing},
    defaultPrinterOpts {poIndentWheres = pure True},
    defaultPrinterOpts {poOneLevelIfs = pure True},
    defaultPrinterOpts {poLetNewline = pure True},
    defaultPrinterOpts {poRecordBraceSpace = pure True},
    defaultPrinterOpts {poDiffFriendlyImportExport = pure False},
    defaultPrinterOpts {poRespectful = pure False},
    defaultPrinterOpts {poHaddockStyle = pure HaddockSingleLine},
    defaultPrinterOpts {poNewlinesBetweenDecls = pure 2},
    defaultPrinterOpts {poAddSpaceBetweenImportedTypeAndConstructor = pure False},
    defaultPrinterOpts {poRecordConstructorsHanging = pure True}
  ]

getPoLabel :: PrinterOptsTotal -> String
getPoLabel po =
  case filter (isJust . snd) $ zip $$poFieldNames ($displayCustomPrinterOpts po) of
    [(k, Just v)] -> k <> "=" <> v
    _ -> error "Failed to generate PrinterOpts label"

spec :: Spec
spec = do
  es <- runIO locateExamples
  sequence_ $ checkExample <$> [(po, n, "-bundle=" <> n) | (n, po) <- bundles] <*> es
  sequence_ $ checkExample <$> [(po, getPoLabel po, "-option=" <> getPoLabel po) | po <- singleOpts] <*> es

-- | Check a single given example.
checkExample :: (PrinterOptsTotal, String, String) -> Path Rel File -> Spec
checkExample (po, label, suffix) srcPath' = it (fromRelFile srcPath' ++ " works (" ++ label ++ ")") . withNiceExceptions $ do
  let srcPath = examplesDir </> srcPath'
      inputPath = fromRelFile srcPath
      config =
        defaultConfig
          { cfgPrinterOpts = po,
            cfgSourceType = detectSourceType inputPath
          }
  expectedOutputPath <- deriveOutput suffix srcPath
  -- 1. Given input snippet of source code parse it and pretty print it.
  -- 2. Parse the result of pretty-printing again and make sure that AST
  -- is the same as AST of the original snippet. (This happens in
  -- 'ormoluFile' automatically.)
  formatted0 <- ormoluFile config inputPath
  -- 3. Check the output against expected output. Thus all tests should
  -- include two files: input and expected output.
  when shouldRegenerateOutput $
    T.writeFile (fromRelFile expectedOutputPath) formatted0
  expected <- readFileUtf8 $ fromRelFile expectedOutputPath
  shouldMatch False formatted0 expected
  -- 4. Check that running the formatter on the output produces the same
  -- output again (the transformation is idempotent).
  formatted1 <- ormolu config "<formatted>" (T.unpack formatted0)
  shouldMatch True formatted1 formatted0

-- | Build list of examples for testing.
locateExamples :: IO [Path Rel File]
locateExamples =
  filter isInput . snd <$> listDirRecurRel examplesDir

-- | Does given path look like input path (as opposed to expected output
-- path)?
isInput :: Path Rel File -> Bool
isInput path =
  let s = fromRelFile path
      (s', exts) = F.splitExtensions s
   in exts `elem` [".hs", ".hsig"] && not ("-out" `isSuffixOf` s')

-- | For given path of input file return expected name of output.
deriveOutput :: String -> Path Rel File -> IO (Path Rel File)
deriveOutput suffix path =
  parseRelFile $
    F.addExtension (radical ++ suffix ++ "-out") exts
  where
    (radical, exts) = F.splitExtensions (fromRelFile path)

-- | A version of 'shouldBe' that is specialized to comparing 'Text' values.
-- It also prints multi-line snippets in a more readable form.
shouldMatch :: Bool -> Text -> Text -> Expectation
shouldMatch idempotenceTest actual expected =
  when (actual /= expected) . expectationFailure $
    unlines
      [ ">>>>>>>>>>>>>>>>>>>>>> expected (" ++ pass ++ "):",
        T.unpack expected,
        ">>>>>>>>>>>>>>>>>>>>>> but got:",
        T.unpack actual
      ]
  where
    pass =
      if idempotenceTest
        then "idempotence pass"
        else "first pass"

examplesDir :: Path Rel Dir
examplesDir = $(mkRelDir "data/examples")

-- | Inside this wrapper 'OrmoluException' will be caught and displayed
-- nicely using 'displayException'.
withNiceExceptions ::
  -- | Action that may throw the exception
  Expectation ->
  Expectation
withNiceExceptions m = m `catch` h
  where
    h :: OrmoluException -> IO ()
    h = expectationFailure . displayException

shouldRegenerateOutput :: Bool
shouldRegenerateOutput =
  unsafePerformIO $ isJust <$> lookupEnv "ORMOLU_REGENERATE_EXAMPLES"
{-# NOINLINE shouldRegenerateOutput #-}
