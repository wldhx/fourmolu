{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import Options.Applicative
import Ormolu
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromJust)
import Ormolu.CLI
import Ormolu.CLI.TH

-- | Entry point of the program.
main :: IO ()
main = do
  let x = execParserPure defaultPrefs (info printerOptsParser mempty) ["--respectful", "false"]
  --handleParseResult  x
  let y = fromJust $ getParseResult x
  print y
  print $ fillMissingPrinterOpts y defaultPrinterOpts 
  forM_ (toCLI <$> $$poBoolFieldNames) putStrLn 
  --forM_ ($$poFieldTypes) putStrLn 

-- main = withPrettyOrmoluExceptions $ do
--   opts@Opts {..} <- execParser optsParserInfo
--   let formatStdIn = do
--         cur <- getCurrentDirectory
--         cfg <- mkConfig cur opts
--         formatOne optMode cfg Nothing
--   case optInputFiles of
--     [] -> formatStdIn
--     ["-"] -> formatStdIn
--     [x] -> flip (formatOne optMode) (Just x) =<< mkConfig x opts
--     xs@(x : _) -> do
--       cfg <- mkConfig x opts
--       -- It is possible to get IOException, error's and 'OrmoluException's
--       -- from 'formatOne', so we just catch everything.
--       errs <-
--         lefts
--           <$> mapM
--             (try @SomeException . formatOne optMode cfg . Just)
--             (sort xs)
--       unless (null errs) $ do
--         mapM_ (hPutStrLn stderr . displayException) errs
--         exitWith (ExitFailure 102)

-- | Format a single input.
formatOne ::
  -- | Mode of operation
  Mode ->
  -- | Configuration
  Config RegionIndices ->
  -- | File to format or stdin as 'Nothing'
  Maybe FilePath ->
  IO ()
formatOne mode config = \case
  Nothing -> do
    r <- ormoluStdin config
    case mode of
      Stdout -> TIO.putStr r
      _ -> do
        hPutStrLn
          stderr
          "This feature is not supported when input comes from stdin."
        -- 101 is different from all the other exit codes we already use.
        exitWith (ExitFailure 101)
  Just inputFile -> do
    originalInput <- TIO.readFile inputFile
    formattedInput <- ormoluFile config inputFile
    case mode of
      Stdout ->
        TIO.putStr formattedInput
      InPlace -> do
        -- Only write when the contents have changed, in order to avoid
        -- updating the modified timestamp if the file was already correctly
        -- formatted.
        when (formattedInput /= originalInput) $
          TIO.writeFile inputFile formattedInput
      Check -> do
        when (formattedInput /= originalInput) $
          -- 100 is different to all the other exit code that are emitted
          -- either from an 'OrmoluException' or from 'error' and
          -- 'notImplemented'.
          exitWith (ExitFailure 100)


-- | Build the full config, by adding 'PrinterOpts' from a file, if found.
mkConfig :: FilePath -> Opts -> IO (Config RegionIndices)
mkConfig path Opts {..} = do
  filePrinterOpts <-
    loadConfigFile path >>= \case
      ConfigLoaded f po -> do
        hPutStrLn stderr $ "Loaded config from: " <> f
        printDebug $ show po
        return $ Just po
      ConfigParseError f (_pos, err) -> do
        -- we ignore '_pos' due to the note on 'Data.YAML.Aeson.decode1'
        hPutStrLn stderr $
          unlines
            [ "Failed to load " <> f <> ":",
              "  " <> err
            ]
        exitWith $ ExitFailure 400
      ConfigNotFound searchDirs -> do
        printDebug
          . unlines
          $ ("No " ++ show configFileName ++ " found in any of:") :
          map ("  " ++) searchDirs
        return Nothing
  return $
    optConfig
      { cfgPrinterOpts =
          fillMissingPrinterOpts
            (optPrinterOpts <> fromMaybe mempty filePrinterOpts)
            (cfgPrinterOpts optConfig)
      }
  where
    printDebug = when (cfgDebug optConfig) . hPutStrLn stderr
