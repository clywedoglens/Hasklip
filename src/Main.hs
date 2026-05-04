{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Hasklip.Parser (readConfig, readManifest)
import Hasklip.Types (
    Config(..)
    , Manifest(..)
    , ParseError(..)
    , ValidatedTask(..)
    , TaskError(..)
    , TaskResult(..))
import Hasklip.Validator (validateManifest)
import Hasklip.Processor (processTasks)
import Hasklip.Util (
    displayText
    , displayError
    , taskId
    , partitionValidated
    , matchTaskId
    , summarizeTaskResults)

import qualified Data.Text as T

main :: IO ()
main = do
    -- let config = Config { config_dbPath = "/home/george/Projects/Hasklip/src/Hasklip/clips.db"
    --                     , config_clipsDir = "/home/george/Videos/Clips/"
    --                     , config_requestPipe   = "/tmp/cliparbeitinfo.fifo"
    --                     , config_responsePipe = "/tmp/cliparbeitresults.fifo"
    --                     }
    cfgResult <- readConfig "config.yaml"
    case cfgResult of
        Left err     -> displayError $ errorMsg err
        Right config -> do
            result <- readManifest $ config_manifest config
            case result of
                Left err  -> displayError $ errorMsg err
                Right manifest -> do
                    let validated        = validateManifest manifest
                        (valid, invalid) = partitionValidated validated
                        invalidResults   = [invalidFail t es |(InvalidTask t es) <- invalid]
                    results <- processTasks config valid
                    let finalRes = results ++ invalidResults
                        tasks    = manifest_tasks manifest
                        pairs    = [(t, r) | t <- tasks, r <- finalRes, matchTaskId t r]
                        header   = "FINAL RESULTS\n"
                    displayText $ header <> (summarizeTaskResults pairs)
  where
    errorMsg (FileError e)   = e
    errorMsg (ParserError e) = e
    errString e              = map taskError_description e
    invalidFail t e          = TaskFailure (taskId t) (T.intercalate "\n" $ errString e)
