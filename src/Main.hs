{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Hasklip.Parser (readManifest)
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
    , summarizeValidatedTasks
    , summarizeTaskResults)

import qualified Data.Text as T

main :: IO ()
main = do
    let config = Config { config_dbPath = "/home/george/Projects/Hasklip/src/Hasklip/clips.db"
                        , config_clipsDir = "/home/george/Videos/Clips"
                        }
    result <- readManifest "manifest.yaml"
    case result of
        Left (FileError e)  -> displayError e
        Left (ParserError e) -> displayError e
        Right manifest -> do
            let validated             = validateManifest manifest
                (valid, invalid)      = partitionValidated validated
                validatedHeader       = "VALIDATION RESULTS\n"
            displayText $ validatedHeader <> (summarizeValidatedTasks validated)
            results <- processTasks config valid
            let finalRes = results ++ [invalidFail t es |(InvalidTask t es) <- invalid]
                tasks    = manifest_tasks manifest
                pairs    = [(t, r) | t <- tasks, r <- finalRes, matchTaskId t r]
                header   = "FINAL RESULTS\n"
            displayText $ header <> (summarizeTaskResults pairs)
  where
    errString e     = map taskError_description e
    invalidFail t e = TaskFailure (taskId t) (T.intercalate "\n" $ errString e)
