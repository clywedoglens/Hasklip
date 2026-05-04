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
    , summarizeTaskResults)

import qualified Data.Text as T

main :: IO ()
main = do
    let config = Config { config_dbPath = "/home/george/Projects/Hasklip/src/Hasklip/clips.db"
                        , config_clipsDir = "/home/george/Videos/Clips/"
                        , config_requestPipe   = "/tmp/cliparbeitinfo.fifo"
                        , config_responsePipe = "/tmp/cliparbeitresults.fifo"
                        }
    result <- readManifest "manifest.yaml"
    case result of
        Left (FileError e)  -> displayError e
        Left (ParserError e) -> displayError e
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
    errString e     = map taskError_description e
    invalidFail t e = TaskFailure (taskId t) (T.intercalate "\n" $ errString e)
