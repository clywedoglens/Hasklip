{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Hasklip.Parser (readConfig, readManifest)
import Hasklip.Types (
    Config(..)
    , Manifest(..)
    , ValidatedTask(..)
    , TaskError(..)
    , TaskResult(..))
import Hasklip.Validator (validateManifest)
import Hasklip.Processor (processTasks)
import Hasklip.Util (
    displayText
    , displayError
    , parseErrorMessage
    , taskId
    , partitionValidated
    , matchTaskId
    , summarizeTaskResults)

import qualified Data.Text as T

main :: IO ()
main = do
    cfgResult <- readConfig "config.yaml"
    case cfgResult of
        Left err     -> displayError $ parseErrorMessage err
        Right config -> runHasklip config

runHasklip :: Config -> IO ()
runHasklip config = do 
    manifestResult <- readManifest manifestPath
    case manifestResult of
        Left err  -> displayError $ parseErrorMessage err
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
    manifestPath    = config_manifest config
    errString e     = map taskError_description e
    invalidFail t e = TaskFailure (taskId t) (T.intercalate "\n" $ errString e)
