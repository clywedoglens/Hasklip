{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Hasklip.Parser (readManifest)
import Hasklip.Types 
import Hasklip.Validator (validateManifest, partitionValidated, validatedTasksSummary)
import Hasklip.Processor (processTasks)

import qualified Data.Text as T

main :: IO ()
main = do
    let config = Config { dbPath = "src/Hasklip/clips.db"
                        , clipsDir = "/home/george/Videos/Clips"
                        }
    result <- readManifest "manifest.yaml"
    let displayError = putStr . T.unpack . (T.append "ERROR: ")
    case result of
        Left (FileError e)  -> displayError e
        Left (ParserError e) -> displayError e
        Right manifest -> do
            let validated        = validateManifest manifest
                (valid, invalid) = partitionValidated validated
            (putStrLn . T.unpack) $ validatedTasksSummary validated
            results <- processTasks config valid
            (putStrLn . T.unpack) results
         
