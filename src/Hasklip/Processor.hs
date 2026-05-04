{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Processor where

import Hasklip.Database (updateDatabase)
import Hasklip.Parser (readClipArbeitResults)
import Hasklip.Types (
    Config(..)
    , Task(..)
    , ManageOp(..)
    , TaskResult(..)
    , ClipArbeit(..)
    , MgmtArbeit(..)
    , ArbeitResult(..))
import Hasklip.Util (partitionDBTasks, taskToQuery)

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory (removeFile)
import Control.Exception (try)
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)


processTasks :: Config -> [Task] -> IO [TaskResult]
processTasks config ts = do
    crs <- processClipArbeit config clipArbeit
    mrs <- processMgmtArbeit clipsDir mgmtArbeit
    let rs        = crs ++ mrs
        (ts', ft) = partitionDBTasks rs ts
    dbResults <- updateDatabase config $ map taskToQuery ts'
    return $ dbResults ++ ft
  where
    clipsDir   = config_clipsDir config
    clipArbeit = [ClipArbeit t i o s e g | (ClipTask t i o s e g _) <- ts]
    mgmtArbeit = [MgmtArbeit t f | (ManageTask t (DeleteClip f)) <- ts]

processClipArbeit :: Config -> [ClipArbeit] -> IO [ArbeitResult]
processClipArbeit _ []      = return []
processClipArbeit config cs = do
    BL.writeFile reqPath jsonData
    maybeResult <- timeout timeLimit (readClipArbeitResults resPath)
    case maybeResult of
        Nothing ->
            return []
        Just (Left _) ->
            return []
        Just (Right results) ->
            return results
  where
    jsonData  = encode cs
    timeLimit = 600000000
    reqPath   = config_requestPipe config
    resPath   = config_responsePipe config

processMgmtArbeit :: FilePath -> [MgmtArbeit] -> IO [ArbeitResult]
processMgmtArbeit clipsDir = mapM process
  where
    process (MgmtArbeit i f) = let filePath = clipsDir ++ (T.unpack f)
        in deleteClip i filePath

deleteClip :: T.Text -> FilePath -> IO ArbeitResult
deleteClip i f = do
    result <- try (removeFile $ f) :: IO (Either IOError ())
    case result of
        Left e
            | isDoesNotExistError e -> return $ Fehler i "File does not exist"
            | otherwise             -> return $ Fehler i ((T.pack . show) e)
        Right _                     -> return $ Erfolg i
