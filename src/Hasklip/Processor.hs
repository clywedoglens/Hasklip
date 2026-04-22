{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Processor where

import Hasklip.Database (updateDatabase)
import Hasklip.Parser (readClipArbeitResults)
import Hasklip.Types (
    Config
    , Task(..)
    , ManageOp(..)
    , TaskResult(..)
    , ClipArbeit(..)
    , MgmtArbeit(..)
    , ArbeitResult(..))
import Hasklip.Util (
    partitionTasks
    , taskToQuery
    , displayText
    , matchClipId
    , summarizeClipResults)

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Directory
import Control.Exception (try)
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)


processTasks :: Config -> [Task] -> IO [TaskResult]
processTasks config ts = do
    let
        (cs, gs, ms) = partitionTasks ts
        clipArbeit = [ClipArbeit t i o s e (title g) | (ClipTask t i o s e g _) <- cs]
        mgmtArbeit = [MgmtArbeit t f | (ManageTask t (DeleteClip f)) <- ms]
    rs <- processClipArbeit clipArbeit
    mrs <- processMgmtArbeit mgmtArbeit
    let cs'    = [c | c@(ClipTask t _ _ _ _ _ _) <- cs, (Erfolg i) <- rs, t == i]
        ams    = [m | m@(ManageTask t _) <- ms, (Erfolg i) <- mrs, t == i]
        ms'    = [m | m@(ManageTask _ o) <- ms, nonMgmtArbeit o] ++ ams
        rs'    = rs ++ mrs
        fs     = [TaskFailure t e | (Fehler t e) <- rs']
    dbResults <- updateDatabase config $ map taskToQuery $ cs' ++ gs ++ ms'
    return $ dbResults ++ fs
  where
    title (Just s) = s
    title Nothing  = ""
    nonMgmtArbeit (DeleteClip _)       = False
    nonMgmtArbeit (DeleteTag _)        = True
    nonMgmtArbeit (RemoveClipTags _ _) = True

processClipArbeit :: [ClipArbeit] -> IO [ArbeitResult]
processClipArbeit [] = return $ []
processClipArbeit cs = do
    BL.writeFile pipePath jsonData
    maybeResult <- timeout (60 * 2000000) (readClipArbeitResults resultPath)
    case maybeResult of
        Nothing ->
            return []
        Just (Left _) ->
            return []
        Just (Right results) ->
            return $ results
  where
    jsonData   = encode cs
    pipePath   = "/tmp/cliparbeitinfo.fifo"
    resultPath = "/tmp/cliparbeitresults.fifo"

processMgmtArbeit :: [MgmtArbeit] -> IO [ArbeitResult]
processMgmtArbeit = mapM process
  where
    process (MgmtArbeit i f) = deleteClip i $ T.unpack f

deleteClip :: T.Text -> FilePath -> IO ArbeitResult
deleteClip i f = do
    result <- try (removeFile $ fullPath) :: IO (Either IOError ())
    case result of
        Left e
            | isDoesNotExistError e -> return $ Fehler i "File does not exist"
            | otherwise             -> return $ Fehler i ((T.pack . show) e)
        Right _                     -> return $ Erfolg i
  where
    fullPath = "/home/george/Videos/Clips/" ++ f