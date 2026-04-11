{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Processor where

import Hasklip.Database (updateDatabase)
import Hasklip.Parser (readClipArbeitResults)
import Hasklip.Types (Config, Task(..), TaskResult(..), ClipArbeit(..), ArbeitResult(..))
import Hasklip.Util (partitionTasks, taskToQuery, displayText, matchClipId, summarizeClipResults)

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
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
        ms'    = [m | m@(ManageTask t _) <- ms, (Erfolg i) <- mrs, t == i]
        rs'    = rs ++ mrs
        fs     = [TaskFailure t e | (Fehler t e) <- rs']
    dbResults <- updateDatabase config $ map taskToQuery $ cs' ++ gs ++ ms'
    return $ dbResults ++ fs
  where
    title (Just s)   = s
    title Nothing    = ""

processClipArbeit :: [ClipArbeit] -> IO [ArbeitResult]
processClipArbeit cs = do
    let jsonData = encode cs
        pipePath = "/tmp/cliparbeitinfo.fifo"
    BL.writeFile pipePath jsonData
    maybeResult <- timeout (60 * 2000000) readClipArbeitResults
    case maybeResult of
        Nothing ->
            return []
        Just (Left _) ->
            return []
        Just (Right results) ->
            return $ results

processMgmtArbeit :: [MgmtArbeit] -> IO [ArbeitResult]
processMgmtArbeit = mapM process
  where
    process (MgmtArbeit i f) = deleteClip i f

deleteClip :: T.Text -> FilePath -> IO ArbeitResult
deleteClip i f = do
    removeFile f `catch` handleExists
  where 
    handleExists e
        | isDoesNotExistError e = return $ Fehler i "File does not exist"
        | otherwise

