{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Processor where

import Hasklip.Database (updateDatabase)
import Hasklip.Parser (readClipArbeitResults)
import Hasklip.Types (Config, Task(..), TaskResult(..), ClipArbeit(..), ArbeitResult(..))
import Hasklip.Util (partitionTasks, taskToQuery, displayText, matchClipId, summarizeClipResults)

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import System.Timeout (timeout)

processTasks :: Config -> [Task] -> IO [TaskResult]
processTasks config ts = do
    let
        (cs, gs, ms) = partitionTasks ts
        clipArbeit = [ClipArbeit t i o s e (title g) | (ClipTask t i o s e g _) <- cs]
        mgmtArbeit = [MgmtArbeit t o | (ManageTask t o) <- ms]
    rs <- processClipArbeit clipArbeit
    mrs <- processMgmtArbeit mgmtArbeit
    let pairs  = [(c, r) | c <- clipArbeit, r <- rs, matchClipId c r]
        cs'    = [c | c@(ClipTask t _ _ _ _ _ _) <- cs, (Erfolg i) <- rs, t == i]
        fs     = [TaskFailure t e | (Fehler t e) <- rs]
        header = "CLIP ARBEITER RESULTS\n"
    displayText $ header <> (summarizeClipResults pairs)
    dbResults <- updateDatabase config $ map taskToQuery $ cs' ++ gs ++ ms
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

-- processMgmtArbeit :: [MgmtArbeit] -> IO [ArbeitResult]
-- processMgmtArbeit ms = do
--     return $ [] 
