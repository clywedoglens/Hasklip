{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Processor where

import Hasklip.Types
import Hasklip.Parser (readArbeitResults)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode)
import System.Timeout (timeout)

processTasks :: Config -> [Task] -> IO T.Text
processTasks config ts = do
    let 
        (cs, gs, ms) = partitionTasks ts
        clipArbeit = [ClipArbeit tid i o s e (title g) | (ClipTask tid i o s e g _) <- cs]
        -- mgmtArbeit = [MgmtArbeit tid p o | (ManageTask tid p o) <- ms]
    clipResult <- processClipArbeit clipArbeit
    -- mgmtResult <- processMgmtArbeit mgmtArbeit
    let
        successClips = [t | t@(ClipTask tid _ _ _ _ _ _) <- cs, a@(Erfolg aid) <- clipResult, aid == tid]
    dbResult <- updateDatabase $ map taskToQuery $ concat [successClips, gs, ms]
    return dbResult
  where
    title (Just s) = s
    title Nothing  = ""
    taskToQuery (ClipTask tid _ o s e g tgs) = ClipQuery $ ClipFields o (e-s) g tgs
    taskToQuery (TagTask tid p tgs)          = TagQuery $ TagFields p tgs
    taskToQuery (ManageTask tid p o)         = ManageQuery $ ManageFields p o

partitionTasks :: [Task] -> ([ArbeitInfo], [Task])
partitionTasks = foldr go ([], [], [])
  where 
    go t@(ClipTask _ _ _ _ _ _ _) (cs, ts, ms) = (t:cs, ts, ms)
    go t@(TagTask _ _ _) (cs, ts, ms)          = (cs, t:ts, ms)
    go t@(ManageTask _ _ _) (cs, ts, ms)       = (cs, ts, t:ms)

processClipArbeit :: [ClipArbeit] -> IO [ArbeitResult]
processClipArbeit cs = do
    let jsonData = encode cs
        pipePath = "/tmp/cliparbeitinfo.fifo"
    BL.writeFile pipePath jsonData
    maybeResult <- timeout (60 * 1000000) readClipArbeitResults
    case maybeResult of
        Nothing ->
            return []
        Just (Left parseErr) ->
            return []
        Just (Right results) ->
            return $ results

processMgmtArbeit :: [MgmtArbeit] -> IO [ArbeitResult]
processMgmtArbeit ms = do
    return $ [] 
