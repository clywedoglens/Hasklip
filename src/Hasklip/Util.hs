{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Util where

import Hasklip.Types (
    Task(..)
    , TaskError(..)
    , ValidatedTask(..)
    , QueryInfo(..)
    , ClipFields(..)
    , TagFields(..)
    , ManageFields(..)
    , ClipArbeit(..)
    , ArbeitResult(..)
    , TaskResult(..))

import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

nonEmpty :: T.Text -> Bool
nonEmpty = not . T.null

displayText :: T.Text -> IO ()
displayText = putStrLn . T.unpack

displayError :: T.Text -> IO ()
displayError = (hPutStrLn stderr) . T.unpack . ("ERROR: " <>)

taskId :: Task -> T.Text
taskId (ClipTask i _ _ _ _ _ _) = i
taskId (TagTask i _ _)          = i
taskId (ManageTask i _ _ )      = i

taskDesc :: Task -> T.Text
taskDesc (ClipTask i _ _ _ _ _ _) = "Clip task id " <> i
taskDesc (TagTask i _ _)          = "Tag task id " <> i
taskDesc (ManageTask i _ _ )      = "Manage task id " <> i

summarizeValidatedTasks :: [ValidatedTask] -> T.Text
summarizeValidatedTasks vs = T.intercalate "\n" [validatedTaskSummary v | v <- vs]
  where
    errors es              = T.intercalate "\n" $ map errorDesc es
    errorDesc e            = "-" <> (taskError_description e)
    validatedTaskSummary v =
        case v of
            ValidTask t      -> (taskDesc t) <> " was validated successfully"
            InvalidTask t es -> (taskDesc t) <> " had these errors:\n" <> errors es

taskToQuery :: Task -> QueryInfo
taskToQuery (ClipTask tid _ o s e g tgs) = ClipQuery $ ClipFields tid o (e-s) g tgs
taskToQuery (TagTask tid p tgs)          = TagQuery $ TagFields tid p tgs
taskToQuery (ManageTask tid p o)         = ManageQuery $ ManageFields tid p o

matchTaskId :: Task -> TaskResult -> Bool
matchTaskId t r = 
    let
        taskId   = case t of
            ClipTask i _ _ _ _ _ _ -> i
            TagTask i _ _          -> i
            ManageTask i _ _       -> i
        resultId = case r of
            TaskSuccess i _ -> i
            TaskFailure i _ -> i
    in
        taskId == resultId

matchClipId :: ClipArbeit -> ArbeitResult -> Bool
matchClipId c r = 
    let
        clipId   = clipArbeitId c
        resultId = case r of
            (Erfolg i)   -> i
            (Fehler i _) -> i
    in
        clipId == resultId

summarizeClipResults :: [(ClipArbeit, ArbeitResult)] -> T.Text
summarizeClipResults cs = T.intercalate "\n" [summarizeClipResult c r | (c, r) <- cs]
  where
    clipText c                           = "Clip Task " <> (clipArbeitId c)
    summarizeClipResult c (Erfolg _)     = (clipText c) <> " successful conversion."
    summarizeClipResult c (Fehler _ msg) = (clipText c) <> " failed conversion.\n" <> msg

summarizeTaskResults :: [(Task, TaskResult)] -> T.Text
summarizeTaskResults ts = T.intercalate "\n" [summarizeTaskResult t r | (t, r) <- ts]
  where
    summarizeTaskResult t (TaskSuccess _ msg) = 
      (taskDesc t) <> " was successful.\n" <> msg
    summarizeTaskResult t (TaskFailure _ msg) =
      (taskDesc t) <> " failed.\n" <> msg

partitionValidated :: [ValidatedTask] -> ([Task], [ValidatedTask])
partitionValidated ts = ([t | (ValidTask t) <- ts], [i | i@(InvalidTask _ _) <- ts])

partitionTasks :: [Task] -> ([Task], [Task], [Task])
partitionTasks = foldr go ([], [], [])
  where 
    go t@(ClipTask _ _ _ _ _ _ _) (cs, ts, ms) = (t:cs, ts, ms)
    go t@(TagTask _ _ _) (cs, ts, ms)          = (cs, t:ts, ms)
    go t@(ManageTask _ _ _) (cs, ts, ms)       = (cs, ts, t:ms)