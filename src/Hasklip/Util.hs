{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Util where

import Hasklip.Types (
    ParseError(..)
    , Task(..)
    , ManageOp(..)
    , ListOp(..)
    , TaskError(..)
    , ValidatedTask(..)
    , QueryInfo(..)
    , ClipFields(..)
    , TagFields(..)
    , ManageFields(..)
    , ListFields(..)
    , ClipArbeit(..)
    , ArbeitResult(..)
    , ListResult(..)
    , TaskResult(..))

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Log.FastLogger (LoggerSet, pushLogStrLn, toLogStr)
import System.IO (hPutStrLn, stderr)

nonEmpty :: T.Text -> Bool
nonEmpty = not . T.null

displayText :: T.Text -> IO ()
displayText = putStrLn . T.unpack

displayError :: T.Text -> IO ()
displayError = (hPutStrLn stderr) . T.unpack . ("ERROR: " <>)

parseErrorMessage :: ParseError -> T.Text
parseErrorMessage (FileError e)   = e
parseErrorMessage (ParserError e) = e

taskId :: Task -> T.Text
taskId (ClipTask i _ _ _ _ _ _) = i
taskId (TagTask i _ _)          = i
taskId (ManageTask i _)         = i
taskId (ListTask i _)           = i

taskDesc :: Task -> T.Text
taskDesc (ClipTask i _ _ _ _ _ _) = "Clip task id " <> i
taskDesc (TagTask i _ _)          = "Tag task id " <> i
taskDesc (ManageTask i o)         = "Manage task id " <> i <> (mgmtOpDesc o)
taskDesc (ListTask i o)           = "List task id " <> i <> (listOpDesc o)

mgmtOpDesc :: ManageOp -> T.Text
mgmtOpDesc (DeleteClip _)       = " Delete clip"
mgmtOpDesc (DeleteTag _)        = " Delete tag"
mgmtOpDesc (RemoveClipTags _ _) = " Remove clip tags"

listOpDesc :: ListOp -> T.Text
listOpDesc (TagSearch tgs) = " Tag search"
listOpDesc (NameSearch n)  = " Name search"

summarizeValidatedTasks :: [ValidatedTask] -> T.Text
summarizeValidatedTasks vs = T.intercalate "\n" [validatedTaskSummary v | v <- vs]
  where
    errors es              = T.intercalate "\n" $ map errorDesc es
    errorDesc e            = "-" <> (taskError_description e)
    validatedTaskSummary v = case v of
        ValidTask t      -> (taskDesc t) <> " was validated successfully"
        InvalidTask t es -> (taskDesc t) <> " had these errors:\n" <> errors es

partitionDBTasks :: [ArbeitResult] -> [Task] -> ([Task], [TaskResult])
partitionDBTasks rs = foldr go ([], [])
  where
    findResult tid             = find (compareId tid) rs
    compareId tid (Erfolg i)   = tid == i
    compareId tid (Fehler i _) = tid == i
    notFound i                 = TaskFailure i msg
    msg                        = "ClipTask not found in result"
    arbeitResult i t (e, f)    = case findResult i of
        Nothing           -> (e, (notFound i):f)
        Just (Erfolg _)   -> (t:e, f)
        Just (Fehler _ m) -> (e, (TaskFailure i m):f)
    go t@(ClipTask tid _ _ _ _ _ _) acc        = arbeitResult tid t acc
    go t@(ManageTask tid (DeleteClip _)) acc   = arbeitResult tid t acc
    go t (e, f)                                = (t:e, f)

taskToQuery :: Task -> QueryInfo
taskToQuery (ClipTask tid _ o s e g tgs) = ClipQuery $ ClipFields tid o (e-s) g tgs
taskToQuery (TagTask tid p tgs)          = TagQuery $ TagFields tid p tgs
taskToQuery (ManageTask tid o)           = ManageQuery $ case o of
    (DeleteClip f)        -> DeleteClipFields tid f
    (DeleteTag t)         -> DeleteTagFields tid t
    (RemoveClipTags f ts) -> RemoveClipTagsFields tid f ts
taskToQuery (ListTask tid o)             = ListQuery $ case o of
    (TagSearch tgs) -> TagSearchFields tid tgs
    (NameSearch n)  -> NameSearchFields tid n

matchTaskId :: Task -> TaskResult -> Bool
matchTaskId t r = 
    let tid      = taskId t
        resultId = case r of
            (TaskSuccess i _) -> i
            (TaskFailure i _) -> i
    in tid == resultId

matchClipId :: ClipArbeit -> ArbeitResult -> Bool
matchClipId c r = 
    let clipId   = clipArbeitId c
        resultId = case r of
            (Erfolg i)   -> i
            (Fehler i _) -> i
    in clipId == resultId

summarizeClipResults :: [(ClipArbeit, ArbeitResult)] -> T.Text
summarizeClipResults cs = T.intercalate "\n" [summarizeClipResult c r | (c, r) <- cs]
  where
    clipText c                           = "Clip Task " <> (clipArbeitId c)
    summarizeClipResult c (Erfolg _)     = (clipText c) <> " successful conversion."
    summarizeClipResult c (Fehler _ msg) = (clipText c) <> " failed conversion.\n" <> msg

summarizeTaskResults :: T.Text -> LoggerSet -> [(Task, TaskResult)] -> IO ()
summarizeTaskResults dir logger = mapM_ summarize
  where
    summarize (t@(ListTask _ _), (TaskSuccess _ l)) = do
        displayText $ dir <> l
        pushLogStrLn logger (toLogStr $ (taskDesc t) <> " was successful.")
    summarize (t, tr)                               = logTask t tr
    logTask t (TaskSuccess _ msg) = do
        pushLogStrLn logger (toLogStr $ (taskDesc t) <> " was successful.\n" <> msg)
    logTask t (TaskFailure _ msg) = do
        pushLogStrLn logger (toLogStr $ (taskDesc t) <> " failed.\n" <> msg)

partitionValidated :: [ValidatedTask] -> ([Task], [ValidatedTask])
partitionValidated ts = ([t | (ValidTask t) <- ts], [i | i@(InvalidTask _ _) <- ts])

partitionTasks :: [Task] -> ([Task], [Task], [Task], [Task])
partitionTasks = foldr go ([], [], [], [])
  where 
    go t@(ClipTask _ _ _ _ _ _ _) (cs, ts, ms, ls) = (t:cs, ts, ms, ls)
    go t@(TagTask _ _ _) (cs, ts, ms, ls)          = (cs, t:ts, ms, ls)
    go t@(ManageTask _ _) (cs, ts, ms, ls)         = (cs, ts, t:ms, ls)
    go t@(ListTask _ _) (cs, ts, ms, ls)           = (cs, ts, ms, t:ls)

buildPlaylist :: [ListResult] -> T.Text
buildPlaylist = T.intercalate "\n" . map buildResult
  where
    buildResult (ListResult fp g) = (gameDir $ fromMaybe "" g) <> fp
    gameDir g                     = if T.null g
                                        then g
                                        else g <> "/"
                                                    