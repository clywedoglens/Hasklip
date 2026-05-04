{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Database where

import Hasklip.Types (
    Config(..)
    , TaskResult(..)
    , QueryInfo(..)
    , ClipFields(..)
    , TagFields(..)
    , ManageFields(..)
    , TaskResult(..))
import Hasklip.Util (displayError)

import Control.Monad (forM, forM_)
import Database.SQLite.Simple (
    Connection
    , open
    , execute
    , execute_
    , query
    , changes
    , lastInsertRowId
    , Only(..))
import qualified Data.Text as T

updateDatabase :: Config -> [QueryInfo] -> IO [TaskResult]
updateDatabase config qs = do
    conn <- open db
    execute_ conn "PRAGMA foreign_keys = ON"
    mapM (runQuery conn) qs
  where
    db = config_dbPath config

runQuery :: Connection -> QueryInfo -> IO TaskResult
runQuery conn (ClipQuery f) = insertClip conn f
runQuery conn (TagQuery f) = insertClipTags conn f
runQuery conn (ManageQuery f) = runManageQuery conn f

insertClip :: Connection -> ClipFields -> IO TaskResult
insertClip conn (ClipFields taskId path dur game tags) = do
    gameId <- handleGameId conn game
    let clipParams = (path, gameId, dur)
    execute conn clipQuery clipParams
    count <- changes conn
    if count == 1
        then do
            clipId <- fromIntegral <$> lastInsertRowId conn
            case tags of
                Nothing -> return ()
                Just tagList -> mapM_ (insertTag conn clipId) tagList
            return $ TaskSuccess taskId successMsg
        else
            return $ TaskFailure taskId failMsg
  where 
    clipQuery  = "INSERT INTO clips (file_path, game_id, duration) VALUES (?,?,?) ON CONFLICT(file_path) DO NOTHING"
    successMsg = "Success: Inserted clip"
    failMsg    = "Failed: Could not insert clip"

insertTag :: Connection -> Int -> T.Text -> IO ()
insertTag conn clipId tag = do
    rows <- query conn "SELECT id FROM tags WHERE name = ?" (Only tag) :: IO [Only Int]
    tagId <- case rows of
        [Only tid] -> return $ Just tid
        []         -> do
            execute conn insertTagQuery (Only tag) :: IO ()
            Just <$> fromIntegral <$> lastInsertRowId conn
        _          -> do
            return Nothing
    case tagId of
        Just i -> 
            execute conn insertCTQuery (clipId, i) :: IO ()
        Nothing ->
            displayError $ "Multiple tags with the same name: " <> tag
  where
    insertTagQuery = "INSERT INTO tags (name) VALUES (?) ON CONFLICT(name) DO NOTHING"
    insertCTQuery  = "INSERT INTO clip_tags (clip_id, tag_id) VALUES (?,?)"

insertClipTags :: Connection -> TagFields -> IO TaskResult
insertClipTags conn (TagFields tid fileName tags) = do
    clipId <- findClipIdByFileName conn fileName
    case clipId of
        Just i -> do
            mapM_ (insertTag conn i) tags
            return $ TaskSuccess tid successMsg
        Nothing -> 
            return $ TaskFailure tid failMsg
  where
    successMsg = "Successfully inserted tags"
    failMsg    = "A clip with that file name could not be found"


handleGameId :: Connection -> Maybe T.Text -> IO (Maybe Int)
handleGameId _ Nothing = return Nothing
handleGameId conn (Just name) = do
    rows <- query conn selectGamesQuery (Only name) :: IO [Only Int]
    case rows of
        [Only gameId] -> return $ Just gameId
        []         -> do
            execute conn insertGamesQuery (Only name) :: IO ()
            gameId <- fromIntegral <$> lastInsertRowId conn
            return $ Just gameId
        _          -> do
            displayError $ multipleGamesErr <> name
            return Nothing
  where
    selectGamesQuery = "SELECT id FROM games WHERE name = ?"
    insertGamesQuery = "INSERT INTO games (name) VALUES (?)"
    multipleGamesErr = "Multiple game with the same name: "

findClipIdByFileName :: Connection -> T.Text -> IO (Maybe Int)
findClipIdByFileName conn fileName = do
    rows <- query conn selectClipsQuery (Only fileName) :: IO [Only Int]
    case rows of
        [Only clipId] -> return $ Just clipId
        _             -> return Nothing
  where
    selectClipsQuery = "SELECT id FROM clips WHERE file_path = ?"

findTagIdByName :: Connection -> T.Text -> IO (Maybe Int)
findTagIdByName conn tag = do
    rows <- query conn selectQuery (Only tag) :: IO [Only Int]
    case rows of
        [Only tagId] -> return $ Just tagId
        _            -> return Nothing
  where
    selectQuery = "SELECT id FROM tags WHERE name = ?"

runManageQuery :: Connection -> ManageFields -> IO TaskResult
runManageQuery conn (DeleteClipFields i f)       = deleteClip conn i f
runManageQuery conn (DeleteTagFields i t)        = deleteTag conn i t
runManageQuery conn (RemoveClipTagsFields i f t) = removeClipTags conn i f t

deleteClip :: Connection -> T.Text -> T.Text -> IO TaskResult
deleteClip conn tid fileName = do
    result <- findClipIdByFileName conn fileName
    case result of
        Just clipId -> delete clipId
        Nothing     -> return $ TaskFailure tid failMsg
  where
    delete cid  = do
        execute conn deleteQuery (Only cid)
        return $ TaskSuccess tid successMsg
    deleteQuery = "DELETE FROM clips WHERE id = ?"
    successMsg  = "Successfully deleted clip"
    failMsg     = "There is no clip with that filename"

deleteTag :: Connection -> T.Text -> T.Text -> IO TaskResult
deleteTag conn tid tagName = do
    result <- findTagIdByName conn tagName
    case result of
        Just tagId -> delete tagId
        Nothing    -> return $ TaskFailure tid failMsg
  where
    delete gid  = do
        execute conn deleteQuery (Only gid)
        return $ TaskSuccess tid successMsg
    deleteQuery = "DELETE FROM tags WHERE id = ?"
    successMsg  = "Successfully deleted tag"
    failMsg     = "Could not find tag"

removeClipTags :: Connection -> T.Text -> T.Text -> [T.Text] -> IO TaskResult
removeClipTags conn tid fileName tags = do
    result <- findClipIdByFileName conn fileName
    case result of
        Just clipId -> do
            tagsResult <- mapM (findTagIdByName conn) tags
            let tags' = [t | (Just t) <- tagsResult]
            delete clipId tags'
        Nothing     -> return $ TaskFailure tid failMsg
  where
    delete cid ts = do
        let args = [(cid, t) | t <- ts]
        mapM_ (execute conn deleteQuery) args
        return $ TaskSuccess tid successMsg
    deleteQuery   = "DELETE FROM clip_tags WHERE clip_id = ? AND tag_id = ?"
    successMsg    = "Removed tag association with clip"
    failMsg       = "Could not find clip with that name"