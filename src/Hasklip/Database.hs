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
    , query
    , changes
    , lastInsertRowId
    , Only(..))
import qualified Data.Text as T

updateDatabase :: Config -> [QueryInfo] -> IO [TaskResult]
updateDatabase (Config db _) qs = do
    conn <- open db
    mapM (runQuery conn) qs
    -- results <- forM qs runQuery
    -- return $ T.concat results

runQuery :: Connection -> QueryInfo -> IO TaskResult
runQuery conn (ClipQuery f) = insertClip conn f
runQuery conn (TagQuery (TagFields tagId _ _)) =
    return $ TaskSuccess tagId "Tag handling implemented soon"
runQuery conn (ManageQuery (ManageFields mgmtId _ _)) =
    return $ TaskSuccess mgmtId "Manage handling implemented soon"

insertClip :: Connection -> ClipFields -> IO TaskResult
insertClip conn (ClipFields taskId path dur game tags) = do
    gameId <- findGameIdByName conn game
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

findGameIdByName :: Connection -> Maybe T.Text -> IO (Maybe Int)
findGameIdByName _ Nothing = return Nothing
findGameIdByName conn (Just name) = do
    rows <- query conn selectGamesQuery (Only name) :: IO [Only Int]
    case rows of
        [Only gameId] -> return $ Just gameId
        []         -> do
            execute conn insertGamesQuery (Only name) :: IO ()
            gameId <- fromIntegral <$> lastInsertRowId conn
            return $ Just gameId
        _          -> do
            displayError $ multipleGamesErr <> name
            return $ Nothing
  where
    selectGamesQuery = "SELECT id FROM games WHERE name = ?"
    insertGamesQuery = "INSERT INTO games (name) VALUES (?)"
    multipleGamesErr = "Multiple game with the same name: "