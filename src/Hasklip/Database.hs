{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Database where

import Hasklip.Types

import Control.Monad (forM, forM_)
import Database.SQLite.Simple (Connection, open, execute, query, lastInsertRowId, Only(..))
import qualified Data.Text as T


updateDatabase :: Config -> [QueryInfo] -> IO T.Text
updateDatabase (Config _ dbPath) qs = do
    conn <- open dbPath
    results <- forM qs runQuery
    return $ T.concat results

runQuery :: Connection -> QueryInfo -> IO T.Text
runQuery conn (ClipQuery f) = insertClip conn f
runQuery conn (TagQuery f) = return "Tag handling implemented soon"
runQuery conn (ManageQuery f) = return "Manage handling implemented soon"

insertClip :: Connection -> ClipFields -> IO T.Text
insertClip conn (ClipFields path dur game tags) = do
    gameId <- findGameIdByName conn game
    count <- execute conn clipQuery clipParams
    if count == 1
        then do
            clipId <- lastInsertRowId conn
            case tags of
                Nothing -> return ()
                Just tagList -> mapM_ (insertTag conn clipId) tagList
            return $ "Success: Inserted clip " <> path
        else 
            return $ "Failed: Could not insert clip " <> path
  where 
    clipQuery = "INSERT INTO clips (file_path, game_id, duration) VALUES (?,?,?)"
    clipParams = (path, gameId, dur)

insertTag :: Connection -> Int -> T.Text -> IO ()
insertTag conn clipId tag = do
    rows <- query conn "SELECT id FROM tags WHERE name = ?" (Only tag)
    tagId <- case rows of
        [Only tid] -> return tid
        []         -> do
            execute conn "INSERT INTO tags (name) VALUES (?)" (Only tag)
            lastInsertRowId conn
        _          -> error "Multiple tags with same name"
    execute conn "INSERT INTO clip_tags (clip_id, tag_id) VALUES (?,?)" (clipId, tagId)

findGameIdByName :: Connection -> Maybe T.Text -> IO (Maybe Int)
findGameIdByName _ Nothing = return Nothing
findGameIdByName conn (Just name) = do
    rows <- query conn "SELECT id FROM games WHERE name = ?" (Only name)
    case rows of
        [Only gameId] -> return $ Just gameId
        []         -> do
            execute conn "INSERT INTO games (name) VALUES (?)" (Only name)
            gameId <- lastInsertRowId conn
            return $ Just gameId
        _          -> error "Multiple games with same name"