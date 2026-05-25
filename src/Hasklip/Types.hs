{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Hasklip.Types where

import GHC.Generics (Generic)
import Data.Aeson (
    FromJSON
    , parseJSON
    , withObject
    , (.:)
    , (.:?)
    , ToJSON
    , toJSON
    , genericToJSON
    , defaultOptions
    , fieldLabelModifier
    , camelTo2)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Database.SQLite.Simple (FromRow(..), field)

data Config = Config
    {
      config_manifest     :: FilePath
    , config_dbPath       :: FilePath
    , config_clipsDir     :: FilePath
    , config_requestPipe  :: FilePath
    , config_responsePipe :: FilePath
    } deriving (Show, Eq)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config
        <$> v .: "manifest"
        <*> v .: "dbPath"
        <*> v .: "clipsDir"
        <*> v .: "requestPipe"
        <*> v .: "responsePipe"

type ParseResult a = Either ParseError a

newtype Manifest = Manifest { manifest_tasks :: [Task] } deriving (Show, Eq)

instance FromJSON Manifest where
    parseJSON = withObject "Manifest" $ \v -> do
        tasks <- v .: "tasks" :: Parser [Task]
        pure Manifest{manifest_tasks=tasks}

-- | An operation to perform
data Task 
    = -- | Clip a video
        ClipTask 
            T.Text            -- ^ Task id
            T.Text            -- ^ Input filepath
            T.Text            -- ^ Output filepath
            Int               -- ^ Start time in seconds
            Int               -- ^ End time in seconds
            (Maybe T.Text)    -- ^ Game
            (Maybe [T.Text])  -- ^ Tags 
    | -- | Tag an existing clip
        TagTask 
            T.Text    -- ^ Task id
            T.Text    -- ^ Clip filepath
            [T.Text]  -- ^ Tags
    | -- | Manage existing clips
        ManageTask 
            T.Text    -- ^ Task id
            ManageOp  -- ^ Operation to perform with details
    | -- Create a listing of clips from a name or tag search
        ListTask
            T.Text  -- ^ Task id
            ListOp  -- ^ Type of search
    deriving (Show, Eq)

data ManageOp
    = -- | Delete a clip
        DeleteClip
            T.Text  -- ^ Clip filepath
    | -- Delete a tag
        DeleteTag
            T.Text  -- ^ Tag name
    | -- Remove a tag from a clip
        RemoveClipTags
            T.Text    -- ^ Clip filepath
            [T.Text]  -- ^ Tags
    deriving (Show, Eq)

data ListOp
    = -- | Search by tag
        TagSearch
            [T.Text] -- ^ Tags to look up
    | -- | Search by name
        NameSearch
            T.Text   -- ^ Word within clip name to search
    deriving (Show, Eq)

instance FromJSON Task where
    parseJSON = withObject "Task" $ \v -> do
        typ <- v .: "type" :: Parser T.Text
        case typ of
            "clip"   -> ClipTask
                <$> v .:  "id"
                <*> v .:  "input"
                <*> v .:  "output"
                <*> v .:  "start"
                <*> v .:  "end"
                <*> v .:? "game"
                <*> v .:? "tags"
            "tag"    -> TagTask
                <$> v .: "id"
                <*> v .: "clip"
                <*> v .: "tags"
            "manage" -> ManageTask
                <$> v .: "id"
                <*> v .: "operation"
            "list"   -> ListTask
                <$> v .: "id"
                <*> v .: "operation"
            _        -> fail $ "Unknown task type: " ++ show typ

instance FromJSON ManageOp where
    parseJSON = withObject "operation" $ \v -> do
        op <- v .: "type" :: Parser T.Text
        case op of
            "deleteClip"    -> DeleteClip
                <$> v .: "filepath"
            "deleteTag"     -> DeleteTag
                <$> v .: "tag"
            "removeClipTags" -> RemoveClipTags
                <$> v .: "filepath"
                <*> v .: "tags"
            _                -> fail $ "Unknown op type: " ++ show op

instance FromJSON ListOp where
    parseJSON = withObject "operation" $ \v -> do
        op .: "type" :: Parser T.Text
        case op of
            "tagSearch"  -> TagSearch
                <$> v .: "tags"
            "nameSearch" -> NameSearch
                <$> v .: "search"
            _            -> fail $ "Unknown op type: " ++ show op

data ParseError
    = 
        FileError 
            T.Text  -- ^ Error message
    |   ParserError 
            T.Text  -- ^ Error message
    deriving (Show, Eq)

data ValidatedTask
    = 
        ValidTask 
            Task  -- ^ Valid task
    |   InvalidTask 
            Task         -- ^ Invalid task
            [TaskError]  -- ^ Associated error messages
    deriving (Show, Eq)

data TaskError
    = TaskError { taskError_description :: T.Text}
    deriving (Show, Eq)

data ClipArbeit = ClipArbeit
    {
      clipArbeitId :: T.Text
    , inputPath    :: T.Text
    , outputPath   :: T.Text
    , start        :: Int
    , end          :: Int
    , game         :: Maybe T.Text
    } deriving (Generic)

instance ToJSON ClipArbeit where
    toJSON = genericToJSON defaultOptions
      { fieldLabelModifier = camelTo2 '_' }

data MgmtArbeit = MgmtArbeit
    {
      mgmtArbeit_mgmtArbeitId :: T.Text
    , mgmtArbeit_clipFilePath :: T.Text
    }

data ArbeitResult 
    = 
        Erfolg 
            T.Text  -- ^ Corresponding task id
    |   Fehler 
            T.Text  -- ^ Corresponding task id
            T.Text  -- ^ Failure message
    deriving (Show, Generic)

instance FromJSON ArbeitResult where
    parseJSON = withObject "ArbeitResult" $ \v -> do
        success <- v .: "success" :: Parser Bool
        taskId <- v .: "task_id"  :: Parser T.Text
        if success
            then return $ Erfolg taskId
            else do
                msg <- v .: "message"
                return $ Fehler taskId msg

data QueryInfo 
    = -- | Clip a video
        ClipQuery ClipFields
    | -- | Tag an existing clip
        TagQuery TagFields
    | -- | Manage existing clips
        ManageQuery ManageFields
    | -- | Find all clips from a criteria (tags/name keyboard search)
        ListQuery ListFields
    deriving (Show, Eq)

data ClipFields = ClipFields
    T.Text            -- ^ id
    T.Text            -- ^ Filepath
    Int               -- ^ Duration in seconds
    (Maybe T.Text)    -- ^ Game
    (Maybe [T.Text])  -- ^ Tags
    deriving (Show, Eq)

data TagFields = TagFields
    T.Text    -- ^ id
    T.Text    -- ^ Clip filepath
    [T.Text]  -- ^ Tags
    deriving (Show, Eq)

data ManageFields 
    =
        DeleteClipFields
            T.Text  -- ^ id
            T.Text  -- ^ Clip filepath
    |   DeleteTagFields
            T.Text  -- ^ id
            T.Text  -- ^ Tag name
    |   RemoveClipTagsFields
            T.Text -- ^ id
            T.Text -- ^ Clip filepath
            [T.Text] -- ^ tags
    deriving (Show, Eq)

data ListFields
    =
        TagSearchFields
            T.Text   -- ^ id
            [T.Text] -- ^ tags
    |   NameSearchFields
            T.Text -- ^ id
            T.Text -- ^ name
    deriving (Show, Eq)

data ListResult
    =
        ListResult
            T.Text
            Maybe T.Text
    deriving (Show, Eq)

instance FromRow ListResult where
    fromRow = ListResult <$> field <*> field

data TaskResult 
    = 
        TaskSuccess 
            T.Text  -- ^ Task id
            T.Text  -- ^ Success message if applicable
    |   TaskFailure 
            T.Text  -- ^ Task id
            T.Text  -- ^ Failure message
    deriving (Show, Generic)