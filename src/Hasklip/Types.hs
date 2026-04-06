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

data Config = Config
    {
      config_dbPath   :: FilePath
    , config_clipsDir :: FilePath
    } deriving (Show, Eq)

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
            T.Text  -- ^ Task id
            T.Text  -- ^ Clip filepath
            T.Text  -- ^ Operation to perform (might be changed to a type)
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
                <*> v .: "clip"
                <*> v .: "operation"
            _        -> fail $ "Unknown task type: " ++ show typ

data ParseError
    = FileError T.Text
    | ParserError T.Text
    deriving (Show, Eq)

data ValidatedTask
    = ValidTask Task
    | InvalidTask Task [TaskError]
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
    , game         :: T.Text
    } deriving (Generic)

instance ToJSON ClipArbeit where
    toJSON = genericToJSON defaultOptions
      { fieldLabelModifier = camelTo2 '_' }

data MgmtArbeit = MgmtArbeit
    {
      mgmtArbeit_mgmtArbeitId :: T.Text
    , mgmtArbeit_filePath     :: T.Text
    }

data ArbeitResult 
    = Erfolg T.Text 
    | Fehler T.Text T.Text
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
    deriving (Show, Eq)

data ClipFields = ClipFields
    T.Text            -- ^ id
    T.Text            -- ^ Filepath
    Int               -- ^ Duration in seconds
    (Maybe T.Text)    -- ^ Game
    (Maybe [T.Text])  -- ^ Tags
    deriving (Show, Eq)

data TagFields = TagFields
    T.Text            -- ^ id
    T.Text            -- ^ Clip filepath
    [T.Text]          -- ^ Tags
    deriving (Show, Eq)

data ManageFields = ManageFields
    T.Text            -- ^ id
    T.Text            -- ^ Clip filepath
    T.Text            -- ^ Operation to perform (might be changed to a type)
    deriving (Show, Eq)

data TaskResult 
    = TaskSuccess T.Text T.Text
    | TaskFailure T.Text T.Text
    deriving (Generic)