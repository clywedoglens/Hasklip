{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Validator where

import Hasklip.Types (Manifest(..), Task(..), TaskError(..), ValidatedTask(..))
import Hasklip.Util (nonEmpty)

import qualified Data.Text as T

validateManifest :: Manifest -> [ValidatedTask]
validateManifest (Manifest ts) = map validateTask ts

validateTask :: Task -> ValidatedTask
validateTask t = 
      let
          checks = case t of
            (ClipTask _ i o s e g tg) -> 
                ([("Input/Output cannot be non-empty", nonEmpty i && nonEmpty o)
                , ("Start must be between 0 and end", validateStartEnd s e)
                , ("Tag cannot be non-empty", validateMaybeTags tg)
                , ("Game cannot be non-empty", validateGame g)])
            (TagTask _ c tg)          ->
                ([("Clip path cannot be non-empty", nonEmpty c)
                , ("Tags cannot be non-empty", validateTags tg)])
            (ManageTask _ c o)        -> 
                ([("Clip path cannot be non-empty", nonEmpty c)
                , ("Operation cannot be non-empty", nonEmpty o)])
          errors = [TaskError desc | (desc, False) <- checks]
      in 
          if null errors then ValidTask t else InvalidTask t errors

validateStartEnd :: Int -> Int -> Bool
validateStartEnd s e = e > s && s >= 0

validateGame :: Maybe (T.Text) -> Bool
validateGame (Just g) = nonEmpty g
validateGame Nothing  = True

validateMaybeTags :: Maybe [T.Text] -> Bool
validateMaybeTags (Just ts) = all nonEmpty ts
validateMaybeTags Nothing   = True

validateTags :: [T.Text] -> Bool
validateTags = all nonEmpty