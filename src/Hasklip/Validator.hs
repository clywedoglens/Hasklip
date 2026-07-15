{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Validator where

import Hasklip.Types (
    Manifest(..)
    , Task(..)
    , ManageOp(..)
    , ListOp(..)
    , TaskError(..)
    , ValidatedTask(..))
import Hasklip.Util (nonEmpty)

import qualified Data.Text as T

validateManifest :: Manifest -> [ValidatedTask]
validateManifest (Manifest ts) = map validateTask ts

validateTask :: Task -> ValidatedTask
validateTask t = 
    let checks = case t of
            (ClipTask _ i o s e g tg) -> 
                ([("Input/Output cannot be non-empty", nonEmpty i && nonEmpty o)
                , ("Start must be between 0 and end", validateStartEnd s e)
                , ("Tag cannot be non-empty", validateMaybeTags tg)
                , ("Game cannot be non-empty", validateGame g)])
            (TagTask _ c tg)          ->
                ([("Clip path cannot be non-empty", nonEmpty c)
                , ("Tags cannot be non-empty", validateTags tg)])
            (ManageTask _ o)          ->
                validateManageOp o
            (ListTask _ o)            ->
                validateListOp o
        errors = [TaskError desc | (desc, False) <- checks]
    in if null errors 
            then ValidTask t 
            else InvalidTask t errors

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

validateManageOp :: ManageOp -> [(T.Text, Bool)]
validateManageOp (DeleteClip f)       = [("Clip path cannot be empty", nonEmpty f)]
validateManageOp (DeleteTag tag)      = [("Tag name cannot be empty", nonEmpty tag)]
validateManageOp (RemoveClipTags f t) = [("Clip path cannot be empty", nonEmpty f)
                                        , ("Tag name cannot be empty", validateTags t)]

validateListOp :: ListOp -> [(T.Text, Bool)]
validateListOp (TagSearch ts) = [("Must include tags", validateTags ts)]
validateListOp (NameSearch n) = [("Search name cannot be empty", nonEmpty n)]
    
    
    