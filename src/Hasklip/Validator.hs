{-# LANGUAGE OverloadedStrings #-}
module Hasklip.Validator where

import Hasklip.Types
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
  
nonEmpty :: T.Text -> Bool
nonEmpty = not . T.null

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

partitionValidated :: [ValidatedTask] -> ([Task], [ValidatedTask])
partitionValidated ts = ([t | (ValidTask t) <- ts], [i | i@(InvalidTask _ _) <- ts])

validatedTasksSummary :: [ValidatedTask] -> T.Text
validatedTasksSummary = foldr accSummary "" 
  where accSummary t acc = T.concat [displayValidatedTask t, "\n", acc]

displayValidatedTask :: ValidatedTask -> T.Text
displayValidatedTask v =
    T.concat $ case v of
        ValidTask t     -> [taskDesc t, " was validated successfully"]
        InvalidTask t es -> let errors      = T.intercalate "\n" $ map errorDesc es
                                errorDesc e = T.append "-" $ taskError_description e
                           in [taskDesc t, " had the following errors:\n", errors]

taskDesc :: Task -> T.Text
taskDesc (ClipTask i _ _ _ _ _ _) = T.append "Clip task id " i
taskDesc (TagTask i _ _)          = T.append "Tag task id " i
taskDesc (ManageTask i _ _ )      = T.append "Manage task id " i