module ProgressTree ( ProgressTree(..)
                    , includes
                    , makeMap) where

import Data.Map 

data ProgressTree = Include String
                  | ProcedureDef String [ProgressTree]
                  | ProcedureCall String
                  | FunctionDef String [ProgressTree]
                  | FunctionCall String [ProgressTree]
                  | FileNotFound String
                  | Null
                  deriving (Show, Eq)


includes :: [ProgressTree] -> [ProgressTree]
includes [] = []
includes toFilter = helperIncludes toFilter []

helperIncludes                       :: [ProgressTree] -> [ProgressTree] -> [ProgressTree]
helperIncludes [] included           = included
helperIncludes (first:rest) included = case first of 
   Include _ -> helperIncludes rest (first:included)
   ProcedureDef _ children -> (helperIncludes rest included) ++ (helperIncludes children [])
   FunctionDef _ children -> (helperIncludes rest included) ++ (helperIncludes children [])
   FunctionCall _ children -> (helperIncludes rest included) ++ (helperIncludes children [])
   _ -> helperIncludes rest included

makeMap       :: [ProgressTree] -> Map String [ProgressTree]
makeMap trees = Prelude.foldl funcsAndProcs (fromList []) trees
   where funcsAndProcs = (\acc x -> case x of
            ProcedureDef name body -> union (fromList [(name, body)]) acc
            FunctionDef name body -> union (fromList [(name, body)]) acc
            _ -> acc)    
