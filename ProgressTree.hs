module ProgressTree ( ProgressTree(..)
                    , CompletedTree(..)
                    , completeTree
                    , includes
                    , makeMap) where

import Data.Map as M

data ProgressTree = Include String
                  | ProcedureDef String [ProgressTree]
                  | ProcedureCall String
                  | FunctionDef String [ProgressTree]
                  | FunctionCall String
                  | FileNotFound String
                  | Null
                  deriving (Show, Read, Eq)

data CompletedTree = CompletedBlock String [CompletedTree]
                   | CodeNotFound String
                   | CompletedFunctionCall String
                   | RecursiveProcedureCall String
                   | RecursiveFunctionCall String
                   | IncludeCall String
                   | NullComplete
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
   FunctionCall _ -> (helperIncludes rest included)
   _ -> helperIncludes rest included

makeMap       :: [ProgressTree] -> M.Map String [ProgressTree]
makeMap trees = Prelude.foldl funcsAndProcs (fromList []) trees
   where funcsAndProcs = (\acc x -> case x of
            ProcedureDef name body -> M.union (fromList [(name, body)]) acc
            FunctionDef name body -> M.union (fromList [(name, body)]) acc
            _ -> acc)    

completeTree :: String -> M.Map String [ProgressTree] -> [String] -> CompletedTree
completeTree procName knownBlocks parentCalls = if (M.member procName knownBlocks) /= True then
      CodeNotFound procName
   else
      CompletedBlock procName $ Prelude.map (completeCode knownBlocks (procName:parentCalls)) (knownBlocks M.! procName)

completeCode            :: M.Map String [ProgressTree] -> [String] -> ProgressTree -> CompletedTree
completeCode knownBlocks parentBlocks toComplete = case toComplete of
   ProcedureCall name -> if (elem name parentBlocks) then
         RecursiveProcedureCall name
      else
         completeTree name knownBlocks parentBlocks
   FunctionCall name -> if (elem name parentBlocks) then
         RecursiveFunctionCall name
      else
         (completeTree name knownBlocks parentBlocks)
   Include name -> IncludeCall name
   _ -> NullComplete
