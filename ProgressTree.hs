module ProgressTree (ProgressTree(..)) where

data ProgressTree = Include String
                  | ProcedureDef String [ProgressTree]
                  | ProcedureCall String
                  | FunctionCall String
                  | Null
                  deriving (Show, Eq)
