module ConfigParser ( Config(..)
                    , config) where

import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Data.ByteString.Char8 as B

data Config = Config { linkedFiles :: [String]
                     , codeBlocksToIgnore :: [String]}

toLink :: Parser [String]
toLink = do
   string "link"
   spaces
   char '='
   spaces
   char '{'
   files <- manyTill anyChar $ char '}' 
   return $ words files   

toIgnore :: Parser [String]
toIgnore = do
   string "ignore"
   spaces
   char '='
   spaces
   char '{'
   files <- manyTill anyChar $ char '}' 
   return $ words files

config :: Parser Config
config = do
   links <- toLink
   spaces
   ignores <- toIgnore
   return $ Config links ignores
