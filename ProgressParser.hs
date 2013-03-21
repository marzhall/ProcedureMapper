module ProgressParser (procedures) where
import ProgressTree
import Text.Parsec
import Text.Parsec.ByteString
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Data.ByteString.Char8 as B

populated                 :: [ProgressTree] -> [ProgressTree] -> [ProgressTree]
populated [] filtered     = filtered
populated (x:xs) filtered = case x of
   Null -> populated xs filtered
   x -> populated xs (filtered ++ [x])

nullRemoved          :: [ProgressTree] -> [ProgressTree]
nullRemoved toFilter = populated toFilter []

escapedDoubleQuote :: Parser ProgressTree
escapedDoubleQuote = do
   many $ noneOf "~\""
   char '~'
   char '\"'
   return Null

escapedSingleQuote :: Parser ProgressTree
escapedSingleQuote = do
   many $ noneOf "~\'"
   char '~'
   char '\''
   return Null

doubleQuoted :: Parser ProgressTree
doubleQuoted = do
   char '\"'
   many $ try escapedDoubleQuote 
   many $ noneOf "\""
   char '\"'
   return Null

singleQuoted :: Parser ProgressTree
singleQuoted = do
   char '\''
   many $ try escapedSingleQuote 
   many $ noneOf "\'"
   char '\''
   return Null

quoted :: Parser ProgressTree
quoted = do
   try singleQuoted <|> doubleQuoted

endComment :: Parser ProgressTree
endComment = do
   char '*'
   char '/'
   return Null

randomStar :: Parser ProgressTree
randomStar = do
   char '*'
   notFollowedBy $ char '/'
   return Null

randomSlash :: Parser ProgressTree
randomSlash = do
   char '/'
   notFollowedBy $ char '*'
   return Null

junkword :: Parser ProgressTree
junkword = do
   anyChar <?> "a character not important to us"
   return Null

junk :: Parser ProgressTree
junk = do
    noneOf " /{\'\"PRE" <?> "special characters or the beginning of commands important to us"
    return Null

endProcedure :: Parser ProgressTree
endProcedure = do
   string "END" >> many1 space >> string "PROCEDURE"
   try $ char '.'
   return Null

functionCall :: Parser ProgressTree
functionCall = do
   functionName <- many1 (alphaNum <|> noneOf "\\/?*\"'><|&}(): " <?> "the name of the procedure being defined")
   char '('
   manyTill (try functionCall <|> junkword) ( try $ char ')')
   return $ FunctionCall functionName

procedure :: Parser ProgressTree
procedure = do
   string "PROCEDURE"
   many1 space
   procedureName <- many1 (alphaNum <|> noneOf "\\/?*\"'><|&}{: " <?> "the name of the procedure being defined")
   spaces
   char ':'
   many1 space
   procedureCalls <- manyTill (try call <|> try functionCall <|> try randomStar <|> try randomSlash <|> try comment <|> try quoted <|> try preprocessor <|> try include <|> junkword <?> "internal procedure code") (try endProcedure)
   return $ ProcedureDef procedureName (nullRemoved procedureCalls)

call :: Parser ProgressTree
call = do
   string "RUN"
   many1 space
   procName <- many1 (noneOf "\\/?*\"'><(|&}{. " <?> "the name of the procedure being called")
   return $ ProcedureCall procName

commentJunk :: Parser ProgressTree
commentJunk = do
   many1 $ try $ noneOf "*/"
   return Null

comment :: Parser ProgressTree
comment = do
        char '/' >> char '*'
        many (try commentJunk <|> try comment <|> try randomSlash <|> try randomStar <?> "commented out text")
        endComment
        return Null

includeJunk :: Parser ProgressTree
includeJunk = do
   many1 $ noneOf "{}"
   return Null

include :: Parser ProgressTree
include = do
    char '{'
    many $ char '"' <|> char '\''
    fileName <- many1 ( alphaNum <|> noneOf "\\/?*\"'><|&}{ " <?> "An include's filename")
    try $ many $ includeJunk <|> preprocessor 
    char '}'
    return $ Include fileName

preprocessor :: Parser ProgressTree
preprocessor = do
    char '{'
    lookAhead $ oneOf "{&1234567890"
    many $ try includeJunk <|> preprocessor
    many $ noneOf "}"
    char '}'
    return Null

procedures :: Parser [ProgressTree]
procedures = do
    spaces
    allProcedures <- many (try procedure <|> try randomStar <|> try randomSlash <|> try comment <|> try quoted <|> try preprocessor <|> try include <|> junkword <?> "a new procedure, definition, or top-level code. You should never get this error; if you do, I done goofed")
    eof
    return $ nullRemoved allProcedures
