module TraceHelpers (followIncludes) where

import ProgressTree
import ProgressParser
import Data.Map as M hiding (foldl, filter)
import Data.List as D
import Data.Foldable (foldlM)
import System.Directory
import Text.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import qualified Data.ByteString.Char8 as B

parseFile          :: String -> IO (Either ParseError [ProgressTree])
parseFile fileName = catch (readFile fileName >>= (\contents -> return $ parse procedures contents (B.pack contents)))
                           (\_ -> return $ Left $ newErrorMessage (UnExpect ": file cannot be read or found.") (newPos fileName 0 0)) 

followIncludes                    :: ProgressTree -> IO [ProgressTree]
followIncludes (Include fileName) = do
    parsedFile <- parseFile fileName
    case parsedFile of
        Left err -> return [FileNotFound fileName]
        Right results -> if (includes results) /= [] 
                            then do
                                parsedResults <- mapM followIncludes (includes results)
                                return $ (concat parsedResults) ++ results
                            else
                                return results

{--calledCode                              :: Int -> IncludeTree (Either ParseError String) -> [Char]
calledCode indent (NoIncludes filename) = "\n" ++ addSpaces "" indent ++ stripEndlines (show filename)
calledCode indent (IncludeFile filename includes) = "\n" ++ addSpaces "" indent
    ++ (stripEndlines $ show filename)
    ++ (concatMap (childTrees (indent + 4)) includes)

printTree (NoIncludes filename) = (show filename)
printTree (IncludeFile filename includes) = stripEndlines (show filename)
    ++ concatMap (calledCode 4) includes

-dependencyFold                                     :: Map String [String] -> Map String [String] -> String -> String -> Map String [String]
dependencyFold dependMap includeMap child fileName = do
    if member fileName includeMap then
        if includes /= [] 
            then
                unionsWith (++) (D.map (dependencyFold dbWithNewFile includeMap fileName) includes)
        else
            dbWithNewFile
    else
        insertWith (++) fileName (("File not found"):(child:(dependMap ! child))) dependMap
    where includes = (includeMap ! fileName)
          dbWithNewFile = insertWith (++) fileName (child:(dependMap ! child)) dependMap--}
