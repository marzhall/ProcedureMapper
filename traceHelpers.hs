module TraceHelpers (followIncludes) where

import ProgressTree
import ProgressParser
import Data.Map as M hiding (foldl, filter)
import Data.List as D
import Data.Char as C
import Data.Foldable (foldlM)
import System.Directory
import Text.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import qualified Data.ByteString.Char8 as B

lowerCase = D.map toLower

parseFile          :: String -> IO (Either ParseError [ProgressTree])
parseFile fileName = catch (readFile fileName >>= (\contents -> return $ parse procedures contents (B.pack $ lowerCase contents)))
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
