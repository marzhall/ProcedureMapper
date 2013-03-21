import ProgressParser
import Data.Map as M hiding (foldl, filter)
import Data.List as D
import Data.Foldable (foldlM)
import System.Directory
import Text.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import qualified Data.ByteString.Char8 as B
import System.Environment
import Control.Monad
import System.IO
import System.Exit
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import ProgressTree

parseFile          :: String -> IO (Either ParseError [ProgressTree])
parseFile fileName = catch (readFile fileName >>= (\contents -> return $ parse procedures contents (B.pack contents)))
                           (\_ -> return $ Left $ newErrorMessage (UnExpect ": file cannot be read or found.") (newPos fileName 0 0)) 

test = "PROCEDURE lol : END PROCEDURE."

main = do
    args <- getArgs 
    let askUser = (>=) 1 (length args)
    filename <- if askUser 
        then do
                print "What's the filename, boss?" 
                getLine
        else return (args !! 1)
    parsedFile <- parseFile filename
    writeFile (filename ++ ".trace") (show parsedFile)
    --print $ show parsedFile

