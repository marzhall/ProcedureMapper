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

test = "PROCEDURE lol : END PROCEDURE."

main = do
   args <- getArgs 
   let askUser = (>=) 1 (length args)
   filename <- if askUser 
      then do
            print "What's the filename, boss?" 
            getLine
      else return (args !! 1)
   parsedFile <- followIncludes (Include filename)
   writeFile (filename ++ ".trace") (show parsedFile)

   pTree <- catch (readFile (filename ++ ".trace" >>= (\contents -> return $ read contents))
                  (\_ -> do
                     print "I don't see the file. Have you run 'trace.exe' on it, and is it named <filename>.[p|i|cls].trace?")

   forever $ do
      print "Which procedure/function?" 
      toDraw <- getLine
      if (member toDraw (makeMap pTree)) then
         drawTree toDraw (makeMap pTree)
