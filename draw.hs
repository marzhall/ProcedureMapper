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

addSpaces     :: String -> Int -> String
addSpaces spaces 0 = spaces
addSpaces spaces indent = addSpaces (' ':spaces) (indent - 1)

childTrees                              :: Int -> CompletedTree -> [Char]
childTrees indent (CompletedBlock name code) = "\n" ++ addSpaces "" indent
    ++ name
    ++ (concatMap (childTrees (indent + 4)) code)
childTrees indent (RecursiveProcedureCall name) = "\n" ++ addSpaces "" indent ++ name ++ " (Recursive)"
childTrees indent (RecursiveFunctionCall name) = "\n" ++ addSpaces "" indent ++ name ++ " (Recursive)"
childTrees indent (CodeNotFound name) = "\n" ++ addSpaces "" indent ++ name ++ "*"
childTrees indent other = "\n" ++ addSpaces "" indent ++ (show other)

printTree      :: CompletedTree -> String
printTree item = childTrees 0 item

main = do
   args <- getArgs 
   let askUser = (>=) 1 (length args)
   filename <- if askUser 
      then do
            print "What's the filename, boss?" 
            getLine
      else return (args !! 1)
   pTree <- catch (readFile (filename ++ ".trace") >>= (\contents -> return $ read contents))
                  (\_ -> do
                     print "I don't see a '.trace' file with that name. Have you run the 'trace' program on your file, and is there a file named <filename>.[p|i|cls].trace in your directory?"
                     System.Exit.exitWith $ ExitSuccess)
   forever $ do
      print "Which procedure/function?" 
      toDraw <- getLine
      writeFile (toDraw ++ "." ++ filename ++ ".drawn") (tail $ printTree $ drawnTree toDraw pTree)
      where 
         drawnTree x n = completeTree x (makeMap n) []
