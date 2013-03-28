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
import ConfigParser
import ProgressTree

addSpaces     :: String -> Int -> String
addSpaces spaces 0 = spaces
addSpaces spaces indent = addSpaces (' ':spaces) (indent - 1)

childTrees                              :: [String] -> Int -> CompletedTree -> [Char]
childTrees ignores indent (CompletedBlock name code) = if not (elem name ignores) then
      "\n" ++ addSpaces "" indent
       ++ name
       ++ (concatMap (childTrees ignores (indent + 4)) code)
   else
      "" 
childTrees ignores indent (RecursiveProcedureCall name) = if not (elem name ignores) then
      "\n" ++ addSpaces "" indent ++ name ++ " (Recursive)"
   else
      ""
childTrees ignores indent (RecursiveFunctionCall name) = if not (elem name ignores) then
      "\n" ++ addSpaces "" (indent - 4) ++ name ++ " (Recursive)"
   else
      ""
childTrees ignores indent (CodeNotFound name) = if not (elem name ignores) then
      "\n" ++ addSpaces "" indent ++ name ++ "*"
   else
      ""
childTrees ignores indent other = "\n" ++ addSpaces "" indent ++ (show other)

printTree      :: [String] -> CompletedTree -> String
printTree ignores item  = childTrees ignores 0 item

main = do
   args <- getArgs 
   configFile <- catch (readFile "draw.rc")
                       (return $ return "")
   let myConfig = if (configFile == "") then
            Config [] []
         else
            case (parse config "configFile" (B.pack configFile)) of
               Right conf -> conf
               Left err -> Config [] []
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
      writeFile (toDraw ++ "." ++ filename ++ ".drawn") (tail $ printTree (codeBlocksToIgnore myConfig) $ drawnTree toDraw pTree)
      where 
         drawnTree x n = completeTree x (makeMap n) []
