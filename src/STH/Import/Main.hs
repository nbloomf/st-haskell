-- import: splice contents of a file into stdin

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import Data.List (unfoldr)
import STH.Lib
  (lineFilterIO, getWords, getLines,
   putStrLns, takeBetween, reportErrorMsgs)


-- We accept two kinds of import commands:
data Import
  = Whole   String
  | Between String String String


main :: IO ()
main = do
  args <- getArgs

  keyword <- case args of
    []             -> return "import"
    ["--with",str] -> return str
    otherwise      -> argErr >> exitFailure

  let
    -- see if a line is an import command
    readCommand :: String -> Maybe Import
    readCommand str = case getWords str of
      [x,file] -> if x == keyword
        then Just $ Whole file
        else Nothing
      [x,file,"between",open,"and",close] -> if x == keyword
        then Just $ Between file open close
        else Nothing
      otherwise -> Nothing

    -- process a single line
    splice :: String -> IO ()
    splice line = case readCommand line of
      Nothing -> do
        putStrLn line
      Just (Whole name) -> do
        input <- fmap getLines $ readFile name
        putStrLns input
      Just (Between name open close) -> do
        input <- fmap getLines $ readFile name
        putStrLns $ takeBetween (open,close) input

  lineFilterIO splice
  exitSuccess

argErr :: IO ()
argErr = reportErrorMsgs
  [ "usage"
  , "  import            : expand any 'import' lines on stdin"
  , "  import --with STR : same, using custom import keyword STR"
  , "this program recognizes two kinds of import commands:"
  , "  import FILENAME"
  , "    insert contents of FILENAME in place of this line"
  , "  import FILENAME from START to END"
  , "    same, but cut out all lines except those between START and END lines."
  ]
