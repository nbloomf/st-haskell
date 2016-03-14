-- concat: concatenate files

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  args <- getArgs

  -- just in case
  stdin <- getContents

  let
    process name = case name of
      "-"       -> putStr stdin
      otherwise -> readFile name >>= putStr

  mapM_ process args

  exitSuccess
