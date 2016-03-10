-- sth-sentcount: count sentences on stdin

module Main where

import System.Exit (exitSuccess)
import STH.Lib
  (charFilter, putNewLine, count,
   getSentences)


main :: IO ()
main = do
  charFilter (show . count . getSentences)
  putNewLine
  exitSuccess
