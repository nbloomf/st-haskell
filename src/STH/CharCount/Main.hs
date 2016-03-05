-- sth-charcount: count characters on stdin
--   character-oriented

module Main where

import System.Exit (exitSuccess)
import STH.Lib
  (charFilter, putNewLine, count)

main :: IO ()
main = do
  charFilter (show . count)
  putNewLine
  exitSuccess
