-- sth-wordcount: count words on stdin

module Main where

import System.Exit (exitSuccess)
import STH.Lib
  (charFilter, putNewLine, count, getWords)

main :: IO ()
main = do
  charFilter (show . count . getWords)
  putNewLine
  exitSuccess
