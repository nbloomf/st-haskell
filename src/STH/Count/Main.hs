-- sth-count: count lines or chars on stdin

module Main where

import System.Exit (exitSuccess)
import System.Environment (getArgs)
import Control.Arrow ((>>>))
import STH.Lib
  (count, getLines, putNewLine, charFilter)

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--char"] -> do
      charFilter (show . count)
      putNewLine
    _ -> do
      charFilter (getLines >>> count >>> show)
      putNewLine

  exitSuccess
