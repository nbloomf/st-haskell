-- sth-copy: copy characters from stdin to stdout

module Main where

import System.Exit (exitSuccess)
import System.Environment (getArgs)
import STH.Lib (charFilter, lineFilter)

data Mode = Chars | Lines

main :: IO ()
main = do
  args <- getArgs

  mode <- case args of
    ["--char"] -> return Chars
    otherwise  -> return Lines

  case mode of
    Chars -> charFilter id
    Lines -> lineFilter id

  exitSuccess
