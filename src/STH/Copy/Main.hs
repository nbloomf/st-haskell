-- sth-copy: copy characters from stdin to stdout

module Main where

import System.Exit (exitSuccess)
import STH.Lib (charFilter)

main :: IO ()
main = do
  charFilter id
  exitSuccess
