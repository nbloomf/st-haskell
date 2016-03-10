-- sth-overstrike: interpret backspaces using line printer control codes

module Main where

import System.Exit (exitSuccess)
import STH.Lib (lineFilter, overstrike)


main :: IO ()
main = do
  lineFilter overstrike
  exitSuccess
