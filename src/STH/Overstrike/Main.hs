-- sth-overstrike: interpret backspaces using line printer control codes

module Main where

import System.Exit (exitSuccess)
import STH.Lib (lineFilter, toCCLine, renderCCLine)


main :: IO ()
main = do
  lineFilter (renderCCLine . toCCLine)
  exitSuccess
