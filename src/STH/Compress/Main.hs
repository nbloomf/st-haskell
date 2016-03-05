-- sth-compress: compress stdin (run length encoding)
--   character-oriented

module Main where

import System.Exit (exitSuccess)
import STH.Lib (charFilter, rlEncode)

main :: IO ()
main = do
  charFilter (rlEncode '\BEL' 5)
  exitSuccess
