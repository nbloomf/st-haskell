-- sth-unescape: interpret C and ASCII backslash escape codes on stdin

module Main where

import System.Exit (exitSuccess)
import STH.Lib (charFilter, bsUnEsc)


main :: IO ()
main = do
  charFilter bsUnEsc
  exitSuccess
