-- sth-charfullwidth: replace characters with fullwidth equivalents

module Main where

import System.Exit (exitSuccess)
import STH.Lib (charFilter, toFullwidth)


main :: IO ()
main = do
  charFilter (map toFullwidth)
  exitSuccess
