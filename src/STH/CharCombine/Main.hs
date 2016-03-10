-- sth-charcombine: replace combining unicode chars with precomposed chars

module Main where

import System.Exit (exitSuccess)
import STH.Lib
  (charFilter, composeGlyphs)

main :: IO ()
main = do
  charFilter composeGlyphs
  exitSuccess
