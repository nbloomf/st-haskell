-- sth-glyphcount: count glyphs on stdin

module Main where

import System.Exit (exitSuccess)
import STH.Lib
  (charFilter, putNewLine, count, getGlyphs)

main :: IO ()
main = do
  charFilter (show . count . getGlyphs)
  putNewLine
  exitSuccess
