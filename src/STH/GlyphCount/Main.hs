-- sth-glyphcount: count glyphs on stdin

module Main where

import STH.Lib.IO (charFilter, putNewLine)
import STH.Lib.List (count)
import STH.Lib.Text (getGlyphs)

main :: IO ()
main = do
  charFilter (show . count . getGlyphs)
  putNewLine
