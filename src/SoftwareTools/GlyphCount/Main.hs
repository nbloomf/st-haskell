-- sth-glyphcount: count glyphs on stdin

module Main where

import SoftwareTools.Lib.IO (charFilter, putNewLine)
import SoftwareTools.Lib.List (count)
import SoftwareTools.Lib.Text (getGlyphs)

main :: IO ()
main = do
  charFilter (show . count . getGlyphs)
  putNewLine
