-- sth-glyphcount: count glyphs on stdin

module Main where

import SoftwareTools.Lib ((>>>))
import SoftwareTools.Lib.List (count)
import SoftwareTools.Lib.Text (getGlyphs)

main :: IO ()
main = getContents >>=
  (getGlyphs >>> count >>> show >>> putStrLn)
