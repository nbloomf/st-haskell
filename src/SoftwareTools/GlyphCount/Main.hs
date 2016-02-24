-- sth-glyphcount: count glyphs on stdin

module Main where

import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary (count, getGlyphs)

main :: IO ()
main = getContents >>=
  (getGlyphs >>> count >>> show >>> putStrLn)
