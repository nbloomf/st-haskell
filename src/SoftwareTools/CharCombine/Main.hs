-- sth-charcombine: replace combining unicode chars with precomposed chars

module Main where

import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary (getGlyphs, composeGlyph)

main :: IO ()
main = getContents >>=
  (getGlyphs >>> map composeGlyph >>> concat >>> putStr)
