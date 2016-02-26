-- sth-charfullwidth: replace characters with fullwidth equivalents

module Main where

import System.IO (getChar, putChar)
import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary (catchEOF, toFullwidth)

main :: IO ()
main = catchEOF getChar
  >>= (toFullwidth >>> putChar) >> main
