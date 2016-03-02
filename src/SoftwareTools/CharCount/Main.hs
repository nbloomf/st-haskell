-- sth-charcount: count characters on stdin

module Main where

import SoftwareTools.Lib.IO (charFilter, putNewLine)
import SoftwareTools.Lib.List (count)

main :: IO ()
main = do
  charFilter (show . count)
  putNewLine
