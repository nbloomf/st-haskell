-- sth-wordcount: count words on stdin

module Main where

import SoftwareTools.Lib.IO (charFilter, putNewLine)
import SoftwareTools.Lib.List (count)
import SoftwareTools.Lib.Text (getWords)

main :: IO ()
main = do
  charFilter (show . count . getWords)
  putNewLine
