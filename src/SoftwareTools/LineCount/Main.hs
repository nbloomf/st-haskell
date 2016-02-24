-- sth-linecount: count lines on stdin

module Main where

import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary (getLines, count)

main :: IO ()
main = getContents >>=
  (getLines >>> count >>> show >>> putStrLn)
