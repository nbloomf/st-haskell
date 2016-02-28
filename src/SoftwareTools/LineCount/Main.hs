-- sth-linecount: count lines on stdin

module Main where

import SoftwareTools.Lib ((>>>))
import SoftwareTools.Lib.List (count)
import SoftwareTools.Lib.Text (getLines)

main :: IO ()
main = getContents >>=
  (getLines >>> count >>> show >>> putStrLn)
