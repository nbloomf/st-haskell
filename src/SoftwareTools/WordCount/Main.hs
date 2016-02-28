-- sth-wordcount: count words on stdin

module Main where

import Control.Arrow ((>>>))
import SoftwareTools.Lib.List (count)
import SoftwareTools.Lib.Text (getWords)

main :: IO ()
main = getContents >>=
  (getWords >>> count >>> show >>> putStrLn)
