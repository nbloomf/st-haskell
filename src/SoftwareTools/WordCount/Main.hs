-- sth-wordcount: count words on stdin

module Main where

import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary (count, getWords)

main :: IO ()
main = getContents >>=
  (getWords >>> count >>> show >>> putStrLn)
