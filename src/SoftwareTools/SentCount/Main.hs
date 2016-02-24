-- sth-sentcount: count sentences on stdin

module Main where

import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary (count, getSentences)

main :: IO ()
main = getContents >>=
  (getSentences >>> count >>> show >>> putStrLn)
