-- sth-wordcount: count words on stdin

module Main where

import STH.Lib.IO (charFilter, putNewLine)
import STH.Lib.List (count)
import STH.Lib.Text (getWords)

main :: IO ()
main = do
  charFilter (show . count . getWords)
  putNewLine
