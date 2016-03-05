-- sth-charcount: count characters on stdin

module Main where

import STH.Lib.IO (charFilter, putNewLine)
import STH.Lib.List (count)

main :: IO ()
main = do
  charFilter (show . count)
  putNewLine
