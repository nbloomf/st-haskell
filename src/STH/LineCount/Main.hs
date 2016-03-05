-- sth-linecount: count lines on stdin

module Main where

import STH.Lib ((>>>))
import STH.Lib.List (count)
import STH.Lib.Text (getLines)

main :: IO ()
main = getContents >>=
  (getLines >>> count >>> show >>> putStrLn)
