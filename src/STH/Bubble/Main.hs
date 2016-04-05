-- sth-bubble: (bubble)sort lines on stdin

module Main where

import System.Exit (exitSuccess)
import STH.Lib (putStrLns, bubble, getLines)

main :: IO ()
main = do
  lns <- fmap getLines getContents
  mapM_ putStrLn $ bubble lns
  exitSuccess
