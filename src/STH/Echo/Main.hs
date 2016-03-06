-- sth-echo: write arguments to stdout

module Main where

import STH.Lib (getArgs, exitSuccess)
import STH.Lib.IO (putStrLns)

main :: IO ()
main = do
  args <- getArgs

  case args of
    ("--char":xs) -> putStr (concat xs)
    xs            -> putStrLns xs

  exitSuccess
