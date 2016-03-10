-- sth-echo: write arguments to stdout

module Main where

import System.Exit (exitSuccess)
import System.Environment (getArgs)
import STH.Lib (putStrLns)

main :: IO ()
main = do
  args <- getArgs

  case args of
    ("--char":xs) -> putStr (concat xs)
    xs            -> putStrLns xs

  exitSuccess
