-- sth-echo: write arguments to stdout

module Main where

import SoftwareTools.Lib
  ((>>>), getArgs, unwords, exitSuccess)

main :: IO ()
main = getArgs
  >>= (unwords >>> putStrLn)
  >>  exitSuccess
