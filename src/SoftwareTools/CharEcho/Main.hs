-- sth-charecho: write arguments to stdout, without trailing newline

module Main where

import SoftwareTools.Lib
  ((>>>), getArgs, unwords, exitSuccess)

main :: IO ()
main = getArgs
  >>= (unwords >>> putStr)
  >>  exitSuccess
