-- sth-charecho: write arguments to stdout, without trailing newline

module Main where

import SoftwareTools.Lib
  ((>>>), getArgs, exitSuccess, intercalate)

main :: IO ()
main = getArgs
  >>= (intercalate " " >>> putStr)
  >>  exitSuccess
