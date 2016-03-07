-- import:

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  exitSuccess
