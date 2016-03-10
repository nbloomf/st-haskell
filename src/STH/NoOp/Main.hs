-- sth-noop: exit successfully

module Main where

import System.Exit (exitSuccess)

main :: IO ()
main = do
  -- real tools do stuff here
  exitSuccess
