-- sth-echo: write arguments to stdout, one per line

module Main where

import SoftwareTools.Lib (getArgs, exitSuccess)
import SoftwareTools.Lib.IO (putStrLns)

main :: IO ()
main = getArgs >>= putStrLns >> exitSuccess
