-- sth-echo: write arguments to stdout, one per line

module Main where

import STH.Lib (getArgs, exitSuccess)
import STH.Lib.IO (putStrLns)

main :: IO ()
main = getArgs >>= putStrLns >> exitSuccess
