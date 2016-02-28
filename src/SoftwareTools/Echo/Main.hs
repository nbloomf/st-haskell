-- sth-echo: write arguments to stdout

module Main where

import System.Environment (getArgs)
import Data.List (unwords)
import Control.Arrow ((>>>))

main :: IO ()
main = getArgs >>= (unwords >>> putStrLn)
