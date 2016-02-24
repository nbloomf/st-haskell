-- sth-copy: copy characters from stdin to stdout

module Main where

import System.IO (getChar, putChar)
import SoftwareTools.FunctionLibrary (catchEOF)

main :: IO ()
main = catchEOF getChar >>= putChar >> main
