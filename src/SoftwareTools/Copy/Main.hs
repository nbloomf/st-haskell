-- sth-copy: copy characters from stdin to stdout

module Main where

import SoftwareTools.Lib (getChar, putChar)
import SoftwareTools.Lib.Error (catchEOF)

main :: IO ()
main = catchEOF getChar >>= putChar >> main
