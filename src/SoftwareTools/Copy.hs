-- sth-copy: copy characters from stdin to stdout

module Main where

import System.IO (getChar, putChar)
import System.IO.Error (isEOFError, catchIOError)
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = catchIOError getChar handler >>= putChar >> main

handler :: IOError -> IO a
handler err
  | isEOFError err = exitSuccess
  | otherwise      = exitFailure
