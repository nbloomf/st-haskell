-- sth-copy: copy characters from stdin to stdout

module Main where


import System.IO
  (getChar, putChar)
import System.IO.Error
  (isEOFError, catchIOError)
import System.Exit
  (exitSuccess, exitFailure)


{-
This program copies characters from stdin to stdout until
EOF is encountered, whereupon it exits. Any other error
causes the program to fail.

In an imperative language this pattern (read a character,
write a character, repeat) would be written with a loop.
In Haskell we instead use recursion.
-}

main :: IO ()
main = catchIOError getChar handler >>= putChar >> main


{-
IO actions can throw IO errors, which (unlike other 
error handling strategies in Haskell) can be silently,
and thus accidentally, ignored. These errors are caught
using catchIOError, which takes an action to be attempted
and an error handler.
-}

handler :: IOError -> IO a
handler err
  | isEOFError err = exitSuccess
  | otherwise      = exitFailure
