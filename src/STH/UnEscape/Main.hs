-- sth-unescape: interpret C and ASCII backslash escape codes on stdin
--   character-oriented

module Main where

import STH.Lib (exitSuccess)
import STH.Lib.IO (charFilter)
import STH.Lib.Text.Esc (bsUnEsc)


main :: IO ()
main = do
  charFilter bsUnEsc
  exitSuccess
