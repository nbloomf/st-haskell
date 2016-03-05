-- sth-copy: copy characters from stdin to stdout
--   character-oriented

module Main where

import STH.Lib (exitSuccess)
import STH.Lib.IO (charFilter)

main :: IO ()
main = do
  charFilter id
  exitSuccess
