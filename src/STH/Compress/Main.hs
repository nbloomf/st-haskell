-- sth-compress: compress stdin (run length encoding)
--   character-oriented

module Main where

import STH.Lib (exitSuccess)
import STH.Lib.IO (charFilter)
import STH.Lib.Text.RLE (rlEncode)

main :: IO ()
main = do
  charFilter (rlEncode '\BEL' 5)
  exitSuccess
