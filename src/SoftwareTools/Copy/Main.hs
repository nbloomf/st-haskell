-- sth-copy: copy characters from stdin to stdout
--   character-oriented

module Main where

import SoftwareTools.Lib (exitSuccess)
import SoftwareTools.Lib.IO (charFilter)

main :: IO ()
main = do
  charFilter id
  exitSuccess
