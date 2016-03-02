-- sth-unescape: interpret C and ASCII backslash escape codes on stdin
--   character-oriented

module Main where

import SoftwareTools.Lib
  (exitSuccess)
import SoftwareTools.Lib.IO
  (charFilter)
import SoftwareTools.Lib.Text
  (backslashUnEscape)


main :: IO ()
main = do
  charFilter backslashUnEscape
  exitSuccess
