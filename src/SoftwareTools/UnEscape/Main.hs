-- sth-unescape: interpret C and ASCII backslash escape codes on stdin

module Main where

import SoftwareTools.Lib
  ((>>>), exitSuccess)
import SoftwareTools.Lib.Text
  (backslashUnEscape)


main :: IO ()
main = do
  getContents >>= (backslashUnEscape >>> putStr)

  exitSuccess
