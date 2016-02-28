-- sth-charcount: count characters on stdin

module Main where

import SoftwareTools.Lib.List (count)

main :: IO ()
main = getContents
  >>= (putStrLn . show . count)
