-- sth-charcount: count characters on stdin

module Main where

import SoftwareTools.FunctionLibrary (count)

main :: IO ()
main = getContents
  >>= (putStrLn . show . count)
