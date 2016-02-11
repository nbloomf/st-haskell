-- sth-charcount: count characters on stdin

module Main where

import Data.Foldable (foldl')

main :: IO ()
main = getContents >>= (putStrLn . show . count)

count :: [a] -> Integer
count = foldl' inc 0
  where inc n _ = n+1
