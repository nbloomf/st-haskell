-- sth-linecount: count lines on stdin

module Main where

import Data.Foldable (foldl')
import Control.Arrow ((>>>))

main :: IO ()
main = getContents >>=
        (getNewlines >>> count >>> show >>> putStrLn)

getNewlines :: [Char] -> [Char]
getNewlines []     = []
getNewlines [_]    = ['\n']
getNewlines (x:xs)
  | x == '\n' = '\n' : getNewlines xs
  | otherwise = getNewlines xs

count :: [a] -> Integer
count = foldl' inc 0
  where inc n _ = n+1
