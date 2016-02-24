-- sth-wordcount: count words on stdin

module Main where

import Data.Char (isSpace)
import Data.List (break, unfoldr)
import Data.Foldable (foldl')
import Control.Arrow ((>>>))

main :: IO ()
main = getContents >>=
  (getWords >>> count >>> show >>> putStrLn)

getWords :: String -> [String]
getWords = unfoldr firstWord
  where
    firstWord :: String -> Maybe (String, String)
    firstWord xs = case dropWhile isSpace xs of
      "" -> Nothing
      ys -> Just $ break isSpace ys

count :: [a] -> Integer
count = foldl' inc 0
  where inc n _ = n+1
