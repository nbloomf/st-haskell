-- sth-linecount: count lines on stdin

module Main where

import Data.List (break, unfoldr)
import Data.Foldable (foldl')
import Control.Arrow ((>>>))

main :: IO ()
main = getContents >>=
  (getLines >>> count >>> show >>> putStrLn)

getLines :: String -> [String]
getLines = unfoldr firstLine
  where
    firstLine :: String -> Maybe (String, String)
    firstLine xs = case break (== '\n') xs of
      ("","")   -> Nothing
      (as,"")   -> Just (as,"")
      (as,b:bs) -> Just (as,bs)

count :: [a] -> Integer
count = foldl' inc 0
  where inc n _ = n+1
