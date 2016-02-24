-- sth-sentcount: count sentences on stdin

module Main where

import Data.Char (isSpace, isUpper)
import Data.List (break, unfoldr, isPrefixOf, intercalate)
import Data.Foldable (foldl')
import Control.Arrow ((>>>))

main :: IO ()
main = getContents >>=
  (getSentences >>> count >>> show >>> putStrLn)

getSentences :: String -> [String]
getSentences = map (intercalate " ") . unfoldr firstSentence . getWords
  where
    firstSentence :: [String] -> Maybe ([String],[String])
    firstSentence [] = Nothing
    firstSentence xs = Just $ break2 isBoundary xs

break2 :: (a -> a -> Bool) -> [a] -> ([a],[a])
break2 p xs = foo [] xs
  where
    foo acc []  = (reverse acc, [])
    foo acc [y] = (reverse (y:acc), [])
    foo acc (y1:y2:ys) = if p y1 y2
      then (reverse (y1:acc), y2:ys)
      else foo (y1:acc) (y2:ys)

isBoundary :: String -> String -> Bool
isBoundary xs ys
  | or [ (xs `endsWith` ".")   && (xs `notEndsWith` "...")
       , (xs `endsWith` ".\"") && (xs `notEndsWith` "...\"")
       , (xs `endsWith` ".'")  && (xs `notEndsWith` "...'")
       , (xs `endsWith` ".)")  && (xs `notEndsWith` "...)")
       , xs `endsWith` "!"
       , xs `endsWith` "!\""
       , xs `endsWith` "!'"
       , xs `endsWith` "!)"
       , xs `endsWith` "?"
       , xs `endsWith` "?\""
       , xs `endsWith` "?'"
       , xs `endsWith` "?)"
       ]
      &&
    or [ isCapitalized ys
       , ys `startsWith` "\""
       , ys `startsWith` "'"
       , ys `startsWith` "("
       ]
    = True
  | otherwise = False
  where
    isCapitalized ""    = False
    isCapitalized (x:_) = isUpper x
 
    endsWith as bs = (reverse bs) `isPrefixOf` (reverse as)
    notEndsWith as bs = not (as `endsWith` bs)

    startsWith as bs = bs `isPrefixOf` as
    notStartsWith as bs = not (as `startsWith` bs)

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
