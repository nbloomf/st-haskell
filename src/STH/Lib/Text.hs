module STH.Lib.Text (
  getLines, getWords, getGlyphs, toCodePoint, fromCodePoint
) where

import Data.List (unfoldr)
import Data.Char (isSpace, isMark, ord, chr)
import STH.Lib.List (count, unfoldrMaybe)


{-|
  The 'getLines' function takes a string of
  characters and splits it at any instances of
  the newline character ('\n'). The resulting
  strings do not contain any newlines.
-}
getLines :: String -> [String]
getLines = unfoldr firstLine
  where
    firstLine :: String -> Maybe (String, String)
    firstLine xs = case break (== '\n') xs of
      ("","")   -> Nothing
      (as,"")   -> Just (as,"")
      (as,b:bs) -> Just (as,bs)


{-|
  The 'getWords' function takes a string of
  characters and splits it into "words", which
  are defined here as maximal substrings not
  containing any whitespace characters.
-}
getWords :: String -> [String]
getWords = unfoldr firstWord
  where
    firstWord :: String -> Maybe (String, String)
    firstWord xs = case dropWhile isSpace xs of
      "" -> Nothing
      ys -> Just $ break isSpace ys


{-|
  The 'getGlyphs' function splits a string of characters
  into "glyphs": non-mark characters followed by maximal
  substrings of mark characters.
-}
getGlyphs :: String -> [String]
getGlyphs = unfoldr firstGlyph
  where
    firstGlyph :: String -> Maybe (String, String)
    firstGlyph "" = Nothing
    firstGlyph (x:xs) = if isMark x
      then Just $ break (not . isMark) (x:xs)
      else do
        let (as,bs) = break (not . isMark) xs
        Just (x:as, bs)


toCodePoint :: Char -> Int
toCodePoint = ord

fromCodePoint :: Int -> Char
fromCodePoint = chr
