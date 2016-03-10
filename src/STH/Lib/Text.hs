module STH.Lib.Text (
  fromLines, getWords, getSentences, getGlyphs
) where

import Data.List (unfoldr, isPrefixOf, isSuffixOf, intercalate)
import Data.Char (isSpace, isMark, ord, chr, isUpper)
import STH.Lib.List (count, unfoldrMaybe, break2)


fromLines :: [String] -> String
fromLines xs = concat $ zipWith (++) xs (repeat "\n")


{-|
  The 'getWords' function takes a string of
  characters and splits it into "words", which
  are defined here as maximal substrings not
  containing any whitespace characters.
-}
--getWords.S
getWords :: String -> [String]
getWords = unfoldr firstWord
  where
    firstWord :: String -> Maybe (String, String)
    firstWord xs = case dropWhile isSpace xs of
      "" -> Nothing
      ys -> Just $ break isSpace ys
--getWords.E


{-|
  The 'getSentences' function takes a string of
  characters and splits it into "sentences". It works by
  first splitting the string into words, and then
  using heuristics to decide which word boundaries are
  also sentence boundaries. These heuristics are
  carried out by the 'isSentenceBoundary' helper function.
  Beware that this function assumes its input is standard
  English prose, and even then will fall down on many
  special cases.
-}
--getSentences.S
getSentences :: String -> [String]
getSentences = map (intercalate " ") . unfoldr firstSentence . getWords
  where
    firstSentence :: [String] -> Maybe ([String],[String])
    firstSentence [] = Nothing
    firstSentence xs = Just $ break2 isSentenceBoundary xs

    isSentenceBoundary :: String -> String -> Bool
    isSentenceBoundary xs ys
      | finalEllipsis xs && not (isSentenceStart ys) = False
      | isSentenceEnd xs && isSentenceStart ys = True
      | otherwise = False
      where
        isCapitalized ""    = False
        isCapitalized (x:_) = isUpper x

        finalEllipsis :: String -> Bool
        finalEllipsis xs = or $ map (`isSuffixOf` xs)
          ["...", "...\"", "...)"]

        isSentenceEnd :: String -> Bool
        isSentenceEnd xs = or $ map (`isSuffixOf` xs)
          [ ".", ".\"", ".'", ".)"
          , "!", "!\"", "!'", "!)"
          , "?", "?\"", "?'", "?)"
          ]

        isSentenceStart :: String -> Bool
        isSentenceStart ys = (isCapitalized ys)
          || (or $ map (`isPrefixOf` ys) ["\"", "'", "("])
--getSentences.E


{-|
  The 'getGlyphs' function splits a string of characters
  into "glyphs": non-mark characters followed by maximal
  substrings of mark characters.
-}
--getGlyphs.S
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
--getGlyphs.E


unSnoc :: [a] -> Maybe ([a],a)
unSnoc xs = case reverse xs of
  []     -> Nothing
  (x:xs) -> Just (reverse xs, x)
