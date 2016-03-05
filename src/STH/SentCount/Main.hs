-- sth-sentcount: count sentences on stdin

module Main where

import STH.Lib
  ((>>>), unfoldr, intercalate,
   isPrefixOf, isSuffixOf, isUpper)
import STH.Lib.IO (charFilter, putNewLine)
import STH.Lib.List (count, break2)
import STH.Lib.Text (getWords)


main :: IO ()
main = do
  charFilter (show . count . getSentences)
  putNewLine


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
