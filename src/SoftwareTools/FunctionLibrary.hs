module SoftwareTools.FunctionLibrary (
  catchEOF,

  count, break2,

  getLines, getWords, getSentences, getGlyphs
) where

import System.IO.Error (isEOFError, catchIOError)
import System.Exit (exitSuccess, exitFailure)
import Data.Foldable (foldl')
import Data.List (break, unfoldr, isPrefixOf, isSuffixOf, intercalate)
import Data.Char (isSpace, isUpper, isMark)



-- |
-- = IO Utilities

{-|
  The 'catchEOF' function takes an IO action
  and attempts to run it. If an EOFError is
  detected, the action exits the program
  successfully. Any other error exits with failure.
-}
catchEOF :: IO a -> IO a
catchEOF x = catchIOError x handler
  where
    handler err
      | isEOFError err = exitSuccess
      | otherwise      = exitFailure



-- |
-- = List Utilities

{-|
  The 'count' function finds the 'Integer'
  length of its list argument.
-}
count :: [a] -> Integer
count = foldl' inc 0
  where inc n _ = n+1


{-|
  The 'break2' function acts much like 'break'
  from 'Data.List', with the difference that it
  looks ahead one list element when deciding
  whether to split at a given index.
-}
break2 :: (a -> a -> Bool) -> [a] -> ([a],[a])
break2 p xs = foo [] xs
  where
    foo acc []  = (reverse acc, [])
    foo acc [y] = (reverse (y:acc), [])
    foo acc (y1:y2:ys) = if p y1 y2
      then (reverse (y1:acc), y2:ys)
      else foo (y1:acc) (y2:ys)



-- |
-- = Text Utilities

{-|
  The 'getLines' function takes a string of
  characters and splits it at any instances of
  the newline character ('\n'). The resulting
  lines do not contain any newlines.
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
