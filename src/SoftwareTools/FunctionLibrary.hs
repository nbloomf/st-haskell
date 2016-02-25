module SoftwareTools.FunctionLibrary (
  catchEOF,

  readDecimalNat, readPosIntList,

  count, break2,

  getLines, getWords, getSentences, getGlyphs,
  convertTabStops
) where

import System.IO.Error (isEOFError, catchIOError)
import System.Exit (exitSuccess, exitFailure)
import Data.Foldable (foldl')
import Data.List (break, unfoldr, isPrefixOf, isSuffixOf, intercalate, lookup)
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
-- = Parsing Utilities

{-|
  Convert a list of strings of decimal digits
  to a list of integers. (Natural numbers only.)
-}
readPosIntList :: [String] -> Maybe [Int]
readPosIntList =
  sequence . map (guardMaybe (>0)) . map readDecimalNat


{-|
  Convert a string (decimal digits) to an integer.
  (Natural numbers only.)
-}
readDecimalNat :: String -> Maybe Int
readDecimalNat xs = do
  ys <- sequence $ map decToInt $ reverse xs
  return $ sum $ zipWith (*) ys [10^t | t <- [0..]]
  where
    decToInt :: Char -> Maybe Int
    decToInt x = lookup x
      [ ('0',0), ('1',1), ('2',2), ('3',3), ('4',4)
      , ('5',5), ('6',6), ('7',7), ('8',8), ('9',9)
      ]



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
break2 p xs = accum [] xs
  where
    accum zs []  = (reverse zs, [])
    accum zs [y] = (reverse (y:zs), [])
    accum zs (y1:y2:ys) = if p y1 y2
      then (reverse (y1:zs), y2:ys)
      else accum (y1:zs) (y2:ys)


{-|
  'spanAtMostWhile' breaks at most k initial
  elements which satisfy predicate p.
-}
spanAtMostWhile :: Int -> (a -> Bool) -> [a] -> Maybe ([a],[a])
spanAtMostWhile k p xs
  | k < 0     = Nothing
  | otherwise = Just $ acc k [] xs
  where
    acc 0 as bs = (reverse as, bs)
    acc _ as [] = (reverse as, [])
    acc t as (b:bs) = if p b
      then acc (t-1) (b:as) bs
      else (reverse as, b:bs)


{-|
  'padToByAfter' takes an integer k, a thing
  z, and a list xs, and pads xs to length k
  by postpending copies of z. If xs is longer
  than k there is an error. (We take "pad" very
  seriously.)
-}
padToByAfter :: Int -> a -> [a] -> Maybe [a]
padToByAfter k z xs
  | k < 0     = Nothing
  | otherwise = acc k [] xs
  where
    acc 0 as [] = Just $ reverse as
    acc 0 _  _  = Nothing
    acc t as [] = Just $ reverse as ++ replicate t z
    acc t as (b:bs) = acc (t-1) (b:as) bs



-- |
-- = Maybe Utilities

{-|

-}
guardMaybe :: (a -> Bool) -> Maybe a -> Maybe a
guardMaybe p x = do
  y <- x
  case p y of
    True  -> Just y
    False -> Nothing



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


{-|
  'convertTabStops' takes a list of tab stop widths
  and a string and replaces all '\t's in the string
  with spaces, padded to the given tab stop widths
  in order. If the string has more '\t's than there
  are tab stop widths, then the final tab stop width
  is repeated indefinitely. If no tab stop widths are
  given the function returns Nothing.
-}
convertTabStops :: [Int] -> String -> Maybe String
convertTabStops [] _  = Nothing
convertTabStops ks xs = accum [] ks xs
  where
    accum zs _   "" = Just $ concat $ reverse zs
    accum zs [t] ys = do
      (as,bs) <- splitTabStop t ys
      accum (as:zs) [t] bs
    accum zs (t:ts) ys = do
      (as,bs) <- splitTabStop t ys
      accum (as:zs) ts bs

    splitTabStop :: Int -> String -> Maybe (String, String)
    splitTabStop k xs
      | k <= 0    = Nothing
      | otherwise = do
          (as,bs) <- spanAtMostWhile k (/= '\t') xs
          cs      <- padToByAfter k ' ' as
          case bs of
            ""      -> return (cs,"")
            '\t':ds -> return (cs,ds)
            ds      -> return (cs,ds)
