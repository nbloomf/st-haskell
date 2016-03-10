module STH.Lib.Read.CharSeq (
  readCharSeq
) where

import STH.Lib.List (unfoldrMaybe)
import STH.Lib.Text.Esc (bsUnEsc)

--CharSeq.S
data CharSeq
  = Single Char
  | Range  Char Char
  deriving (Show)

readCharSeq :: String -> Maybe String
readCharSeq = fmap charSeqsToList . readCharSeqs . bsUnEsc

charSeqsToList :: [CharSeq] -> String
charSeqsToList = concatMap charSeqToList
  where
    charSeqToList (Single x)  = [x]
    charSeqToList (Range x y) = enumFromTo x y

readCharSeqs :: String -> Maybe [CharSeq]
readCharSeqs = unfoldrMaybe firstCharSeq
  where
    firstCharSeq :: String -> Maybe (Maybe (CharSeq, String))
    firstCharSeq ""      = Just Nothing
    firstCharSeq [x]     = Just (Just (Single x, ""))
    firstCharSeq ('-':_) = Nothing
    firstCharSeq [x,y]   = Just (Just (Single x, [y]))
    firstCharSeq (x:y:z:xs) = case y of
      '-' -> Just (Just (Range x z, xs))
      otherwise -> Just (Just (Single x, y:z:xs))
--CharSeq.E
