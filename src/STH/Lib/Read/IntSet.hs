module STH.Lib.Read.IntSet (
  readIntSet
) where

import STH.Lib.Read (readDecimalNat)
import STH.Lib.List (breakAt, unfoldrMaybe)

--IntSet.S
data IntSet
  = Single Int
  | Range  Int Int
  | Skip   Int Int
  deriving (Show)

inIntSet :: Int -> [IntSet] -> Bool
inIntSet k ms = or $ map (inIntSet' k) ms
  where
    inIntSet' :: Int -> IntSet -> Bool
    inIntSet' k (Single m)  = k == m
    inIntSet' k (Range a b) = (a <= k) && (k <= b)
    inIntSet' k (Skip a b)  = (k >= a) && ((k-a)`rem`b == 0)

readIntSet :: String -> Maybe (Int -> Bool)
readIntSet xs = do
  cs <- readIntSet' xs
  return (\k -> inIntSet k cs)

readIntSet' :: String -> Maybe [IntSet]
readIntSet' = sequence . map oneIntSeq . breakAt ','
  where
    oneIntSeq :: String -> Maybe IntSet
    oneIntSeq "" = Nothing
    oneIntSeq xs = case readDecimalNat xs of
      Just k  -> Just $ Single k
      Nothing -> case map readDecimalNat $ breakAt '-' xs of
        [Just a, Just b] -> Just $ Range a b
        otherwise        -> case map readDecimalNat $ breakAt '+' xs of
          [Just a, Just b] -> Just $ Skip a b
          otherwise        -> Nothing
--IntSet.E
