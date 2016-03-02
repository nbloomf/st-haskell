module SoftwareTools.Lib.List (
  count, applyListMap, break2, fromSparseList,
  spanAtMostWhile, padToByAfter, maxMonoSubseqsBy,
  unfoldrMaybe
) where

import Data.Foldable (foldl')
import Data.List (unfoldr)


{-|
  The 'count' function finds the length
  of its list argument; it is polymorphic
  in the result type.
-}
count :: (Num t) => [a] -> t
count = foldl' inc 0
  where inc n _ = n+1


{-|
  'applyListMap' treats a list of pairs as
  a mapping, like the standard function 'lookup'.
  Unlike lookup, 'applyListMap' is the identity
  on any elements not in the domain of its list
  map argument.

  applyListMap [] === id

  applyListMap [(1,5),(2,7)] 1 === 5
  applyListMap [(1,5),(2,7)] 3 === 3
  applyListMap [(3,5),(3,7)] 3 === 5
-}
applyListMap :: (Eq a) => [(a,a)] -> a -> a
applyListMap zs x = case lookup x zs of
  Nothing -> x
  Just y  -> y


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
  A *sparse list* is a list of element-index pairs
  and a filler element. It allows a more compact
  representation of large lists consisting of entries
  that are mostly some fixed element. 'fromSparseList'
  converts sparse lists to the standard list form.
  Note that by convention, if two indices appear
  out of strict order in a sparse list, the second has
  no effect. (Indices are processed in order.)

  fromSparseList ' ' [('a',1),('b',5)] === "a   b"
  fromSparseList ' ' [('b',5),('a',1)] === "    b"
  fromSparseList ' ' [('b',5),('a',5)] === "    b"
-}
fromSparseList :: a -> [(a,Int)] -> [a]
fromSparseList x [] = []
fromSparseList x ys = accum 1 [] ys
  where
    accum _ as [] = reverse as
    accum t as ((z,h):zs) = case compare t h of
      EQ -> accum (t+1) (z:as) zs
      LT -> accum (t+1) (x:as) ((z,h):zs)
      GT -> accum (t+1) as zs


{-|
  'spanAtMostWhile' breaks at most k initial
  elements which satisfy predicate p.
-}
spanAtMostWhile :: Int -> (a -> Bool) -> [a] -> ([a],[a])
spanAtMostWhile k p xs
  | k < 0     = ([],xs)
  | otherwise = acc k [] xs
  where
    acc 0 as bs = (reverse as, bs)
    acc _ as [] = (reverse as, [])
    acc t as (b:bs) = if p b
      then acc (t-1) (b:as) bs
      else (reverse as, b:bs)


{-|
  'padToByAfter' takes an integer k, a thing
  z, and a list xs, and returns a list of length
  k consisting of the elements of xs pads xs to length k
  by postpending copies of z. If xs is longer
  than k there is an error. (We take "pad" very
  seriously.)
-}
padToByAfter :: Int -> a -> [a] -> [a]
padToByAfter k z xs = take k (xs ++ repeat z)


{-|
  A subsequence of a list is a selection of some
  elements in increasing index order. A subsequence
  is monotone by a given relation p if any two 
  consecutive elements in the subsequence satisfy p.
  A subsequence, monotone by some relation, is said to
  be maximal if it includes the head of the list and
  cannot be made longer. Every list has a *unique* 
  maximal monotone subsequence by a given relation.
  'maxMonoSubseqsBy' returns the list of these
  maximal monotone subsequences in order.
-}
maxMonoSubseqsBy :: (a -> a -> Bool) -> [a] -> [[a]]
maxMonoSubseqsBy p = unfoldr maxMonoSubseq
  where
    maxMonoSubseq [] = Nothing
    maxMonoSubseq xs = accum [] [] xs

    accum as bs [] = Just (reverse as, reverse bs)
    accum [] bs (z:zs) = accum [z] bs zs
    accum (a:as) bs (z:zs) = if p a z
      then accum (z:a:as) bs zs
      else accum (a:as) (z:bs) zs


unfoldrMaybe :: (b -> Maybe (Maybe (a,b))) -> b -> Maybe [a]
unfoldrMaybe f x = case f x of
  Nothing -> Nothing
  Just Nothing -> Just []
  Just (Just (a,b)) -> do
    as <- unfoldrMaybe f b
    Just (a:as)
