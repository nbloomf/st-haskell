module STH.Lib.List (
  count, applyListMap, break2, fromSparseList,
  spanAtMostWhile, padToByAfter, maxMonoSubseqsBy,
  unfoldrMaybe, getRuns, fromRuns, padLast, padToLongerWith,
  diffList, diffLists, peelPrefix, takeBetween, breakAt,
  getEltsByIndex, chunksOf
) where

import Data.Foldable (foldl')
import Data.List (unfoldr, sort)


{-|
  The 'count' function finds the length
  of its list argument; it is polymorphic
  in the result type.
-}
--count.S
count :: (Num t) => [a] -> t
count = foldl' inc 0
  where inc n _ = n+1
--count.E


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
--applyListMap.S
applyListMap :: (Eq a) => [(a,a)] -> a -> a
applyListMap zs x = case lookup x zs of
  Nothing -> x
  Just y  -> y
--applyListMap.E


{-|
  The 'break2' function acts much like 'break'
  from 'Data.List', with the difference that it
  looks ahead one list element when deciding
  whether to split at a given index.
-}
--break2.S
break2 :: (a -> a -> Bool) -> [a] -> ([a],[a])
break2 p xs = accum [] xs
  where
    accum zs []  = (reverse zs, [])
    accum zs [y] = (reverse (y:zs), [])
    accum zs (y1:y2:ys) = if p y1 y2
      then (reverse (y1:zs), y2:ys)
      else accum (y1:zs) (y2:ys)
--break2.E


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
--fromSparseList.S
fromSparseList :: a -> [(a,Int)] -> [a]
fromSparseList x [] = []
fromSparseList x ys = accum 1 [] ys
  where
    accum _ as [] = reverse as
    accum t as ((z,h):zs) = case compare t h of
      EQ -> accum (t+1) (z:as) zs
      LT -> accum (t+1) (x:as) ((z,h):zs)
      GT -> accum (t+1) as zs
--fromSparseList.E


{-|
  'spanAtMostWhile' breaks at most k initial
  elements which satisfy predicate p.
-}
--spanAtMostWhile.S
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
--spanAtMostWhile.E


{-|
  'padToByAfter' takes an integer k, a thing
  z, and a list xs, and returns a list of length
  k consisting of the elements of xs pads xs to length k
  by postpending copies of z. If xs is longer
  than k there is an error. (We take "pad" very
  seriously.)
-}
--padToByAfter.S
padToByAfter :: Int -> a -> [a] -> [a]
padToByAfter k z xs = take k (xs ++ repeat z)
--padToByAfter.E


padToLongerWith :: a -> [a] -> [a] -> ([a], [a])
padToLongerWith _ [] [] = ([],[])
padToLongerWith z [] ys = unzip $ zip (repeat z) ys
padToLongerWith z xs [] = unzip $ zip xs (repeat z)
padToLongerWith z (x:xs) (y:ys) =
  let (as,bs) = padToLongerWith z xs ys
  in (x:as, y:bs)


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
--maxMonoSubseqsBy.S
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
--maxMonoSubseqsBy.E


unfoldrMaybe :: (b -> Maybe (Maybe (a,b))) -> b -> Maybe [a]
unfoldrMaybe f x = case f x of
  Nothing -> Nothing
  Just Nothing -> Just []
  Just (Just (a,b)) -> do
    as <- unfoldrMaybe f b
    Just (a:as)


--getRuns.S
getRuns :: (Eq a) => [a] -> [(a, Int)]
getRuns = unfoldr firstRun
  where
    firstRun :: (Eq a) => [a] -> Maybe ((a, Int), [a])
    firstRun []     = Nothing
    firstRun (x:xs) = let (as,bs) = span (== x) xs in
      Just ((x, 1 + count as), bs)
--getRuns.E

--fromRuns.S
fromRuns :: [(a, Int)] -> [a]
fromRuns = concatMap (\(x,k) -> replicate k x)
--fromRuns.E

padLast :: [a] -> [a]
padLast []     = []
padLast [x]    = repeat x
padLast (x:xs) = x : padLast xs

--diffList.S
diffList :: (Eq a) => [a] -> [a] -> (Maybe a, Maybe a, Integer)
diffList = comp 1
  where
    comp _ []     []     = (Nothing, Nothing, 0)
    comp k []     (y:_)  = (Nothing, Just y, k)
    comp k (x:_)  []     = (Just x, Nothing, k)
    comp k (x:xs) (y:ys) = if x == y
      then comp (k+1) xs ys
      else (Just x, Just y, k)
--diffList.E


--diffLists.S
diffLists :: (Eq a) => [[a]] -> [[a]]
  -> (Maybe [a], Maybe [a], Integer, Integer)
diffLists = comp 1
  where
    comp _ []       []       = (Nothing, Nothing, 0, 0)
    comp m []       (ys:yss) = (Nothing, Just ys, m, 1)
    comp m (xs:xss) []       = (Just xs, Nothing, m, 1)
    comp m (xs:xss) (ys:yss) = case diffList xs ys of
      (Nothing, Nothing, _) -> comp (m+1) xss yss
      (_,_,n) -> (Just xs, Just ys, m, n)
--diffLists.E

peelPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
peelPrefix [] ys = Just ys
peelPrefix _  [] = Nothing
peelPrefix (x:xs) (y:ys) = if x == y
  then peelPrefix xs ys
  else Nothing

--takeBetween.S
takeBetween :: (Eq a) => (a,a) -> [a] -> [a]
takeBetween (u,v) = concat . unfoldr (firstCut (u,v))
  where
    firstCut (u,v) ys = case dropWhile (/= u) ys of
      []     -> Nothing
      (_:zs) -> Just $ span (/= v) zs
--takeBetween.E

chunksOf :: Int -> [a] -> Maybe [[a]]
chunksOf k xs
  | k <= 0    = Nothing
  | otherwise = Just $ chunk xs
  where
    chunk [] = []
    chunk ys = let (as,bs) = splitAt k ys in as : chunk bs

breakAt :: (Eq a) => a -> [a] -> [[a]]
breakAt x = breakBy (== x)

breakBy :: (a -> Bool) -> [a] -> [[a]]
breakBy _ [] = [[]]
breakBy p xs = case break p xs of
  (ys,[])   -> [ys]
  (ys,_:zs) -> ys : breakBy p zs


--getEltsByIndex.S
getEltsByIndex :: (Int -> Bool) -> [a] -> [a]
getEltsByIndex p xs = map snd $ filter (p . fst) $ zip [1..] xs
--getEltsByIndex.E
