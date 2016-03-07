---
title: Software Tools in Haskell: compare
subtitle: find the first position where two text streams differ
author: nbloomf
---

The purpose of ``compare`` is to detect whether or not two streams of text are byte-for-byte identical. This alone is simple enough, but the job is complicated by the fact that (1) there are a few useful places these text streams might come from and (2) in the (typical) case that the streams are *not* identical, we want to report the position of the first difference.

The ``diffList`` function takes two lists and returns the position of the earliest difference as well as the differing elements (if they exist).


```haskell
diffList :: (Eq a) => [a] -> [a] -> (Maybe a, Maybe a, Integer)
diffList = comp 1
  where
    comp _ []     []     = (Nothing, Nothing, 0)
    comp k []     (y:_)  = (Nothing, Just y, k)
    comp k (x:_)  []     = (Just x, Nothing, k)
    comp k (x:xs) (y:ys) = if x == y
      then comp (k+1) xs ys
      else (Just x, Just y, k)
```


This isn't quite what we want, though. The problem is that word "position". The most useful *position* information will depend on what kind of text is being compared. For instance, when comparing line text we'd like to report the line and column numbers of the earliest difference, rather than just the character index. The ``diffLists`` function does this.


```haskell
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
```


Like we did with ``echo``, we'll allow the user to specify which kind of position they mean with a ``--char`` option (default is line). Now the streams to be compared (of which we need two) can come from one of three places:

1. ``stdin``,
2. a file (one or two), or
3. command line arguments (interpreted like ``echo``).

The main program first reads the arguments and extracts (1) the mode (char or line) of text being compared, (2) the name and contents of the first stream to be compared, and (3) the name and contents of the second stream to be compared. Then we evaluate either ``diffList`` or ``diffLists`` and report the results.


```haskell
-- compare: find the first position where two text streams differ

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import Data.Maybe (maybeToList)
import STH.Lib
  (reportErrorMsgs, getLines, bsUnEsc,
   padToLongerWith, diffList, diffLists, fromLines)


data Mode = Chars | Lines


main :: IO ()
main = do
  args <- getArgs

  -- interpret arguments
  (mode,(name1,stream1),(name2,stream2)) <- do
    let
      (flag,rest) = case args of
        ("--char":xss) -> (Chars,xss)
        xss            -> (Lines,xss)

    case rest of
      ("--to":ys) -> do
        let
          as = bsUnEsc $ case flag of
            Chars -> concat ys
            Lines -> fromLines ys
        bs <- getContents
        return (flag,("args",as),("stdin",bs))
      [xs] -> do
        as <- readFile xs
        bs <- getContents
        return (flag,(xs,as),("stdin",bs))
      [xs,ys] -> do
        as <- readFile xs
        bs <- readFile ys
        return (flag,(xs,as),(ys,bs))
      otherwise -> argError >> exitFailure


  -- Some helpers
  let
    (label1,label2) = padToLongerWith ' ' name1 name2

    report label [] = putStrLn $ label ++ ": (empty)"
    report label xs = putStrLn $ label ++ ": " ++ xs


  case mode of
    Chars -> case diffList stream1 stream2 of
      (Nothing, Nothing, _) -> return ()
      (x, y, t) -> do
        putStrLn $ "first differ at column " ++ show t
        report label1 (maybeToList x)
        report label2 (maybeToList y)

    Lines -> case diffLists (getLines stream1) (getLines stream2) of
      (Nothing, Nothing, _, _) -> return ()
      (x, y, m, n) -> do
        putStrLn $ "first differ at line " ++ show m ++ ", column " ++ show n
        report label1 (concat $ maybeToList x)
        report label2 (concat $ maybeToList y) 

  exitSuccess


argError :: IO ()
argError = do
  reportErrorMsgs
    [ "usage:"
    , "  compare FILE1 FILE2  -- find first discrepancy between FILE1 and FILE2"
    , "  compare FILE         -- find first discrepancy between FILE and stdin"
    , "  compare --to STR ... -- find first discrepancy between STRs and stdin"
    , "options:"
    , "  --char : compare as (unlined) character streams"
    ]
```


``compare`` can be used to implement a very simple testing scheme by comparing the output of some program under development to its "expected" output. One improvement I can think of is to have ``compare`` optionally output delimited data, to make it easier to extract this information with other tools.
