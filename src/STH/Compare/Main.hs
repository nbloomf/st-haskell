-- compare: find the first position where two text streams differ

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import STH.Lib (reportErrorMsgs, getLines, bsUnEsc)

main :: IO ()
main = do
  args <- getArgs

  params <- case args of
    ["--char",xs]    -> return (Chars,xs,"")
    ["--char",xs,ys] -> return (Chars,xs,ys)
    ["--to",xs]      -> return (ToStr,xs,"")
    [ys,"--to",xs]   -> return (ToStr,xs,ys)
    [xs]             -> return (Lines,xs,"")
    [xs,ys]          -> return (Lines,xs,ys)
    otherwise        -> argError >> exitFailure

  let
    compChar (na,as) (nb,bs) = case diffList as bs of
      (Nothing, Nothing, _) -> return ()
      (Just a, Nothing, t) -> do
        putStrLn $ "first differ at column " ++ show t
        putStrLn $ ma ++ ": " ++ [a]
        putStrLn $ mb ++ ": (empty)"
      (Nothing, Just b, t) -> do
        putStrLn $ "first differ at column " ++ show t
        putStrLn $ ma ++ ": (empty)"
        putStrLn $ mb ++ ": " ++ [b]
      (Just a, Just b, t) -> do
        putStrLn $ "first differ at column " ++ show t
        putStrLn $ ma ++ ": " ++ [a]
        putStrLn $ mb ++ ": " ++ [b]
      where
        (ma,mb) = padToLonger na nb

    compLine (na,as) (nb,bs) = case diffLists as bs of
      (Nothing, Nothing, _, _) -> return ()
      (Just a, Nothing, m, n) -> do
        putStrLn $ "first differ at line " ++ show m ++ ", column " ++ show n
        putStrLn $ ma ++ ": " ++ a
        putStrLn $ mb ++ ": (empty)"
      (Nothing, Just b, m, n) -> do
        putStrLn $ "first differ at line " ++ show m ++ ", column " ++ show n
        putStrLn $ ma ++ ": (empty)"
        putStrLn $ mb ++ ": " ++ b
      (Just a, Just b, m, n) -> do
        putStrLn $ "first differ at line " ++ show m ++ ", column " ++ show n
        putStrLn $ ma ++ ": " ++ a
        putStrLn $ mb ++ ": " ++ b
      where
        (ma,mb) = padToLonger na nb

  case params of
    (Chars,xs,"") -> do
      as <- readFile xs
      bs <- getContents
      compChar (xs,as) ("stdin", bs)
    (Chars,xs,ys) -> do
      as <- readFile xs
      bs <- readFile ys
      compChar (xs,as) (ys,bs)
    (ToStr,xs,"") -> do
      bs <- getContents
      compChar ("arg", bsUnEsc xs) ("stdin", bs)
    (ToStr,xs,ys) -> do
      bs <- readFile ys
      compChar ("arg", bsUnEsc xs) (ys,bs)
    (Lines,xs,"") -> do
      as <- fmap getLines $ readFile xs
      bs <- fmap getLines $ getContents
      compLine (xs,as) ("stdin",bs)
    (Lines,xs,ys) -> do
      as <- fmap getLines $ readFile xs
      bs <- fmap getLines $ readFile ys
      compLine (xs,as) (ys,bs)

  exitSuccess


data Flag = Chars | Lines | ToStr


argError :: IO ()
argError = do
  reportErrorMsgs
    [ "usage:"
    , "  compare [FILE1] [FILE2] -- find first discrepancy between FILE1 and FILE2"
    , "  compare [FILE]          -- find first discrepancy between FILE and stdin"
    , "options:"
    , "  --char   : compare as (unlined) character strings"
    , "  --to STR : compare file or stdin to string argument"
    ]

diffList :: (Eq a) => [a] -> [a] -> (Maybe a, Maybe a, Integer)
diffList = comp 1
  where
    comp _ []     []     = (Nothing, Nothing, 0)
    comp k []     (y:_)  = (Nothing, Just y, k)
    comp k (x:_)  []     = (Just x, Nothing, k)
    comp k (x:xs) (y:ys) = if x == y
      then comp (k+1) xs ys
      else (Just x, Just y, k)

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


padToLonger :: String -> String -> (String, String)
padToLonger [] ys = unzip $ zip (repeat ' ') ys
padToLonger xs [] = unzip $ zip xs (repeat ' ')
padToLonger (x:xs) (y:ys) =
  let (as,bs) = padToLonger xs ys
  in (x:as, y:bs)
