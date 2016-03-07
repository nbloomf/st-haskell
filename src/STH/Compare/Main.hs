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
