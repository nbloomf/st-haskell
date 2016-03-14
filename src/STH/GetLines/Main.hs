-- getlines: extract lines from stdin by index

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import STH.Lib
  (getEltsByIndex, reportErrorMsgs, readIntSet,
   putStrLns, getLines, readCCLines, renderCCLine,
   putCCLns)


data Mode = Lines | ASACC


main :: IO ()
main = do
  args <- getArgs

  -- interpret arguments
  (mode,tests) <- do
    let
      (flag,rest) = case args of
        ("--asacc":zs) -> (ASACC, zs)
        zs             -> (Lines, zs)

    ps <- case sequence $ map readIntSet rest of
      Just xs -> return xs
      Nothing -> argErr >> exitFailure

    return (flag,ps)

  let get xs p = getEltsByIndex p xs

  case mode of
    Lines -> do
      lines <- fmap getLines getContents
      putStrLns $ concatMap (get lines) tests
    ASACC -> do
      lines <- fmap readCCLines getContents
      case lines of
        Nothing -> corrErr >> exitFailure
        Just zs -> putCCLns $ concatMap (get zs) tests

  exitSuccess


argErr :: IO ()
argErr = reportErrorMsgs
  [ "usage:"
  , "  getlines INTSET ... : extract lines from stdin at indices in RANGE (sorted)"
  , "options:"
  , "  --asacc : read as ASA carriage control lines"
  ]


corrErr :: IO ()
corrErr = reportErrorMsgs
  [ "corrupt input" ]
