-- tail: 

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import STH.Lib (readDecimalNat, reportErrorMsgs, getLines, putStrLns)

data Mode = Chars | Lines

main :: IO ()
main = do
  args <- getArgs

  (mode,k) <- do
    (flag,rest) <- case args of
      ("--char":xss) -> return (Chars,xss)
      xss            -> return (Lines,xss)

    case rest of
      []   -> return (flag,10)
      [xs] -> case readDecimalNat xs of
                Nothing -> argErr >> exitFailure
                Just t  -> return (flag,t)
      _    -> argErr >> exitFailure

  let
    getTail = reverse . take k . reverse

  case mode of
    Chars -> fmap getTail getContents
               >>= putStr
    Lines -> fmap (getTail . getLines) getContents
               >>= putStrLns

  exitSuccess


argErr :: IO ()
argErr = reportErrorMsgs
  [ "usage:"
  , "  tail     : send the last 10 lines of stdin to stdout"
  , "  tail INT : send the last INT lines of stdin to stdout"
  , "options:"
  , "  --char : get chars instead of lines"
  ]