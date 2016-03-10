-- sth-charreplace: replace chars with strings on stdin

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import STH.Lib
  (charFilter, bsUnEsc, readCharSeq,
   reportErrorMsgs)

main :: IO ()
main = do
  args <- getArgs

  (notflag,from,to) <- do
    let
      (flag,rest) = case args of
        ("--not":xs) -> (True,xs)
        xs           -> (False,xs)

    (from,to) <- case rest of
      [as] -> case readCharSeq (bsUnEsc as) of
        Just xs -> return (xs, "")
        Nothing -> argError
      [as,bs] -> case readCharSeq (bsUnEsc as) of
        Just xs -> return (xs, bsUnEsc bs)
        Nothing -> argError
      otherwise -> argError

    return (flag,from,to)

  let
    remove = case notflag of
      False -> filter (not . (`elem` from))
      True  -> filter (`elem` from)

  let
    replace = case notflag of
      False -> concatMap
                 (\x -> if x`elem`from then to else [x])
      True  -> concatMap
                 (\x -> if x`elem`from then [x] else to)

  case to of
    ""        -> charFilter remove
    otherwise -> charFilter replace

  exitSuccess


argError :: IO a
argError = reportErrorMsgs
  [ "usage:"
  , "  charreplace [SOURCE] [TARGET] -- replace each char in SOURCE with TARGET string"
  , "  charreplace [REMOVE]          -- remove each char in REMOVE string"
  , "option:"
  , "  --not  : invert selection (e.g. replace all *except* SOURCE)"
  ] >> exitFailure
