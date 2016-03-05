-- sth-charreplace: replace chars with strings on stdin
--   character-oriented

module Main where

import STH.Lib
  (getArgs, exitSuccess, exitFailure)
import STH.Lib.IO (charFilter)
import STH.Lib.Text.Esc (bsUnEsc)
import STH.Lib.Read (readCharRange)
import STH.Lib.Error (reportErrorMsgs)

main :: IO ()
main = do
  args <- getArgs

  (notflag,from,to) <- case args of
    [as] -> case readCharRange as of
      Just xs -> return (False, xs, "")
      Nothing -> argError
    ["--not",as] -> case readCharRange as of
      Just xs -> return (True, xs, "")
      Nothing -> argError
    [as,bs] -> case readCharRange as of
      Just xs -> return (False, xs, bsUnEsc bs)
      Nothing -> argError
    ["--not",as,bs] -> case readCharRange as of
      Just xs -> return (True, xs, bsUnEsc bs)
      Nothing -> argError
    otherwise          -> argError

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
  [ "Args should be a list of chars and a replacement string."
  , "either 'from to' or 'remove'."
  , "Optional argument: --not to invert selection."
  ] >> exitFailure
