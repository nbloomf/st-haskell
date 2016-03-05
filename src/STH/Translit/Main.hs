-- sth-translit: transliterate characters on stdin
--   character-oriented

module Main where

import STH.Lib
  (getArgs, exitSuccess, exitFailure)
import STH.Lib.IO (charFilter)
import STH.Lib.List (applyListMap, padLast)
import STH.Lib.Read (readCharRange)
import STH.Lib.Error (reportErrorMsgs)

main :: IO ()
main = do
  args <- getArgs

  (from,to) <- case map readCharRange args of
    [Just as]          -> return (as, "")
    [Just as, Just bs] -> return (as, bs)
    otherwise          -> argError

  case to of
    ""        -> charFilter $
                   filter (not . (`elem` from))
    otherwise -> charFilter $
                   map (applyListMap $ zip from (padLast to))

  exitSuccess


argError :: IO a
argError = reportErrorMsgs
  [ "Args should be two or one lists/ranges of characters."
  , "either 'from to' or 'remove'"
  ] >> exitFailure
