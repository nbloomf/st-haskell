-- sth-translit: transliterate characters on stdin
--   character-oriented

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import STH.Lib
  (charFilter, applyListMap, padLast,
   readCharRange, reportErrorMsgs)


main :: IO ()
main = do
  args <- getArgs

  (from,to) <- case map readCharRange args of
    [Just as]          -> return (as, "")
    [Just as, Just bs] -> return (as, bs)
    otherwise          -> argError

  let
    remove   = filter (not . (`elem` from))
    translit = map (applyListMap $ zip from (padLast to))

  case to of
    ""        -> charFilter remove
    otherwise -> charFilter translit

  exitSuccess


argError :: IO a
argError = reportErrorMsgs
  [ "usage:"
  , "  translit [FROM] [TO]  -- replace chars in FROM by those in TO"
  , "  translit [REMOVE]     -- remove chars in REMOVE"
  ] >> exitFailure
