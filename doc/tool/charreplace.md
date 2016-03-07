---
title: Software Tools in Haskell: charreplace
subtitle: replace chars by strings on stdin
author: nbloomf
---

The program ``charreplace`` is a companion to [``translit``](/pages/sth/tool/translit.html) and requires no new machinery.


```haskell
-- sth-charreplace: replace chars with strings on stdin
--   character-oriented

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import STH.Lib
  (charFilter, bsUnEsc, readCharRange, reportErrorMsgs)

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
  [ "usage:"
  , "  charreplace [SOURCE] [TARGET] -- replace each char in SOURCE with TARGET string"
  , "  charreplace [REMOVE]          -- remove each char in REMOVE string"
  , "option:"
  , "  --not  : invert selection (e.g. replace all *except* SOURCE)"
  ] >> exitFailure
```


It may seem like overkill to split the functionality from ``translit`` and ``charreplace`` just to make the interface more consistent. But note that ``charreplace`` naturally does something we couldn't have done if the two were rolled together, at least not without making the interface even *more* inconsistent: ``charreplace`` naturally replaces characters by **strings**, not just characters. This is not a trivial distinction; for example, if we have a text file which uses unix-style line endings (``\n``) and want to convert them to Windows-style line endings (``\r\n``) we can do this with

    charreplace "\n" "\r\n"

I can't think of a way to do this with ``translit`` alone.
