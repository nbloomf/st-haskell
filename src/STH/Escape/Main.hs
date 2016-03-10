-- sth-escape: replace non-printable, non-ascii chars on stdin with c escape sequences

module Main where

import System.Exit (exitSuccess)
import System.Environment (getArgs)
import STH.Lib
  (charFilter, lineFilter, bsEsc)


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--char"] -> charFilter bsEsc
    otherwise  -> lineFilter bsEsc

  exitSuccess
