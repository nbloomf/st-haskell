-- sth-escape: replace non-printable, non-ascii chars on stdin with c escape sequences
--   character-oriented

module Main where

import System.Exit (exitSuccess)
import STH.Lib (charFilter, bsEsc)


main :: IO ()
main = do
  charFilter bsEsc
  exitSuccess
