-- sth-escape: replace non-printable, non-ascii chars on stdin with c escape sequences
--   character-oriented

module Main where

import STH.Lib (exitSuccess)
import STH.Lib.IO (charFilter)
import STH.Lib.Text.Esc (bsEsc)


main :: IO ()
main = do
  charFilter bsEsc
  exitSuccess
