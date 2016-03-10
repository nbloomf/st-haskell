-- sth-expand: uncompress stdin (run length encoding)

module Main where

import System.Exit (exitSuccess, exitFailure)
import STH.Lib (charFilter, rlDecode, reportErrorMsgs)

main :: IO ()
main = do
  xs <- getContents

  ys <- case rlDecode '\BEL' xs of
          Just zs -> return zs
          Nothing -> reportErrorMsgs
                       [ "corrupt input"
                       ] >> exitFailure

  putStr ys
  exitSuccess
