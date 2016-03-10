-- sth-entab: replace spaces on stdin with tabs

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import STH.Lib
  (lineFilter, readPosIntList,
   insertTabStops, reportErrorMsgs)


main :: IO ()
main = do
  args <- getArgs

  -- Read positive integer tabstop arguments.
  -- Default is [8].
  ts <- case readPosIntList args of
    Just [] -> return [8]
    Just ks -> return ks
    Nothing -> reportErrorMsgs
                 ["tab widths must be positive integers."
                 ] >> exitFailure

  -- Do it!
  lineFilter (insertTabStops ts)
  exitSuccess
