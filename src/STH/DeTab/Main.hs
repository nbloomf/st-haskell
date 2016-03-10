-- sth-detab: convert tabs to spaces

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import STH.Lib
  (lineFilter, readPosIntList, getLines,
   spanAtMostWhile, padToByAfter, reportErrorMsgs,
   convertTabStops)

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
  lineFilter (convertTabStops ts)
  exitSuccess
