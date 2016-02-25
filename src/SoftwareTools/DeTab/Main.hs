-- sth-detab: convert tabs to spaces

module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary
  (getLines, convertTabStops, readPosIntList)


main :: IO ()
main = do
  args <- getArgs

  -- Read positive integer tabstop arguments.
  -- Default is [8].
  ts <- case readPosIntList args of
    Just [] -> return [8]
    Just ks -> return ks
    Nothing -> errorParsingArgs

  -- Detab a single line ln with tabstops ts.
  let detab ln = case convertTabStops ts ln of
                   Nothing -> errorDeTabbing ts ln
                   Just cs -> putStrLn cs

  -- Do it!
  getContents
    >>= (getLines >>> map detab >>> sequence_)
    >> exitSuccess


-- We don't bother trying to distinguish among
-- different possible errors; just make the user
-- try again. (Worse is better!)
errorParsingArgs :: IO a
errorParsingArgs = do
  name <- getProgName
  hPutStrLn stderr $
    name ++ " error: tab widths must be positive natural numbers in base 10."
  exitFailure


-- This should not happen.
errorDeTabbing :: [Int] -> String -> IO a
errorDeTabbing ts ln = do
  name <- getProgName
  hPutStrLn stderr $ name ++ " error"
  hPutStrLn stderr $ "LINE: " ++ ln
  hPutStrLn stderr $ "TABS: " ++ show ts
  exitFailure
