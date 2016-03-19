-- examine: interactively view a file

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import STH.Lib
  (reportErrorMsgs, readDecimalNat, getLines,
   chunksOf, makeZipperList, zlFocus, zlNext,
   zlPrev, putStrLns, putOutOfBand)

main :: IO ()
main = do
  args <- getArgs

  -- interpret arguments
  (num, name) <- case args of
    [str]   -> return (10, str)
    [k,str] -> case readDecimalNat k of
      Nothing -> argErr >> exitFailure
      Just m  -> return (m, str)
    otherwise -> argErr >> exitFailure

  lns <- fmap getLines $ readFile name

  zipper <- case chunksOf num lns of
    Nothing -> argErr >> exitFailure
    Just xs -> case makeZipperList xs of
      Nothing -> argErr >> exitFailure
      Just ys -> return ys

  let
    showInstructions
      = putOutOfBand ["", "[n]ext pre[v] e[x]it [w]rite?"]

    prompt z = do
      putOutOfBand $ zlFocus z
      showInstructions
      input <- getLine
      putOutOfBand [""]
      case input of
        "x" -> return ()

        "n" -> case zlNext z of
          Just w  -> prompt w
          Nothing -> do
            putOutOfBand ["end of file. (press any key)"]
            prompt z

        "v" -> case zlPrev z of
          Just w  -> prompt w
          Nothing -> do
            putOutOfBand ["start of file. (press any key)"]
            prompt z

        "w" -> do
          putStrLns $ zlFocus z
          prompt z

        _ -> prompt z

  prompt zipper

  exitSuccess


argErr :: IO ()
argErr = reportErrorMsgs
  [ "usage:"
  , "  examine FILE     -- interactively view FILE 10 lines at a time"
  , "  examine INT FILE -- interactively view FILE INT lines at a time"
  ]
