-- wye: write stdin to files and stdout

module Main where

import System.Exit (exitSuccess)
import System.Environment (getArgs)
import System.IO
  (openFile, hClose, IOMode(AppendMode,WriteMode), hPutStrLn)
import STH.Lib (lineFilterIO)

main :: IO ()
main = do
  args <- getArgs

  -- interpret arguments
  handles <- do
    let
      (mode,rest) = case args of
        ("--append":xs) -> (AppendMode,xs)
        xs              -> (WriteMode, xs)

    mapM (\name -> openFile name mode) rest


  let
    split str = do
      putStrLn str
      mapM_ (\handle -> hPutStrLn handle str) handles

  lineFilterIO split

  mapM_ hClose handles

  exitSuccess
