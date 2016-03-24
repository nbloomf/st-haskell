-- linenumber: number lines on stdin

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import STH.Lib
  (reportErrorMsgs, readDecimalNat, getLines,
   putStrLns, padToByBefore)

data Mode = Tab | Pad

main :: IO ()
main = do
  args <- getArgs

  (mode,from) <- do
    let
      (m,rest) = case args of
        ("--pad":xs) -> (Pad, xs)
        xs           -> (Tab, xs)

    case rest of
      ["--from",k] -> case readDecimalNat k of
        Nothing -> argErr >> exitFailure
        Just t  -> return (m,t)
      otherwise -> return (m,1)

  lns <- fmap ((zip [from..]) . getLines) getContents

  case mode of
    Tab -> do
      let wr (a,str) = show a ++ "\t" ++ str
      putStrLns $ map wr lns

    Pad -> do
      let
        len = case lns of
          [] -> 0
          zs -> length $ show $ fst $ last zs
        pad n = padToByBefore len ' ' (show n)
        wr (a,str) = pad a ++ " " ++ str
      putStrLns $ map wr lns

  exitSuccess



argErr :: IO ()
argErr = reportErrorMsgs
  [ "usage:"
  , "  linenumber       : prepend line numbers (tab separated)"
  , "  linenumber --pad : prepend line numbers (padded with spaces)"
  , "options:"
  , "  --from NAT : start numbering at NAT; default is 1"
  ]
