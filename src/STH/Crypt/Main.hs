-- sth-crypt: xor chars on stdin with a list of keys

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import STH.Lib (charFilter, bsUnEsc, xor)


main :: IO ()
main = do
  keys <- fmap (map bsUnEsc) getArgs
  charFilter (cryptWith keys)
  exitSuccess


cryptWith :: [String] -> String -> String
cryptWith ks str = foldr crypt str ks
  where
    crypt :: String -> String -> String
    crypt ""  str = str
    crypt key str = zipWith xor str (concat $ repeat key)
