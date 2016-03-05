-- sth-crypt: xor stdin with a list of keys
--   character-oriented

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import STH.Lib
  (charFilter, toCodePoint, fromCodePoint,
   bsUnEsc, intXOR)


main :: IO ()
main = do
  keys <- fmap (map bsUnEsc) getArgs
  charFilter (cryptWithKeys keys)
  exitSuccess


cryptWithKeys :: [String] -> String -> String
cryptWithKeys []     str = str
cryptWithKeys (k:ks) str = cryptWithKeys ks (crypt k str)

crypt :: String -> String -> String
crypt ""  str = str
crypt key str = zipWith xorChar str (concat $ repeat key)
  where
    xorChar :: Char -> Char -> Char
    xorChar x y
      = fromCodePoint $ intXOR (toCodePoint x) (toCodePoint y)
