-- sth-crypt: xor stdin with a list of keys
--   character-oriented

module Main where

import STH.Lib (getArgs, exitSuccess)
import STH.Lib.IO (charFilter)
import STH.Lib.Text
  (toCodePoint, fromCodePoint)
import STH.Lib.Text.Esc (bsUnEsc)
import STH.Lib.Bit (intXOR)

main :: IO ()
main = do
  keys <- getArgs
  charFilter (cryptKeys (map bsUnEsc keys))
  exitSuccess


cryptKeys :: [String] -> String -> String
cryptKeys []     str = str
cryptKeys (k:ks) str = cryptKeys ks (crypt k str)

crypt :: String -> String -> String
crypt ""  str  = str
crypt key str = zipWith xorChar str (concat $ repeat key)
  where
    xorChar :: Char -> Char -> Char
    xorChar x y
      = fromCodePoint $ intXOR (toCodePoint x) (toCodePoint y)
