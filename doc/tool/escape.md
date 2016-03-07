---
title: Software Tools in Haskell: escape
subtitle: replace strange chars on stdin with escape sequences
author: nbloomf
---

The ``escape`` program is the companion of [``unescape``](/pages/sth/tool/unescape.html); it replaces any non-printing, non-ASCII characters with C-style escape sequences using only visible ASCII.


```haskell
-- sth-escape: replace non-printable, non-ascii chars on stdin with c escape sequences
--   character-oriented

module Main where

import System.Exit (exitSuccess)
import STH.Lib (charFilter, bsEsc)


main :: IO ()
main = do
  charFilter bsEsc
  exitSuccess
```


The work is done by ``bsEsc``:

```haskell
bsEsc :: String -> String
bsEsc = concatMap esc

esc :: Char -> String
esc x
  | 32 <= k && k <= 126 = [x]
  | k == 7    = "\\a"
  | k == 8    = "\\b"
  | k == 9    = "\\t"
  | k == 10   = "\\n"
  | k == 11   = "\\v"
  | k == 12   = "\\f"
  | k == 13   = "\\r"
  | k == 27   = "\\e"
  | k < 256   = "\\x" ++ show2Hex k
  | k < 65536 = "\\u" ++ show4Hex k
  | otherwise = "\\U" ++ show8Hex k
  where
    k = ord x

    show2Hex t = reverse $ take 2 (reverse (showHex t) ++ (repeat '0'))
    show4Hex t = reverse $ take 4 (reverse (showHex t) ++ (repeat '0'))
    show8Hex t = reverse $ take 8 (reverse (showHex t) ++ (repeat '0'))
```


``showHex`` is a library function that returns the hexadecimal expansion of a natural number.


```haskell
showHex :: (Integral n) => n -> String
showHex n
  | n < 0  = '-' : showHex (-n)
  | n == 0 = "0"
  | otherwise = map toHexDigit (digitsToBase 16 n)
      where
        toHexDigit k
          | k == 0    = '0'
          | k == 1    = '1'
          | k == 2    = '2'
          | k == 3    = '3'
          | k == 4    = '4'
          | k == 5    = '5'
          | k == 6    = '6'
          | k == 7    = '7'
          | k == 8    = '8'
          | k == 9    = '9'
          | k == 10   = 'a'
          | k == 11   = 'b'
          | k == 12   = 'c'
          | k == 13   = 'd'
          | k == 14   = 'e'
          | otherwise = 'f'
```
