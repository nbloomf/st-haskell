module STH.Lib.Int (
  digitsToBase, showHex
) where

digitsToBase :: (Integral n) => n -> n -> [n]
digitsToBase b k
  | b <= 1 || k <= 0 = []
  | otherwise = reverse $ foo k
      where
        foo t
          | t < b = [t]
          | otherwise = (t`rem`b) : (foo (t`quot`b))

--showHex.S
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
--showHex.E
