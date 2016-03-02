module SoftwareTools.Lib.Read (
  readDecimalNat, readPosIntList, readHexadecimalNat, showBase86Nat, readBase86Nat
) where

import Control.Arrow ((>>>))
import SoftwareTools.Lib.Maybe (filterMaybe)


{-|
  Convert a list of strings of decimal digits
  to a list of integers. (Natural numbers only.)
  If any individual read fails, the whole read fails.
-}
readPosIntList :: [String] -> Maybe [Int]
readPosIntList = map readDecimalNat
  >>> map (filterMaybe (>0))
  >>> sequence


{-|
  Convert a string (decimal digits) to an integer.
  (Natural numbers only.)
-}
readDecimalNat :: String -> Maybe Int
readDecimalNat xs = do
  ys <- sequence $ map decToInt $ reverse xs
  return $ sum $ zipWith (*) ys [10^t | t <- [0..]]
  where
    decToInt :: Char -> Maybe Int
    decToInt x = lookup x
      [ ('0',0), ('1',1), ('2',2), ('3',3), ('4',4)
      , ('5',5), ('6',6), ('7',7), ('8',8), ('9',9)
      ]


{-|
  Convert a string (hexadecimal digits) to an integer.
  (Natural numbers only.)
-}
readHexadecimalNat :: String -> Maybe Int
readHexadecimalNat xs = do
  ys <- sequence $ map hexToInt $ reverse xs
  return $ sum $ zipWith (*) ys [16^t | t <- [0..]]
  where
    hexToInt :: Char -> Maybe Int
    hexToInt x = lookup x
      [ ('0',0), ('1',1), ('2',2), ('3',3), ('4',4)
      , ('5',5), ('6',6), ('7',7), ('8',8), ('9',9)
      , ('a',10), ('b',11), ('c',12), ('d',13), ('e',14), ('f',15)
      , ('A',10), ('B',11), ('C',12), ('D',13), ('E',14), ('F',15)
      ]


{-|
  Convert a string (base 86) to an integer.
  (Natural numbers only.)
-}
readBase86Nat :: String -> Maybe Int
readBase86Nat xs = do
  ys <- sequence $ map charToInt $ reverse xs
  return $ sum $ zipWith (*) ys [86^t | t <- [0..]]
  where
    charToInt :: Char -> Maybe Int
    charToInt x = lookup x
      [ ('0',0),  ('1',1),  ('2',2),  ('3',3),  ('4',4)
      , ('5',5),  ('6',6),  ('7',7),  ('8',8),  ('9',9)
      , ('a',10), ('b',11), ('c',12), ('d',13), ('e',14)
      , ('f',15), ('g',16), ('h',17), ('i',18), ('j',19)
      , ('k',20), ('l',21), ('m',22), ('n',23), ('o',24)
      , ('p',25), ('q',26), ('r',27), ('s',28), ('t',29)
      , ('u',30), ('v',31), ('w',32), ('x',33), ('y',34)
      , ('z',35), ('A',36), ('B',37), ('C',38), ('D',39)
      , ('E',40), ('F',41), ('G',42), ('H',43), ('I',44)
      , ('J',45), ('K',46), ('L',47), ('M',48), ('N',49)
      , ('O',50), ('P',51), ('Q',52), ('R',53), ('S',54)
      , ('T',55), ('U',56), ('V',57), ('W',58), ('X',59)
      , ('Y',60), ('Z',61), ('?',62), ('!',63), ('#',64)
      , ('&',65), ('@',66), ('$',67), ('=',68), ('+',69)
      , ('-',70), ('~',71), ('<',72), ('>',73), ('[',74)
      , (']',75), ('(',76), (')',77), ('{',78), ('}',79)
      , ('|',80), ('/',81), ('*',82), ('^',83), (':',84)
      , (';',85)
      ]


digitsToBase :: Int -> Int -> [Int]
digitsToBase b k
  | b <= 1 || k < 0 = []
  | otherwise = reverse $ foo k
      where
        foo t
          | t < b = [t]
          | otherwise = (t`rem`b) : (foo (t`quot`b))


showBase86Nat :: Int -> String
showBase86Nat k
  | k < 0     = ""
  | otherwise = case sequence $ map intToChar $ digitsToBase 86 k of
      Nothing -> error "showBase86Nat"
      Just x  -> x
      
  where
    intToChar :: Int -> Maybe Char
    intToChar x = lookup x $ map swap
      [ ('0',0),  ('1',1),  ('2',2),  ('3',3),  ('4',4)
      , ('5',5),  ('6',6),  ('7',7),  ('8',8),  ('9',9)
      , ('a',10), ('b',11), ('c',12), ('d',13), ('e',14)
      , ('f',15), ('g',16), ('h',17), ('i',18), ('j',19)
      , ('k',20), ('l',21), ('m',22), ('n',23), ('o',24)
      , ('p',25), ('q',26), ('r',27), ('s',28), ('t',29)
      , ('u',30), ('v',31), ('w',32), ('x',33), ('y',34)
      , ('z',35), ('A',36), ('B',37), ('C',38), ('D',39)
      , ('E',40), ('F',41), ('G',42), ('H',43), ('I',44)
      , ('J',45), ('K',46), ('L',47), ('M',48), ('N',49)
      , ('O',50), ('P',51), ('Q',52), ('R',53), ('S',54)
      , ('T',55), ('U',56), ('V',57), ('W',58), ('X',59)
      , ('Y',60), ('Z',61), ('?',62), ('!',63), ('#',64)
      , ('&',65), ('@',66), ('$',67), ('=',68), ('+',69)
      , ('-',70), ('~',71), ('<',72), ('>',73), ('[',74)
      , (']',75), ('(',76), (')',77), ('{',78), ('}',79)
      , ('|',80), ('/',81), ('*',82), ('^',83), (':',84)
      , (';',85)
      ]

    swap (x,y) = (y,x)
