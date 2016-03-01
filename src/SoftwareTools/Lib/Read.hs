module SoftwareTools.Lib.Read (
  readDecimalNat, readPosIntList, readHexadecimalNat
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
