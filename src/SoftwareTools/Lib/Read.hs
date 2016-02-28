module SoftwareTools.Lib.Read (
  readDecimalNat, readPosIntList,
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
