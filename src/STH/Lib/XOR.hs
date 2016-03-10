module STH.Lib.XOR (
  xor
) where

import Data.Char (ord, chr)

--XOR.S
class XOR t where
  xor :: t -> t -> t

  xors :: [t] -> [t] -> [t]
  xors [] ys = ys
  xors xs [] = xs
  xors (x:xs) (y:ys) = (xor x y) : xors xs ys
--XOR.E


--Bit.S
data Bit
  = Zero | One
  deriving (Eq, Show)

instance XOR Bit where
  xor Zero Zero = Zero
  xor Zero One  = One
  xor One  Zero = One
  xor One  One  = Zero
--Bit.E


--int.XOR.S
instance XOR Int where
  xor a b = bitsToInt $ xors (intToBits a) (intToBits b)
    where
      intToBits :: (Integral n) => n -> [Bit]
      intToBits k = case getBits k of
        [] -> [Zero]
        bs -> bs
        where
          getBits t
            | t <= 0    = []
            | otherwise = case even t of
                True  -> Zero : (getBits $ t`quot`2)
                False -> One  : (getBits $ (t-1)`quot`2)

      bitsToInt :: (Integral n) => [Bit] -> n
      bitsToInt = sum . zipWith (*) [2^t | t <- [0..]] . map bitToInt
        where
          bitToInt Zero = 0
          bitToInt One  = 1
--int.XOR.E


--char.XOR.S
instance XOR Char where
  xor x y = chr $ xor (ord x) (ord y)
--char.XOR.E
