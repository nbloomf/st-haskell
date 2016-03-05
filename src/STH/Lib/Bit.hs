module STH.Lib.Bit (
  intXOR
) where


data Bit
  = Zero | One
  deriving (Eq, Show)

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

bitToInt :: (Integral n) => Bit -> n
bitToInt Zero = 0
bitToInt One  = 1

bitsToInt :: (Integral n) => [Bit] -> n
bitsToInt = sum . zipWith (*) [2^t | t <- [0..]] . map bitToInt

bitXOR :: Bit -> Bit -> Bit
bitXOR Zero Zero = Zero
bitXOR Zero One  = One
bitXOR One  Zero = One
bitXOR One  One  = Zero

bitsXOR :: [Bit] -> [Bit] -> [Bit]
bitsXOR [] ys = ys
bitsXOR xs [] = xs
bitsXOR (x:xs) (y:ys)
  = (bitXOR x y) : bitsXOR xs ys

intXOR :: (Integral n) => n -> n -> n
intXOR a b = bitsToInt $ bitsXOR (intToBits a) (intToBits b)
