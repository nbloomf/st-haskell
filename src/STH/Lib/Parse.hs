module STH.Lib.Parse (

) where

import Data.List (unfoldr)

data ParseError a
  = EndOfInput
  | Unrecognized a
  deriving Show

data Letter = A | B | BB deriving Show

testRec :: String -> Maybe (Letter, String)
testRec "A"  = Just (A, "")
testRec "B"  = Just (B, "")
testRec "BB" = Just (BB, "")
testRec _    = Nothing

prefixParse :: Int -> ([a] -> Maybe (b,[a])) -> [a] -> Either a [b]
prefixParse _ _   [] = Right []
prefixParse k rec xs = case getFirstPrefix k rec xs of
  Left EndOfInput       -> Right []
  Left (Unrecognized x) -> Left x
  Right (x,ys)          -> case prefixParse k rec ys of
    Left z -> Left z
    Right zs -> Right (x:zs)

getFirstPrefix :: Int -> ([a] -> Maybe (b,[a])) -> [a] -> Either (ParseError a) (b,[a])
getFirstPrefix _ _ [] = Left EndOfInput
getFirstPrefix k rec ws@(x:_) = accum (map (\t -> splitAt t ws) (reverse [1..k]))
  where
    accum [] = Left (Unrecognized x)
    accum ((as,bs):zs) = case rec as of
      Nothing     -> accum zs
      Just (m,cs) -> Right (m, cs ++ bs)

splits :: [a] -> [([a],[a])]
splits []     = []
splits (x:xs) = map revFst (iterateMaybe rot ([x],xs))
  where
    revFst (as,bs) = (reverse as, bs)

    rot (_ ,[])   = Nothing
    rot (as,b:bs) = Just (b:as,bs)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x :
  case f x of
    Nothing -> []
    Just y  -> iterateMaybe f y


