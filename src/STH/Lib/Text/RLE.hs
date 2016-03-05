module STH.Lib.Text.RLE (
  rlEncode, rlDecode
) where

import STH.Lib.Read (readBase86Nat, showBase86Nat)
import STH.Lib.List (unfoldrMaybe, fromRuns, getRuns)
import Data.List (unfoldr)


data RLE a
  = Chunk   [a]
  | Repeat  Int a
  | Literal Int
  deriving Show


rlEncode :: Char -> Int -> String -> String
rlEncode sig k = showRLE sig . runLengthEncode sig k
  where
    showRLE :: Char -> [RLE Char] -> String
    showRLE sig = concatMap write
      where
        write :: RLE Char -> String
        write (Repeat k x) = concat
          [[sig], [x], showBase86Nat k, [sig]]
        write (Chunk xs) = xs
        write (Literal 1) = [sig,sig,sig]
        write (Literal k) = concat $
          [[sig], [sig], showBase86Nat k, [sig]]

    runLengthEncode :: (Eq a) => a -> Int -> [a] -> [RLE a]
    runLengthEncode sig t = unfoldr (getFirst t) . getRuns
      where
        getFirst _ [] = Nothing
        getFirst t ((x,k):xs)
          | t <= k    = Just (Repeat k x, xs)
          | x == sig  = Just (Literal k, xs)
          | otherwise = let (ys,zs) = split ((x,k):xs)
                        in Just (Chunk $ fromRuns ys, zs)
              where
                split = span (\(z,h) -> t > h && z /= sig)


rlDecode :: Char -> String -> Maybe String
rlDecode sig = fmap (runLengthDecode sig) . readRLE sig
  where
    runLengthDecode :: (Eq a) => a -> [RLE a] -> [a]
    runLengthDecode sig = concatMap decodeRLE
      where
        decodeRLE (Chunk  xs)  = xs
        decodeRLE (Repeat k x) = replicate k x
        decodeRLE (Literal k)  = replicate k sig

    readRLE :: Char -> String -> Maybe [RLE Char]
    readRLE sig = unfoldrMaybe readFirstRLE
      where
        readFirstRLE :: String -> Maybe (Maybe (RLE Char, String))
        readFirstRLE ""  = Just Nothing
        readFirstRLE [x] =
          if x == sig then Nothing else Just (Just (Chunk [x], ""))
        readFirstRLE [x,y] =
          if x == sig then Nothing else Just (Just (Chunk [x], [y]))
        readFirstRLE (x:y:z:xs)
          | x == sig && y == sig && z == sig
              = Just (Just (Literal 1, xs))
          | x == sig && y == sig && z /= sig
              = do
                  let (as,bs) = span (/= sig) (z:xs)
                  k <- readBase86Nat as
                  case bs of
                    ""     -> Just (Just (Repeat k y, ""))
                    (_:cs) -> Just (Just (Repeat k y, cs))
          | x == sig && y /= sig
              = do
                  let (as,bs) = span (/= sig) (z:xs)
                  k <- readBase86Nat as
                  case bs of
                    ""     -> Just (Just (Repeat k y, ""))
                    (_:cs) -> Just (Just (Repeat k y, cs))
          | otherwise
              = do
                  let (as,bs) = span (/= sig) (x:y:z:xs)
                  Just (Just (Chunk as, bs))
