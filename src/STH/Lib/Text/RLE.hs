module STH.Lib.Text.RLE (
  rlEncode, rlDecode
) where

import STH.Lib.List (unfoldrMaybe, fromRuns, getRuns)
import STH.Lib.Int (digitsToBase)
import Data.List (unfoldr)


{-|
  Internal representation of run-length encoded data.
-}
--RLE.S
data RLE a
  = Chunk   [a]
  | Repeat  Int a
  | Literal Int
  deriving Show
--RLE.E


--rlEncode.S
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
--rlEncode.E


--rlDecode.S
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
--rlDecode.E


{-|
  Convert a string (base 86) to an integer.
  (Natural numbers only.) Internal to this module.
-}
--readBase86Nat.S
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
--readBase86Nat.E


{-|
  Print an integer to a base-86 representation.
  Internal to this module.
-}
--showBase86Nat.S
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
--showBase86Nat.E
