module STH.Lib.Text.Format.ASACarriageControl (
  CCLine(), renderCCLine, toCCLine, readCCLines
) where

import Control.Arrow ((>>>))
import Data.List (intercalate, sortBy, isPrefixOf)
import STH.Lib.List (maxMonoSubseqsBy, fromSparseList, unfoldrMaybe)
import STH.Lib.Text.Format.Line (getLines)


--CCLine.S
data CCLine
  = CCLine [String]
  deriving (Show)

renderCCLine :: CCLine -> String
renderCCLine (CCLine xs)
  = intercalate "\n" $ zipWith (:) (' ' : (repeat '+')) xs
--CCLine.E


toCCLines :: String -> [CCLine]
toCCLines = map toCCLine . getLines


{-|
  'toCCLine' takes a string which may include
  some backspace characters (but no newlines!)
  and returns a CCLine; that is, a list of lines
  which reproduce the effect of this sequence of
  typewriter keypresses when fed to a line printer
  by overstriking.
-}
--toCCLine.S
toCCLine :: String -> CCLine
toCCLine =
  columnIndices
    >>> filter (\(c,_) -> c /= ' ')          -- try omitting
    >>> sortBy (\(_,a) (_,b) -> compare a b) -- these lines
    >>> maxMonoSubseqsBy p
    >>> map (fromSparseList ' ')
    >>> CCLine
  where
    p u v = if snd u < snd v
              then True else False

    -- Assign a column index 
    columnIndices :: String -> [(Char,Int)]
    columnIndices = accum [] 1
      where
        accum zs _ ""     = reverse zs
        accum zs k (c:cs) = case c of
          '\b'      -> accum zs (max 1 (k-1)) cs
          otherwise -> accum ((c,k):zs) (k+1) cs
--toCCLine.E

--readCCLines.S
readCCLines :: String -> Maybe [CCLine]
readCCLines = unfoldrMaybe readFirstCCLine . getLines
  where
    readFirstCCLine :: [String] -> Maybe (Maybe (CCLine, [String]))
    readFirstCCLine [] = Just Nothing
    readFirstCCLine ((' ':cs):ds) = do
      let
        (us,vs) = span (isPrefixOf "+") ds

        stripPlus xs = case xs of
          '+':ys    -> Just ys
          otherwise -> Nothing

      case sequence $ map stripPlus us of
        Just ws -> Just (Just (CCLine $ cs:ws, vs))
        Nothing -> Nothing
    readFirstCCLine _ = Nothing
--readCCLines.E
