-- sth-overstrike: interpret backspaces using line printer control codes
--   line-oriented

module Main where

import STH.Lib
  ((>>>), exitSuccess, sortBy, intercalate)
import STH.Lib.IO
  (lineFilter)
import STH.Lib.List
  (maxMonoSubseqsBy, fromSparseList)
import STH.Lib.Text
  (getLines)


main :: IO ()
main = do
  lineFilter overstrike
  exitSuccess



overstrike :: String -> String
overstrike = overstrikeLines
  >>> zipWith (:) (' ' : (repeat '+'))
  >>> intercalate "\n"


{-|
  'overstrikeLines' takes a string which may
  include some backspace characters (but no newlines!)
  and returns a list of lines which reproduce the
  effect of this sequence of typewriter keypresses
  when fed to a line printer by overstriking.
-}
overstrikeLines :: String -> [String]
overstrikeLines =
  columnIndices
    >>> filter (\(c,_) -> c /= ' ')          -- try omitting
    >>> sortBy (\(_,a) (_,b) -> compare a b) -- these lines
    >>> maxMonoSubseqsBy p
    >>> map (fromSparseList ' ')
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