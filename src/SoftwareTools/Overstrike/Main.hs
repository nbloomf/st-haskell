-- sth-overstrike: interpret backspaces using line printer control codes

module Main where

import SoftwareTools.Lib
  ((>>>), exitSuccess, sortBy)
import SoftwareTools.Lib.List
  (maxMonoSubseqsBy, fromSparseList)
import SoftwareTools.Lib.Text (getLines)


main :: IO ()
main = do
  let overstrike = overstrikeLines
                     >>> zipWith (:) (' ' : (repeat '+'))
                     >>> map putStrLn
                     >>> sequence_

  getContents
    >>= (getLines >>> map overstrike >>> sequence_)

  exitSuccess



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
