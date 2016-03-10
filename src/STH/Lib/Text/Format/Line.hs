module STH.Lib.Text.Format.Line (
  getLines
) where

import Data.List (break, unfoldr)

{-|
  The 'getLines' function takes a string of
  characters and splits it at any instances of
  the newline character ('\n'). The resulting
  strings do not contain any newlines.
-}
--getLines.S
getLines :: String -> [String]
getLines = unfoldr firstLine
  where
    firstLine :: String -> Maybe (String, String)
    firstLine xs = case break (== '\n') xs of
      ("","")   -> Nothing
      (as,"")   -> Just (as,"")
      (as,b:bs) -> Just (as,bs)
--getLines.E
