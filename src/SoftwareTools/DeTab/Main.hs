-- sth-detab: convert tabs to spaces
--   line-oriented

module Main where

import SoftwareTools.Lib
  ((>>>), exitSuccess, exitFailure, getArgs)
import SoftwareTools.Lib.IO    (lineFilter)
import SoftwareTools.Lib.Read  (readPosIntList)
import SoftwareTools.Lib.Text  (getLines)
import SoftwareTools.Lib.List  (spanAtMostWhile, padToByAfter)
import SoftwareTools.Lib.Error (reportErrorMsgs)


main :: IO ()
main = do
  args <- getArgs

  -- Read positive integer tabstop arguments.
  -- Default is [8].
  ts <- case readPosIntList args of
    Just [] -> return [8]
    Just ks -> return ks
    Nothing -> reportErrorMsgs
                 ["tab widths must be positive integers."
                 ] >> exitFailure

  -- Do it!
  lineFilter (convertTabStops ts)
  exitSuccess



{-|
  'convertTabStops' takes a list of tab stop widths
  and a string and replaces all '\t's in the string
  with spaces, padded to the given tab stop widths
  in order. If the string has more '\t's than there
  are tab stop widths, then the final tab stop width
  is repeated indefinitely. If no tab stop widths are
  given the function returns Nothing. This function is
  a partial inverse of 'insertTabStops'.
-}
convertTabStops :: [Int] -> String -> String
convertTabStops [] xs = xs
convertTabStops ks xs = accum [] ks xs
  where
    accum zs _   "" = concat $ reverse zs
    accum zs [t] ys =
      let (as,bs) = splitTabStop t ys in
      accum (as:zs) [t] bs
    accum zs (t:ts) ys =
      let (as,bs) = splitTabStop t ys in
      accum (as:zs) ts bs

    splitTabStop :: Int -> String -> (String, String)
    splitTabStop k xs
      | k <= 0    = (xs,"")
      | otherwise = 
          case spanAtMostWhile k (/= '\t') xs of
            (as,"") -> (stripTrailingSpaces as, "")
            (as,bs) -> let cs = padToByAfter k ' ' as in
              case bs of
                '\t':ds -> (cs,ds)
                ds      -> (cs,ds)
      where
        stripTrailingSpaces = reverse . dropWhile (==' ') . reverse
