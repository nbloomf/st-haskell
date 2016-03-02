-- sth-entab: replace spaces on stdin with tabs
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
  lineFilter (insertTabStops ts)
  exitSuccess


{-|
  'insertTabStops' chops the string xs into substrings
  of lengths ks, replaces any trailing spaces on the 
  substrings by tabs, and concatenates. It is a partial
  inverse of 'convertTabStops'.
-}
insertTabStops :: [Int] -> String -> String
insertTabStops [] xs = xs
insertTabStops ks xs = accum [] ks xs
  where
    accum zs _ "" = concat $ reverse zs
    accum zs [t] ys =
      let (as,bs) = splitColumn t ys in
      accum (as:zs) [t] bs
    accum zs (t:ts) ys =
      let (as,bs) = splitColumn t ys in
      accum (as:zs) ts bs

    splitColumn :: Int -> String -> (String, String)
    splitColumn k xs
      | k  <= 0   = (xs,"")
      | xs == ""  = ("","")
      | otherwise = (ds,bs)
          where
            (as,bs) = splitAt k xs
            munch = dropWhile (== ' ')
            cs = reverse as
            ds = if bs == ""
                     then let es = reverse $ munch cs in
                       if es == "" then "\t" else es
                     else case cs of
                       ' ':_ -> reverse ('\t':(munch cs))
                       otherwise -> as
