-- sth-entab: replace spaces on stdin with tabs

module Main where

import SoftwareTools.Lib
  ((>>>), exitSuccess, exitFailure, getArgs)
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

  -- Entab a single line ln with tabstops ts.
  let entab ln = case insertTabStops ts ln of
                   Nothing -> reportErrorMsgs
                                [ "LINE: " ++ ln
                                , "TABS: " ++ show ts
                                ] >> exitFailure
                   Just cs -> putStrLn cs

  -- Do it!
  getContents
    >>= (getLines >>> map entab >>> sequence_)
    >>  exitSuccess


{-|
  'insertTabStops' chops the string xs into substrings
  of lengths ks, replaces any trailing spaces on the 
  substrings by tabs, and concatenates. It is a partial
  inverse of 'convertTabStops'.
-}
insertTabStops :: [Int] -> String -> Maybe String
insertTabStops [] xs = Just xs
insertTabStops ks xs = accum [] ks xs
  where
    accum zs _ "" = Just $ concat $ reverse zs
    accum zs [t] ys = do
      (as,bs) <- splitColumn t ys
      accum (as:zs) [t] bs
    accum zs (t:ts) ys = do
      (as,bs) <- splitColumn t ys
      accum (as:zs) ts bs

    splitColumn :: Int -> String -> Maybe (String, String)
    splitColumn k xs
      | k  <= 0   = Nothing
      | xs == ""  = Nothing
      | otherwise = do
          let (as,bs) = splitAt k xs
          let munch = dropWhile (== ' ')
          let cs = reverse as
          let ds = if bs == ""
                     then let es = reverse $ munch cs in
                       if es == "" then "\t" else es
                     else case cs of
                       ' ':_ -> reverse ('\t':(munch cs))
                       otherwise -> as
          Just (ds,bs)
