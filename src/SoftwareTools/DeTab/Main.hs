-- sth-detab: convert tabs to spaces

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

  -- Detab a single line ln with tabstops ts.
  let detab ln = case convertTabStops ts ln of
                   Nothing -> reportErrorMsgs
                                [ "LINE: " ++ ln
                                , "TABS: " ++ show ts
                                ] >> exitFailure
                   Just cs -> putStrLn cs

  -- Do it!
  getContents
    >>= (getLines >>> map detab >>> sequence_)

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
convertTabStops :: [Int] -> String -> Maybe String
convertTabStops [] _  = Nothing
convertTabStops ks xs = accum [] ks xs
  where
    accum zs _   "" = Just $ concat $ reverse zs
    accum zs [t] ys = do
      (as,bs) <- splitTabStop t ys
      accum (as:zs) [t] bs
    accum zs (t:ts) ys = do
      (as,bs) <- splitTabStop t ys
      accum (as:zs) ts bs

    splitTabStop :: Int -> String -> Maybe (String, String)
    splitTabStop k xs
      | k <= 0    = Nothing
      | otherwise = do
          (as,bs) <- spanAtMostWhile k (/= '\t') xs
          if bs == ""
            then do
              let cs = reverse $ dropWhile (==' ') $ reverse as
              return (cs,"")
            else do
              cs <- padToByAfter k ' ' as
              case bs of
                '\t':ds -> return (cs,ds)
                ds      -> return (cs,ds)
