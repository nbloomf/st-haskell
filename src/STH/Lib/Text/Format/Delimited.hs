module STH.Lib.Text.Format.Delimited (
  convertTabStops, insertTabStops
) where

import STH.Lib.List
  (padToByAfter, spanAtMostWhile)

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
--convertTabStops.S
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
--convertTabStops.E


{-|
  'insertTabStops' chops the string xs into substrings
  of lengths ks, replaces any trailing spaces on the 
  substrings by tabs, and concatenates. It is a partial
  inverse of 'convertTabStops'.
-}
--insertTabStops.S
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
--insertTabStops.E
