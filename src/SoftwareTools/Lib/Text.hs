module SoftwareTools.Lib.Text (
  getLines, getWords, getGlyphs, backslashUnEscape
) where

import Data.List (unfoldr)
import Data.Char (isSpace, isMark, readLitChar)


{-|
  The 'getLines' function takes a string of
  characters and splits it at any instances of
  the newline character ('\n'). The resulting
  strings do not contain any newlines.
-}
getLines :: String -> [String]
getLines = unfoldr firstLine
  where
    firstLine :: String -> Maybe (String, String)
    firstLine xs = case break (== '\n') xs of
      ("","")   -> Nothing
      (as,"")   -> Just (as,"")
      (as,b:bs) -> Just (as,bs)


{-|
  The 'getWords' function takes a string of
  characters and splits it into "words", which
  are defined here as maximal substrings not
  containing any whitespace characters.
-}
getWords :: String -> [String]
getWords = unfoldr firstWord
  where
    firstWord :: String -> Maybe (String, String)
    firstWord xs = case dropWhile isSpace xs of
      "" -> Nothing
      ys -> Just $ break isSpace ys


{-|
  The 'getGlyphs' function splits a string of characters
  into "glyphs": non-mark characters followed by maximal
  substrings of mark characters.
-}
getGlyphs :: String -> [String]
getGlyphs = unfoldr firstGlyph
  where
    firstGlyph :: String -> Maybe (String, String)
    firstGlyph "" = Nothing
    firstGlyph (x:xs) = if isMark x
      then Just $ break (not . isMark) (x:xs)
      else do
        let (as,bs) = break (not . isMark) xs
        Just (x:as, bs)


{-|
  'backslashUnEscape' interprets any C-style
  backslash codes in a string as escape codes,
  replacing them with their ASCII or Unicode
  referents.
-}
backslashUnEscape :: String -> String
backslashUnEscape = concat . unfoldr firstChar
  where
    firstChar :: String -> Maybe (String, String)
    firstChar "" = Nothing
    firstChar (c:cs) = case c of
      '\\' -> case cs of
        -- Basic C-style escape characters
        ('a' :ds) -> Just ("\a\&",ds)
        ('b' :ds) -> Just ("\b\&",ds)
        ('f' :ds) -> Just ("\f\&",ds)
        ('n' :ds) -> Just ("\n\&",ds)
        ('r' :ds) -> Just ("\r\&",ds)
        ('t' :ds) -> Just ("\t\&",ds)
        ('v' :ds) -> Just ("\v\&",ds)
        ('\\':ds) -> Just ("\\\&",ds)
        ('\'':ds) -> Just ("'\&" ,ds)
        ('"' :ds) -> Just ("\"\&",ds)
        ('?' :ds) -> Just ("?\&" ,ds)

        -- 3-digit octal ASCII codes
        ('0':k2:k3:ds) -> octalCode ['0',k2,k3] ds
        ('1':k2:k3:ds) -> octalCode ['1',k2,k3] ds
        ('2':k2:k3:ds) -> octalCode ['2',k2,k3] ds
        ('3':k2:k3:ds) -> octalCode ['3',k2,k3] ds
        ('4':k2:k3:ds) -> octalCode ['4',k2,k3] ds
        ('5':k2:k3:ds) -> octalCode ['5',k2,k3] ds
        ('6':k2:k3:ds) -> octalCode ['6',k2,k3] ds
        ('7':k2:k3:ds) -> octalCode ['7',k2,k3] ds

        -- 2-digit hex ASCII codes
        ('x':k1:k2:ds) -> case all isHexDigit digs of
          True  -> case readLitChar ("\\x" ++ digs) of
            []        -> Just ("\\x" ++ digs, ds)
            ((x,_):_) -> Just ([x],ds)
          False -> Just ("\\x" ++ digs, ds)
          where
            digs = [k1,k2]

        -- Standard ASCII abbreviations
        ('N':'U':'L':ds) -> Just ("\NUL\&", ds)
        ('S':'O':'H':ds) -> Just ("\SOH\&", ds)
        ('S':'T':'X':ds) -> Just ("\STX\&", ds)
        ('E':'T':'X':ds) -> Just ("\ETX\&", ds)
        ('E':'O':'T':ds) -> Just ("\EOT\&", ds)
        ('E':'N':'Q':ds) -> Just ("\ENQ\&", ds)
        ('A':'C':'K':ds) -> Just ("\ACK\&", ds)
        ('B':'E':'L':ds) -> Just ("\BEL\&", ds)
        ('D':'L':'E':ds) -> Just ("\DLE\&", ds)
        ('D':'C':'1':ds) -> Just ("\DC1\&", ds)
        ('D':'C':'2':ds) -> Just ("\DC2\&", ds)
        ('D':'C':'3':ds) -> Just ("\DC3\&", ds)
        ('D':'C':'4':ds) -> Just ("\DC4\&", ds)
        ('N':'A':'K':ds) -> Just ("\NAK\&", ds)
        ('S':'Y':'N':ds) -> Just ("\SYN\&", ds)
        ('E':'T':'B':ds) -> Just ("\ETB\&", ds)
        ('C':'A':'N':ds) -> Just ("\CAN\&", ds)
        ('S':'U':'B':ds) -> Just ("\SUB\&", ds)
        ('E':'S':'C':ds) -> Just ("\ESC\&", ds)
        ('D':'E':'L':ds) -> Just ("\DEL\&", ds)
        ('E':'M'    :ds) -> Just ("\EM\&",  ds)
        ('F':'S'    :ds) -> Just ("\FS\&",  ds)
        ('G':'S'    :ds) -> Just ("\GS\&",  ds)
        ('R':'S'    :ds) -> Just ("\RS\&",  ds)
        ('U':'S'    :ds) -> Just ("\US\&",  ds)
        ('S':'P'    :ds) -> Just ("\SP\&",  ds)
        ('B':'S'    :ds) -> Just ("\BS\&",  ds)
        ('H':'T'    :ds) -> Just ("\HT\&",  ds)
        ('L':'F'    :ds) -> Just ("\LF\&",  ds)
        ('V':'T'    :ds) -> Just ("\VT\&",  ds)
        ('F':'F'    :ds) -> Just ("\FF\&",  ds)
        ('C':'R'    :ds) -> Just ("\CR\&",  ds)
        ('S':'O'    :ds) -> Just ("\SO\&",  ds)
        ('S':'I'    :ds) -> Just ("\SI\&",  ds)

        -- C99-style universal character names
        ('u':k1:k2:k3:k4:ds) -> case all isHexDigit digs of
          True  -> case readLitChar ("\\x" ++ digs) of
            []        -> Just ("\\u" ++ digs, ds)
            ((x,_):_) -> Just ([x],ds)
          False -> Just ("\\u" ++ digs, ds)
          where
            digs = [k1,k2,k3,k4]

        ('U':k1:k2:k3:k4:k5:k6:k7:k8:ds) -> case all isHexDigit digs of
          True  -> case readLitChar ("\\x" ++ digs) of
            []        -> Just ("\\U" ++ digs, ds)
            ((x,_):_) -> Just ([x],ds)
          False -> Just ("\\U" ++ digs, ds)
          where
            digs = [k1,k2,k3,k4,k5,k6,k7,k8]

        -- stolen from haskell
        ('&':ds) -> Just ("",ds)

        -- If we don't see a valid esc code, just move on.
        ds -> Just ("\\\&", ds)

      -- No backslash
      otherwise -> Just ([c],cs)
      where
        isHexDigit :: Char -> Bool
        isHexDigit = (`elem` "0123456789aAbBcCdDeEfF")

        isOctDigit :: Char -> Bool
        isOctDigit = (`elem` "01234567")

        octalCode digs ds = case all isOctDigit digs of
          True  -> case readLitChar ("\\o" ++ digs) of
            []        -> Just ('\\':digs, ds)
            ((x,_):_) -> Just ([x],ds)
          False -> Just ('\\':digs, ds)
