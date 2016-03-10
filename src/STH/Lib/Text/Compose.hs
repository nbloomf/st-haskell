module STH.Lib.Text.Compose (
  composeGlyphs
) where

import STH.Lib.Text (getGlyphs)

--composeGlyphs.S
composeGlyphs :: String -> String
composeGlyphs = concatMap composeGlyph . getGlyphs
--composeGlyphs.E

{-|
  Replace glyphs (defined here as a noncombining
  character followed by zero or more combining
  characters) by precomposed versions.
-}
--composeGlyph.S
composeGlyph :: String -> String
composeGlyph ""  = ""
composeGlyph [c] = [c]
composeGlyph [x, '\x0301'] = case lookup x acute of
  Just y  -> y
  Nothing -> [x, '\x0301']
  where
    acute =
      [ ('A',"Á"), ('Æ',"Ǽ"), ('C',"Ć"), ('E',"É"), ('G',"Ǵ")
      , ('I',"Í"), ('K',"Ḱ"), ('L',"Ĺ"), ('M',"Ḿ"), ('N',"Ń")
      , ('O',"Ó"), ('Ø',"Ǿ"), ('P',"Ṕ"), ('R',"Ŕ"), ('S',"Ś")
      , ('U',"Ú"), ('W',"Ẃ"), ('Y',"Ý"), ('Z',"Ź")
      , ('a',"á"), ('æ',"ǽ"), ('c',"ć"), ('e',"é"), ('g',"ǵ")
      , ('i',"í"), ('k',"ḱ"), ('l',"ĺ"), ('m',"ḿ"), ('n',"ń")
      , ('o',"ó"), ('ø',"ǿ"), ('p',"ṕ"), ('r',"ŕ"), ('s',"ś")
      , ('u',"ú"), ('w',"ẃ"), ('y',"ý"), ('z',"ź")
      ]
toPrecomposed x = x
--composeGlyph.E
