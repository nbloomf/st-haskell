-- sth-charcombine: replace combining unicode chars with precomposed chars

module Main where

import SoftwareTools.Lib ((>>>))
import SoftwareTools.Lib.Text (getGlyphs)

main :: IO ()
main = getContents >>=
  (getGlyphs >>> concatMap toPrecomposed >>> putStr)



{-|
  Replace glyphs (defined here as a noncombining
  character followed by zero or more combining
  characters) by precomposed versions.
-}
toPrecomposed :: String -> String
toPrecomposed ""  = ""
toPrecomposed [c] = [c]
toPrecomposed [x, '\x0301'] = case lookup x acute of
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
