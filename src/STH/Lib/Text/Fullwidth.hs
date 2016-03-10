module STH.Lib.Text.Fullwidth (
  toFullwidth
) where

import STH.Lib.List (applyListMap)

{-|
  Replace unicode characters by fullwidth equivalents.
-}
--toFullwidth.S
toFullwidth :: Char -> Char
toFullwidth = applyListMap full
  where
    full =
      [ ('!','！'), ('"','＂'),  ('#','＃'), ('$','＄'), ('%','％')
      , ('&','＆'), ('\'','＇'), ('(','（'), (')','）'), ('*','＊')
      , ('+','＋'), (',','，'),  ('-','－'), ('.','．'), ('/','／')
      , ('0','０'), ('1','１'),  ('2','２'), ('3','３'), ('4','４')
      , ('5','５'), ('6','６'),  ('7','７'), ('8','８'), ('9','９')
      , (':','：'), (';','；'),  ('<','＜'), ('=','＝'), ('>','＞')
      , ('?','？'), ('@','＠'),  ('A','Ａ'), ('B','Ｂ'), ('C','Ｃ')
      , ('D','Ｄ'), ('E','Ｅ'),  ('F','Ｆ'), ('G','Ｇ'), ('H','Ｈ')
      , ('I','Ｉ'), ('J','Ｊ'),  ('K','Ｋ'), ('L','Ｌ'), ('M','Ｍ')
      , ('N','Ｎ'), ('O','Ｏ'),  ('P','Ｐ'), ('Q','Ｑ'), ('R','Ｒ')
      , ('S','Ｓ'), ('T','Ｔ'),  ('U','Ｕ'), ('V','Ｖ'), ('W','Ｗ')
      , ('X','Ｘ'), ('Y','Ｙ'),  ('Z','Ｚ'), ('[','［'), ('\\','＼')
      , (']','］'), ('^','＾'),  ('_','＿'), ('`','｀'), ('a','ａ')
      , ('b','ｂ'), ('c','ｃ'),  ('d','ｄ'), ('e','ｅ'), ('f','ｆ')
      , ('g','ｇ'), ('h','ｈ'),  ('i','ｉ'), ('j','ｊ'), ('k','ｋ')
      , ('l','ｌ'), ('m','ｍ'),  ('n','ｎ'), ('o','ｏ'), ('p','ｐ')
      , ('q','ｑ'), ('r','ｒ'),  ('s','ｓ'), ('t','ｔ'), ('u','ｕ')
      , ('v','ｖ'), ('w','ｗ'),  ('x','ｘ'), ('y','ｙ'), ('z','ｚ')
      , ('{','｛'), ('|','｜'),  ('}','｝'), ('~','～'), (' ','　')
      ]
--toFullwidth.E
