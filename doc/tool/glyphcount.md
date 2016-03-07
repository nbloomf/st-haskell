---
title: Software Tools in Haskell: glyphcount
subtitle: count glyphs on stdin
author: nbloomf
---

When we wrote [``count``](/pages/sth/tool/count.html), we saw that there is a serious ambiguity in the meaning of "character" in Unicode. On one hand Unicode defines a list of character code points, but on the other hand sequences of code points do not necessarily correspond to symbols on the screen in the way ASCII characters do. In fact there is a [Unicode Technical Report](http://www.unicode.org/reports/tr17/#CharactersVsGlyphs) which addresses this ambiguity; unfortunately the conclusion there is that

> The correspondence between glyphs and characters is generally not one-to-one, and cannot be predicted from the text alone. <cite>Unicode Technical Report #17</cite>

Given this difficulty, we will make a simplifying assumption. **A "glyph" is a non-combining character followed by zero or more combining characters.** Fortunately there is a standard library function, ``isMark``, which detects which characters are combining diacritics. The ``getGlyphs`` function splits a string into a list of glyphs.


```haskell
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
```


Now the main function is much like that of ``count``.


```haskell
-- sth-glyphcount: count glyphs on stdin

module Main where

import SoftwareTools.Lib.IO (charFilter, putNewLine)
import SoftwareTools.Lib.List (count)
import SoftwareTools.Lib.Text (getGlyphs)

main :: IO ()
main = do
  charFilter (show . count . getGlyphs)
  putNewLine
```
