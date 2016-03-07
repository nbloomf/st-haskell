---
title: Software Tools in Haskell: charcombine
subtitle: replace chars on stdin with precomposed equivalents
author: nbloomf
---

One of the fundamental problems with our [``detab``](/pages/sth/tool/detab.html) program is that it assumes plain text characters can be displayed on a rectangular array of character cells in a one-to-one way, with each character taking one cell and each cell having at most one character. This was a reasonable expectation at the time *Software Tools* was written, when character encodings were simpler. But Unicode makes this assumption wrong for several reasons. The first, which we will address now, is that *diacritics* like acute accents or carons can be expressed using *combining characters*. These are special unicode code points that are not displayed independently but rather modify the character which they succeed. You can read more about these on the [wiki](https://en.wikipedia.org/wiki/Combining_character). This is a problem for us because ``detab`` counts tab stop widths in terms of characters.

There is something we can do to fix this. Unicode also includes a large number of *precomposed* characters; single code points that are semantically equivalent to characters with combining diacritics. For example, code point ``U+0041`` (latin capital A) followed by ``U+0301`` (combining acute accent above) is canonically equivalent to the single code point ``U+00C1`` (latin capital A with acute accent above). There is a helpful [wiki page](https://en.wikipedia.org/wiki/List_of_precomposed_Latin_characters_in_Unicode) with a list of these precombined characters.

We could make ``detab`` aware of this equivalence. This is a bad idea, though, for a few reasons. First, it would make ``detab`` more complicated with only marginal benefit. Most of the text that I work with is plain ASCII, and making ``detab`` fiddle with unicode issues on such files will slow it down for no reason. We could give ``detab`` a command line flag to explicitly enable this feature, but it is important not to clutter up the interface of a program without a good reason. Second, ``detab`` is surely not the only program that might need to deal with this unicode issue. If each one solves the problem in its own way there will be lots of duplicated code, and duplicated code (while sometimes justifiable) is a breeding ground for bugs. Moreover the Unicode standard changes every few years, possibly requiring a time consuming edit of all the programs that work with unicode-encoded text. **A far better solution is to make a separate tool, ``charcombine``, to handle this problem.** Because our programs are designed to communicate via stdin and stdout, then we can send text through ``charcombine`` before giving it to ``detab``. This way ``detab`` can stay simple, and ``charcombine`` can be a small, general-purpose tool for replacing combining characters with precomposed characters.

Since we already have a function, ``getGlyphs``, which splits a stream of characters into noncombining+combining substrings, the main function of ``charcombine`` is quite succinct.


```haskell
-- sth-charcombine: replace combining unicode chars with precomposed chars
--   character-oriented

module Main where

import SoftwareTools.Lib (exitSuccess)
import SoftwareTools.Lib.IO   (charFilter)
import SoftwareTools.Lib.Text (getGlyphs)

main :: IO ()
main = do
  charFilter (concatMap toPrecomposed . getGlyphs)
  exitSuccess
```


All that remains is to write a function, ``composeGlyph``, that takes a string of characters and replaces it by an equivalent precomposed character.


```haskell
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
```


And OH MY GOSH THIS IS SO BORING. There are dozens more precomposed characters, and it's pretty clear how to extend this function to those. I will leave finishing this to another day.
