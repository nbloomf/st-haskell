---
title: Software Tools in Haskell: expand
subtitle: uncompress text on stdin (run length encoding)
author: nbloomf
date: 2016-03-02
---

The companion to [``compress``](/pages/sth/tool/compress.html) is ``expand``. It reads a string of characters that was run length encoded by ``compress`` and uncompresses it. This program has an error condition; the input may not be valid. This can happen for a few reasons; if a repeat count is incorrectly encoded (i.e. includes invalid digits or does not terminate in a sigil), or if the file ends in the middle of a repeat encoding.

```haskell
&splice src/STH/Expand/Main.hs
```


``rlDecode`` does all the work:


```haskell
&splice src/STH/Lib/Text/RLE.hs between --rlDecode.S and --rlDecode.E
```


One big improvement we could make to ``expand`` is to try to handle invalid input more gracefully; we could output the partially expanded text, for instance, or tell the user exactly where the error occurs. The first idea would not be too difficult. (Write the output to stderr.) The second idea, though, while possibly useful, would make the implementation much more complicated. (We'd have to keep track of the position of each character in the original source.) Doable, but until the need is demonstrated I'd prefer to keep the implementation simple.
