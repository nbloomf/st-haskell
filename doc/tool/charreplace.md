---
title: Software Tools in Haskell: charreplace
subtitle: replace chars by strings on stdin
author: nbloomf
---

The program ``charreplace`` is a companion to [``translit``](/pages/sth/tool/translit.html) and requires no new machinery.


```haskell
&splice src/STH/CharReplace/Main.hs
```


It may seem like overkill to split the functionality from ``translit`` and ``charreplace`` just to make the interface more consistent. But note that ``charreplace`` naturally does something we couldn't have done if the two were rolled together, at least not without making the interface even *more* inconsistent: ``charreplace`` naturally replaces characters by **strings**, not just characters. This is not a trivial distinction; for example, if we have a text file which uses unix-style line endings (``\n``) and want to convert them to Windows-style line endings (``\r\n``) we can do this with

    charreplace "\n" "\r\n"

I can't think of a way to do this with ``translit`` alone.
