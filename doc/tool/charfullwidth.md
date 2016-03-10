---
title: Software Tools in Haskell: charfullwidth
subtitle: replace chars with fullwidth equivalents
author: nbloomf
---

Replacing "normal" characters with fullwidth forms is much simpler. We reuse the structure of ``copy --char``, with a filter to map characters.


```haskell
&splice src/STH/CharFullwidth/Main.hs
```


And the map:


```haskell
&splice src/STH/Lib/Text/Fullwidth.hs between --toFullwidth.S and --toFullwidth.E
```


This probably looks terrible in your browser because unicode coverage. The ``applyListMap`` function treats a list of pairs like a mapping.


```haskell
&splice src/STH/Lib/List.hs between --applyListMap.S and --applyListMap.E
```
