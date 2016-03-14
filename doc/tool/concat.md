---
title: Software Tools in Haskell: concat
subtitle: concatenate files
author: nbloomf
---

``concat`` reads a list of file names from the command line and writes the contents of these files, in order, to ``stdout``. It takes no arguments. There is one special case: a single hyphen is interpreted as the name of ``stdin``.


```haskell
&splice src/STH/Concat/Main.hs
```
