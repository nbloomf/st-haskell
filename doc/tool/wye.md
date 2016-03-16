---
title: Software Tools in Haskell: wye
subtitle: write stdin to files and stdout
author: nbloomf
---

``wye`` acts like the standard tool ``tee``; it writes ``stdin`` to one or more files as well as ``stdout``. It takes one argument: ``--append`` opens the given files in append mode rather than (default) overwrite mode. Since this program processes lines from ``stdin`` one at a time, we use file handles rather than writing files all in one go. We have to be sure to close these file handles before exiting.


```haskell
&splice src/STH/Wye/Main.hs
```
