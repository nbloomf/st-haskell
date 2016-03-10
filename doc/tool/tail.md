---
title: Software Tools in Haskell: tail
subtitle: get the last k lines or chars from stdin
author: nbloomf
---

The version of ``tail`` on my system accepts 10 optional arguments, not including ``--help`` and ``--version``. I am sure that there are very good reasons for these. But this version will take only two: ``--char`` specifies that we want to take the last few characters, rather than lines, and an optional integer argument specifies the number to take.

We use a data type, ``Mode``, to represent the processing mode (lines or chars). Most of the complexity is in reading arguments and reporting errors.


```haskell
&splice src/STH/Tail/Main.hs
```
