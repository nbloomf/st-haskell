---
title: Software Tools in Haskell: line number
subtitle: number lines on stdin
author: nbloomf
---

This is another utility intended for use with ``pslineprint``: it prepends lines on ``stdin`` with line numbers.

By default, this program separates the line number from the line contents by a tab; the output is tab-delimited text. This reflects the fact that the line number is semantically distinct from the line contents. The ``--pad`` option instead separates the numbers from the contents by a space, and also left-pads the numbers so that they are vertically aligned and right-justified. Finally, the ``--from`` option allows the user to specify the number of the first line (natural numbers only).

We made tab-separated output the default because it is the simplest; we can march down the list of lines on ``stdin`` and send them to ``stdout`` with the numer prefixed. We only need to keep track of the current line number. Nicely padded line numbers with spaces, however, require us to know in advance the total number of lines required before we can begin producing output.


```haskell
&splice src/STH/LineNumber/Main.hs
```


This program uses one new library function: ``padToByBefore``, the companion of ``padToByAfter``.


```haskell
&splice src/STH/Lib/List.hs between --padToByBefore.S and --padToByBefore.E
```
