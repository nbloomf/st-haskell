---
title: Software Tools in Haskell: entab
subtitle: replace spaces on stdin with tabs
author: nbloomf
---

The [``detab``](/pages/sth/tool/detab.html) program replaced tab characters with spaces, taking arguments at the command line to let the user specify the width of the tab stops. The ``entab`` program reverses this process. It takes input which we assume represents some tabular data where different columns start on specific character columns, chops the input lines into columns, and replaces any trailing spaces in a given column by a single ``\t`` character. Just like ``detab``, the default tab stop width is 8, and we allow the user to specify a list of tab stop widths at the command line with the convention that the *last* user-specified width is assumed to repeat indefinitely.

The basic structure of this program is nearly identical to that of ``detab`` (which is not surprising).


```haskell
&splice src/STH/EnTab/Main.hs
```


We reuse the functions for reading lists of nonnegative integers that we wrote for ``detab``. The heavly lifting is done by ``insertTabStops``.


```haskell
&splice src/STH/Lib/Text/Format/Delimited.hs between --insertTabStops.S and --insertTabStops.E
```


Even the shape of this function on the page resembles that of its counterpart from ``detab``. Note the use of an accumulating parameter helper function.

In Exercise 2-2, Kernighan and Plauger ask us to make the simplest change to ``entab`` to make it handle tabs correctly. After thinking about this, I've decided the right thing to do is **nothing**. Let's imagine what it means if the user is trying to use ``entab`` on data that contains tabs. I can think of two possible situations.

1. The tabs are "semantic tabs", used to delimit data. That is, the input either is already tab-delimited, or contains a mixture of tab-delimited and column-delimited data. In this case the user has other problems. The right thing to do in the first case is nothing, and in the second case depends on the user's intent. We could assume that a semantic tab means "advance to the next tab stop", but this now changes the column indices of the characters in the remainder of the line unpredictably, so the intent of any tab stop width input is unclear. It would be better here to run the data through ``detab`` first to remove the tabs, then run through ``entab`` to put them back.
2. The tabs are "literal tabs", as in the data itself involves tab characters for some reason, and they have a different meaning in whatever context the user cares about. This is, after all, a valid reason to use a column-delimited format. Of course in this case the right thing to do is leave the tabs alone.

If we ignore tabs altogether, then at best this is the Right Thing and at worst the user has to use ``detab`` first (or has other problems). On the other hand, trying to make ``entab`` do something useful with tabs would make the program more complicated (and probably clutter the interface) with little benefit.
