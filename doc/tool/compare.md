---
title: Software Tools in Haskell: compare
subtitle: find the first position where two text streams differ
author: nbloomf
---

The purpose of ``compare`` is to detect whether or not two streams of text are byte-for-byte identical. This alone is simple enough, but the job is complicated by the fact that (1) there are a few useful places these text streams might come from and (2) in the (typical) case that the streams are *not* identical, we want to report the position of the first difference.

The ``diffList`` function takes two lists and returns the position of the earliest difference as well as the differing elements (if they exist).


```haskell
&splice src/STH/Lib/List.hs between --diffList.S and --diffList.E
```


This isn't quite what we want, though. The problem is that word "position". The most useful *position* information will depend on what kind of text is being compared. For instance, when comparing line text we'd like to report the line and column numbers of the earliest difference, rather than just the character index. The ``diffLists`` function does this.


```haskell
&splice src/STH/Lib/List.hs between --diffLists.S and --diffLists.E
```


Like we did with ``echo``, we'll allow the user to specify which kind of position they mean with a ``--char`` option (default is line). Now the streams to be compared (of which we need two) can come from one of three places:

1. ``stdin``,
2. a file (one or two), or
3. command line arguments (interpreted like ``echo``).

The main program first reads the arguments and extracts (1) the mode (char or line) of text being compared, (2) the name and contents of the first stream to be compared, and (3) the name and contents of the second stream to be compared. Then we evaluate either ``diffList`` or ``diffLists`` and report the results.


```haskell
&splice src/STH/Compare/Main.hs
```


``compare`` can be used to implement a very simple testing scheme by comparing the output of some program under development to its "expected" output. One improvement I can think of is to have ``compare`` optionally output delimited data, to make it easier to extract this information with other tools.
