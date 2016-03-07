---
title: Software Tools in Haskell: copy
subtitle: copy characters from stdin to stdout
author: nbloomf
---

Many simple tools are designed to act as *filters*: programs which take a stream of data, manipulate it in some way, and send it along. The ``copy`` program is the simplest possible example of a filter -- the identity filter. This is even simpler than ``cat``, which at least reads and catenates files.

Because it is so simple, we can think of ``copy`` as just a character filter -- it reads characters on ``stdin`` and writes them, unmodified, to ``stdout``.

We will write a general-purpose character filter program, parameterized on the mapping used to transform the input. ``charFilter`` simply reads everything it can from ``stdin``, applies a function to it, and writes out the result. Note that the standard library function ``getContents`` reads from stdin lazily, so despite appearances this function does not read all of ``stdin`` at once before getting to work.


```haskell
charFilter :: (String -> String) -> IO ()
charFilter f = do
  xs <- getContents
  putStr $ f xs
```


By wrapping the basic behavior of a character-oriented filter behind a higher order function like this, we can write at a higher level. The ``copy`` program is then the simplest possible character filter.


```haskell
-- sth-copy: copy characters from stdin to stdout

module Main where

import SoftwareTools.Lib.IO (charFilter)

main :: IO ()
main = charFilter id
```


I have to confess that I don't see what the practical use of ``copy`` is. However, it is valuable to see that our environment for compiling, running, and testing programs is working properly.
