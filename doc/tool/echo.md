---
title: Software Tools in Haskell: echo
subtitle: write arguments to stdout
author: nbloomf
---

All the programs we've written so far are strictly *filters*: they read data from stdin and write data to stdout. The metaphor here is that small programs are chained together in a larger "pipeline", and data flows from one end to the other; along the way, each filter changes the data in some way. By reading and writing from stdin and stdout, individual programs do not need to worry about where their data comes from and goes.

``echo`` is the first program we've written that *produces* data without needing to take any from stdin; it is a *source*. (The converse, a program which consumes data without producing any, is a *sink*). ``echo`` simply takes a list of arguments at the command line and writes them to stdout. Unlike the standard echo, we write the arguments one per line.


```haskell
-- sth-echo: write arguments to stdout, one per line

module Main where

import SoftwareTools.Lib (getArgs, exitSuccess)
import SoftwareTools.Lib.IO (putStrLns)

main :: IO ()
main = getArgs >>= putStrLns >> exitSuccess
```


We can now use ``echo`` to test our other programs. For instance, using the shell to run


```
sth-echo "hello" | sth-count --char
```


prints ``6`` to the terminal. Woo!
