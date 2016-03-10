---
title: Software Tools in Haskell: copy
subtitle: copy characters from stdin to stdout
author: nbloomf
---

Many simple tools are designed to act as *filters*: programs which take a stream of data, manipulate it in some way, and send it along. The ``copy`` program is the simplest possible example of a filter -- the identity filter. This is even simpler than ``cat``, which at least reads and concatenates files.

We can think of ``copy`` as just a character filter -- it reads characters on ``stdin`` and writes them, unmodified, to ``stdout``. But there is another, subtler way to think about how ``copy`` should behave. If the data passed in to ``copy`` is lined text, then every (logical) line should be terminated by a newline character. This may not be the case, though; specifically, the last line in a text file may not be newline-terminated. This is a simple error, though, and so we'd like for ``copy`` to correct this problem. It appears then that ``copy`` has two possible "modes": character mode and line mode. In character mode, ``copy`` passes bytes from ``stdin`` to ``stdout``, and in line mode it should additionally make sure that the last character is a newline.

These two uses of ``copy`` are fundamentally different (consider systems where the line separator is something other than ``\n``) but not so different as to warrant two separate programs. Instead, we'll provide an argument to let the user specify which mode to use. Rather than making the user specify a mode every time, it is better to make one the default and only require an explicit flag for the other. So: which mode should be the default? Which is more likely to be used? With some experience, lined text seems to be the most common textual format, so we'll make this the default and enable straight character copying with a ``--char`` flag.

The way we get command line arguments in Haskell is with the ``getArgs`` function in ``System.Environment``. This function has signature ``IO [String]`` and returns a list of all the arguments given to our program at the command line.

Depending on the mode, ``copy`` does one of two things: read each character from ``stdin`` and write it to ``stdout``, or read each line from ``stdin`` and write it to ``stdout``. We can think of these actions as specific instances of a more general pattern: read a character from ``stdin``, *apply a mapping to it*, and write the result to ``stdout``, or read a line from ``stdin``, *apply a mapping to it*, and write the result to ``stdout``. As we will see, many programs are of this form. So we will write general-purpose character and line filter programs, parameterized on the mapping used to transform the input. ``charFilter`` simply reads everything it can from ``stdin``, applies a function to it, and writes out the result. Note that the standard library function ``getContents`` reads from stdin lazily, so despite appearances this function does not read all of ``stdin`` at once before getting to work.


```haskell
&splice src/STH/Lib/IO.hs between --charFilter.S and --charFilter.E
```


``lineFilter`` does the same, only it extracts the (logical) lines from ``stdin`` first. (The function ``getLines`` extracts the logical lines from a string; we will see this function in detail when we discuss [``count``](/pages/sth/tool/count.html).)


```haskell
&splice src/STH/Lib/IO.hs between --lineFilter.S and --lineFilter.E
```


By wrapping the basic behavior of filters behind a higher order function like this, we can write at a higher level. The ``copy`` program then just needs to determine whether to process characters or lines and filter with the identity.


```haskell
&splice src/STH/Copy/Main.hs
```


I have to confess that I don't see what the practical use of ``copy`` is. However, it is valuable to see that our environment for compiling, running, and testing programs is working properly.
