---
title: Software Tools in Haskell: import
subtitle: splice contents of a file into stdin
author: nbloomf
---

The ``import`` tool takes lines one at a time, writing them back to ``stdout`` until it sees one of the form

    import FILENAME

These lines should instead be replaced by the contents of ``FILENAME`` (relative to the working directory). I will make two tweaks: first, this program implicitly imposes a format on its input. To avoid being too opinionated, I will make the import "keyword" a parameter, so that if the user runs

    import --with "go-go-gadget"

then the program will instead look for lines of the form

    go-go-gadget FILENAME

This is because the text being filtered may have some other implicit format, where the word "import" means something. The second tweak is to allow the user to import only part of a file. If an import command has the form

    import FILENAME between OPEN and CLOSE

then only those lines from FILENAME which are between lines OPEN and CLOSE are spliced in.

First, we write a generic function called ``takeBetween`` which cuts out portions of a list.


```haskell
&splice src/STH/Lib/List.hs between --takeBetween.S and --takeBetween.E
```


We use a custom data type, ``Import``, to represent the two kinds of import commands. The ``readCommand`` function tries to interpret a line of text as an import command, and the ``splice`` function processes a single line of text (from reading a command to splicing in text from an external file). Now the main program behaves very much like a line filter, which we recall takes a mapping ``String -> String`` and applies it to all lines on ``stdin``. Because ``splice`` reads files and writes to ``stdout``, it must take place in the ``IO`` monad; its signature is ``String -> IO ()``. We write a variant of ``lineFilter`` to handle programs of this type.


```haskell
&splice src/STH/Lib/IO.hs between --lineFilterIO.S and --lineFilterIO.E
```


The program is then not terribly complicated:


```haskell
&splice src/STH/Import/Main.hs
```


``import`` is used to help produce this documentation. These pages contain lots of code snippets, which are taken from the actual program source code using import commands. This way we don't have to worry about keeping (at least part of) the documentation and the code in sync by hand as the code changes (as it does, frequently).

This tool could be improved in a few ways. First, the import command, filename, and open and close lines must not include any spaces. This may prove to be too restrictive; we could allow for quoted arguments or escaped arguments. Second, we could do something a litte more informative when ``readFile`` fails because a file does not exist; this version of ``import`` knows nothing about where a given import command came from. In a large pipeline, or a small pipeline operating on lots of data, this may be a problem.
