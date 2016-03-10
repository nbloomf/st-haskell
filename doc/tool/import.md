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


Bugs: the import command, filename, and open and close lines must not include any space characters. Not even escaped spaces!
