---
title: Software Tools in Haskell: archive
subtitle: bundle text files
author: nbloomf
---

``archive`` is an extremely limited form of the standard tool ``tar``. From a bird's eye view, it bundles one or more text files into a single archive, which can be thought of as a mapping from file names to file contents. It does not maintain a hierarchy of its contents and only works with text files. In fact the archive "format" is little more than the concatenation of file names and contents.

An archive consists of a sequence of entries of the form

    #name of file
    >line 1 of file
    >line 2 of file

Lines starting with any character other than ``#`` or ``>`` are ignored. The program is called in one of the following ways:

    archive NAME --list
    archive NAME --add FILE ...
    archive NAME --get FILE ...
    archive NAME --remove FILE ...
    archive NAME --replace FILE ...

``list`` prints the contents of the files in an archive. ``add`` puts new files (given by name as arguments) into an existing archive, and ``get`` retrieves them again, printing to ``stdout``. ``remove`` deletes an entry in an archive, and ``replace`` is equivalent to a remove followed by an add, swapping out the contents of a file in the archive. Trying to operate on a nonexistent archive creates it on the fly.
