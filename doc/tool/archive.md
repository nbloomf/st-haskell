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

Lines starting with any character other than ``#`` or ``>`` are ignored. (We shouldn't be editing the archive file by hand, and we'll be careful that ``archive`` writes correctly formatted archives.) If two items in an archive have the same name, we ignore all but the first. This shouldn't happen if we only manipulate archives with ``archive``, but our archive format is such that the concatenation of two archives may be a valid archive. The program is called in one of the following ways:

    archive NAME --list
    archive NAME --add FILE ...
    archive NAME --get FILE ...
    archive NAME --remove FILE ...
    archive NAME --replace FILE ...

``list`` prints the contents of the files in an archive. ``add`` puts new files (given by name as arguments) into an existing archive, and ``get`` retrieves them again, printing to ``stdout``. ``remove`` deletes an entry in an archive, and ``replace`` is equivalent to a remove followed by an add, swapping out the contents of a file in the archive. Destructive operations (remove and replace) send the altered archive to ``stdout`` rather than destroying the original. Trying to operate on a nonexistent archive creates it on the fly.

First, we'll define an abstract "archive" type to keep track of the information we want: an archive is a list of named items.


```haskell
&splice src/STH/Lib/Archive.hs between --Archive.S and --Archive.E
```


The use of the synonym ``Name`` for ``String`` is an example of a common idiom in languages with type inference. Names are Strings, but by introducing a lightweight type to indicate that a particular string is not just a *String*, but a *Name*, will make our type signatures more meaningful with no additional overhead. Due in part to purity, type signatures in Haskell, and I imagine in languages of similar descent like ML and F#, are a powerful kind of documentation. Just knowing the signature of a function tells us quite a bit about what it may -- or may not! -- do. But now I'm rambling.

Our archive type needs to support the five basic operations: adding, retrieving, removing, viewing, and listing items. This is straightforward.


```haskell
&splice src/STH/Lib/Archive.hs between --ArchiveOps.S and --ArchiveOps.E
```


Note that implementing archives as a (pure) type means that in principle we do have to read in an entire archive before we can operate on it, laziness notwithstanding. It's possible this will cause performance issues on large archives, but this is fine for now.

The final functions we need for our abstract Archive type allow us to read from, and write to, lists of strings.


```haskell
&splice src/STH/Lib/Archive.hs between --ArchiveIO.S and --ArchiveIO.E
```


The Archive type and basic operations are kept in a separate module; users of the module can only create and manipulate ``Archive``s via the provided interface. In this way we have the ability to change the implementation later if needed; for instance, we could store items in an archive using a tree rather than a list. (Although, since in typical usage ``archive`` has to read an entire archive anyway, I doubt using an asymptotically better structure will have much effect on the overall efficiency of the entire program. In such cases I think the simpler implementation is preferable.)

The main program is reasonably straightforward. We read the command line arguments to determine the name of the archive being manipulated, which of the five commands is being invoked, and the name(s) of the file(s) used.


```haskell
&splice src/STH/Archive/Main.hs
```


This program could be improved in several ways. As is, if the user tries to add an existing name to an archive, we bail. This seems like the Right Thing. But if the user tries to add a list of several names to an archive, and only one of them already exists, the entire operation fails. The same is true of the get operation. Would it be better to add/get what we can? Maybe, maybe not. The "fail early" strategy means that archive operations are atomic; each operation either succeeds entirely or fails entirely. This property can help us avoid inconsistent state, if, say, ``archive`` is used as a component in another program. Kernighan and Plauger also suggest keeping track of the date and time that a particular file is archived in the item header; this could be done by adding a ``DateTime`` field to the ``Item`` type and passing the date and time as an extra argument from the main function.

One possible drawback of this design is that we add some overhead to the files being archived -- one extra character per line. For some files this might be a significant burden.
