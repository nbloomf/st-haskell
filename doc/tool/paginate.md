---
title: Software Tools in Haskell: paginate
subtitle: format lines with page numbers and headers
author: nbloomf
---

Our virtual line printer [``pslineprint``](/pages/sth/tool/pslineprint.html) is nice enough, but extremely simple; it does nothing at all to prettify the documents it prints. Our first attempt at this is ``paginate``. This program will split a sequence of lines into "pages", giving each page a header and page number. It will also be able to print more than one file sequentially, making each file start on its own page and ensuring that page numbers are correct across files. If any file name happens to be ``-``, we read lines from ``stdin``. Finally, it will optionally print a table of contents page in case we are printing a large number of long files.

I think this is the ugliest tool in our kit so far, and that part of the reason for this is that ``paginate`` depends on several arbitrary choices; frequently if there is a single "natural" or "obvious" choice, the resulting code is simple, but if we are making an arbitrary choice among several options our code feels complicated.

First lets look at the main program. Like ``pslineprint``, there are enough options to make it worth our while to use ``GetOpt`` to process them. For now lets suppose we have functions ``paginateLines`` and ``tableOfContents`` that handle all the heavy lifting; the main program logic is mostly straightforward.


```haskell
&splice src/STH/Paginate/Main.hs
```


Now for the actual pagination. Generally speaking, ``paginate`` takes a list of lines and inserts new lines -- the headers -- as well as some blank lines in appropriate places, so that the lines can then be taken in chunks of $n$ at a time (called "pages"). But exactly what a "line" is is already ambiguous; of course the usual "text separated by newlines" consists of lines, but so also does a file formatted using ASA carriage control codes. Both kinds of "line" are handled properly by ``pslineprint``, and we already have at least one program, ``overstrike``, which produces carriage control formatted text. So it seems worth our while to make ``paginate`` handle carriage controls as well.

An initial version of this program handled both kinds of line separately, which led to lots of duplicated code. To avoid this, we introduce an abstract ``Line`` type class.


```haskell
&splice src/STH/Lib/Text/Paginate.hs between --Line.S and --Line.E
```


Also, there are a few tweakable parameters we'd like to be able to adjust: the number of "lines" to appear on each page, and the width (in characters) of the header lines. We wrap these into a type, ``PaginateOpts``, that can be more easily (and meaningfully) be passed around.


```haskell
&splice src/STH/Lib/Text/Paginate.hs between --PaginateOpts.S and --PaginateOpts.E
```


Note that from a ``PaginateOpts`` and a list of (abstract) documents we can compute the total number of pages used and the starting page numbers of each document. These will be used later.

Next we define an abstract page header. Our headers will include three pieces of information: the name of the file being paginated, the current page number, and the total number of pages. We also need a way to convert an abstract header to a list of lines; this is done with ``renderHeader``. We define this function as part of a type class so that we can have different implementations for each kind of line.


```haskell
&splice src/STH/Lib/Text/Paginate.hs between --Header.S and --Header.E
```


The actual pagination is handled by a few different functions:

* ``splitPages`` divides a document into abstract pages, without proper page numbers.
* ``numberPagesFromOf`` fixes the page numbers of a list of abstract pages, with parameters allowing us to specify where to begin counting from and the total number of pages.
* ``renderPage`` converts an abstract page to a list of lines.
* ``paginateOfFrom`` combines ``splitPages``, ``numberPagesOfFrom``, and ``renderPage`` to paginate a single document.
* ``paginateDocs`` paginates a list of named documents.


```haskell
&splice src/STH/Lib/Text/Paginate.hs between --Pagination.S and --Pagination.E
```


The actual functions we expose from this module are ``paginateLines`` and ``paginateCCLines``, which are just monomorphic synonyms of ``paginateDocs`` for ordinary lines and carriage control formatted lines, and the constructor for ``PaginateOpts``. As far as consumers of this module are concerned, these two black-boxes are implemented separately. Since (as of this writing) the ``Line`` class has only two instances there is no reason to expose the guts of pagination. But by writing our code against an abstract ``Line`` class, it will be easier to extend in the future if needed.


```haskell
&splice src/STH/Lib/Text/Paginate.hs between --Spec.S and --Spec.E
```


All that remains is to provide a function for building the table of contents. This part is kind of gross.


```haskell
&splice src/STH/Lib/Text/Paginate.hs between --TOC.S and --TOC.E
```


A few comments about the default options. I expect that the main use of ``paginate`` will be to prepare documents for ``pslineprint``, and the default settings of that program produce pages with 52 lines per page and about 75 characters per line. Using these as the defaults for ``paginate`` means we can say things like

    paginate foo.txt | pslineprint

and get reasonable results.
