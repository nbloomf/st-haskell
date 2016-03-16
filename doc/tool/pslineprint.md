---
title: Software Tools in Haskell: pslineprint
subtitle: print lines to postscript
author: nbloomf
---

We will separate the ``print`` example program in *Software Tools* into several utilities. I don't have access to a real line printer (are these even made anymore? I can't find any info) so I'll start by building a "virtual" line printer. A line printer consumes lines of text and prints them onto paper one by one. Our virtual line printer should do the same, or something like it -- consume lines of text and convert them to a format that can be sent to a real printer.

*PostScript* is a programming language, in use since the mid-1980s, which allows us to succinctly describe the appearance of a printed page. Many printers can directly interpret programs written in PostScript and there is a large ecosystem of tools which can manipulate PostScript documents. The language is a de facto standard, and so we choose PostScript as the output format of our virtual line printer, ``pslineprint``.


### About PostScript

PostScript is a very mature stack-oriented programming language, described in the [PostScript Language Reference Manual](https://www.adobe.com/products/postscript/pdfs/PLRM.pdf). We will only need to use a few features of the language: to print text (including strange characters) using a specific typeface to specific locations on a page and to print multiple pages.

Every PS document must start with the line

    %!PS

Then to print text we have to select a font.

    /Courier findfont
    12 scalefont
    setfont

Every point on the page has a pair of coordinates; the lower left corner of the page is $(0,0)$, and $(h,k)$ is $h$ points from the left edge and $k$ points from the bottom edge. At any given time while a PostScript program is running there is a "current" position where all drawing commands are carried out; we can change the current position with ``moveto``. For example the following command moves the position to $(10,20)$.

    10 20 moveto

This line pushes the numbers 10 and 20 onto a virtual stack, and then ``moveto`` pops the first two items (which are numbers) off of the stack and has the side effect of moving the current position to those coordinates. (A full discussion of [stack-oriented languages](https://en.wikipedia.org/wiki/Stack-oriented_programming_language) is beyond our scope, but they are a very interesting topic and are very amenable to code generation.)

The ``show`` command prints a string at the current position using the current font; it even implicitly updates the current position so that multiple ``show`` commands work as expected. To actually render drawing commands to the page, we use ``showpage``, and ``%%Page:`` starts a new page. (That last command is not clear to me, but my PostScript viewer requires it.) Putting this together, try copying the following to a file called ``test.ps`` and opening it in a PostScript viewer. (Note that the file must end with a newline!)

    %!PS
    /Courier findfont
    12 scalefont
    setfont

    %%Page: 1
    10 20 moveto
    (hello world!) show
    showpage

    %%Page: 2
    10 20 moveto
    (what is this) show
    showpage

Okay. But there is one major limitation of PostScript that we'll have to work around: PostScript does not play very nicely with Unicode. The ``show`` function only behaves as expected on 8-bit ASCII text. The workaround is to use the function ``glyphshow``, which takes special symbolic names for glyphs escaped with a forward slash. For instance,

    /alpha glyphshow

prints the greek letter alpha as long as the current font has a glyph for it. (The version of Courier on my system does not have glyphs for very many special characters, so I will instead use ``FreeMono`` from the [GNU Foundation](https://www.gnu.org/software/freefont/).) Julian Wiseman has compiled a nice [table of PostScript glyph names](http://www.jdawiseman.com/papers/trivia/character-entities.html) ([archive link](https://web.archive.org/web/20160315094516/http://jdawiseman.com/papers/trivia/character-entities.html)).

First, we'll write a black-box module that exports only one function, ``unicodeToPS``, with the sole purpose of taking a string of unicode characters and returning the PostScript code needed to print it. The implementation of ``unicodeToPS`` is straigtforward, but tedious; we march down the string and chop it into ASCII and non-ASCII parts. The ASCII parts are handled with ``show`` and the non-ASCII parts handled one character at a time with ``glyphshow``.


### Limiting the Scope

Before writing any code for the printer itself, let's decide what it will and will not do:

* It **will** print lines of unformatted unicode text in a fixed-width font (as nature intended).
* It **will not** worry about wrapping lines. If we try to print a line that is too long for the page, it will silently march off the edge.
* It **will not** worry about page headings or page numbers.
* It **will not** print multiple files -- only lines from ``stdin``.
* It **will not** interpret escape codes or formatting commands.

All of the "will not" tasks will be left for other tools to deal with; the virtual printer itself will be as simple as possible.


### Modeling a Printer

Now for the virtual line printer. We will think of a line printer as a machine which prints lines in the context of some basic internal state. Some state we can think of as configurable, but fixed: page margins, font size, line spacing, and page size. We use an extremely basic model of page geometry. Other state is updated as the line printer prints lines: the current page number and the current line number. (The ``pageIsDirty`` flag is set whenever the current page has text printed on it; this is needed later.)


```haskell
&splice src/STH/Lib/Monad/LinePrinter.hs between --PageGeom.S and --PageGeom.E


&splice src/STH/Lib/Monad/LinePrinter.hs between --LPState.S and --LPState.E
```


From the page geometry, we can compute two important quantities: the number of lines that can be printed on one page, and the position on the page of each line.


```haskell
&splice src/STH/Lib/Monad/LinePrinter.hs between --PageGeomComp.S and --PageGeomComp.E
```


We'd like to expose a small number of primitive commands that the printer accepts: print a line of text, advance to the next line or the next page. This problem is well modeled by a monad. We use a custom monad stack with explicit state and IO.


```haskell
&splice src/STH/Lib/Monad/LinePrinter.hs between --LinePrinter.S and --LinePrinter.E
```


Now the printer interface we expose is a small number of monadic functions. For instance, ``lpPutStr`` prints a string at the current line.


```haskell
&splice src/STH/Lib/Monad/LinePrinter.hs between --lpPutStr.S and --lpPutStr.E
```


``lpLineFeed`` advances the "print head" to the next line.


```haskell
&splice src/STH/Lib/Monad/LinePrinter.hs between --lpLineFeed.S and --lpLineFeed.E
```


Putting these together, the ``lpPrintLns`` function prints a list of lines.


```haskell
&splice src/STH/Lib/Monad/LinePrinter.hs between --lpPrintLns.S and --lpPrintLns.E
```


It is straightforward to write versions of ``lpPutStr`` and ``lpPrintLns`` that work on carriage control encoded lines we worked with in ``linecount`` -- doing so allows our virtual line printer to produce overstruck characters.


### The Main Program

All we've built so far is a small monadic language which simulates a line printer; all the complexity of PostScript (and integrating unicode with PostScript) is hidden away behind the ``LinePrinter`` interface. Next we need to wire this language to the command line so it can interact seamlessly with our other tools.

This tool differs from the others we've built so far in that our virtual line printer has a few different knobs the user may want to adjust: specifically, the page geometry parameters. To make our tool unopinionated, we should expose these parameters to the user at the command line. And to minimize the burden on the user the order in which these parameters are specified should not matter. Handling several different possible options in any order *by hand* is tedious and error prone, so we will instead use the standard ``GetOpt`` library. This library allows us to declaratively specify what options our program has and provides several useful functions for reading and processing those options. For programs with very simple parameters the full power of ``GetOpt`` is overkill, but it is appropriate here.

The main program logic is very simple. We read the command line options, interpret them as a page geometry specification, and the run our monadic line printer on ``stdin``.


```haskell
&splice src/STH/PSLinePrint/Main.hs
```


Unlike our other tools, it is not so easy to write automated tests of a program like this. As an example, though, try running the following pipeline:

    sth-echo "hello\b\b\b\b\b_____" "world!"
     | sth-unescape
     | sth-overstrike
     | sth-pslineprint --asacc
     > psprinttest.ps

This should produce a page with two lines: the first with the word "hello" underlined.

When every program operates on plain text, making minimal assumptions about formats, it is easy to chain them together to perform complex tasks. Later on we will write programs to make the output of ``pslineprint`` prettier: line wrapping, pagination, and so on. By keeping this functionality out of the printer itself, all programs can stay small and modular.

Later, we may find occasion to enhance our virtual line printer with the ability to change to a bold or italicized font. Because of the modular, monadic design, this will be straightforward.
