---
title: Software Tools in Haskell
author: nbloomf
---

*Software Tools* is a little book about tool building by Brian Kernighan and P. J. Plauger. It's a classic, and people far more qualified than me have written very positive reviews of it. The book includes several example programs which are designed individually to solve simple problems and collectively to work together readily to solve larger problems.

I've written some small tools for my own use, the largest of which (by far) is the feivel templating language. But I'm not particularly good at it, and would like to improve. So I will be reading through *Software Tools* and porting the examples to Haskell. Along the way, I expect to supplement the text's examples with my own tools.

Because I enjoy pain, all of this will be done publicly, with code hosted at [GitHub](http://github.com/nbloomf/st-haskell) and narrative documentation posted here.


## A Taxonomy of Tools

One of the lessons of *Software Tools* is that well designed programs gain simplicity and expressive power by working together. I'm writing and using these tools on a unix-like system, where three special files are used to facilitate this: ``stdin``, ``stdout``, and ``stderr``. Programs which read from ``stdin`` and/or write to ``stdout`` immediately gain lots of interesting features (like cheap concurrency and output redirection) with no additional effort. I'll use the standard terms *source*, *sink*, and *filter* to refer to programs which write to ``stdout``, read from ``stdin``, or both, respectively.

A file in unix (including ``stdin`` and ``stdout``) is simply a sequence of bytes. These bytes may be interpreted as unicode text via some encoding like UTF-8. If this is the intended use of the file it is called (appropriately enough) a *text file*, and otherwise a *binary file*. We can thus further classify tools by whether they are *textual* or *binary* sources/filters/sinks. Of course a program may also be a binary to text converter (or vice versa); we'll think of this as a kind of filter.

Text files can be further distinguished from one another. A sequence of raw characters can be given additional meaning if it conforms to a *format*. One of the simplest nontrivial textual formats, and one which is given preferential treatment in unix, is *line text*. This is a sequence of characters which contains zero or more instances of the *newline* character, denoted ``\n``; maximal subsequences of characters which contain no ``\n``s are called *lines*. A specialization of line text is *delimited* line text; this is line text where each line contains zero or more instances of a *field separator* character. Maximal subsequences containing no newlines or field separators are called *fields*. Unlike the newline character, there is no single standard field separator character, so we have to specify whether a particular file is (e.g.) tab-delimited or comma-delimited (or something else). There are more complex textual formats, like JSON, YAML, HTML, XML, markdown, and so on, but characters, lines, and delimited fields are the simplest and most universal.

Every line text file and delimited line text file can of course be thought of as a sequence of characters, but the point is that these are standard formats with extra semantic content baked in. Some textual tools may need to operate differently depending on what format they expect to consume or produce. So we can further classify textual tools as *character*, *line*, or *delimiter* oriented sources/filters/sinks.

All this navel-gazing has concrete consequences for the design of tools. Writing a program requires us to make choices and trade-offs, and keeping in mind **what kind of tool** we are writing can help us determine which choice leads to the simplest, most consistent design.


## Ground Rules

> Okay, let's party. But first, let's go over the rules, because what is fun without the rules?
> <cite>Gru in *Despicable Me 2*</cite>

The purpose of this project is to learn, and so there are some self-imposed rules. (Subject to change.)

1. Reproduce the behavior of the original tools. (Maybe with extensions.) (Note: it turns out that this is more complicated than I thought thanks to unicode.)
2. Write idiomatic Haskell but follow the spirit of the originals. Don't be clever.
3. Produce executable programs which interact with my own working environment.
4. Follow established conventions regarding things like command-line arguments and return codes. (I will prefer GNU-style long options, though not GNU-style with lots of options.)
5. Use standard library functions where possible, unless doing so makes the program a one-liner, thus removing all the fun.
6. Don't hesitate to move common code to a library, especially if doing so makes the main program logic more clear. But library functions should be appropriately general to justify this.
7. Include tests.
8. Think very hard before adding special cases or optional arguments to a tool. These are good candidates for separate tools.
9. Programs that don't need to be interactive or muck with files, shouldn't. If a program naturally acts as a filter, it should act *exclusively* as a filter unless there's a good excuse.

I will prefix the names of these ports with ``sth-``, to avoid clashing with existing real programs. And of course all should be considered works-in-progress.


## The Tools (Book Order)

Chapter 1: Basic Filters

* [``copy``](/pages/sth/tool/copy.html): copy characters from ``stdin`` to ``stdout``
* [``count``](/pages/sth/tool/count.html): count lines or chars on ``stdin``
* [``wordcount``](/pages/sth/tool/wordcount.html): count words on ``stdin``
* [``sentcount``](/pages/sth/tool/sentcount.html): count sentences on ``stdin``
* [``glyphcount``](/pages/sth/tool/glyphcount.html): count glyphs on ``stdin``
* [``detab``](/pages/sth/tool/detab.html): replace tabs on ``stdin`` with spaces
* [``charcombine``](/pages/sth/tool/charcombine.html): replace chars on ``stdin`` with precomposed equivalents
* [``charfullwidth``](/pages/sth/tool/charfullwidth.html): replace chars on ``stdin`` with fullwidth equivalents
* [``entab``](/pages/sth/tool/entab.html): replace spaces on ``stdin`` with tabs
* [``echo``](/pages/sth/tool/echo.html): write arguments to ``stdout``
* [``overstrike``](/pages/sth/tool/overstrike.html): interpret backspaces on ``stdin``
* [``unescape``](/pages/sth/tool/unescape): interpret escape codes on ``stdin``
* [``escape``](/pages/sth/tool/escape.html): replace strange chars on ``stdin`` with escape sequences
* [``compress``](/pages/sth/tool/compress.html): compress text on ``stdin`` (run length encoding)
* [``expand``](/pages/sth/tool/expand.html): uncompress text on ``stdin`` (run length encoding)
* [``crypt``](/pages/sth/tool/crypt.html): xor text on ``stdin`` with a list of keys
* [``translit``](/pages/sth/tool/translit.html): transliterate or remove chars on ``stdin``
* [``charreplace``](/pages/sth/tool/charreplace.html): replace chars by strings on ``stdin``

Chapter 2: Files

* [``compare``](/pages/sth/tool/compare.html): find the first position where two text streams differ


## Why Haskell?

The programs in *Software Tools* are written in [Ratfor](https://en.wikipedia.org/wiki/Ratfor), a purpose-built extension of Fortran with control-flow statements. (At the time, control flow in Fortran was done by hand with GOTO.) Kernighan and Plauger explain that this was a pragmatic choice, as no language at the time had the right mix of ubiquity and expressiveness. With 40 years(!) of hindsight, though, I'd say that this was an inspired choice. Books written in real languages quickly become hopelessly outdated. But books written in toy languages can focus on timeless principles. *TAOCP* by Knuth (which I've never read) and *Functional Programming: Practice and Theory* by MacLennan (which I have) are positive examples of this, and I have a shelf full of nameless algebra books written in APL and Pascal to serve as negative examples.

So why Haskell. I've been using Haskell for several years as a "[tool of thought](www.jsoftware.com/papers/tot.htm)", to paraphrase Ken Iverson, mostly for one-off experiments. Haskell is good for that, and I find that it fits my problem-solving style very well. (Programs are arrows in a category? Of course!) But I want to improve my ability to write "real" programs in the language. So here we are.
