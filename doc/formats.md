---
title: Software Tools in Haskell: On Formats
author: nbloomf
---



## A Taxonomy of Tools

One of the lessons of *Software Tools* is that well designed programs gain simplicity and expressive power by working together. I'm writing and using these tools on a unix-like system, where three special files are used to facilitate this: ``stdin``, ``stdout``, and ``stderr``. Programs which read from ``stdin`` and/or write to ``stdout`` immediately gain lots of interesting features (like cheap concurrency and output redirection) with no additional effort. I'll use the standard terms *source*, *sink*, and *filter* to refer to programs which write to ``stdout``, read from ``stdin``, or both, respectively.

A file in unix (including ``stdin`` and ``stdout``) is simply a sequence of bytes. These bytes may be interpreted as unicode text via some encoding like UTF-8. If this is the intended use of the file it is called (appropriately enough) a *text file*, and otherwise a *binary file*. We can thus further classify tools by whether they are *textual* or *binary* sources/filters/sinks. Of course a program may also be a binary to text converter (or vice versa); we'll think of this as a kind of filter.

Text files can be further distinguished from one another. A sequence of raw characters can be given additional meaning if it conforms to a *format*. One of the simplest nontrivial textual formats, and one which is given preferential treatment in unix, is *line text*. This is a sequence of characters which contains zero or more instances of the *newline* character, denoted ``\n``; maximal subsequences of characters which contain no ``\n``s are called *lines*. A specialization of line text is *delimited* line text; this is line text where each line contains zero or more instances of a *field separator* character. Maximal subsequences containing no newlines or field separators are called *fields*. Unlike the newline character, there is no single standard field separator character, so we have to specify whether a particular file is (e.g.) tab-delimited or comma-delimited (or something else). There are more complex textual formats, like JSON, YAML, HTML, XML, markdown, and so on, but characters, lines, and delimited fields are the simplest and most universal.

Every line text file and delimited line text file can of course be thought of as a sequence of characters, but the point is that these are standard formats with extra semantic content baked in. Some textual tools may need to operate differently depending on what format they expect to consume or produce. So we can further classify textual tools as *character*, *line*, or *delimiter* oriented sources/filters/sinks.

All this navel-gazing has concrete consequences for the design of tools. Writing a program requires us to make choices and trade-offs, and keeping in mind **what kind of tool** we are writing can help us determine which choice leads to the simplest, most consistent design.
