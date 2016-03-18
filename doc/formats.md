---
title: Software Tools in Haskell: On Formats
author: nbloomf
---

Ostensibly, many software tools operate on streams of text. But not all text is created equal: some text is *formatted*. Whenever a character in a text stream doesn't "mean what it says", so to speak, we have a format. For example many tools follow the convention that the character sequence ``\n`` denotes the unicode character with code point 0A. The backslash is not a literal backslash. One of the reasons why text is so useful is that many different human-readable formats can be layered on top of it (even more than one at a time, though this can be dangerous).

Keeping in mind what *kind* of text a tool expects to receive and produce can help lead us to simpler and more consistent programs. On this page we'll keep a list of a few different textual formats with links to more information about them.


## Unformats

* **Unformatted Text.** Uninterpreted streams of unicode characters.
* **Escaped Text.** This is a stream of characters where certain sequences, called *escape codes*, are shorthand for other characters. This is a useful way to type otherwise untypeable characters. There are a few widely used escape code standards such as [C-style](https://en.wikipedia.org/wiki/Escape_sequences_in_C), [ASCII-style](https://en.wikipedia.org/wiki/ASCII#ASCII_control_code_chart), and [HTML-style](https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references).
* **Lined Text.** Copies of a character or sequence of characters (such as ``\n`` on unix or ``\r\n`` on some other systems) are singled out and called *line separators*. Maximal subsequences of characters which contain no parts of a line separator are called *lines*. This format is given preferential treatment in unix-like systems and many other formats are built on it.
* **Delimited Text.** A special character or sequence of characters, different from the line separator, is singled out and called called the *field separator*. Lines are called *records* and maximal subsequences containing no line separators or field separators are called *fields*; delimited text is an extremely cheap matrix or database. There is no single standard field separator, so we have to specify whether a particular file is (e.g.) *tab-delimited* or *comma-delimited* (or something else).

Of course delimited text can be thought of as lined text, which can be thought of as a stream of characters. But the point is that these formats have extra meaning baked in which informs how a tool should behave.


## Markup

* **HTML.** Markup for web browsers. Most recent version is HTML5. Maintained by [W3C](https://www.w3.org/TR/html5/).
* **Markdown.** An attempt to standardize some semantic conventions people have been using with plain text for decades. (This site is maintained in Markdown.) As of 2016 there is an effort to produce a better formal spec called [CommonMark](http://commonmark.org/), but it is notable that even the original, buggy, poorly defined version worked well enough to become popular.
* **JSON.** The name is short for JavaScript Object Notation. This is a markup language for structured data. Described in [RFC 7159](https://tools.ietf.org/html/rfc7159). Also has its own [webpage](http://www.json.org/).


## Really complicated

* **PostScript.** A stack-oriented page description language. Meant to be written by people or programs and read by printers. Specified by the third edition of the [PostScript Language Reference Manual](https://www.adobe.com/products/postscript/pdfs/PLRM.pdf).
