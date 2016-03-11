---
title: Software Tools in Haskell: On Formats
author: nbloomf
---

Ostensibly, many software tools operate on streams of text. But not all text is created equal: some text is *formatted*. Whenever a character in a text stream doesn't "mean what it says", so to speak, we have a format. For example many tools follow the convention that the character sequence ``\n`` denotes the unicode character with code point 0A. The backslash is not a literal backslash; we have a format. One of the reasons why text is so useful is that many different human-readable formats can be layered on top of it (even more than one at a time, though this can be dangerous).

Keeping in mind what *kind* of text a tool expects to receive and produce can help lead us to simpler and more consistent programs. On this page we'll keep a list of a few different textual formats with links to more information about them.

* **Unformatted Text.** Uninterpreted streams of unicode characters.
* **Escaped Text.** This is a stream of characters where certain sequences, called *escape codes*, are shorthand for other characters. This is a useful way to type otherwise untypeable characters. There are a few widely used escape code standards such as [C-style](https://en.wikipedia.org/wiki/Escape_sequences_in_C), [ASCII-style](https://en.wikipedia.org/wiki/ASCII#ASCII_control_code_chart), and [HTML-style](https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references).
* **Lined Text.** Copies of a character or sequence of characters (such as ``\n`` on unix or ``\r\n`` on some other systems) are singled out and called *line separators*. Maximal subsequences of characters which contain no parts of a line separator are called *lines*. This format is given preferential treatment in unix-like systems and many other formats are built on it.
* **Delimited Text.** A special character or sequence of characters, different from the line separator, is singled out and called called the *field separator*. Lines are called *records* and maximal subsequences containing no line separators or field separators are called *fields*; delimited text is an extremely cheap matrix or database. There is no single standard field separator, so we have to specify whether a particular file is (e.g.) *tab-delimited* or *comma-delimited* (or something else).

There are more complex textual formats, like JSON, YAML, HTML, XML, markdown, and so on, but characters, lines, and delimited fields are the simplest and most universal.

Of course delimited text can be thought of as lined text, which can be thought of as a stream of characters. But the point is that standard formats have extra semantic context baked in, and that context can inform how a tool should behave.
