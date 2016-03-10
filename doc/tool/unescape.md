---
title: Software Tools in Haskell: unescape
subtitle: interpret C and ASCII escape codes on stdin
author: nbloomf
---

While testing the [``overstrike``](/pages/sth/tool/overstrike.html) program I ran into an inconvenient problem: I couldn't find an easy and consistent way to type control characters (namely backspace, but others have the same problem) that works both in my terminal and in my golden test suite. It seems like every program -- the terminal, the shell, the test runner -- wants to interpret these characters slightly differently. This problem is a good candidate for a filter-style program. ``unescape`` reads lines from stdin and interprets any C-style escape sequences or ASCII abbreviations it finds. (There is a nice wiki page on [C-style escape sequences](https://en.wikipedia.org/wiki/Escape_sequences_in_C), and the page on [ASCII](https://en.wikipedia.org/wiki/ASCII#ASCII_control_code_chart) includes a table of abbreviations.)

The main program is simple enough, as it simply munches through the lines on stdin looking for escape codes.


```haskell
&splice src/STH/UnEscape/Main.hs
```


The real work is done by the library function ``bsUnEsc``. There's no nice way to do this, so we pattern match on the prefix of a string looking for escape codes. One gotcha: the ASCII abbreviations are not prefix free; for instance the meaning of the string ``\SOHello`` is ambiguous; is it ``\SO`` (Shift Out) followed by ``Hello`` or ``\SOH`` (Start Of Heading) followed by ``ello``? To make sure these are detected correctly, we pattern match ``\SOH`` first (since otherwise that string would always match as ``\SO`` followed by ``H``) and add one more escape code, ``\&`` (taken from Haskell), for those occasions when you *really* want ``\SO`` followed by ``H`` not interpreted as ``\SOH``. ``\&`` represents the empty string, so we can do this with ``\SO\&H``. So, you know, for all those times you need carefully formatted ASCII control codes.

``bsUnEsc`` is a library function because it will probably be useful later for parsing command line arguments.


```haskell
&splice src/STH/Lib/Text/Esc.hs between --bsUnEsc.S and --bsUnEsc.E
```
