---
title: Software Tools in Haskell: escape
subtitle: replace strange chars on stdin with escape sequences
author: nbloomf
---

The ``escape`` program is the companion of [``unescape``](/pages/sth/tool/unescape.html); it replaces any non-printing, non-ASCII characters with C-style escape sequences using only visible ASCII. We need to be careful about exactly which characters to escape; lined text is delimited by ``\n``s, and converting these to escaped form would destroy the format. Keeping with the convention that line text is the most common, by default we leave newlines alone. The ``--char`` flag instructs ``escape`` to escape all characters.


```haskell
&splice src/STH/Escape/Main.hs
```


The work is done by ``bsEsc``:

```haskell
&splice src/STH/Lib/Text/Esc.hs between --bsEsc.S and --bsEsc.E


&splice src/STH/Lib/Text/Esc.hs between --esc.S and --esc.E
```


``showHex`` is a library function that returns the hexadecimal expansion of a natural number.


```haskell
&splice src/STH/Lib/Int.hs between --showHex.S and --showHex.E
```
