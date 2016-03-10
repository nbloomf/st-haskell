---
title: Software Tools in Haskell: compress
subtitle: compress text on stdin (run length encoding)
author: nbloomf
---

In a list of characters, a *run* is a sublist of characters which are all the same. For example, the list

    bookkeeper

has three runs, each of two characters. If a list contains many long runs, it can be losslessly compressed using a technique called [run length encoding](https://en.wikipedia.org/wiki/Run-length_encoding). With such a scheme, instead of storing a literal run like ``aaaaaaa`` we store the repeated character and the number of times it repeats. Kernighan and Plauger do this by breaking a sequence of characters into two kinds of sublists: *runs*, sublists of repeated characters (longer than some threshold length), and *chunks*, sublists containing no runs. The run length encoding scheme in *Software Tools* then transforms a stream of characters into blocks of the form

    (symbol denoting repeat)
    (character to be repeated)
    (repeat count)

and

    (chunk count)
    (list of that many characters)

Where (symbol denoting repeat) is a special character, which we will call the **sigil**. Let's tweak this scheme just slightly. As K&P point out, no compression scheme can perform well on all input (in fact every compression scheme must make some inputs *bigger*). But we are wise to consider "how bad it gets when it gets bad". How bad does this scheme get? The worst possible input for a run length encoding scheme is one with no runs at all, since there are no opportunities for compression. But notice what happens in this case; the entire input is a "chunk", and so must be encoded with its count. The amount of space required to store an arbitrarily large integer, regardless of the scheme required, is proportional to its number of digits. (We can make that proportion smaller by choosing a larger base, but only to a point.) That means an input stream of length $n$ with no runs will require about $\log(n)$ characters just to store the chunk count, for a "compressed" file size of about $n + \log(n)$. Not great!

What if, instead of keeping track of both chunk sizes and repeat counts, we only keep track of repeat counts and reuse the sigil to also denote the *end* of the repeat count. This way, an encoded stream looks like a stream of

    (sigil)
    (character to be repeated)
    (repeat count)
    (sigil)

and

    (characters not including the sigil)

Of course now we also need to provide a way to encode literal instances of the sigil. What is the simplest way to do this? All characters *other than* the sigil are interpreted literally, unless we want to introduce another escape character. We can't use a single sigil, since that means "start a new encoded run". And we cannot use two sigils, because that means "start a new encoded run of sigils". But three copies of the sigil character in a row does not mean anything, if we remember not to use the sigil character to encode numbers. So we interpret the string

    (sigil)(sigil)(sigil)

to mean a literally encoded sigil. (Note that it is then more space efficient to encode two or more literal sigils as a run. (Two literal sigils is 6 characters in this scheme, but only 4 as a run.))

What is the worst case now? Well, an input stream with no runs *and no sigils* -- a chunk -- will be encoded as is with no overhead. An input stream with no runs and including sigils will require two extra characters for each sigil. In the worst case, a stream of $n$ characters will require about $2n$ characters for the sigils, for a "compressed" size of about $3n$. Really not great!

On the face of it the second scheme is much worse, in the worst case, than the first. But which is worse on realistic data? If we plan to use ``compress`` on textual data we can choose the sigil to be a rarely used character. ASCII includes several control characters, like ``\BEL``, which do not appear in text. Note that if our input does not have any literal sigils, then the second scheme can never compress its input to a larger size as long as we only compress runs of at least 5 characters (as the only way this happens is by encoding literal sigils). On the other hand, the first scheme adds overhead proportional to $\log(n)$ for every chunk of length $n$ -- so unless our input includes long runs, or lots of short runs, the size may not decrease much and can easily increase.

The main program is basic; ``compress`` is a character-oriented filter.


```haskell
&splice src/STH/Compress/Main.hs
```


The actual run length encoding is a little complicated. We define an internal representation for run length encoded data.


```haskell
&splice src/STH/Lib/Text/RLE.hs between --RLE.S and --RLE.E
```


Doing this is not strictly necessary, but introducing a type for run length encoded data makes it easier to decompose algorithms (algebraic data types are a big win here). Now ``rlEncode`` works in two phases: first it reads a stream of characters into the internal representation of RLEs, and then it serializes that representation as a string.


```haskell
&splice src/STH/Lib/Text/RLE.hs between --rlEncode.S and --rlEncode.E
```


We use two library functions to work with run-encoded lists.


```haskell
&splice src/STH/Lib/List.hs between --getRuns.S and --getRuns.E


&splice src/STH/Lib/List.hs between --fromRuns.S and --fromRuns.E
```


Repeat counts are encoded in base 86 for space efficiency. (Counts up to 85 need only one character, counts up to 7395 need at most two, and up to 636055 need at most three.)


```haskell
&splice src/STH/Lib/Text/RLE.hs between --showBase86Nat.S and --showBase86Nat.E
```

It will be difficult to test ``compress`` until we've also written its companion, [``expand``](/pages/sth/tool/expand.html).
