---
title: Software Tools in Haskell: overstrike
subtitle: interpret backspaces on stdin
author: nbloomf
---

At first I was going to skip this program, because I cannot imagine myself having a use for it. But I started thinking about the problem and before I knew it the program was written, so I might as well write about it. :)

In ASCII (and unicode) there is a backspace control character. This character is only kind of related to the backspace key on a modern keyboard. On a typewriter, the backspace key physically moves the carriage "back" one "space", without erasing any characters that might already be printed there. This allowed the typist to physically print one character on top of another, called *overstriking*. Doing so allowed for things like underlining words (overstrike with underscore), striking out words (overstrike with hyphen), creating new characters (overstrike hyphen and colon to get a division sign, maybe), and elaborate typewriter art (a precursor of ASCII art). For example, the literal sequence of key strokes

    Hello world!\b\b\b\b\b\b\b\b\b\b\b\b____________

would appear on the page as underlined text.

According to Kernighan and Plauger, line printers (I've unfortunately never used one, so I'll take their word for it) can achieve overstriking by not advancing the paper between lines. On some machines this was done by prepending each line to be printed with a control code: either a space or a plus sign. For instance, the lines

     Hello world!
    +____________

would be printed one on top of the other, effectively underlining "Hello world!". A space at the beginning of the line means "advance the paper to the next line", and a plus sign means "don't".

The ``overstrike`` program turns a sequence of typewriter-style keystrokes into a sequence of line printer-style print commands. Our strategy is this: every character to be printed has an associated *column index*; this is the column where that character belongs. For instance, in the line

    abcde

The character ``a`` has column index 1, ``b`` has column index 2, and so on. (That's right, I think of strings as 1-indexed. What's it to ya?) Anyway, normally the next character in the string has column index one more than the one before it. Except for those pesky backspace characters! They decrement (by two, really) the column index of everything that comes after them (unless the index is 1, since we cannot print to the left of the left edge of a line). So in the line

    abcde\b\b\bfg

the character ``a`` has index 1, ``b`` is 2, ``c`` is 3, ``d`` is 4, ``e`` is 5, ``f`` is **3**, and ``g`` is **4**.

The trick is this: we can march down a string and determine the column index of every non-backspace character. This results in a big list of char-int pairs. Now in principle each one of these can be turned into an overstrike line. For instance, the last example could be expressed as

     a
    + b
    +  c
    +   d
    +    e
    +  f
    +   g

But there is a problem here: while correct, having only one printed character per line is extremely inefficient! If two lines do not "overlap", they can be merged. So a more efficient way to achieve the same effect is with

     abcde
    +  fg

In fact this is a *most* efficient set of overstrike lines, since 2 is the minimal number of lines required. It isn't too hard to see that if a given line has at most $k$ characters with the same column index, then $k$ is the minimum number of overstrike lines required to render that line (and there is always a set of $k$ overstrike lines that works). Why? We can think of this as a graph coloring problem. The char-int pairs are vertices (ignoring those where the character is a blank), and two vertices are adjacent if they have the same column index. Colorings of this graph correspond to valid sets of overstrike lines. But this graph is a disjoint union of complete graphs, with one component for each column index. The minimum number of colors required is the size of the largest component.

To identify a coloring of the char-int graph, we (1) drop all the blanks, (2) sort the list by column index, and (3) split the list into maximal subsequences by column index. (What?) Finally, (4) thinking of these char-int pairs as sparse lists, convert to real actual lists, using space characters for any missing indices.


```haskell
overstrike :: String -> String
overstrike = overstrikeLines
  >>> zipWith (:) (' ' : (repeat '+'))
  >>> intercalate "\n"

overstrikeLines :: String -> [String]
overstrikeLines =
  columnIndices
    >>> filter (\(c,_) -> c /= ' ')          -- try omitting
    >>> sortBy (\(_,a) (_,b) -> compare a b) -- these lines
    >>> maxMonoSubseqsBy p
    >>> map (fromSparseList ' ')
  where
    p u v = if snd u < snd v
              then True else False

    -- Assign a column index 
    columnIndices :: String -> [(Char,Int)]
    columnIndices = accum [] 1
      where
        accum zs _ ""     = reverse zs
        accum zs k (c:cs) = case c of
          '\b'      -> accum zs (max 1 (k-1)) cs
          otherwise -> accum ((c,k):zs) (k+1) cs

maxMonoSubseqsBy :: (a -> a -> Bool) -> [a] -> [[a]]
maxMonoSubseqsBy p = unfoldr maxMonoSubseq
  where
    maxMonoSubseq [] = Nothing
    maxMonoSubseq xs = accum [] [] xs

    accum as bs [] = Just (reverse as, reverse bs)
    accum [] bs (z:zs) = accum [z] bs zs
    accum (a:as) bs (z:zs) = if p a z
      then accum (z:a:as) bs zs
      else accum (a:as) (z:bs) zs

fromSparseList :: a -> [(a,Int)] -> [a]
fromSparseList x [] = []
fromSparseList x ys = accum 1 [] ys
  where
    accum _ as [] = reverse as
    accum t as ((z,h):zs) = case compare t h of
      EQ -> accum (t+1) (z:as) zs
      LT -> accum (t+1) (x:as) ((z,h):zs)
      GT -> accum (t+1) as zs
```


As an aside: I pulled out ``maxMonoSubseqsBy`` and ``fromSparseList`` as abstractly as possible. When writing Haskell (this is probably true in other languages as well) writing code with the most general possible type usually makes it **easier** to write. Working with a less specific type means there are fewer meaningful things to say, and when there are fewer paths to choose from the correct one is easier to find. This code, for instance, was nearly written in one pass with no substantial editing needed. Not because I am particularly good but because **there's essentially only one way to do it**. Pick a type signature and a recursion pattern (here, either ``unfoldr`` or accumulating parameter) and the rest practically writes itself.

After all that, the main program is pretty straightforward.


```haskell
-- sth-overstrike: interpret backspaces using line printer control codes
--   line-oriented

module Main where

import SoftwareTools.Lib
  ((>>>), exitSuccess, sortBy, intercalate)
import SoftwareTools.Lib.IO
  (lineFilter)
import SoftwareTools.Lib.List
  (maxMonoSubseqsBy, fromSparseList)
import SoftwareTools.Lib.Text
  (getLines)


main :: IO ()
main = do
  lineFilter overstrike
  exitSuccess
```
