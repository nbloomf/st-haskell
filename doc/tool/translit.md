---
title: Software Tools in Haskell: translit
subtitle: transliterate or remove chars on stdin
author: nbloomf
---

The purpose of ``translit`` is to replace characters by other characters; it applies a mapping with signature ``Char -> Char`` to each character on stdin. (While simple, this is surprisingly useful.) The most succinct way to specify such a mapping is with two lists of characters, one denoting the domain of the character mapping and the other the codomain. For instance, calling

    translit "abc" "xyzw"

replaces all ``a``s by ``x``, ``b``s by ``y``, and ``c``s by ``z``. (The ``w`` has no effect.) It is useful if we can also specify *ranges* of characters with hyphens. For instance the string ``a-g`` should be shorthand for ``abcdefg``. If the second list is nonempty, but shorter than the first, we pretend its final character is repeated indefinitely, so that

    translit "abc" "x"

replaces all copies of ``a``, ``b``, or ``c`` by ``x``.

If the second list is empty or not given, we can reasonably interpret this to mean we should remove the characters in the first list from the input. This is all well and good.

I have one quibble with Kernighan and Plauger's design, though; they introduce another special input case. If the second list contains 0 or 1 characters, then the first list can be prepended by a "not" symbol like so:

    translit -"abc" "x"

This means to replace every character *except* ``a``, ``b``, and ``c`` by ``x``. I respectfully claim that this is the Wrong Thing. The arguments to ``translit`` are shorthand for a mapping, given in the form of two **lists**. We can tell by the significance of order; ``translit ab xy`` is not the same as ``translit ba xy``. It is only by a quirk of combinatorics that this dependence on order goes away if the second list has 0 or 1 entry. But the special "not" option implicitly treats the arguments of ``translit`` as **sets**. To see this, think about what the complement of a list of characters is. In principle we could say that ``-"a-z"``, as a list, means all characters (in order) except for the lower case roman letters. But is this useful? For instance, what if the user tries to run

    translit -"abc" "xy"

What does this mean? Is the ``y`` just ignored? (User input should not be silently ignored.) Does the first unicode code point get mapped to ``x``, and all others to ``y``? Is this useful? Is it useful enough to warrant complicating ``translit`` with the extra code needed to handle this special case of a special case? I don't think it is.

But the ability to replace or remove characters from a *set complement* is useful. And so I will split ``translit`` into two: ``translit`` will handle the list-wise mapping of characters, and a separate program, ``charreplace``, will handle set-wise mappings. My opinion (and it's just an opinion!) is that these two uses are different enough, semantically, to deserve separate tools; this avoids burdening the user with too many special cases and cluttering the interface with delicate options.

We already have most of the machinery needed for ``translit``, except for the code needed to interpret command line arguments. We introduce an internal representation of character sequences: a character sequence is a list of either single characters or ranges of characters.


```haskell
data CharSeq
  = Single Char
  | Range  Char Char
  deriving (Show)

interpArg :: String -> Maybe String
interpArg = fmap charSeqsToList . readCharSeqs . backslashUnEscape

charSeqsToList :: [CharSeq] -> String
charSeqsToList = concatMap charSeqToList
  where
    charSeqToList (Single x) = [x]
    charSeqToList (Range x y) = enumFromTo x y

readCharSeqs :: String -> Maybe [CharSeq]
readCharSeqs = unfoldrMaybe firstCharSeq
  where
    firstCharSeq :: String -> Maybe (Maybe (CharSeq, String))
    firstCharSeq ""      = Just Nothing
    firstCharSeq [x]     = Just (Just (Single x, ""))
    firstCharSeq ('-':_) = Nothing
    firstCharSeq [x,y]   = Just (Just (Single x, [y]))
    firstCharSeq (x:y:z:xs) = case y of
      '-' -> Just (Just (Range x z, xs))
      otherwise -> Just (Just (Single x, y:z:xs))
```


Now the main program just has to interpret its arguments and call some library functions.


```haskell
-- sth-translit: transliterate characters on stdin
--   character-oriented

module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import STH.Lib
  (charFilter, applyListMap, padLast,
   readCharRange, reportErrorMsgs)


main :: IO ()
main = do
  args <- getArgs

  (from,to) <- case map readCharRange args of
    [Just as]          -> return (as, "")
    [Just as, Just bs] -> return (as, bs)
    otherwise          -> argError

  let
    remove   = filter (not . (`elem` from))
    translit = map (applyListMap $ zip from (padLast to))

  case to of
    ""        -> charFilter remove
    otherwise -> charFilter translit

  exitSuccess


argError :: IO a
argError = reportErrorMsgs
  [ "usage:"
  , "  translit [FROM] [TO]  -- replace chars in FROM by those in TO"
  , "  translit [REMOVE]     -- remove chars in REMOVE"
  ] >> exitFailure
```


Note that the arguments of ``translit`` are run through ``bsUnEsc``, so that we can easily work with otherwise untypeable characters. (We could, for example, use this to replace ``charfullwidth``.) With ``translit``, many of the small tools we've written so far can suddenly be combined to do neat things. As a simple example, put the following text in a file called ``unicode-test.txt``.


```
\uxyz0 \uxyz1 \uxyz2 \uxyz3
\uxyz4 \uxyz5 \uxyz6 \uxyz7
\uxyz8 \uxyz9 \uxyza \uxyzb
\uxyzc \uxyzd \uxyze \uxyzf
```


Now the pipeline

    cat unicode-test.txt | translit "xyz" "001" | unescape

replaces the ``x``, ``y``, and ``z`` with ``0``, ``0``, and ``1`` and interprets the ``\uXXXX`` as escape codes. This lets us see what several unicode code points look like at one time. With a larger "template" file we could see more characters at a time.

