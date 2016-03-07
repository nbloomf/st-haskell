---
title: Software Tools in Haskell: crypt
subtitle: xor stdin with a list of keys
author: nbloomf
---

This program performs very (very!) simple encryption by xor-ing ``stdin`` with a list of keys, supplied by the user at the command line.

At the bit level, ``a xor b`` is 0 if ``a`` and ``b`` are equal and is ``1`` otherwise. Two lists of bits are ``xor``ed entrywise, with the shorter list padded with $0$s. We can think of ``xor`` as an operation on natural numbers by converting to and from base 2, and finally we can think of ``xor`` as an operation on *characters* by converting to and from natural numbers (a.k.a. code points). Then to ``xor`` two strings we ``xor`` characterwise.

We will implement these operations bare-handed.


```haskell
data Bit
  = Zero | One
  deriving (Eq, Show)

intToBits :: (Integral n) => n -> [Bit]
intToBits k = case getBits k of
  [] -> [Zero]
  bs -> bs
  where
    getBits t
      | t <= 0    = []
      | otherwise = case even t of
          True  -> Zero : (getBits $ t`quot`2)
          False -> One  : (getBits $ (t-1)`quot`2)

bitToInt :: (Integral n) => Bit -> n
bitToInt Zero = 0
bitToInt One  = 1

bitsToInt :: (Integral n) => [Bit] -> n
bitsToInt = sum . zipWith (*) [2^t | t <- [0..]] . map bitToInt

bitXOR :: Bit -> Bit -> Bit
bitXOR Zero Zero = Zero
bitXOR Zero One  = One
bitXOR One  Zero = One
bitXOR One  One  = Zero

bitsXOR :: [Bit] -> [Bit] -> [Bit]
bitsXOR [] ys = ys
bitsXOR xs [] = xs
bitsXOR (x:xs) (y:ys)
  = (bitXOR x y) : bitsXOR xs ys

intXOR :: (Integral n) => n -> n -> n
intXOR a b = bitsToInt $ bitsXOR (intToBits a) (intToBits b)
```


When we ``xor`` two strings together, one is called the *plaintext* and the other is called the *key*. If the key is shorter than the plaintext we simply repeat it from the beginning as many times as needed. The result is a new string, the *ciphertext*, which will generally not be recognizable. However, we can recover the plaintext by repeating the ``xor`` operation with the same key.

This method of encrytion has several interesting properties. (I am hesitant to call these unequivocal "pros" or "cons", since every encryption scheme involves tradeoffs.)

* Extremely simple to implement.
* Is a symmetric cipher; in fact, exactly the same key is used for encryption and decryption.
* If the key is short compared to the text, is vulnerable to statistical attacks. (Beyond the usual brute-force attacks that come with short keys.)
* Can be used to implement a [one-time pad](https://www.wikipedia.org/wiki/One-time_pad), which to my knowledge is the only provably secure encryption scheme. (To do this, the key must be the same length as the plaintext.)
* Can only be used on character encodings of size $2^n$ (which unicode is), which preferably have a canonical mapping to the integers from $0$ to $2^n - 1$ (which unicode does).
* When used on a large alphabet, most significant bits can play an important role. For instance, if we are encrypting a text which consists only of ASCII (the first 128 characters of unicode) and use a key consisting of characters with a bit higher than the 7th set, then these high bits will never be ``xor``ed away. I haven't thought about this in depth, but I suspect this opens up a new class of statistical attacks.
* Related to the previous property, practical text does not use the full range of unicode -- more likely it is restricted to the characters used in a particular language. This may open a class of attacks.
* When used with ASCII or unicode, generally the ciphertext will include control characters. This can make it inconvenient to use text-oriented tools on encrypted text. We could fix this with a different mapping from characters to numbers, but doing so would probably weaken the encryption even further by reducing the alphabet size.

Here's the main program.


```haskell
-- sth-crypt: xor stdin with a list of keys
--   character-oriented

module Main where

import SoftwareTools.Lib (getArgs, exitSuccess)
import SoftwareTools.Lib.IO (charFilter)
import SoftwareTools.Lib.Text
  (toCodePoint, fromCodePoint, backslashUnEscape)
import SoftwareTools.Lib.Bit (intXOR)

main :: IO ()
main = do
  keys <- getArgs
  charFilter (cryptKeys (map backslashUnEscape keys))
  exitSuccess


cryptKeys :: [String] -> String -> String
cryptKeys []     str = str
cryptKeys (k:ks) str = cryptKeys ks (crypt k str)

crypt :: String -> String -> String
crypt ""  str  = str
crypt key str = zipWith xorChar str (concat $ repeat key)
  where
    xorChar :: Char -> Char -> Char
    xorChar x y
      = fromCodePoint $ intXOR (toCodePoint x) (toCodePoint y)
```


Some notes: ``toCodePoint`` and ``fromCodePoint`` are synonyms for standard library functions which (surprise!) convert characters to and from their unicode code points.

We definitely want the user to specify an encryption key from the command line. But generally, the user can specify many (or no!) command line arguments. What should we do if that happens?

* If the user does not specify a key, then the simplest thing to do is return the plaintext unchanged. We could return an error, but it is feasible that some future user would prefer that "no key" be treated the same as "empty key". Moreover, the recursive definition of ``cryptKeys`` is much cleaner with this decision.
* We could just silently ignore any arguments after the first, but then the presence of extra arguments likely signifies that the user misunderstands how to use the program.
* We could simply concatenate the arguments together as a single larget key.
* We could interpret the command line arguments as a *list* of keys, and use all of them, one at a time.

Concatenating the arguments to a single key would be fine. But interpreting the arguments as multiple keys, to be used independently, has a nice side effect. It provides a simple (if not maximally secure) way for the user to increase the effective size of the key. As K&P note, if we ``xor`` encrypt a given file twice with two different keys, of lengths $m$ and $n$, then this is equivalent to ``xor``ing once with a single key of length $\mathrm{lcm}(m,n)$. For instance,

    crypt "foo" "quuux"

with two keys of length 3 and 5, is equivalent to running ``crypt`` with a key of length 15. Saying

    crypt "foo" "quuux" "mungely"

is just like using a key of length 105.

The keys are also run through ``backslashUnEscape`` by default, meaning that any C or ASCII style escape codes are interpreted. This is the Right Thing because we want the user to have easy access to the widest possible range of keys. It is not necessary to clutter the interface by making this functionality optional with an extra command line argument.

I can think of one important possible improvement to ``crypt``: it would be nice if we could specify the keys as files as well as arguments. For instance, an invocation like

    crypt "foo" "quuux" --keyfiles key1.txt key2.txt

would treat the contents of ``key1.txt`` and ``key2.txt`` as keys, just like ``foo`` and ``quuux``. We generally do not want to leave encryption keys lying around in files. But this would make it easier to use very large keys, for instance to implement a one-time pad. That's an idea for another day.
