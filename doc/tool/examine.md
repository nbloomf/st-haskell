---
title: Software Tools in Haskell: examine
subtitle: interactively view a file
author: nbloomf
---

This is not one of the examples in *Software Tools*, but is a program I've found myself wishing I had in the past. It's a simple interactive file viewer. ``examine`` takes a file and presents it to the user a little bit at a time, and accepts some simple commands: pressing ``n`` advances to the next chunk of the file, ``v`` the previous chunk, ``x`` exits the program, and ``w`` writes the current chunk to ``stdout``. That last command, ``w``, can be used to make ``examine`` act like a human-powered filter. To avoid cluttering up ``stdout``, the interaction happens on ``stderr``. (This is a minor abuse of ``stderr``.) I'd prefer to use the arrow keys for navigation, but doing this in a platform-independent way is not so simple.


```haskell
&splice src/STH/Examine/Main.hs
```


One idea of interest is the way we represent our file internally. The file being examined can be thought of as a list of chunks, but we'd like one of these, the "current chunk", to be special. Using a straight list to represent the chunks, we could use the list index to keep track of the current chunk. Updating the reference to the current chunk takes constant time (add or subtract 1), but retrieving the current chunk takes linear time. In a large file this could be a problem. Can we do better? In other languages we might use pointers to randomly access parts of a file, but in Haskell this strategy is less straightforward.

Note, though, that our design doesn't really want *random* access to the parts of a file. We just want one part to be singled out as the "current" chunk and to have efficient ways to move to the "next" and "previous" chunk. We can represent this as a triple: the current chunk, a list of all the next chunks, and a list of all the previous chunks.


```haskell
&splice src/STH/Lib/ZipperList.hs between --ZipperList.S and --ZipperList.E
```


I've called this a "zipper list" because it is related to the well-studied type theoretic concept of [zippers](https://en.wikibooks.org/wiki/Haskell/Zippers).

Zippers are **wild**, and one of the reasons I got turned on to Haskell in the first place. So Haskell types are objects in a category, right? And this category (among other properties) has all binary products and coproducts: tuples ``(a,b)`` and eithers ``Either a b``. With a slight change in notation we can use ordinary multiplication and addition symbols to represent products and coproducts, so that a type like ``(Either a b, c)`` is denoted ``(a+b)c``. A similar convention applies to algebraic data types. A type like

    data Foo a b c = A a | BC b c

can be denoted ``Foo(a,b,c) = a + bc``. The ``data`` keyword signals that we are naming a type-level synonym, or "function" of sorts, and this function looks an awful lot like a polynomial.

This is where zippers come in: any algebraic type has an associated "zipper" type, which represents the type of "one-hole contexts" over that type. And the zipper type is found by symbolically **differentiating** the original type function.

Here's an extremely hand-wavy example. Recursive types can also play this game; so the type of lists, which we might denote

    data L a = Nil | Cons a (L a)

can be represented as $L(a) = 1 + a L(a)$. Blurring the line for a moment between functions and types we can "solve the equation" for $L(a)$ as $$L(a) = \frac{1}{1-a}.$$ What does this even mean, though? Addition and multiplication of types is meaningful, but I'm not sure about subtraction and division. But if we convert this to its power series representation, we have $$L(a) = 1 + a + a^2 + a^3 + \cdots.$$ This makes a little more sense. A list is either empty, or contains one element, or contains two elements, or three, and so on. (This singles out the unique "smallest" solution of the defining equation of $L(a)$, excluding infinite lists. Hand wave!) Implicitly differentiating $L(a) = 1 + aL(a)$ with respect to $a$, we get $$L^\prime(a) = \frac{L(a)}{1-a},$$ which (using the power series hand wave) is $$L^\prime(a) = L(a)L(a).$$ That is, the type of one-hole contexts of a list is a *pair* of lists, representing the part before and the part after the hole. With one element to fill the hole, this is the type of our ``ZipperList``.

There is some deep magic here which I do not completely understand. For instance, the differentiation of types can be done automatically, and higher derivatives give us contexts with more than one hole. And does the analogy between types and calculus hold for codata (potentially infinite data)?
