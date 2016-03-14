---
title: Software Tools in Haskell: getlines
subtitle: extract lines from stdin by index
author: nbloomf
---

This program is not an example from *Software Tools*; I wrote it to test some functionality that will eventually go into the print program -- namely, parsing sets of integers.

``getlines`` does one thing: it takes a set of integers as an argument, and extracts from ``stdin`` the lines whose indices (counting from 1) are in the given set. For instance,

    getlines "6"

extracts the line at index 6. We can also specify ranges, like

    getlines "1-5"

which extracts lines 1, 2, 3, 4, and 5, as well as skip counts, like

    getlines "2+3"

which extracts every third line starting with the second (i.e. 2, 5, 8, and so on). We can give several rules separated by commas, and the indices specified will be extracted in order. So

    getlines "7-9,1,2"

will extract lines 1, 2, 7, 8, and 9, in that order. We can give more than one integer set argument, and each will be considered in turn with the results concatenated. So

    getlines "1,2" "1,2" "1,2"

extracts lines 1, 2, 1, 2, 1, and 2, in that order.

We define a data type for each kind of integer set: single integers, ranges, and skip counts.


```haskell
&splice src/STH/Lib/Read/IntSet.hs between --IntSet.S and --IntSet.E
```


This code is in a separate library module which only exports one function: ``readIntSet``. That function takes the string representation of a set and returns a function that detects whether a given integer is in the set specified. Compared to representing a set of integers as a set, this makes representing large ranges more efficient and makes representing infinite sets (like skip lists) possible.

Next we write a library function that extracts items from a list.


```haskell
&splice src/STH/Lib/List.hs between --getEltsByIndex.S and --getEltsByIndex.E
```


Finally, the main program is simple enough. We take one optional argument, ``--asacc``, which interprets "lines" using the ASA carriage control format.


```haskell
&splice src/STH/GetLines/Main.hs
```
