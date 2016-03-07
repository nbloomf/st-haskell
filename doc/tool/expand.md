---
title: Software Tools in Haskell: expand
subtitle: uncompress text on ``stdin`` (run length encoding)
author: nbloomf
date: 2016-03-02
---

The companion to [``compress``](/pages/sth/tool/compress.html) is ``expand``. It reads a string of characters that was run length encoded by ``compress`` and uncompresses it. This program has an error condition; the input may not be valid. This can happen for a few reasons; if a repeat count is incorrectly encoded (i.e. includes invalid digits or does not terminate in a sigil), or if the file ends in the middle of a repeat encoding.

```haskell
-- sth-expand: uncompress stdin (run length encoding)
--   character-oriented

module Main where

import SoftwareTools.Lib
  (exitSuccess, exitFailure)
import SoftwareTools.Lib.IO (charFilter)
import SoftwareTools.Lib.Text (rlDecode)
import SoftwareTools.Lib.Error (reportErrorMsgs)

main :: IO ()
main = do
  xs <- getContents

  ys <- case rlDecode '\BEL' xs of
          Just zs -> return zs
          Nothing -> reportErrorMsgs
                       [ "corrupt input"
                       ] >> exitFailure

  putStr ys
  exitSuccess
```


``rlDecode`` does all the work:


```haskell
rlDecode :: Char -> String -> Maybe String
rlDecode sig = fmap (runLengthDecode sig) . readRLE sig
  where
    runLengthDecode :: (Eq a) => a -> [RLE a] -> [a]
    runLengthDecode sig = concatMap decodeRLE
      where
        decodeRLE (Chunk  xs)  = xs
        decodeRLE (Repeat k x) = replicate k x
        decodeRLE (Literal k)  = replicate k sig

    readRLE :: Char -> String -> Maybe [RLE Char]
    readRLE sig = unfoldrMaybe readFirstRLE
      where
        readFirstRLE :: String -> Maybe (Maybe (RLE Char, String))
        readFirstRLE ""  = Just Nothing
        readFirstRLE [x] =
          if x == sig then Nothing else Just (Just (Chunk [x], ""))
        readFirstRLE [x,y] =
          if x == sig then Nothing else Just (Just (Chunk [x], [y]))
        readFirstRLE (x:y:z:xs)
          | x == sig && y == sig && z == sig
              = Just (Just (Literal 1, xs))
          | x == sig && y == sig && z /= sig
              = do
                  let (as,bs) = span (/= sig) (z:xs)
                  k <- readBase86Nat as
                  case bs of
                    ""     -> Just (Just (Repeat k y, ""))
                    (_:cs) -> Just (Just (Repeat k y, cs))
          | x == sig && y /= sig
              = do
                  let (as,bs) = span (/= sig) (z:xs)
                  k <- readBase86Nat as
                  case bs of
                    ""     -> Just (Just (Repeat k y, ""))
                    (_:cs) -> Just (Just (Repeat k y, cs))
          | otherwise
              = do
                  let (as,bs) = span (/= sig) (x:y:z:xs)
                  Just (Just (Chunk as, bs))
```


One big improvement we could make to ``expand`` is to try to handle invalid input more gracefully; we could output the partially expanded text, for instance, or tell the user exactly where the error occurs. The first idea would not be too difficult. (Write the output to stderr.) The second idea, though, while possibly useful, would make the implementation much more complicated. (We'd have to keep track of the position of each character in the original source.) Doable, but until the need is demonstrated I'd prefer to keep the implementation simple.
