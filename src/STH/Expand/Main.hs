-- sth-expand: uncompress stdin (run length encoding)
--   character-oriented

module Main where

import STH.Lib
  (exitSuccess, exitFailure)
import STH.Lib.IO (charFilter)
import STH.Lib.Text.RLE (rlDecode)
import STH.Lib.Error (reportErrorMsgs)

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
