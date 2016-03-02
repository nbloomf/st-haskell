-- sth-compress: run-length encode
--   character-oriented

module Main where

import SoftwareTools.Lib (exitSuccess, exitFailure, getArgs)
import SoftwareTools.Lib.IO (charFilter)
import SoftwareTools.Lib.Text (rleEncode)
import SoftwareTools.Lib.Read (readDecimalNat)
import SoftwareTools.Lib.Error (reportErrorMsgs)

main :: IO ()
main = do
  args <- getArgs

  k <- case args of
    []  -> return 6
    [t] -> case readDecimalNat t of
             Nothing -> reportErrorMsgs
               [ "Argument must be a positive integer."
               ] >> exitFailure
             Just h  -> return h
    _   -> reportErrorMsgs
             [ "Argument must be a positive integer."
             ] >> exitFailure

  charFilter (rleEncode k)
  exitSuccess
