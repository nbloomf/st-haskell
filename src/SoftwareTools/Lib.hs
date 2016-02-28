module SoftwareTools.Lib (
  getChar, putChar, (>>>), exitSuccess, exitFailure, getArgs, sortBy, unwords
) where

import System.IO (getChar, putChar)
import Control.Arrow ((>>>))
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import Data.List (sortBy, unwords)
