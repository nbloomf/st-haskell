-- sth-overstrike: interpret backspaces using line printer control codes

module Main where

import Control.Arrow ((>>>))
import SoftwareTools.FunctionLibrary (getLines, overstrikeLines)

main :: IO ()
main = do
  let overstrike = overstrikeLines
                     >>> zipWith (:) (' ' : (repeat '+'))
                     >>> map putStrLn
                     >>> sequence_

  getContents
    >>= (getLines >>> map overstrike >>> sequence_)
