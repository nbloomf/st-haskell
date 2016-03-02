module SoftwareTools.Lib.IO (
  charFilter, lineFilter, putNewLine, putStrLns
) where

import SoftwareTools.Lib.Text (getLines)


{-|
  Apply a map to the contents of stdin.
-}
charFilter :: (String -> String) -> IO ()
charFilter f = do
  xs <- getContents
  putStr $ f xs


{-|
  Apply a map to all lines on stdin.
-}
lineFilter :: (String -> String) -> IO ()
lineFilter f = do
  xs <- fmap getLines getContents
  sequence_ $ map (putStrLn . f) xs


putNewLine :: IO ()
putNewLine = putStrLn ""

putStrLns :: [String] -> IO ()
putStrLns = sequence_ . map putStrLn
