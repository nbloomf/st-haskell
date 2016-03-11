module STH.Lib.IO (
  charFilter, lineFilter, lineFilterIO, putNewLine, putStrLns
) where

import STH.Lib.Text.Format.Line (getLines)


{-|
  Apply a map to the characters on stdin,
  sending the results to stdout.
-}
--charFilter.S
charFilter :: (String -> String) -> IO ()
charFilter f = do
  xs <- getContents
  putStr $ f xs
--charFilter.E


{-|
  Apply a map to the lines on stdin,
  sending the results to stdout.
-}
--lineFilter.S
lineFilter :: (String -> String) -> IO ()
lineFilter f = do
  xs <- fmap getLines getContents
  sequence_ $ map (putStrLn . f) xs
--lineFilter.E

--lineFilterIO.S
lineFilterIO :: (String -> IO ()) -> IO ()
lineFilterIO f = do
  xs <- fmap getLines getContents
  sequence_ $ map f xs
--lineFilterIO.E


putNewLine :: IO ()
putNewLine = putStrLn ""

putStrLns :: [String] -> IO ()
putStrLns = sequence_ . map putStrLn
