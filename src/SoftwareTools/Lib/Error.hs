module SoftwareTools.Lib.Error (
  putStrLnErr, reportErrorMsgs, catchEOF
) where

import System.IO (hPutStrLn, stderr)
import System.IO.Error (isEOFError, catchIOError)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)


putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr


reportErrorMsgs :: [String] -> IO ()
reportErrorMsgs errs = do
  name <- getProgName
  sequence_ $ map putStrLnErr $ ((name ++ " error"):errs)


{-|
  The 'catchEOF' function takes an IO action
  and attempts to run it. If an EOFError is
  detected, the action exits the program
  successfully. Any other error exits with failure.
-}
catchEOF :: IO a -> IO a
catchEOF x = catchIOError x handler
  where
    handler err
      | isEOFError err = exitSuccess
      | otherwise      = exitFailure