-- archive: bundle text files

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import STH.Lib
  (reportErrorMsgs, getLines, readStringArchive,
   putStrLns, getNames, getItems, putItems,
   deleteItems, replaceItems, writeStringArchive,
   putFileLns, emptyArchive)

data Action = List | Add | Get | Remove | Replace

main :: IO ()
main = do
  args <- getArgs

  -- process arguments
  (file, act, names) <- case args of
    [x,"--list"]       -> return (x, List, [])
    (x:"--add":xs)     -> return (x, Add, xs)
    (x:"--get":xs)     -> return (x, Get, xs)
    (x:"--remove":xs)  -> return (x, Remove, xs)
    (x:"--replace":xs) -> return (x, Replace, xs)
    otherwise          -> argErr >> exitFailure

  -- read the archive
  arch <- do
    fileExists <- doesFileExist file
    case fileExists of
      True -> do
        x <- fmap getLines $ readFile file
        return $ readStringArchive x
      False -> return emptyArchive

  -- how we process the items
  let
    getItem str = case str of
      "-" -> do
        lns <- fmap getLines $ getContents
        return ("-", lns)
      otherwise -> do
        lns <- fmap getLines $ readFile str
        return (str, lns)

  -- do the thing
  case act of
    List -> putStrLns $ getNames arch

    Add -> do
      items <- mapM getItem names
      case putItems arch items of
        Just x  -> putFileLns file $ writeStringArchive x
        Nothing -> do
          reportErrorMsgs
            [ "name exists in archive." ]
            >> exitFailure

    Get -> do
      case getItems arch names of
        Just xs -> mapM_ putStrLns xs
        Nothing -> do
          reportErrorMsgs
            [ "name does not exist in archive." ]
            >> exitFailure

    Remove -> do
      putStrLns
        $ writeStringArchive
        $ deleteItems arch names

    Replace -> do
      items <- mapM getItem names
      putStrLns
        $ writeStringArchive
        $ replaceItems arch items

  exitSuccess



argErr :: IO ()
argErr = reportErrorMsgs
  [ "usage:"
  , "  archive ARCH --list"
  , "  archive ARCH --add FILE ..."
  , "  archive ARCH --get FILE ..."
  , "  archive ARCH --remove FILE ..."
  , "  archive ARCH --replace FILE ..."
  ]
