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

  arch <- do
    fileExists <- doesFileExist file
    case fileExists of
      True -> do
        x <- fmap getLines $ readFile file
        case readStringArchive x of
          Just a  -> return a
          Nothing -> corErr >> exitFailure
      False -> return emptyArchive

  let
    getItem str = case str of
      "-" -> do
        lns <- fmap getLines $ getContents
        return ("-", lns)
      otherwise -> do
        lns <- fmap getLines $ readFile str
        return (str, lns)

  case act of
    List -> putStrLns $ getNames arch

    Add -> do
      items <- mapM getItem names
      case putItems arch items of
        Nothing -> existsErr >> exitFailure
        Just x  -> putFileLns file $ writeStringArchive x

    Get -> do
      case getItems arch names of
        Nothing -> dneErr >> exitFailure
        Just xs -> mapM_ putStrLns xs

    Remove -> do
      putFileLns file
        $ writeStringArchive
        $ deleteItems arch names

    Replace -> do
      items <- mapM getItem names
      putFileLns file
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

corErr :: IO ()
corErr = reportErrorMsgs
  [ "corrupt archive." ]

existsErr :: IO ()
existsErr = reportErrorMsgs
  [ "name exists in archive." ]

dneErr :: IO ()
dneErr = reportErrorMsgs
  [ "name does not exist in archive." ]
