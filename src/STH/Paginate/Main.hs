-- paginate: format lines with page numbers and headers

module Main where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import STH.Lib
  (getLines, paginateLines, PaginateOpts(..),
   putStrLns, readDecimalNat, reportErrorMsgs, readCCLines,
   paginateCCLines, renderCCLine, tableOfContents)


main :: IO ()
main = do
  args <- getArgs

  let
    argErr  = reportErrorMsgs [usageInfo "options" options]
    corrErr = reportErrorMsgs ["corrupt asacc input"]


  -- read options
  (flags, filenames) <- case getOpt Permute options args of
    (opts, rest, []) -> case foldl (>>=) (Just defaultFlags) opts of
      Nothing -> argErr >> exitFailure
      Just fs -> return (fs, rest)
    otherwise -> argErr >> exitFailure


  -- process options
  let
    pageOpts = PO
      { linesPerPage = fLinesPerPage flags
      , lineLength   = fLineLength flags
      }


  -- paginate files
  case fMode flags of
    Lines -> do
      let
        readLines name = case name of
          "-" -> do
            lns <- fmap getLines getContents
            return ("-", lns)
          otherwise -> do
            lns <- fmap getLines $ readFile name
            return (name, lns)

      docs <- sequence $ map readLines filenames

      if fPrintTOC flags == False
        then return ()
        else putStrLns $ tableOfContents pageOpts docs

      putStrLns $ paginateLines pageOpts docs

    ASACC -> do
      let
        readLines name = case name of
          "-" -> do
            lns <- fmap readCCLines getContents
            case lns of
              Nothing -> corrErr >> exitFailure
              Just xs -> return ("-", xs)
          otherwise -> do
            lns <- fmap readCCLines $ readFile name
            case lns of
              Nothing -> corrErr >> exitFailure
              Just xs -> return (name, xs)

      docs <- sequence $ map readLines filenames

      if fPrintTOC flags == False
        then return ()
        else putStrLns $ map renderCCLine $ tableOfContents pageOpts docs

      putStrLns $ map renderCCLine $ paginateCCLines pageOpts docs


  exitSuccess



data Mode = Lines | ASACC

data Flags = Flags
  { fLinesPerPage :: Int
  , fLineLength   :: Int
  , fPrintTOC     :: Bool
  , fMode         :: Mode
  }

defaultFlags :: Flags
defaultFlags = Flags
  { fLinesPerPage = 52
  , fLineLength   = 75
  , fPrintTOC     = False
  , fMode         = Lines
  }


options :: [OptDescr (Flags -> Maybe Flags)]
options =
  [ Option [] ["lines-per-page"]
      (ReqArg readLinesPerPage "INT")
      "number of lines per page (including header)"

  , Option [] ["line-length"]
      (ReqArg readLineLength "INT")
      "length of header lines"

  , Option [] ["toc"]
      (NoArg (\opts -> Just $ opts { fPrintTOC = True }))
      "print table of contents page"

  , Option [] ["asacc"]
      (NoArg (\opts -> Just $ opts { fMode = ASACC }))
      "interpret basic ASA carriage control codes"
  ]
  where
    readLinesPerPage str opts = do
      k <- readDecimalNat str
      return $ opts { fLinesPerPage = k }

    readLineLength str opts = do
      k <- readDecimalNat str
      return $ opts { fLineLength = k }
