-- pslineprint: print stdin to postscript

module Main where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import STH.Lib
  (getLines, Geom(..), defaultGeom, runLPJob,
   lpPrintLns, lpPrintCCLns, readDecimalNat,
   reportErrorMsgs, readCCLines)


main :: IO ()
main = do
  args <- getArgs

  let
    argErr  = reportErrorMsgs [usageInfo "options" options]
    corrErr = reportErrorMsgs ["corrupt input"]

  -- read options
  flags <- case getOpt Permute options args of
    (opts, [], []) -> return $ foldl (>>=) (Just defaultFlags) opts
    otherwise      -> argErr >> exitFailure

  -- process options
  (geom, mode) <- case flags of
    Nothing -> argErr >> exitFailure
    Just fs -> do
      let
        g = defaultGeom
          { fontSize = fFontSize fs
          , lineSkip = fLineSkip fs
          , vMargin  = fVMargin fs
          , hMargin  = fHMargin fs
          }

      return (g, fMode fs)

  stdin <- getContents

  case mode of
    Lines -> runLPJob geom $ lpPrintLns $ getLines stdin

    ASACC -> case readCCLines stdin of
      Nothing -> corrErr >> exitFailure
      Just xs -> runLPJob geom $ lpPrintCCLns xs

  exitSuccess


{- Options -}

data Mode = Lines | ASACC

data Flags = Flags
  { fFontSize :: Int
  , fLineSkip :: Int
  , fVMargin  :: Int
  , fHMargin  :: Int
  , fMode     :: Mode
  }


defaultFlags :: Flags
defaultFlags = Flags
  { fFontSize = 12
  , fLineSkip = 2
  , fVMargin  = 32
  , fHMargin  = 28
  , fMode     = Lines
  }


options :: [OptDescr (Flags -> Maybe Flags)]
options =
  [ Option [] ["font-size"]
      (ReqArg readFontSize "INT")
      "font size in points"

  , Option [] ["line-skip"]
      (ReqArg readLineSkip "INT")
      "line spacing in points"

  , Option [] ["vmargin"]
      (ReqArg readVMargin "INT")
      "vertical page margins in points"

  , Option [] ["hmargin"]
      (ReqArg readHMargin "INT")
      "left page margin in points"

  , Option [] ["asacc"]
      (NoArg (\opts -> Just $ opts { fMode = ASACC }))
      "interpret basic ASA carriage control codes"
  ]
  where
    readFontSize str opts = do
      k <- readDecimalNat str
      return $ opts { fFontSize = k }

    readLineSkip str opts = do
      k <- readDecimalNat str
      return $ opts { fLineSkip = k }

    readVMargin str opts = do
      k <- readDecimalNat str
      return $ opts { fVMargin = k }

    readHMargin str opts = do
      k <- readDecimalNat str
      return $ opts { fHMargin = k }
