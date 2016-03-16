module STH.Lib.Monad.LinePrinter (
  Geom(..), LinePrinter(), runLPJob, lpPrintLns, defaultGeom, lpPrintCCLns
) where

import Data.List (unfoldr)
import STH.Lib.Text.Format.PostScript (unicodeToPS)
import STH.Lib.Text.Format.ASACarriageControl (CCLine(), fromCCLine)


{-----------------}
{- Page Geometry -}
{-----------------}

--PageGeom.S
data Geom = Geom
  { fontSize   :: Int
  , lineSkip   :: Int
  , vMargin    :: Int
  , hMargin    :: Int
  , pageHeight :: Int
  , pageWidth  :: Int
  } deriving (Show)

-- letter size, 12 pt type
defaultGeom :: Geom
defaultGeom = Geom
  { fontSize   = 12
  , lineSkip   = 2
  , vMargin    = 28
  , hMargin    = 32
  , pageHeight = 792
  , pageWidth  = 612
  }
--PageGeom.E

--PageGeomComp.S
numLinesPerPage :: Geom -> Int
numLinesPerPage geom = floor ((pH - (2*vM)) / (fS + lS))
  where
    pH = fromIntegral $ pageHeight geom
    vM = fromIntegral $ vMargin geom
    fS = fromIntegral $ fontSize geom
    lS = fromIntegral $ lineSkip geom

-- lower left corner of line number k
lineStartPos :: Geom -> Int -> (Int,Int)
lineStartPos geom k = (hM, pH - vM - k*(fS + lS))
  where
    hM = hMargin geom
    vM = vMargin geom
    pH = pageHeight geom
    fS = fontSize geom
    lS = lineSkip geom
--PageGeomComp.E



{----------------------}
{- Line Printer State -}
{----------------------}

--LPState.S
data LPState = LPState
  { pageSettings :: Geom
  , currentLine  :: Int
  , currentPage  :: Int
  , pageIsDirty  :: Bool
  }


makeLPState :: Geom -> LPState
makeLPState geom = LPState
  { pageSettings = geom
  , currentLine  = 1
  , currentPage  = 1
  , pageIsDirty  = False
  }
--LPState.E



{----------------------}
{- Line Printer Monad -}
{----------------------}

--LinePrinter.S
newtype LinePrinter t = LP
  { runLP :: LPState -> IO (t, LPState) }

runLPJob :: Geom -> LinePrinter t -> IO t
runLPJob geom pr = do
  (x,_) <- runLP pr (makeLPState geom)
  return x

instance Monad LinePrinter where
  return x = LP (\st -> return (x, st))

  x >>= f = LP foo
    where
      foo st1 = do
        (y,st2) <- runLP x st1
        runLP (f y) st2
--LinePrinter.E


{--------------}
{- Primitives -}
{--------------}

--LPCommand.S
lpInitialize :: LinePrinter ()
lpInitialize = LP init
  where
    init st = do
      putStrLn "%!PS"
      putStrLn "/FreeMono findfont"
      let k = fontSize $ pageSettings st
      putStrLn $ show k ++ " scalefont"
      putStrLn "setfont\n"
      return ((),st)


lpShutDown :: LinePrinter ()
lpShutDown = LP sd
  where
    sd st = case pageIsDirty st of 
      True -> do
        putStrLn "showpage"
        return ((),st)
      False -> return ((),st)
--LPCommand.E


--lpPutStr.S
lpPutStr :: String -> LinePrinter ()
lpPutStr str = LP write
  where
    write st = do
      case pageIsDirty st of
        True  -> return ()
        False -> do
          let pg = currentPage st
          putStrLn $ "%%Page: " ++ show pg
      let (x,y) = lineStartPos (pageSettings st) (currentLine st)
      putStrLn $ show x ++ " " ++ show y ++ " moveto"
      putStr $ unicodeToPS str
      return ((), st {pageIsDirty = True})
--lpPutStr.E


--lpLineFeed.S
lpLineFeed :: LinePrinter ()
lpLineFeed = LP lf
  where
    lf st = do
      let
        (kOld,mOld) = (currentLine st, currentPage st)
        lpp = numLinesPerPage (pageSettings st)

      if kOld + 1 > lpp
        then do
          putStrLn "showpage\n"
          return ((), st {currentLine = 1, currentPage = mOld+1})
        else do
          return ((), st {currentLine = kOld+1, currentPage = mOld})


lpPutStrLn :: String -> LinePrinter ()
lpPutStrLn str = do
  lpPutStr str
  lpLineFeed
--lpLineFeed.E


--lpPrintLns.S
lpPrintLns :: [String] -> LinePrinter ()
lpPrintLns lns = do
  lpInitialize
  mapM_ lpPutStrLn lns
  lpShutDown
--lpPrintLns.E


lpPutCCStrLn :: CCLine -> LinePrinter ()
lpPutCCStrLn x = do
  mapM_ lpPutStr (fromCCLine x)
  lpLineFeed

lpPrintCCLns :: [CCLine] -> LinePrinter ()
lpPrintCCLns lns = do
  lpInitialize
  mapM_ lpPutCCStrLn lns
  lpShutDown

lpPageFeed :: LinePrinter ()
lpPageFeed = LP pf
  where
    pf st = do
      let mOld = currentPage st
      if pageIsDirty st
        then putStrLn "showpage\n"
        else return ()
      return ((), st { currentLine = 1, currentPage = mOld+1, pageIsDirty = False })


lpBlankPage :: LinePrinter ()
lpBlankPage = LP bp
  where
    bp st = do
      let mOld = currentPage st
      if pageIsDirty st
        then putStrLn "showpage\n"
        else return ()
      putStrLn $ "%%Page: " ++ show (mOld + 1)
      putStrLn "showpage\n"
      return ((), st { currentLine = 1, currentPage = mOld+2, pageIsDirty = False })
