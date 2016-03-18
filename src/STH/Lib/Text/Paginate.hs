{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module STH.Lib.Text.Paginate (
  PaginateOpts(..), paginateLines,
  paginateCCLines, tableOfContents
) where

import STH.Lib.Text.Format.ASACarriageControl (CCLine(..))
import Data.List (unfoldr, inits, splitAt)
import STH.Lib.List (count)


{------------------}
{- abstract lines -}
{------------------}

--Line.S
class Line t where
  fromString :: String -> t

  blankLine :: t
  blankLine = fromString ""

instance Line String where
  fromString x = x

instance Line CCLine where
  fromString x = CCLine [x]
--Line.E


{----------------------}
{- pagination options -}
{----------------------}

--PaginateOpts.S
data PaginateOpts = PO
  { linesPerPage :: Int
  , lineLength   :: Int
  } deriving (Show)


pageCount :: (Line t) => PaginateOpts -> [t] -> Int
pageCount opts xs = if r == 0 then q else q+1
  where
    slpp = (linesPerPage opts) - 2
    (q,r) = ((count xs) `div` slpp, (count xs) `rem` slpp)

startPages :: (Line t) => PaginateOpts -> [[t]] -> [Int]
startPages opts lnss
  = map (\ks -> 1 + sum ks)
      $ inits
      $ map (pageCount opts) lnss

totalPages :: (Line t) => PaginateOpts -> [[t]] -> Int
totalPages opts lnss = sum $ map (pageCount opts) lnss
--PaginateOpts.E


{---------------}
{- page header -}
{---------------}

--Header.S
data Header = Header
  { title      :: String
  , pageNumber :: Int
  , pageTotal  :: Int
  } deriving (Show)


class RenderHeader t where
  renderHeader :: (Line t) => PaginateOpts -> Header -> [t]


instance RenderHeader String where
  renderHeader opts h = [fn ++ (replicate (ll - nfn - npg) ' ') ++ pg, ""]
    where
      pg  = show (pageNumber h) ++ "/" ++ show (pageTotal h)
      npg = count pg
      ll  = lineLength opts
      fn  = if (count $ title h) + npg + 1 > ll
        then abbr
        else title h
      abbr = "..." ++ (reverse $ take (ll - npg - 4) $ reverse $ title h)
      nfn = count fn

instance RenderHeader CCLine where
  renderHeader opts h = map (\x -> CCLine [x]) $ renderHeader opts h
--Header.E


{--------------}
{- Pagination -}
{--------------}

--Pagination.S
splitPages :: (Line t) => PaginateOpts -> String -> [t] -> [(Header, [t])]
splitPages opts name = unfoldr firstPage
  where
    slpp = (linesPerPage opts) - 2

    firstPage :: [a] -> Maybe ((Header,[a]),[a])
    firstPage [] = Nothing
    firstPage ys = do
      let
        (zs,rest) = splitAt slpp ys
        hdr = Header
          { title      = name
          , pageNumber = 0
          , pageTotal  = 0
          }
      return ((hdr, zs), rest)


numberPagesFromOf :: (Line t)
  => Int -> Int -> [(Header, [t])] -> [(Header, [t])]
numberPagesFromOf m n xs = zipWith fix xs [m..]
  where
    fix (h,y) k = (h {pageNumber = k, pageTotal = n}, y)


renderPage :: (Line t, RenderHeader t)
  => PaginateOpts -> (Header, [t]) -> [t]
renderPage opts (hdr,lns)
  = take k ((renderHeader opts hdr) ++ lns ++ repeat blankLine)
  where k = linesPerPage opts


paginateOfFrom :: (Line t, RenderHeader t)
  => PaginateOpts -> Int -> Int -> (String, [t]) -> [t]
paginateOfFrom opts n m (name, lns)
  = concatMap (renderPage opts)
      $ numberPagesFromOf m n
      $ splitPages opts name lns


paginateDocs :: (Line t, RenderHeader t)
  => PaginateOpts -> [(String, [t])] -> [t]
paginateDocs opts docs
  = concat $ zipWith (paginateOfFrom opts tot) starts docs
  where
    starts = startPages opts $ map snd docs
    tot    = totalPages opts $ map snd docs
--Pagination.E


--Spec.S
paginateLines :: PaginateOpts -> [(String, [String])] -> [String]
paginateLines = paginateDocs

paginateCCLines :: PaginateOpts -> [(String, [CCLine])] -> [CCLine]
paginateCCLines = paginateDocs
--Spec.E


{---------------------}
{- table of contents -}
{---------------------}

--TOC.S
tableOfContents :: (Line t, RenderHeader t)
  => PaginateOpts -> [(String, [t])] -> [t]
tableOfContents opts docs = concat $ pad $
  (fromString "Contents") : blankLine : tocLines
  where
    ks = startPages opts $ map snd docs

    tocLines = zipWith tocLine (map fst docs) ks

    tocLine name pg = fromString $ padNum pg ++ " " ++ abbr name

    ll  = lineLength opts
    abbr str = if (count str) + 5 > ll
      then "..." ++ (reverse $ take (ll - 9) $ reverse $ str)
      else str 

    padNum k = reverse $ take 5 $ (reverse $ show k) ++ repeat ' '

    pad = unfoldr padFirst

    padFirst [] = Nothing
    padFirst xs = Just (take (linesPerPage opts) (ys ++ repeat blankLine), rest)
      where (ys, rest) = splitAt (linesPerPage opts) xs
--TOC.E
