module STH.Lib.Archive (
  Archive(), emptyArchive,

  readStringArchive, writeStringArchive,

  getNames, getItem, getItems, putItem, putItems,
  replaceItem, replaceItems, deleteItem, deleteItems
) where

import Data.List (find, span, unfoldr)
import Control.Monad (foldM)



{---------}
{- Types -}
{---------}

type Name = String

data Item a = I
  { nameOf     :: Name
  , contentsOf :: a
  } deriving Show

data Archive a
  = A [Item a]
  deriving Show

emptyArchive :: Archive a
emptyArchive = A []



{-----------------------}
{- Reading and Writing -}
{-----------------------}

readArchiveBy ::
  ([String] -> Maybe a) -> [String] -> Maybe (Archive a)
readArchiveBy rd lns = do
  let
    xs = readArchive lns
    try (name, strs) = do
      x <- rd strs
      return (I { nameOf = name, contentsOf = x })
  fmap A $ mapM try xs

readArchive :: [String] -> [(Name, [String])]
readArchive = unfoldr rdFst
  where
    rdFst :: [String] -> Maybe ((Name, [String]), [String])
    rdFst []             = Nothing
    rdFst (('#':ln):lns) = do
      let
        isContent as = case as of
          '>':_     -> True
          otherwise -> False
        (xs,rest) = span isContent lns
      return ((ln, map tail xs), rest)
    rdFst (_:lns)        = rdFst lns

writeArchiveBy :: (a -> [String]) -> Archive a -> [String]
writeArchiveBy wr (A xs) = concatMap writeItem xs
  where
    writeItem item
      = ('#' : nameOf item) : map ('>':) (wr $ contentsOf item)

readStringArchive :: [String] -> Maybe (Archive [String])
readStringArchive = readArchiveBy Just

writeStringArchive :: Archive [String] -> [String]
writeStringArchive = writeArchiveBy id



getNames :: Archive a -> [Name]
getNames (A xs) = map nameOf xs

getItem :: Archive a -> Name -> Maybe a
getItem (A xs) str = do
  let hasName x = nameOf x == str
  item <- find hasName xs
  return (contentsOf item)

getItems :: Archive a -> [Name] -> Maybe [a]
getItems arch = mapM (getItem arch)



putItem :: Archive a -> (Name, a) -> Maybe (Archive a)
putItem (A []) (str, x) =
  Just $ A [I { nameOf = str, contentsOf = x }]
putItem (A (item:xs)) (str, x) = do
  if nameOf item == str
    then Nothing
    else do
      A zs <- putItem (A xs) (str, x)
      return $ A (item:zs)

putItems :: Archive a -> [(Name, a)] -> Maybe (Archive a)
putItems = foldM putItem



replaceItem :: Archive a -> (Name, a) -> Archive a
replaceItem (A []) (str, x) =
  A [I { nameOf = str, contentsOf = x }]
replaceItem (A (item:xs)) (str, x) =
  if nameOf item == str
    then
      A (item { contentsOf = x } : xs)
    else
      let A zs = replaceItem (A xs) (str, x)
      in A (item:zs)

replaceItems :: Archive a -> [(Name, a)] -> Archive a
replaceItems = foldl replaceItem



deleteItem :: Archive a -> Name -> Archive a
deleteItem (A []) _ = A []
deleteItem (A (item:xs)) str =
  if nameOf item == str
    then A xs
    else
      let A zs = deleteItem (A xs) str
      in A (item:zs)

deleteItems :: Archive a -> [Name] -> Archive a
deleteItems = foldl deleteItem
