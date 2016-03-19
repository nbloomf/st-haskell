module STH.Lib.ZipperList (
  ZipperList(), zlFocus, makeZipperList, zlNext, zlPrev
) where


--ZipperList.S
data ZipperList a = ZL a [a] [a]

zlFocus :: ZipperList a -> a
zlFocus (ZL x _ _) = x

makeZipperList :: [a] -> Maybe (ZipperList a)
makeZipperList []     = Nothing
makeZipperList (x:xs) = Just $ ZL x [] xs

zlNext :: ZipperList a -> Maybe (ZipperList a)
zlNext (ZL _ _  [])     = Nothing
zlNext (ZL x ys (z:zs)) = Just $ ZL z (x:ys) zs

zlPrev :: ZipperList a -> Maybe (ZipperList a)
zlPrev (ZL _ []     _)  = Nothing
zlPrev (ZL x (y:ys) zs) = Just $ ZL y ys (x:zs)
--ZipperList.E
