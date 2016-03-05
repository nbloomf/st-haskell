module STH.Lib.Maybe (
  filterMaybe
) where


{-|
  Like 'filter' for lists, 'filterMaybe'
  tests whether a 'Maybe a' satisfies a
  given predicate and collapses to 'Nothing'
  if not.
-}
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p x = do
  y <- x
  case p y of
    True  -> Just y
    False -> Nothing
