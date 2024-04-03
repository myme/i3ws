module I3WS.Utils
  ( headMaybe,
    lastMaybe,
  )
where

-- | Returns the first element of a list or Nothing if the list is empty.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x' : _) = Just x'

-- | Returns the last element of a list or Nothing if the list is empty.
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)
