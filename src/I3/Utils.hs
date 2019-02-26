module I3.Utils where

(...) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
f ... g = \a b -> f (g a b)
