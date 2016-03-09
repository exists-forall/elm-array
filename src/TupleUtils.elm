module TupleUtils (mapBoth) where

mapBoth : (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) =
  (f x, f y)
