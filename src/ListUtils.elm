module ListUtils
  ( mapAccumL
  , splitAt
  ) where

mapAccumL : (a -> b -> (b, c)) -> b -> List a -> (b, List c)
mapAccumL f init list =
  case list of
    [] -> (init, [])
    (x :: xs) ->
      let
        (xAccum, xMapped) = f x init
        (finalAccum, xsMapped) = mapAccumL f xAccum xs
      in
        (finalAccum, xMapped :: xsMapped)

splitAt : Int -> List a -> (List a, List a)
splitAt i list =
  if i <= 0
    then ([], list)
    else case list of
      [] -> ([], [])
      (x :: xs) ->
        let (xsTaken, xsRest) = splitAt (i - 1) xs
        in (x :: xsTaken, xsRest)
