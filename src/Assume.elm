module Assume (assumeJust) where

assumeJust : String -> Maybe a -> a
assumeJust justification maybeX =
  case maybeX of
    Just x -> x
    Nothing -> Debug.crash ("Invalid assumption: " ++ justification)
