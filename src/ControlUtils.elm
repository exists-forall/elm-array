module ControlUtils
  ( untilJust
  , whileJust
  --, findIndex
  ) where

untilJust : (a -> Maybe b) -> (a -> a) -> a -> b
untilJust condition f init =
  case condition init of
    Just result -> result
    Nothing -> untilJust condition f (f init)

whileJust : (a -> Maybe (b, a)) -> a -> List b
whileJust f init =
  case f init of
    Just (result, new) -> result :: whileJust f new
    Nothing -> []

--findIndex : (Int -> Maybe Bool) -> Int -> Maybe Int
--findIndex condition i =
--  case condition of
--    Just True -> Just i
--    Just False -> findIndex condition (i + 1)
--    Nothing -> Nothing
