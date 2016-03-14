module Main where

import Html exposing (Html)
import CustomArray as Array exposing (Array)

--main =
--  [1, 2, 3]
--  |> Array.fromList
--  |> Array.pushMany [4, 5, 6, 7]
--  |> Array.visualize

--main =
--  --Array.initialize 100 identity
--  Array.initialize 256 (flip (%) 10)
--  |> Array.filter ((/=) 5)
--  |> Array.visualize
--  --|> Array.toIndexedList
--  --|> Array.mapAccumL (\x total -> (total + x, -x)) 0
--  --|> fst
--  --|> Array.visualize
--  --|> toString
--  --|> Html.text

main =
  --Array.initialize 25 identity
  --|> Array.dropLeftOf 1
  --|> Array.visualize
  Array.append (Array.fromList [1..10]) (Array.fromList [101..111])
  |> Array.visualize
