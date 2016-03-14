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
  --Array.append (Array.fromList [0..24]) (Array.fromList [25..49])

  Array.initialize 25 identity
  |> Array.dropRightOf 18
  |> Array.visualize
  --|> Array.visualize
  
  --|> Array.dropLeftOf 2
  --|> Array.visualize

  --|> Array.get 17
  --|> toString
  --|> Html.text
  
  --Array.append (Array.fromList [1..10]) (Array.fromList [101..111])
  --|> Array.visualize
