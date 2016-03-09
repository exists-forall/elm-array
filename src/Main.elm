module Main where

import Html exposing (Html)
import Html.Attributes as Attrs

import NaiveTable as Table exposing (Table)
import Assume exposing (assumeJust)
import CustomArray as Array exposing (Array)

import ListUtils

--main =
--  ["foo", "bar", "baz"]
--  |> Table.fromList
--  |> Table.get 1
--  |> toString
--  |> Html.text

--main =
--  Just "Hello"
--  |> assumeJust "This is a test"
--  |> toString
--  |> Html.text

test : Array String
test =
  Array.Node
    { height = 1
    , children =
      Table.fromList
        [ { startIndex = 0
          , endIndex = 3
          , array = Array.Leaf (Table.fromList ["a", "b", "c"])
          }
        , { startIndex = 3
          , endIndex = 5
          , array = Array.Leaf (Table.fromList ["d", "e"])
          }
        , { startIndex = 5
          , endIndex = 9
          , array = Array.Leaf (Table.fromList ["f", "g", "h", "i"])
          }
        , { startIndex = 9
          , endIndex = 11
          , array = Array.Leaf (Table.fromList ["j", "k"])
          }
        ]
    }

visualize : Array a -> Html
visualize array =
  case array of
    Array.Node node ->
      Html.div [Attrs.style [("margin", "20px")]]
        [ Html.span [Attrs.style [("font-weight", "bold")]] [Html.text "Node "]
        , Html.text ("(" ++ toString node.height ++ ")")
        , Html.div [Attrs.style [("border-left", "solid")]]
          (List.map (visualize << .array) (Table.toList node.children))
        ]

    Array.Leaf leaf ->
      Html.div [Attrs.style [("margin", "20px")]] -- (List.map (Html.text << (\s -> " " ++ s ++ " ") << toString) (Table.toList leaf))
        [ Html.span [Attrs.style [("font-weight", "bold")]] [Html.text "Leaf "]
        , Html.text (toString (Table.toList leaf))
        ]

--main =
--  test
--  |> Array.map ((++) "Hello! ")
--  |> visualize
--  --|> toString
--  --|> Html.text

--main =
--  Array.chunk [1, 2, 3, 4, 5, 6, 7, 8, 9]
--  |> toString
--  |> Html.text

--main = visualize test

--main =
--  [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]
--  |> Array.fromList
--  --|> Array.set 42 100000
--  |> Array.pushMany [-1, -2, -3, -4, -5, -6, -7, -8, -1, -2, -3, -4, -5, -6, -7, -8, -1, -2, -3, -4, -5, -6, -7, -8, -1, -2, -3, -4, -5, -6, -7, -8]
--  |> visualize

--main =
--  ["foo", "bar", "baz"]
--  |> Array.fromList
--  |> Array.pushMany ["FOO", "BAR", "BAZ"]
--  |> Array.pushMany ["biz", "buzz", "quux"]
--  |> Array.pushMany ["FOO", "BAR", "BAZ"]
--  |> Array.pushMany ["biz", "buzz", "quux"] 
--  |> Array.pushMany ["FOO", "BAR", "BAZ"]
--  |> Array.pushMany ["biz", "buzz", "quux"]
--  |> visualize

test1 =
    { height = 1
    , children = Table.fromList
      [ { startIndex = 0, endIndex = 4, array = Array.Leaf (Table.fromList ["a", "b", "c", "d"]) }
      , { startIndex = 4, endIndex = 6, array = Array.Leaf (Table.fromList ["e", "f"]) }
      ]
    }

test2 =
    { height = 1
    , children = Table.fromList
      [ { startIndex = 0, endIndex = 3, array = Array.Leaf (Table.fromList ["g", "h", "i"]) }
      , { startIndex = 3, endIndex = 5, array = Array.Leaf (Table.fromList ["j", "k"]) }
      , { startIndex = 5, endIndex = 7, array = Array.Leaf (Table.fromList ["l", "m"]) }
      , { startIndex = 7, endIndex = 10, array = Array.Leaf (Table.fromList ["n", "o", "p"]) }
      ]
    }

--main =
--  Array.append' (Array.fromList ["foo", "bar", "baz", "foo", "bar", "baz"], Array.fromList ["alpha", "beta", "gamma", "alpha", "beta", "gamma"])
--  |> snd
--  |> visualize

--main =
--  Array.analyzeSearchError (test1, test2)
--  |> toString
--  |> Html.text

range : Int -> Int -> List Int
range i j =
  if i < j
    then i :: range (i + 1) j
    else []

--main =
--  --(Array.Node test1, Array.Node test2)
--  (Array.fromList (range 0 101), Array.fromList (range 101 200))
--  |> Array.append'
--  |> \(l, r) -> Html.span [] [ Html.div [] [ visualize l ], Html.div [] [ Html.text "-----" ], Html.div [] [ visualize r] ]

  --|> always (Html.text "Hello")
  --|> fst
  --|> visualize

--main =
--  ['a', 'b', 'c']
--  |> ListUtils.splitAt 3
--  |> toString
--  |> Html.text

--main =
--  (Array.fromList (range 0 101), Array.fromList (range 101 200))
--  |> Array.append
--  |> List.map (flip Array.get)
--  --|> visualize

main =
  let
    a1 = Array.fromList (range 0 101)
    a2 = Array.fromList (range 101 201)
    a3 = Array.append a1 a2
    a4 = Array.append a3 a1
  in
    --List.map (\i -> Array.get i a4 |> Maybe.map ((==) i)) (range 0 201)
    --List.map (flip Array.get a4 >> assumeJust "") (range 0 302)
    --|> toString
    --|> Html.text
    a4
    |> visualize
