module PortedCoreTests (tests, holeArray) where

import CustomArray as Array exposing (Array)
import Basics exposing (..)
import List
import List exposing ((::))
import Maybe exposing (..)

import Html exposing (Html)
import Html.Attributes as Attrs

{- this module is a very temporary quick-and-dirty way of running the unit tests
from core, without bringing in all of core's testing infrastructure.  Some tests
have been modified to fix typos and type errors. -}

mergeSplit : Int -> Array.Array a -> Array.Array a
mergeSplit n arr =
  let left = Array.slice 0 n arr
      right = Array.slice n (Array.length arr) arr
  in Array.append left right

holeArray : Array.Array Int
holeArray = List.foldl mergeSplit (Array.fromList [0..100]) [0..100]

assertEqual : a -> a -> Bool
assertEqual = (==)

mapArray: Array a -> Array a
mapArray array =
    Array.indexedMap (\i el -> 
        case (Array.get i array) of
            Just x -> x
            otherwise -> el
    ) array

assertArrayEqual : Array a -> Array a -> Bool
assertArrayEqual array1 array2 =
  if Array.length array1 == Array.length array2
    then
      [0..Array.length array1 - 1]
      |> List.map (\i -> Array.get i array1 == Array.get i array2)
      |> List.all identity
    else
      False

type alias Test = Html

suite : String -> List Test -> Test
suite name tests =
  Html.div [] (Html.text name :: tests)

test : String -> Bool -> Test
test name success =
  let color =
    if success
      then "green"
      else "red"
  in Html.div
    [ Attrs.style [("color", color)] ]
    [ Html.text name ]

tests : Test
tests =
  let creationTests = suite "Creation"
        [ test "empty" <| assertArrayEqual Array.empty (Array.fromList [])
        , test "initialize" <| assertArrayEqual (Array.initialize 4 identity) (Array.fromList [0,1,2,3])
        , test "initialize 2" <| assertArrayEqual (Array.initialize 4 (\n -> n*n)) (Array.fromList [0,1,4,9])
        , test "initialize 3" <| assertArrayEqual (Array.initialize 4 (always 0)) (Array.fromList [0,0,0,0])
        , test "initialize Empty" <| assertArrayEqual (Array.initialize 0 identity) Array.empty
        , test "initialize 4" <| assertArrayEqual (Array.initialize 2 (always 0)) (Array.fromList [0,0])
        , test "initialize negative" <| assertArrayEqual (Array.initialize -1 identity) Array.empty
        , test "repeat" <| assertArrayEqual (Array.repeat 5 40) (Array.fromList [40,40,40,40,40])
        , test "repeat 2" <| assertArrayEqual (Array.repeat 5 0) (Array.fromList [0,0,0,0,0])
        , test "repeat 3" <| assertArrayEqual (Array.repeat 3 "cat") (Array.fromList ["cat","cat","cat"])
        , test "fromList" <| assertArrayEqual (Array.fromList []) Array.empty
        ]
      basicsTests = suite "Basics"
        [ test "length" <| assertEqual 3 (Array.length (Array.fromList [1,2,3]))
        , test "length - Long" <| assertEqual 10000 (Array.length (Array.repeat 10000 0))
        , test "push" <| assertArrayEqual (Array.fromList [1,2,3]) (Array.push 3 (Array.fromList [1,2]))
        , test "append" <| assertEqual [42,42,81,81,81] (Array.toList (Array.append (Array.repeat 2 42) (Array.repeat 3 81)))
        , test "appendEmpty 1" <| assertEqual [1..33] (Array.toList (Array.append Array.empty (Array.fromList [1..33])))
        , test "appendEmpty 2" <| assertEqual [1..33] (Array.toList (Array.append (Array.fromList [1..33]) Array.empty))
        , test "appendSmall 1" <| assertEqual [1..33] (Array.toList (Array.append (Array.fromList [1..30]) (Array.fromList [31..33])))
        , test "appendSmall 2" <| assertEqual [1..33] (Array.toList (Array.append (Array.fromList [1..3]) (Array.fromList [4..33])))
        , test "appendAndSlice" <| assertEqual [0..100] (Array.toList holeArray)
        ]
      getAndSetTests = suite "Get and Set"
        [ test "get" <| assertEqual (Just 2) (Array.get 1 (Array.fromList [3,2,1]))
        , test "get 2" <| assertEqual Nothing (Array.get 5 (Array.fromList [3,2,1]))
        , test "get 3" <| assertEqual Nothing (Array.get -1 (Array.fromList [3,2,1]))
        , test "set" <| assertArrayEqual (Array.fromList [1,7,3]) (Array.set 1 7 (Array.fromList [1,2,3]))
        ]
      takingArraysApartTests = suite "Taking Arrays Apart"
        [ test "toList" <| assertEqual [3,5,8] (Array.toList (Array.fromList [3,5,8]))
        , test "toIndexedList" <| assertEqual [(0,"cat"), (1,"dog")] (Array.toIndexedList (Array.fromList ["cat","dog"]))
        , test "slice 1" <| assertArrayEqual (Array.fromList [0,1,2]) (Array.slice  0  3 (Array.fromList [0,1,2,3,4]))
        , test "slice 2" <| assertArrayEqual (Array.fromList [1,2,3]) (Array.slice  1  4 (Array.fromList [0,1,2,3,4]))
        , test "slice 3" <| assertArrayEqual (Array.fromList [1,2,3]) (Array.slice  1 -1 (Array.fromList [0,1,2,3,4]))
        , test "slice 4" <| assertArrayEqual (Array.fromList [2])     (Array.slice -3 -2 (Array.fromList [0,1,2,3,4]))
        , test "slice 5" <| assertEqual 63 (Array.length <| Array.slice 65 (65 + 63) <| Array.fromList [1..200])
        ]
      mappingAndFoldingTests = suite "Mapping and Folding"
        [ test "map" <| assertArrayEqual (Array.fromList [1,2,3]) (Array.map sqrt (Array.fromList [1,4,9]))
        , test "indexedMap 1" <| assertArrayEqual (Array.fromList [0,5,10]) (Array.indexedMap (*) (Array.fromList [5,5,5]))
        , test "indexedMap 2" <| assertEqual [0..99] (Array.toList (Array.indexedMap always (Array.repeat 100 0)))
        , test "large indexed map" <| assertEqual [0..32768 - 1] (Array.toList <| mapArray <| Array.initialize 32768 identity)
        , test "foldl 1" <| assertEqual [3,2,1] (Array.foldl (::) [] (Array.fromList [1,2,3]))
        , test "foldl 2" <| assertEqual 33 (Array.foldl (+) 0 (Array.repeat 33 1))
        , test "foldr 1" <| assertEqual 15 (Array.foldr (+) 0 (Array.repeat 3 5))
        , test "foldr 2" <| assertEqual [1,2,3] (Array.foldr (::) [] (Array.fromList [1,2,3]))
        , test "foldr 3" <| assertEqual 53 (Array.foldr (-) 54 (Array.fromList [10,11]))
        , test "filter" <| assertArrayEqual (Array.fromList [2,4,6]) (Array.filter (\x -> x % 2 == 0) (Array.fromList [1..6]))
        ]
  in
      suite "Array"
        [ creationTests, basicsTests, getAndSetTests
        , takingArraysApartTests, mappingAndFoldingTests
        ]
