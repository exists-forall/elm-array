module Main where

import Html exposing (Html)
import CustomArray as Array exposing (Array)

main =
  Array.fromList [0..50]
  |> Array.slice 15 17
  |> Array.visualize
