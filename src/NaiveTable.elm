module NaiveTable
  ( Table

  , fromList
  , takeFromList
  , toList
  , empty
  , initialize
  , redistributeMany
  , append
  
  , get
  --, concatSlice
  , set

  , push
  , pushMany
  , pushManyFrom
  , pushTake

  --, redistribute
  
  , length

  , map
  , foldl
  , foldr
  , mapAccumL
  , findIndex
  ) where

import ListUtils
import TupleUtils
import ControlUtils

type Table a = Table (List a)

fromList : List a -> Table a
fromList = Table

initialize' : (Int -> a) -> Int -> List a
initialize' f count =
  let
    recurse i =
      if i < count
        then f i :: recurse (i + 1)
        else []
  in
    recurse 0 |> Debug.log "Initialized"

initialize : (Int -> a) -> Int -> Table a
initialize f count =
  initialize' f count |> fromList

takeFromList : Int -> List a -> (Table a, List a)
takeFromList i list =
  {- Quite inefficent, but who cares?  This entire module will be replaced with
  a constant-time javascript implementation soon anyway -}
  (fromList (List.take i list), List.drop i list)

toList : Table a -> List a
toList (Table xs) = xs

empty : Table a
empty = Table []

get' : Int -> List a -> Maybe a
get' i list =
  case list of
    [] -> Nothing
    (x :: xs) ->
      case compare i 0 of
        LT -> Nothing
        EQ -> Just x
        GT -> get' (i - 1) xs

get : Int -> Table a -> Maybe a
get i =
  toList >> get' i

set' : Int -> a -> List a -> List a
set' i newItem list =
  case list of
    [] -> list
    (x :: xs) ->
      case compare i 0 of
        LT -> list
        EQ -> newItem :: xs
        GT -> x :: set' (i - 1) newItem xs

set : Int -> a -> Table a -> Table a
set i newItem =
  toList >> set' i newItem >> fromList

--push' : a -> List a -> List a
--push' x =
--  flip (++) [x]

--push : a -> Table a -> Table a
--push x =
--  toList >> push' x >> fromList

push' : a -> List a -> List a
push' x =
  flip (++) [x]

push : a -> Table a -> Table a
push x =
  toList >> push' x >> fromList

pushMany : List a -> Table a -> Table a
pushMany list =
  toList >> flip (++) list >> fromList

pushManyFrom : Int -> List a -> Table a -> Table a
pushManyFrom i list =
  {- very inefficent -}
  toList >> List.take i >> flip (++) list >> fromList

pushTake : Int -> List a -> Table a -> (Table a, List a)
pushTake i list tab =
  {- once again, inefficient, but it doesn't matter -}
  (fromList (toList tab ++ List.take i list), List.drop i list)

length : Table a -> Int
length =
  toList >> List.length

map : (a -> b) -> Table a -> Table b
map f =
  toList >> List.map f >> fromList

foldl : (a -> b -> b) -> b -> Table a -> b
foldl f init =
  toList >> List.foldl f init

foldr : (a -> b -> b) -> b -> Table a -> b
foldr f init =
  toList >> List.foldr f init

--mapAccumL' : (a -> b -> (b, c)) -> b -> List a -> (b, List c)
--mapAccumL' f init list =
--  case list of
--    [] -> (init, [])
--    (x :: xs) ->
--      let
--        (xAccum, xMapped) = f x init
--        (finalAccum, xsMapped) = mapAccumL' f xAccum xs
--      in
--        (finalAccum, xMapped :: xsMapped)

mapAccumL : (a -> b -> (b, c)) -> b -> Table a -> (b, Table c)
mapAccumL f init (Table list) =
  let (accum, mappedList) = ListUtils.mapAccumL f init list
  in (accum, Table mappedList)

findIndex' : (a -> Bool) -> List a -> Maybe Int
findIndex' condition root =
  let
    recurse i list =
      case list of
        [] -> Nothing
        (x :: xs) ->
          if condition x
            then Just i
            else recurse (i + 1) xs
  in
    recurse 0 root

findIndex : (a -> Bool) -> Table a -> Maybe Int
findIndex condition =
  toList >> findIndex' condition

--redistribute : Int -> (Table a, Table a) -> (Table a, Table a)
--redistribute leftCount (Table list1) (Table list2) =
--  list1 ++ list2
--  |> ListUtils.splitAt leftCount
--  |> TupleUtils.mapBoth fromList

splitAndRequireNonEmpty : Int -> List a -> Maybe (List a, List a)
splitAndRequireNonEmpty size list =
  case ListUtils.splitAt size list of
    ([], _) -> Nothing
    halves -> Just halves |> Debug.log "halves"

redistributeMany : Int -> Table (Table a) -> Table (Table a)
redistributeMany size tables =
  tables
  |> toList
  |> List.map toList
  |> List.concat
  |> ControlUtils.whileJust (splitAndRequireNonEmpty size)
  |> List.map fromList
  |> fromList
  |> Debug.log "Redistributed"

append : Table a -> Table a -> Table a
append (Table list1) (Table list2) =
  fromList (list1 ++ list2)

--concatSlice : Int -> Int -> Table a -> Table a -> Table a
--concatSlice lowerInclusive upperExclusive (Table list1) (Table list2) =
--  list1 ++ list2
--  |> List.drop lowerInclusive
--  |> List.take (upperExclusive - lowerInclusive)
--  |> fromList
