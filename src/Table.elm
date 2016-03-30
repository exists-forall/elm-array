module Table
  ( Table

  , fromList
  , takeFromList
  , toList
  , empty
  , initialize
  , redistributeMany
  , append
  
  , get
  , set

  , push
  , pushMany
  , pushManyFrom

  , length

  , map
  , foldl
  , foldr
  , mapAccumL
  , findIndex
  ) where

import Native.Table

type Table a = Table

fromList : List a -> Table a
fromList = Native.Table.fromList

takeFromList : Int -> List a -> (Table a, List a)
takeFromList = Native.Table.takeFromList

toList : Table a -> List a
toList = Native.Table.toList

empty : Table a
empty = Native.Table.empty

initialize : (Int -> a) -> Int -> Table a
initialize = Native.Table.initialize

redistributeMany : Int -> Table (Table a) -> Table (Table a)
redistributeMany = Native.Table.redistributeMany

append : Table a -> Table a -> Table a
append = Native.Table.append

get : Int -> Table a -> Maybe a
get = Native.Table.get

set : Int -> a -> Table a -> Table a
set = Native.Table.set

push : a -> Table a -> Table a
push = Native.Table.push

pushMany : List a -> Table a -> Table a
pushMany = Native.Table.pushMany

pushManyFrom : Int -> List a -> Table a -> Table a
pushManyFrom = Native.Table.pushManyFrom

length : Table a -> Int
length = Native.Table.length

map : (a -> b) -> Table a -> Table b
map = Native.Table.map

foldl : (a -> b -> b) -> b -> Table a -> b
foldl = Native.Table.foldl

foldr : (a -> b -> b) -> b -> Table a -> b
foldr = Native.Table.foldr

mapAccumL : (a -> b -> (b, c)) -> b -> Table a -> (b, Table c)
mapAccumL = Native.Table.mapAccumL

findIndex : (a -> Bool) -> Table a -> Maybe Int
findIndex = Native.Table.findIndex
