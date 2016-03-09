module CustomArray where

import Bitwise

import NaiveTable as Table exposing (Table)
import Assume exposing (assumeJust)
import ListUtils
import TupleUtils
import ControlUtils

type alias NodeData a =
  { height : Int
  , children : Table (Child a)
  }
{- informal invariants (in Elm pseudocode):

- `node.height == 1 + max (map height node.children)`

- `(get i node.children).startIndex == sum (length (get j node.children)) for 0 <= j < i`

- `(get i node.children).endIndex == sum (length (get j node.children)) for 0 <= j <= i`

-- `minimumBranching <= length node.children <= maximumBranching` (NOT SATISFIED)

- `length node.children > 0`
-}

type alias Child a =
  { startIndex : Int -- inclusive
  , endIndex : Int -- exclusive
  , array : Array a 
  }
{- This stores redundant start AND end index information for each child,
whereas the native implementation stores only the end index.  This method is
more less memory efficient, but has the advance of involving less
index arithmetic when getting start and end indices (no need to look ahead/
behind the child in question, with a special case for the first child).  For now, I am judging the gains in readability and simplicity to outweigh the
small performance penalty. -}


type Array a
  = Node (NodeData a)
  | Leaf (Table a)

--minimumBranching : Int
--minimumBranching = 2 -- 30 -- inclusive bound

maximumBranchingPo2 : Int
maximumBranchingPo2 = 2 -- 5

maximumBranching : Int
maximumBranching = 2^maximumBranchingPo2 -- inclusive bound

maximumSearchError : Int
maximumSearchError = 1 -- 2

-- note: always returns `Nothing` for negative indices
findFrom : (a -> Bool) -> Int -> Table a -> Maybe (Int, a)
findFrom condition i table =
  Maybe.andThen
    (Table.get i table)
    (\item ->
      if condition item
        then Just (i, item)
        else findFrom condition (i + 1) table)

getChildContainingIndex : Int -> NodeData a -> Maybe (Int, Child a)
getChildContainingIndex i node =
  findFrom
    (.endIndex >> flip (>) i)
    (i `Bitwise.shiftRight` (maximumBranchingPo2 * node.height))
    node.children

length : Array a -> Int
length array =
  case array of
    Node node ->
      Table.get (Table.length node.children - 1) node.children
      |> assumeJust "`length node.children > 0`, so the last element exists"
      |> .endIndex

    Leaf leaf ->
      Table.length leaf

get : Int -> Array a -> Maybe a
get i array =
  case array of
    Node node ->
      getChildContainingIndex i node
      |> Maybe.map (\(_, child) ->
        get (i - child.startIndex) child.array
        |> assumeJust "For `getChildContainingIndex i node` to succeed, `i` must be a valid index into the array.  The child-relative index must therefore also be a valid index into the child array.")

    Leaf leaf ->
      Table.get i leaf

set : Int -> a -> Array a -> Array a
set i newItem array =
  case array of
    Node node ->
      case getChildContainingIndex i node of
        Just (childI, oldChild) ->
          let
            newChildArray = set (i - oldChild.startIndex) newItem oldChild.array
            newChild = { oldChild | array = newChildArray }
            newChildren = Table.set childI newChild node.children
            newNode = { node | children = newChildren }
          in
            Node newNode

        Nothing ->
          array

    Leaf leaf ->
      Leaf (Table.set i newItem leaf)

map : (a -> b) -> Array a -> Array b
map f array =
  case array of
    Node node ->
      let
        transformChild child =
          { child | array = map f child.array }

        transformedChildren = Table.map transformChild node.children
      in
        Node { node | children = transformedChildren }

    Leaf leaf ->
      Leaf (Table.map f leaf)

{- should `Array.foldl` and `Array.foldr` be internally refactored into one
higher-order function which is applied twice with `Table.foldl` and
`Table.foldr` to yield the `Array.foldl` and `Array.foldr` implementations,
respectively? -}

foldl : (a -> b -> b) -> b -> Array a -> b
foldl f init array =
  case array of
    Node node ->
      let
        foldChild child accum =
          foldl f accum child.array
      in
        Table.foldl foldChild init node.children

    Leaf leaf ->
      Table.foldl f init leaf

foldr : (a -> b -> b) -> b -> Array a -> b
foldr f init array =
  case array of
    Node node ->
      let
        foldChild child accum =
          foldr f accum child.array
      in
        Table.foldr foldChild init node.children

    Leaf leaf ->
      Table.foldr f init leaf

chunk : List a -> List (Table a)
chunk list =
  case list of
    [] -> []
    _ ->
      let
        (thisChunk, unchunked) = Table.takeFromList maximumBranching list
      in
        thisChunk :: chunk unchunked

height : Array a -> Int
height array =
  case array of
    Node node -> node.height
    Leaf leaf -> 0

subarrayToChild : Array a -> Int -> (Int, Child a)
subarrayToChild array startIndex =
  let
    endIndex = startIndex + length array
  in
    (endIndex, { array = array, startIndex = startIndex, endIndex = endIndex })

subarraysToChildren : Table (Array a) -> Table (Child a)
subarraysToChildren subarrays =
  snd (Table.mapAccumL subarrayToChild 0 subarrays)

maxHeight : Table (Child a) -> Int
maxHeight =
  Table.foldl (max << height << .array) 0

subarraysToNode : Table (Array a) -> Array a
subarraysToNode subarrays =
  let
    children = subarraysToChildren subarrays
    nodeHeight = maxHeight children + 1
  in
    Node { children = children, height = nodeHeight }

subarraysToNodes : List (Array a) -> List (Array a)
subarraysToNodes subarrays =
  List.map subarraysToNode (chunk subarrays)

toSingleRoot : List (Array a) -> Maybe (Array a)
toSingleRoot arrays =
  case arrays of
    [] -> Just (Leaf Table.empty)
    [topNode] -> Just topNode
    _ -> Nothing

fromList : List a -> Array a
fromList list =
  list
  |> chunk
  |> List.map Leaf
  |> ControlUtils.untilJust toSingleRoot subarraysToNodes

type alias PushResult a =
  { extended : Array a
  , overflowed : List (Array a)
  }

push' : List a -> Array a -> PushResult a
push' newItems array =
  case array of
    Leaf leaf ->
      let
        (pushItems, overflowItems) =
          ListUtils.splitAt (maximumBranching - Table.length leaf) newItems
      in
        { extended = Leaf (Table.pushMany pushItems leaf)
        , overflowed = List.map Leaf (chunk overflowItems)
        }

    Node node ->
      let
        childToExtendI = Table.length node.children - 1

        childToExtend =
          Table.get childToExtendI node.children
          |> assumeJust "`length node.children > 0`, so the last element exists"

        subresult = push' newItems childToExtend.array

        (pushArrays, overflowArrays) =
          ListUtils.splitAt
            (maximumBranching - Table.length node.children)
            subresult.overflowed

        pushChildren =
          pushArrays
          |> (::) subresult.extended
          |> ListUtils.mapAccumL subarrayToChild childToExtend.startIndex
          |> snd

        newChildren =
          Table.pushManyFrom
            childToExtendI
            pushChildren
            node.children
      in
        { extended = Node { node | children = newChildren }
        , overflowed = subarraysToNodes overflowArrays
        }

pushMany : List a -> Array a -> Array a
pushMany newItems array =
  let subresult = push' newItems array
  in
    ControlUtils.untilJust
      toSingleRoot
      subarraysToNodes
      (subresult.extended :: subresult.overflowed)

append : Array a -> Array a -> Array a
append array1 array2 =
  let (newArray1, newArray2) = append' (array1, array2)
  in
    case (shallowCount newArray1, shallowCount newArray2) of
      (_, 0) -> newArray1
      (0, _) -> newArray2
      (count1, count2) ->
        if count1 + count2 <= maximumBranching
          then case (newArray1, newArray2) of
            (Leaf leaf1, Leaf leaf2) -> Leaf (Table.append leaf1 leaf2)
            (Node node1, Node node2) ->
              Node
                { height = node1.height
                , children =
                  Table.append
                    node1.children
                    (snd (Table.mapAccumL (subarrayToChild << .array) (length newArray1) node2.children))
                }
            _ -> Debug.crash "This should be impossible"
          else
            let (l1, l2) = (length newArray1, length newArray2)
            in Node
              { height = height newArray1 + 1
              , children = Table.fromList
                [ { startIndex = 0
                  , endIndex = l1
                  , array = newArray1
                  }
                , { startIndex = l1
                  , endIndex = l1 + l2
                  , array = newArray2
                  }
                ]
              }

append' : (Array a, Array a)-> (Array a, Array a)
append' arrays =
  case arrays of
    (Leaf _, Leaf _) ->
      arrays

    (Node node1, Node node2) ->
      case compare node1.height node2.height of
        EQ -> appendCorrespondingNodes (node1, node2)
        LT -> appendWithRightParent (Node node1, node2)
        GT -> appendWithLeftParent (node1, Node node2)

    (leafArray1, Node node2) ->
      appendWithRightParent (leafArray1, node2)

    (Node node1, leafArray2) ->
      appendWithLeftParent (node1, leafArray2)

appendWithLeftParent : (NodeData a, Array a) -> (Array a, Array a)
appendWithLeftParent (node1, array2) =
  append' (Node node1, artificiallyHeighten node1.height array2)

appendWithRightParent : (Array a, NodeData a) -> (Array a, Array a)
appendWithRightParent (array1, node2) =
  append' (artificiallyHeighten node2.height array1, Node node2)

artificiallyHeighten : Int -> Array a -> Array a
artificiallyHeighten desiredHeight array =
  case compare desiredHeight (height array) of
    EQ -> array
    GT ->
      Node
        { height = desiredHeight
        , children = Table.fromList
          [ { startIndex = 0 
            , endIndex = length array
            , array = artificiallyHeighten (desiredHeight - 1) array
            }
          ]
        }
    LT -> Debug.crash "This function should never be called with `desiredHeight < height array`."

shallowCount : Array a -> Int
shallowCount array =
  case array of
    Leaf leaf -> Table.length leaf
    Node node -> Table.length node.children

type alias ErrorCrossing =
  { index : Int
  , searchError : Int
  }

type alias SearchErrorAnalysis =
  { index : Int
  , totalShallowCount : Int
  , lastErrorCrossing : Maybe ErrorCrossing
  }

isNewErrorCrossing : Maybe Int -> Int -> Bool
isNewErrorCrossing oldError newError =
  if newError > maximumSearchError
    then
      oldError
      |> Maybe.map ((>) newError)
      |> Maybe.withDefault True
    else
      False

accumulateSearchError : Child a -> SearchErrorAnalysis -> SearchErrorAnalysis
accumulateSearchError child prevAnalysis =
  let
    subarray = child.array
    newIndex = Debug.log "index" <| prevAnalysis.index + 1
    newShallowCount = Debug.log "totalShallowCount" <| prevAnalysis.totalShallowCount + shallowCount subarray
    idealShallowCount = Debug.log "idealShallowCount" <| maximumBranching * (prevAnalysis.index + 1)
    newSearchError = Debug.log "error" <| ceiling (toFloat (idealShallowCount - newShallowCount) / toFloat maximumBranching)
    newCrossing =
      isNewErrorCrossing
        (Maybe.map .searchError prevAnalysis.lastErrorCrossing)
        newSearchError
  in
    if newCrossing
      then
        { index = newIndex
        , totalShallowCount = newShallowCount
        , lastErrorCrossing =
          Just
            { index = prevAnalysis.index
            , searchError = newSearchError
            }
        }
      else
        { prevAnalysis | index = newIndex, totalShallowCount = newShallowCount }

analyzeSearchError : (NodeData a, NodeData a) -> SearchErrorAnalysis
analyzeSearchError (node1, node2) =
  { index = 0, totalShallowCount = 0, lastErrorCrossing = Nothing }
  |> flip (Table.foldl accumulateSearchError) node1.children
  |> flip (Table.foldl accumulateSearchError) node2.children

hasRoom : Child a -> Bool
hasRoom child =
  shallowCount child.array < maximumBranching

coerceToNode : Array a -> NodeData a
coerceToNode array =
  case array of
    Node node -> node
    Leaf _ -> Debug.crash "Unexpected!"

coerceToLeaf : Array a -> Table a
coerceToLeaf array =
  case array of
    Node _ -> Debug.crash "Unexpected!"
    Leaf leaf -> leaf

redistributeSubarrays : Int -> Table (Array a) -> Table (Array a)
redistributeSubarrays parentHeight subarrays =
  if parentHeight > 1
    then
      subarrays
      |> Table.map (coerceToNode >> .children)
      |> Table.redistributeMany maximumBranching
      |> Table.map (\children -> Node { children = children, height = parentHeight - 1 })
    else
      subarrays
      |> Table.map coerceToLeaf
      |> Table.redistributeMany maximumBranching
      |> Table.map Leaf

appendCorrespondingNodes : (NodeData a, NodeData a) -> (Array a, Array a)
appendCorrespondingNodes (node1, node2) =
  case (Table.length node1.children > 0, Table.length node2.children > 0) of
    (True, False) -> (Node node1, Node node2) -- node2 is empty
    (False, True) -> (Node node2, Node node1) -- node1 is empty
    (False, False) -> (Node node1, Node node2) -- node1 and node2 are empty

    (True, True) ->
      let
        rightmostLeftI = Table.length node1.children - 1
        rightmostLeft =
          node1.children
          |> Table.get rightmostLeftI
          |> assumeJust "length node1.children > 0"

        leftmostRightI = Table.length node2.children
        leftmostRight =
          node2.children
          |> Table.get 0
          |> assumeJust "length node2.children > 0"

        (newRightmostLeft, newLeftmostRight) =
          if node1.height > 1
            then
              appendCorrespondingNodes
                ( coerceToNode rightmostLeft.array
                , coerceToNode leftmostRight.array
                )
            else
              (rightmostLeft.array, leftmostRight.array)

        newLeftChildren =
          Table.set
            rightmostLeftI
            { startIndex = rightmostLeft.startIndex
            , endIndex = rightmostLeft.startIndex + length newRightmostLeft
            , array = newRightmostLeft
            }
            node1.children

        newLeft = { node1 | children = newLeftChildren }

        newRightSubarrays =
          Table.initialize
            (\i ->
              if i == 0
                then newLeftmostRight
                else Table.get i node2.children |> assumeJust "" |> .array)
            (Table.length node2.children)

        newRight = { node2 | children = subarraysToChildren newRightSubarrays }
      in
        redistributeChildren (newLeft, newRight)

-- TODO: Needs cleanup, optimization
redistributeChildren : (NodeData a, NodeData a) -> (Array a, Array a)
redistributeChildren (node1, node2) =
  let
    firstWithRoom = Table.findIndex hasRoom node1.children
    errorAnalysis = analyzeSearchError (node1, node2) |> Debug.log "errorAnalysis"
  in
    case (firstWithRoom, errorAnalysis.lastErrorCrossing |> Maybe.map .index) of
      (Just first, Just last) ->
        let
          getFromOriginal = getConcat node1.children node2.children >> Maybe.map .array

          toRedistribute =
            Table.initialize (\i -> getFromOriginal (i + first) |> assumeJust "") (last + 1 - first)

          redistributed = redistributeSubarrays node1.height toRedistribute -- Table.redistributeMany maximumBranching toRedistribute

          getFromCombined i =
            if Debug.log "i" i < first
              then getFromOriginal i
              else
                if i < first + Table.length redistributed
                  then Table.get (i - first) redistributed
                  else getFromOriginal (i - first - Table.length redistributed + last + 1)

          totalCount
            = Table.length node1.children
            + Table.length node2.children
            - Table.length toRedistribute
            + Table.length redistributed

          toLeft = min maximumBranching totalCount
          toRight = totalCount - toLeft

          _ = Debug.log "combined" (Table.initialize (getFromCombined >> assumeJust "") totalCount) 

          newLeftSubarrays = Table.initialize (getFromCombined >> assumeJust "") toLeft
          newRightSubarrays = Table.initialize (flip (+) toLeft >> getFromCombined >> assumeJust "") toRight
        in
          (subarraysToNode newLeftSubarrays, subarraysToNode newRightSubarrays)

      _ ->
        (Node node1, Node node2)


getConcat : Table a -> Table a -> Int -> Maybe a
getConcat tab1 tab2 i =
  if i < Table.length tab1
    then Table.get i tab1
    else Table.get (i - Table.length tab1) tab2
