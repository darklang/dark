module Selection exposing (..)

-- builtins
import Char

-- lib
import Keyboard.Key as Key

-- dark
import Types exposing (..)
import Graph as G
import Node as N
import Entry

------------------
-- cursor stuff
----------------

isSelected : Model -> Node -> Bool
isSelected m n =
  case m.state of
    Entering _ (Filling node _) -> n == node
    Selecting id -> n.id == id
    Dragging _ _ _ (Entering _ (Filling node _)) -> n == node
    Dragging _ _ _ (Selecting id) -> n.id == id
    _ -> False

entryVisible : State -> Bool
entryVisible state =
  case state of
    Entering _ _ -> True
    _ -> False

getCursorID : State -> Maybe ID
getCursorID s =
  case s of
    Entering _ (Filling node _) -> Just node.id
    Selecting id -> Just id
    _ -> Nothing

selectByLetter : Model -> Key.Key -> Modification
selectByLetter m code =
  code
  |> Key.toChar
  |> Maybe.map Char.toLower
  |> Maybe.map String.fromChar
  |> Maybe.andThen (G.fromLetter m)
  |> Maybe.map (\n -> Select n.id)
  |> Maybe.withDefault NoChange



selectNextNode : Model -> ID -> (Node -> Node -> Bool) -> Modification
selectNextNode m id cond =
  -- if we're currently in a node, follow the direction. For now, pick
  -- the nearest node to it, that it's connected to, that's roughly in
  -- that direction.
  let n = G.getNodeExn m id
  in
    n
    |> G.connectedNodes m
    -- that are above us
    |> List.filter (\o -> cond n o)
    -- the nearest to us
    |> List.sortBy (\other -> G.distance m other n)
    |> List.head
    |> Maybe.map (\n -> Select n.id)
    |> Maybe.withDefault NoChange

deleteSelected : Model -> ID -> Modification
deleteSelected m id =
  let prev = G.incomingNodes m (G.getNodeExn m id) |> List.filter N.isNotBlock
      next = G.outgoingNodes m (G.getNodeExn m id) |> List.filter N.isNotArg in
      -- FocusSame gets called later, after the RPC, so just doesn't change anything
   Many [ RPC (Entry.withNodePositioning m [DeleteNode id], FocusSame)
        , case List.head (List.append prev next) of
            Just n -> Select n.id
            Nothing -> Deselect
        ]

