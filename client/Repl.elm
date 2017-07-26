module Repl exposing (parse)

import Dict

import Ordering
import List.Extra

import Types exposing (..)
import Util exposing (..)

    -- (ADD_DS, SubmitMsg, _) ->
    --   ({ m | state = ADD_DS_FIELD_NAME
    --    }, rpc m <| AddDatastore m.inputValue m.clickPos)

    -- (ADD_DS_FIELD_NAME, SubmitMsg, _) ->
    --     ({ m | state = ADD_DS_FIELD_TYPE
    --          , tempFieldName = m.inputValue
    --      }, Cmd.none)

    -- (ADD_DS_FIELD_TYPE, SubmitMsg, Just id) ->
    --   ({ m | state = ADD_DS_FIELD_NAME
    --    }, rpc m <| AddDatastoreField id m.tempFieldName m.inputValue)
       -- ('C', _, Just id) ->
       --   (m, rpc m <| ClearEdges id)
       -- ('L', _, Just id) ->
       --   (m, rpc m <| RemoveLastField id)




nextNode : Model -> Cursor -> Cursor
nextNode m cursor =
  let nodes = Util.orderedNodes m
      first = nodes |> List.head |> Maybe.map (\n -> n.id)
  in
    case cursor of
      Nothing -> first
      Just (ID id) ->
        let node = Dict.get id m.nodes |> deMaybe
            next = List.Extra.find
                   (\n -> (n.pos.x, n.pos.y, deID n.id) > (node.pos.x, node.pos.y, deID node.id))
                   nodes
        in case next of
             Nothing -> first
             Just node -> Just node.id


parse : Model -> String -> Cursor -> (Model, List RPC)
parse m command cursor =
  let l2id l = (Util.fromLetter m l).id in
  case String.words command of
    [] -> (m, [])
    first :: words ->
      case (String.uncons first, first, words, cursor) of
        (Just ('+', name), _, [], _) ->
          if (rematch "^[\"\'1-9].*" name) then
            (m, [AddValue name m.clickPos])
          else
            (m, [AddFunctionCall name m.clickPos []])

        (Just ('+', name), _, args, _) ->
          (m, [AddFunctionCall name m.clickPos (List.map l2id args)])

        (_, "/rm", [], Just id) ->
          (m, [DeleteNode id])

        (_, "/rm", [n], _) ->
          (m, [DeleteNode (l2id n)])

        (_, "/rm", args, _) ->
          (m, List.map (\n -> DeleteNode (l2id n)) args)

        (_, "/clear", [], Just id) ->
          (m, [ClearEdges id])

        (_, "/clear", args, _) ->
          (m, List.map (\n -> ClearEdges (l2id n)) args)

        (_, "/edge", [src, target, param], _) ->
          (m, [AddEdge (l2id src) ((l2id target), param)])

        (_, "/n", _, c) ->
          case nextNode m c of
            Just id -> ({ m | cursor = Just id }, [])
            Nothing -> (m, [])

        (Just (l, ""), _, [], _) ->
          ({ m | cursor = Just (l |> String.fromChar |> l2id)
           }, [])

        (_, _, _, _) -> (m, [])
