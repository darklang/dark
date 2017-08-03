port module Main exposing (..)

-- builtins
import Result
import Char
import Dict exposing (Dict)
import Maybe

-- lib
import Http
import Html
import Keyboard
import Mouse
import Maybe.Extra
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key


-- dark
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import View
import Defaults
import Repl
import Graph as G
import Canvas
import Entry


-----------------------
-- TOP-LEVEL
-----------------------
main : Program (Maybe Editor) Model Msg
main = Html.programWithFlags
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}


-----------------------
-- MODEL
-----------------------
init : Maybe Editor -> ( Model, Cmd Msg )
init mEditor =
  let e = case mEditor of
                 Just e -> e
                 Nothing -> Defaults.defaultEditor
      m = Defaults.defaultModel e
  in
    (m, Cmd.batch [Canvas.focusEntry, rpc m <| [LoadInitialGraph]])


-----------------------
-- ports, save Editor state in LocalStorage
-----------------------
port setStorage : Editor -> Cmd a

-----------------------
-- updates
-----------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let mods = (update_ msg m)
      (newm, newc) = updateMod mods (m, Cmd.none)
  in
    (newm, Cmd.batch [newc, m |> Defaults.model2editor |> setStorage])

-- applied from left to right
updateMod : Modification -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMod mod (m, cmd) =
  let (newm, newcmd) =
    case mod of
      RPC call -> (m, rpc m [call])
      NoChange -> (m, Cmd.none)
      Error e -> ({ m | error = (e, Util.timestamp ())
                  }, Cmd.none)
      Cursor c -> ({ m | cursor = c
                      }, Canvas.maybeFocusEntry m.cursor c)
      Drag d -> ({ m | drag = d
                    }, Cmd.none)
      ModelMod mm -> (mm m, Cmd.none)
      Many mods -> List.foldl updateMod (m, Cmd.none) mods
  in
    (newm, Cmd.batch [cmd, newcmd])


update_ : Msg -> Model -> Modification
update_ msg m_ =
  let m = { m_ | lastMsg = msg } in
  case (msg, m.cursor) of

    (CheckEscape code, _) ->
      if code == Defaults.escapeKeycode
      then Cursor Deselected
      else NoChange

    (NodeClick node, _) ->
      Cursor <| Canvas.selectNode m node

    (RecordClick pos, _) ->
      -- When we click on a node, drag is set when RecordClick happens.
      -- So this avoids firing if we click outside a node
      if m.drag == NoDrag then
        Cursor <| Creating pos
      else
        NoChange

    ------------------------
    -- dragging nodes
    ------------------------
    (DragNodeStart node event, _) ->
      -- If we're already dragging a slot don't change the node
      if m.drag == NoDrag && event.button == Defaults.leftButton
      then
        let offset = Canvas.findOffset node.pos event.pos in
        Many [ Drag <| DragNode node.id offset
             , Cursor <| Dragging node.id]
      else NoChange

    (DragNodeMove id offset pos, _) ->
      -- While it's kinda nasty to update a node in place, the drawing
      -- code get's really complex if we don't do this.
      let update = Canvas.updateDragPosition pos offset id m.nodes in
      ModelMod (\m -> { m | nodes = update
                      , dragPos = pos})

    (DragNodeEnd id _, _) ->
      let node = G.getNodeExn m id in
      Many [ Drag NoDrag
           , Cursor <| Canvas.selectNode m node
           , RPC <| UpdateNodePosition id node.pos]

    (DragSlotStart target param event, _) ->
      if event.button == Defaults.leftButton
      then Many [ Cursor <| Dragging target.id
                , Drag <| DragSlot target param event.pos]
      else NoChange

    (DragSlotMove mpos, _) ->
      ModelMod (\m -> { m | dragPos = mpos })

    (DragSlotEnd source, _) ->
      case m.drag of
        DragSlot target param starting ->
          Many [ Drag NoDrag
               , RPC <| AddEdge source.id (target.id, param)]
        _ -> NoChange

    (DragSlotStop _, _) ->
      Drag NoDrag

    ------------------------
    -- entry node
    ------------------------
    (EntrySubmitMsg, Filling node pos) ->
      case String.uncons m.entryValue of
        Nothing -> NoChange
        Just ('$', rest) -> Entry.addVar m rest
        Just ('.', fieldname) ->
          -- create two nodes, add them both
          let constant = Constant ("\"" ++ fieldname ++ "\"") "fieldname"
              implicit = Entry.findImplicitEdge m node
          in
            Entry.addNode "." pos [implicit, constant]
        _ ->
          let implicit = Entry.findImplicitEdge m node in
          Entry.addNode m.entryValue pos [implicit]

    (EntrySubmitMsg, Creating pos) ->
      Entry.addNode m.entryValue pos []

    (EntryKeyPress event, cursor) ->
      Entry.updateEntryKeyPress m event cursor

    (GlobalKeyPress code, cursor) ->
      Entry.updateGlobalKeyPress m code cursor

    (RPCCallBack calls (Ok (nodes, edges, justAdded)), _) ->
      let m2 = { m | nodes = nodes
                   , edges = edges
                   , error = ("", 0)}
          cursor = case justAdded of
                     -- if we deleted a node, the cursor is probably
                     -- invalid
                     Nothing ->
                       if m.cursor
                         |> Canvas.getCursorID
                         |> Maybe.andThen (G.getNode m2)
                         |> (==) Nothing
                       then Deselected
                       else m.cursor

                     -- TODO if the just-added node has an outgoing
                     -- edge, which was just selected, choose it
                     -- instead.

                     -- if we added a node, select it
                     Just id ->
                       let node = G.getNodeExn m2 id in
                       Canvas.selectNode m2 node

      in
        Many [ ModelMod (\_ -> G.constantFold { m2 | entryValue = "" } )
             , Cursor cursor]


    ------------------------
    -- plumbing
    ------------------------
    (RPCCallBack _ (Err (Http.BadStatus error)), _) ->
      Error <| "Bad RPC call: " ++ (toString error.body)

    (FocusResult _, _) ->
      -- Yay, you focused a field! Ignore.
      -- TODO: should these be separate events?
      ModelMod (\m -> {m | replValue = ""
                         , entryValue = ""})

      -- Syncs the form with the model. The actual submit is in
      -- EntrySubmitMsg
    (EntryInputMsg target, _) ->
      ModelMod (\m -> { m | entryValue = target })

    t -> Error <| "Nothing for " ++ (toString t)


-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let dragSubs = case m.drag of
                   -- we use IDs here because the node will change
                   -- before they're triggered
                   DragNode id offset -> [ Mouse.moves (DragNodeMove id offset)
                                         , Mouse.ups (DragNodeEnd id)]
                   DragSlot _ _ _ ->
                     [ Mouse.moves DragSlotMove
                     , Mouse.ups DragSlotStop]
                   NoDrag -> [ ]
      keySubs = [ Keyboard.downs GlobalKeyPress]
      standardSubs = [ Keyboard.downs CheckEscape
                     , Mouse.downs RecordClick]
  in Sub.batch
    (List.concat [standardSubs, keySubs, dragSubs])

