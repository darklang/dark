port module Main exposing (..)

-- builtins
import Maybe
import Dict

-- lib
import Json.Decode as JSD
import Http
import Html
import Keyboard.Event
import Keyboard.Key as Key

-- dark
import RPC exposing (rpc)
import Types exposing (..)
import Util exposing (deMaybe)
import View
import Defaults
import Graph as G
import Entry
import Autocomplete
import Selection
import Window.Events exposing (onWindow)


-----------------------
-- TOP-LEVEL
-----------------------
main : Program Flags Model Msg
main = Html.programWithFlags
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}


-----------------------
-- MODEL
-----------------------
init : Flags -> ( Model, Cmd Msg )
init {state, complete} =
  let editor = case state of
            Just e -> e
            Nothing -> Defaults.defaultEditor
      m = Defaults.defaultModel editor
      m2 = { m | complete = Autocomplete.init complete}
  in
    (m2, rpc m <| [LoadInitialGraph])


-----------------------
-- ports, save Editor state in LocalStorage
-----------------------
port setStorage : Editor -> Cmd a

-----------------------
-- updates
-----------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let mods = update_ msg m
      (newm, newc) = updateMod mods (m, Cmd.none)
  in
    ({ newm | lastMsg = msg
            , lastMod = mods}
     , Cmd.batch [newc, m |> Defaults.model2editor |> setStorage])

-- applied from left to right
updateMod : Modification -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMod mod (m, cmd) =
  let (newm, newcmd) =
    case mod of
      Error e -> { m | error = (e, Util.timestamp ())} ! []
      RPC calls -> m ! [rpc m calls]
      NoChange -> m ! []
      Select id -> { m | state = Selecting id
                       , center = G.getNodeExn m id |> .pos} ! []
      Enter entry -> { m | state = Entering entry
                         , center = case entry of
                                      Filling n _ -> n.pos
                                      Creating p -> m.center -- dont move
                     }
                     ! [Entry.focusEntry]
      ModelMod mm -> mm m ! []
      Deselect -> { m | state = Deselected } ! []
      AutocompleteMod mod ->
        let complete = Autocomplete.update mod m.complete
        in
          ({ m | complete = Autocomplete.update mod m.complete
           }, Autocomplete.focusItem complete.index)
      Many mods -> List.foldl updateMod (m, Cmd.none) mods
  in
    (newm, Cmd.batch [cmd, newcmd])


update_ : Msg -> Model -> Modification
update_ msg m =
  case (msg, m.state) of

    (NodeClick node, _) ->
      Select node.id

    (RecordClick event, _) ->
      if event.button == Defaults.leftButton
      then Many [ AutocompleteMod Reset
                , Enter <| Creating (Entry.toAbsolute m event.pos)]
      else NoChange

    ------------------------
    -- entry node
    ------------------------
    (EntrySubmitMsg, _) ->
      NoChange -- just keep this here to prevent the page from loading

    (GlobalKeyPress event_orig, state) ->
      -- my keyboard is broken, so remap Backtick to Escape for now
      let event = {event_orig | keyCode = case event_orig.keyCode of
                                           Key.Unknown 192 -> Key.Escape
                                           k -> k } in
      case state of
        Selecting id ->
          case event.keyCode of
            Key.Backspace ->
              let next = G.incomingNodes m (G.getNodeExn m id) in
              Many [ RPC <| [DeleteNode id]
                   , case List.head next of
                       Just next -> Select next.id
                       Nothing -> Deselect
                   ]
            Key.Up -> Selection.selectNextNode m id (\n o -> n.y > o.y)
            Key.Down -> Selection.selectNextNode m id (\n o -> n.y < o.y)
            Key.Left -> Selection.selectNextNode m id (\n o -> n.x > o.x)
            Key.Right -> Selection.selectNextNode m id (\n o -> n.x < o.x)
            Key.Enter -> Entry.enter m id True
            Key.One -> Entry.reenter m id 0
            Key.Two -> Entry.reenter m id 1
            Key.Three -> Entry.reenter m id 2
            Key.Four -> Entry.reenter m id 3
            Key.Five -> Entry.reenter m id 4
            Key.Six -> Entry.reenter m id 5
            Key.Seven -> Entry.reenter m id 6
            Key.Eight -> Entry.reenter m id 7
            Key.Nine -> Entry.reenter m id 8
            Key.Zero -> Entry.reenter m id 9
            Key.Escape -> Deselect
            code -> Selection.selectByLetter m code

        Entering cursor ->
          if event.ctrlKey then
            case event.keyCode of
              Key.P -> AutocompleteMod SelectUp
              Key.N -> AutocompleteMod SelectDown
              _ -> NoChange
          else
            case event.keyCode of
              Key.Up -> AutocompleteMod SelectUp
              Key.Down -> AutocompleteMod SelectDown
              Key.Right ->
                let sp = Autocomplete.sharedPrefix m.complete in
                if sp == "" then NoChange
                else AutocompleteMod <| Query sp
              Key.Enter ->
                case Autocomplete.highlighted m.complete of
                  Just item -> AutocompleteMod
                               <| Complete (Autocomplete.asString item)
                  Nothing -> Entry.submit m cursor
              Key.Escape ->
                case cursor of
                  Creating _ -> Many [Deselect, AutocompleteMod Reset]
                  Filling node _ -> Many [ Select node.id
                                         , AutocompleteMod Reset]
              key ->
                AutocompleteMod <| Query m.complete.value

        Deselected ->
          case event.keyCode of
            Key.Enter -> Entry.createFindSpace m
            _ -> Selection.selectByLetter m event.keyCode

    (EntryInputMsg target, _) ->
      Entry.updateValue target

    (RPCCallBack calls (Ok (nodes)), _) ->
      let m2 = { m | nodes = nodes }
          reaction = case Nothing of
                       -- if we deleted a node, the cursor is probably
                       -- invalid
                       Nothing ->
                         if Dict.size m2.nodes == 0
                         then Entry.createFindSpace m
                         else if m2.state
                           |> Selection.getCursorID
                           |> Maybe.andThen (G.getNode m2)
                           |> (==) Nothing
                         then Deselect
                         else NoChange

                       -- if we added a node, select it
                       Just id ->
                         case calls of
                           [ AddFunctionCall id name pos
                           , SetEdge sid (tid, p)] ->
                             let target = G.getNodeExn m2 tid
                                 arg = G.getArgument p target
                             in case arg of
                                  Edge sid ->
                                    Entry.enter m2 sid False
                                  _ ->
                                    Entry.enter m2 tid False
                           _ ->
                             Entry.enter m2 id False

      in
        Many [ ModelMod (\_ -> m2)
             , AutocompleteMod Reset
             , Error ""
             , reaction
             ]


    ------------------------
    -- plumbing
    ------------------------
    (RPCCallBack _ (Err (Http.BadStatus error)), _) ->
      Error <| "Bad RPC call: " ++ (toString error.body)

    (RPCCallBack _ (Err (Http.NetworkError)), _) ->
      Error <| "Netork error: is the server running?"

    (FocusResult _, _) ->
      NoChange

    (FocusAutocompleteItem _, _) ->
      NoChange

    t -> Error <| "Nothing for " ++ (toString t)

    ------------------------
    -- datastores
    -------------------------
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



-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let keySubs =
        [onWindow "keydown"
           (JSD.map GlobalKeyPress Keyboard.Event.decodeKeyboardEvent)]
  in Sub.batch
    (List.concat [keySubs])
