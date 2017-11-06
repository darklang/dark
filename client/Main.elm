port module Main exposing (..)


-- builtins
import Maybe
import Dict

-- lib
import Json.Decode as JSD
import Http
import Keyboard.Event
import Keyboard.Key as Key
import Navigation
import Mouse
import List.Extra as LE

-- dark
import RPC exposing (rpc, phantomRpc, saveTest)
import Types exposing (..)
import View
import Defaults
import Graph as G
import Runtime as RT
import Entry
import RandomGraph
import Autocomplete
import Selection
import Viewport
import Window.Events exposing (onWindow)
import VariantTesting exposing (parseVariantTestsFromQueryString)
import Util



-----------------------
-- TOP-LEVEL
-----------------------
main : Program Flags Model Msg
main = Navigation.programWithFlags
       LocationChange
       { init = init
       , view = View.view
       , update = update
       , subscriptions = subscriptions}


-----------------------
-- MODEL
-----------------------
flag2function : FlagFunction -> Function
flag2function fn =
  { name = fn.name
  , description = fn.description
  , returnTipe = RT.str2tipe fn.return_type
  , parameters = List.map (\p -> { name = p.name
                                 , tipe = RT.str2tipe p.tipe
                                 , block_args = p.block_args
                                 , optional = p.optional
                                 , description = p.description}) fn.parameters
  }

init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init {editorState, complete} location =
  let editor = case editorState of
            Just e -> e
            Nothing -> Defaults.defaultEditor
      tests = case parseVariantTestsFromQueryString location.search of
                  Just t  -> t
                  Nothing -> []
      m = Defaults.defaultModel editor
      m2 = { m | complete = Autocomplete.init (List.map flag2function complete), tests = tests }
  in
    (m2, rpc m FocusNothing [])


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
      (newm, newc) = updateMod m mods (m, Cmd.none)
  in
    ({ newm | lastMsg = msg
            , lastMod = mods}
     , Cmd.batch [newc, m |> Defaults.model2editor |> setStorage])

---------------------------------------------
-- TODO: put these into updatemod so it doesn't use out of date info
---------------------------------------------

-- Reenter an existing node to edit the existing inputs
reenter : Model -> ID -> Int -> Modification
reenter m id i =
  -- TODO: Allow the input to be edited
  let n = G.getNodeExn m id
  in
    case LE.getAt i n.arguments of
      Nothing -> NoChange
      Just (p, a) ->
        let enter = Enter True <| Filling n.id (ParamHole n.id p i) in
        case a of
          Edge eid _ -> Many [ enter
                          , AutocompleteMod (ACQuery <| "$" ++ G.toLetter m eid)]
          NoArg -> enter
          ElidedArg -> Debug.crash "ElidedArgs should already be expanded"
          Const c -> Many [ enter
                          , AutocompleteMod (ACQuery c)]

-- Enter this exact node
enterExact : Model -> Node -> Modification
enterExact m selected =
  let hole = G.findHole selected in
  if (hole == ResultHole selected.id)
     && (G.outgoingNodes m selected |> List.isEmpty |> not)
  then Select selected.id
  else
    Filling selected.id hole
    |> cursor2mod m

-- Enter the next needed node, searching from here
enterNext : Model -> Node -> Modification
enterNext m n =
  cursor2mod m <|
    case G.findNextHole m n of
      Nothing -> Filling n.id (ResultHole n.id)
      Just hole -> Filling (Entry.idFromHole hole) hole

cursor2mod : Model -> EntryCursor -> Modification
cursor2mod m cursor =
  let ns = G.orderedNodes m in
  Many [ Enter False cursor
       , case cursor of
           Filling id (ResultHole _) ->
             AutocompleteMod <| ACFilterByLiveValue ((G.getNodeExn m id).liveValue)
           Filling _ (ParamHole _ p _) ->
             Many [ AutocompleteMod <| ACFilterByParamType p.tipe ns
                  , AutocompleteMod <| ACOpen False ]
           Creating _ ->
             NoChange
       ]

selectCenter : Pos -> Pos -> Pos
selectCenter old new =
  -- ignore the Util.windowSize y hack
  let (xSize, ySize) = Util.windowSize ()
      xThreshold     = xSize // 10
      yThreshold     = ySize // 10
      fakeCenter     = Defaults.initialPos
      newY           = if (new.y > (old.y + (ySize - fakeCenter.vy) - yThreshold))
                       || (new.y < (old.y - fakeCenter.vy + yThreshold))
                       then new.y
                       else old.y
      newX           = if (new.x > (old.x + (xSize - fakeCenter.vx) - xThreshold))
                       || (new.x < (old.x - fakeCenter.vx + xThreshold))
                       then new.x
                       else old.x
  in
      { x = newX, y = newY }

updateMod : Model -> Modification -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMod origm mod (m, cmd) =
  -- if you ever have a node in here, you're doing it wrong. Use an ID.
  let (newm, newcmd) =
    case mod of
      Error e -> { m | error = Just e} ! []
      ClearError -> { m | error = Nothing} ! []
      RPC (calls, id) -> m ! [rpc m id calls]
      Phantom ->
        case m.state of
          Entering re cursor ->
            case Entry.submit m re cursor m.complete.value of
              RPC (rpcs, _) -> m ! [phantomRpc m cursor rpcs]
              _ -> m ! []
          _ -> m ! []
      NoChange -> m ! []
      MakeCmd cmd -> m ! [cmd]
      SetState state ->
        -- DOES NOT RECALCULATE VIEW
        { m | state = state } ! []
      Select id ->
        let n = G.getNodeExn m id in
        G.recalculateView
        ({ m | state = Selecting id
            , center = if G.hasRelativePos n
                       then m.center
                       else G.pos m n |> selectCenter origm.center}) ! []
      Enter re entry ->
        G.recalculateView
        ({ m | state = Entering re entry
            , center =
                case entry of
                  Filling id _ ->
                    let n = G.getNodeExn m id in
                    if G.hasRelativePos n
                    then m.center
                    else selectCenter origm.center (G.pos m n)
                  Creating p ->
                    m.center -- dont move
        })
        ! [Entry.focusEntry]
      SetCenter c ->
        { m | center = c } ! []
      SetPhantoms ps ->
        { m | phantoms = ps } ! []
      SetBackingNodes nodes ->
        (G.recalculateView { m | backingNodes = nodes }) ! []
      ToggleOpenNode id ->
        (G.recalculateView <| G.toggleOpenNode m id) ! []
      SetViewNodes nodes ->
        -- viewNodes are pretty temporary. They get overwritten on state
        -- changes, backing node changes, and probably other changes too.
        { m | nodes = nodes } ! []
      Drag id offset hasMoved state ->
        { m | state = Dragging id offset hasMoved state } ! []
      Deselect -> { m | state = Deselected } ! []
      AutocompleteMod mod ->
        let complete = Autocomplete.update mod m.complete
        in
          ({ m | complete = Autocomplete.update mod m.complete
           }, Autocomplete.focusItem complete.index)
      -- applied from left to right
      ChangeCursor step -> case m.state of
        Selecting id -> let calls = Entry.updatePreviewCursor m id step
                        in m ! [rpc m FocusSame calls]
        _ -> m ! []
      Many mods -> List.foldl (updateMod origm) (m, Cmd.none) mods
  in
    (newm, Cmd.batch [cmd, newcmd])


update_ : Msg -> Model -> Modification
update_ msg m =
  case (msg, m.state) of

    ------------------------
    -- entry node
    ------------------------
    (EntrySubmitMsg, _) ->
      NoChange -- just keep this here to prevent the page from loading

    (GlobalKeyPress event, state) ->
      if event.ctrlKey && (event.keyCode == Key.Z || event.keyCode == Key.Y)
      then
        case event.keyCode of
          Key.Z -> RPC ([Undo], FocusNothing)
          Key.Y -> RPC ([Redo], FocusNothing)
          _ -> NoChange
      else
        case state of
          Selecting id_ ->
            -- quick error checking, in case the focus has gone bad
            if not <| G.hasNode m id_ then Entry.createFindSpace m else let id = id_ in
            case event.keyCode of
              Key.Backspace -> Selection.deleteSelected m id
              Key.Up -> Selection.selectNextNode m id (\n o -> G.posy m n > G.posy m o)
              Key.Down -> Selection.selectNextNode m id (\n o -> G.posy m n < G.posy m o)
              Key.Left -> if event.altKey
                then
                  ChangeCursor -1
                else
                  Selection.selectNextNode m id (\n o -> G.posx m n > G.posx m o)
              Key.Right -> if event.altKey
                then
                  ChangeCursor 1
                else
                  Selection.selectNextNode m id (\n o -> G.posx m n < G.posx m o)
              Key.Enter -> enterExact m (G.getNodeExn m id)
              Key.One -> reenter m id 0
              Key.Two -> reenter m id 1
              Key.Three -> reenter m id 2
              Key.Four -> reenter m id 3
              Key.Five -> reenter m id 4
              Key.Six -> reenter m id 5
              Key.Seven -> reenter m id 6
              Key.Eight -> reenter m id 7
              Key.Nine -> reenter m id 8
              Key.Zero -> reenter m id 9
              Key.Escape -> Deselect
              Key.Tab -> ToggleOpenNode id
              code -> Selection.selectByLetter m code

          Entering re cursor ->
            if event.ctrlKey then
              case event.keyCode of
                Key.P -> AutocompleteMod ACSelectUp
                Key.N -> AutocompleteMod ACSelectDown
                _ -> NoChange
            else
              case event.keyCode of
                Key.Up -> AutocompleteMod ACSelectUp
                Key.Down -> Many [ AutocompleteMod (ACOpen True)
                                , AutocompleteMod ACSelectDown]
                Key.Right ->
                  let sp = Autocomplete.sharedPrefix m.complete in
                  if sp == "" then NoChange
                  else
                    AutocompleteMod <| ACQuery sp
                Key.Enter ->
                  let name = case Autocomplete.highlighted m.complete of
                              Just item -> Autocomplete.asName item
                              Nothing -> m.complete.value
                  in
                    Entry.submit m re cursor name

                Key.Escape ->
                  case cursor of
                    Creating _ -> Many [Deselect, AutocompleteMod ACReset]
                    Filling id _ -> Many [ Select id
                                         , AutocompleteMod ACReset]
                key ->
                  AutocompleteMod <| ACQuery m.complete.value

          Deselected ->
            case event.keyCode of
              Key.Enter -> Entry.createFindSpace m
              Key.Up -> SetCenter <| Viewport.moveUp m.center
              Key.Down -> SetCenter <| Viewport.moveDown m.center
              Key.Left -> SetCenter <| Viewport.moveLeft m.center
              Key.Right -> SetCenter <| Viewport.moveRight m.center
              _ -> Selection.selectByLetter m event.keyCode

          Dragging _ _ _ _ -> NoChange

    (EntryInputMsg target, _) ->
      Entry.updateValue target


    ------------------------
    -- mouse
    ------------------------

    -- The interaction between the different mouse states is a little
    -- tricky. RecordClick needs to be a global handler, and so it would
    -- typicaly fire at the same time as NodeClick (which is set on a
    -- Node). We use stopPropagating the prevent them from interacting.

    (GlobalClick event, _) ->
      if event.button == Defaults.leftButton
      then Many [ AutocompleteMod ACReset
                , Enter False <| Creating (Viewport.toAbsolute m event.pos)]
      else NoChange

    (NodeClickDown node event, _) ->
      if event.button == Defaults.leftButton
      then Drag node.id event.pos False m.state
      else NoChange

    (DragNodeMove id mousePos, _) ->
      case m.state of
        Dragging id startVPos _ origState ->
          let xDiff = mousePos.x-startVPos.vx
              yDiff = mousePos.y-startVPos.vy
              (m2, _) = G.moveSubgraph m id xDiff yDiff in
          Many [ SetViewNodes m2.nodes
               -- update the drag so we offset correctly next time
               , Drag id {vx=mousePos.x, vy=mousePos.y} True origState ]
        _ -> NoChange

    (NodeClickUp id event, _) ->
      if event.button == Defaults.leftButton
      then
        case m.state of
          Dragging id startVPos hasMoved origState ->
            if hasMoved
            then
              let xDiff = event.pos.vx-startVPos.vx
                  yDiff = event.pos.vy-startVPos.vy
                  (m2, root) = G.moveSubgraph m id xDiff yDiff in
                Many
                  [ -- final x/y update, tiny diff like in DragNodeMove
                    SetViewNodes m2.nodes
                  , SetState origState
                  , RPC ([UpdateNodePosition root.id root.pos], FocusSame)]
            else Select id
          _ -> Debug.crash "it can never not be dragging"
      else NoChange


    -----------------
    -- Buttons
    -----------------
    (ClearGraph, _) ->
      Many [ RPC ([DeleteAll], FocusNothing), Deselect]

    (SaveTestButton, _) ->
      MakeCmd saveTest

    (AddRandom, _) ->
      Many [ RandomGraph.makeRandomChange m, Deselect]

    (RPCCallBack focus calls (Ok (nodes)), _) ->
      let m2 = { m | backingNodes = nodes, nodes = nodes }
          m3 = G.recalculateView m2
          -- we should calculate this later, but we can't right now
          newState =
            case focus of
              FocusNext id -> enterNext m3 (G.getNodeExn m3 id)
              FocusExact id -> enterExact m3 (G.getNodeExn m3 id)
              FocusSame ->
                case m.state of
                  Selecting id -> if G.getNode m3 id == Nothing then Deselect else NoChange
                  _ -> NoChange
              FocusNothing -> Deselect

      in Many [ SetBackingNodes nodes
              , AutocompleteMod ACReset
              , ClearError
              , newState
              ]

    (PhantomCallBack _ _ (Ok (nodes)), _) ->
      SetPhantoms nodes

    (SaveTestCallBack (Ok msg), _) ->
      Error <| "Success! " ++ msg



    ------------------------
    -- plumbing
    ------------------------
    (RPCCallBack _ _ (Err (Http.BadStatus error)), _) ->
      Error <| "Error: " ++ error.body

    (RPCCallBack _ _ (Err (Http.NetworkError)), _) ->
      Error <| "Network error: is the server running?"

    (PhantomCallBack _ _ (Err (Http.BadStatus error)), _) ->
      SetPhantoms Dict.empty

    (PhantomCallBack _ _ (Err (Http.NetworkError)), _) ->
      Error <| "Network error: is the server running?"

    (SaveTestCallBack (Err err), _) ->
      Error <| "Error: " ++ (toString err)

    (FocusEntry _, _) ->
      NoChange

    (FocusAutocompleteItem _, _) ->
      NoChange

    t -> Error <| "Dark Client Error: nothing for " ++ (toString t)


-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let keySubs =
        [onWindow "keydown"
           (JSD.map GlobalKeyPress Keyboard.Event.decodeKeyboardEvent)]
      dragSubs =
        case m.state of
          -- we use IDs here because the node will change
          -- before they're triggered
          Dragging id offset _ _ ->
            [ Mouse.moves (DragNodeMove id)]
          _ -> []
  in Sub.batch
    (List.concat [keySubs, dragSubs])


