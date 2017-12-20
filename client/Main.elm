port module Main exposing (..)


-- builtins
import Maybe

-- lib
import Json.Decode as JSD
import Http
import Keyboard.Event
import Keyboard.Key as Key
import Navigation
import Mouse
import List.Extra as LE

-- dark
import RPC exposing (rpc, saveTest, integrationRpc)
import Types exposing (..)
import View
import Defaults
import Runtime as RT
import Entry
import Autocomplete as AC
import Viewport
import Window.Events exposing (onWindow)
import VariantTesting exposing (parseVariantTestsFromQueryString)
import Util
import AST
import Selection
import Runtime
import Toplevel as TL
import Analysis
import Util exposing (deMaybe)
import IntegrationTest


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
      m2 = { m | complete = AC.init (List.map flag2function complete)
               , tests = tests
               , toplevels = []
           }
      shouldRunIntegrationTest =
        "/admin/integration_test" == location.pathname
      integrationTestName = String.dropRight 10 location.hostname
  in
    if shouldRunIntegrationTest
    then m2 ! [integrationRpc m integrationTestName]
    else m2 ! [rpc m FocusNothing []]


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

-- cursor2mod : Model -> EntryCursor -> Modification
-- cursor2mod m cursor =
--   let ns = G.orderedNodes m in
--   Many [ Enter cursor
--        , case cursor of
--            Filling id (ResultHole _) ->
--              AutocompleteMod <| ACFilterByLiveValue ((G.getNodeExn m id).liveValue)
--            Filling _ (ParamHole _ p _) ->
--              Many [ AutocompleteMod <| ACFilterByParamType p.tipe ns
--                   , AutocompleteMod <| ACOpen False ]
--            Creating _ ->
--              NoChange
--        ]
--
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
      NoChange -> m ! []
      TriggerIntegrationTest name ->
        let expect = IntegrationTest.trigger name in
        { m | integrationTestState = expect } ! []
      EndIntegrationTest ->
        let expectationFn =
            case m.integrationTestState of
              IntegrationTestExpectation fn -> fn
              IntegrationTestFinished _ ->
                Debug.crash "Attempted to end integration test but one ran + was already finished"
              NoIntegrationTest ->
                Debug.crash "Attempted to end integration test but none was running"
            result = expectationFn m
        in
        { m | integrationTestState = IntegrationTestFinished result } ! []

      MakeCmd cmd -> m ! [cmd]
      SetState state ->
        -- DOES NOT RECALCULATE VIEW
        { m | state = state } ! []
      Select tlid hid ->
        { m | state = Selecting tlid hid } ! []
      Enter entry ->
        let varnames =
              case entry of
                Creating _ -> []
                Filling tlid id ->
                  Analysis.getAvailableVarnames m tlid id
            showFunctions =
              case entry of
                Creating _ -> True
                Filling tlid eid ->
                  let tl = TL.getTL m tlid in
                  case TL.holeType tl eid of
                    ExprHole _ -> True
                    FieldHole _ -> False
                    BindHole _ -> False
                    SpecHole _ -> False
                    DBColNameHole _ -> False
                    DBColTypeHole _ -> False
                    NotAHole -> -- only show functions iff. we've re-entered an expression
                      case tl.data of
                        TLHandler h -> TL.isExpression h eid
                        _ -> False
            lv =
              case entry of
                Creating _ -> Nothing
                Filling tlid eid ->
                  let tl = TL.getTL m tlid in
                  let obj =
                    case TL.holeType tl eid of
                      ExprHole h ->
                        let handler = deMaybe <| TL.asHandler tl
                            p = AST.parentOf_ eid handler.ast
                        in
                            case p of
                              Just (Thread tid exprs) ->
                                let ids  = List.map (AST.toID) exprs
                                    selfi = LE.elemIndex eid ids
                                    prev = Maybe.map (\x -> x - 1) selfi
                                in
                                    Maybe.andThen (\i -> LE.getAt i ids) prev
                              _ ->
                                Nothing

                      FieldHole h ->
                        let handler = deMaybe <| TL.asHandler tl
                            p = AST.parentOf eid handler.ast
                        in
                            case p of
                              FieldAccess id obj _ ->
                                Just <| AST.toID obj
                              _ ->
                                Nothing
                      _ ->
                        Nothing
                  in
                      Maybe.andThen
                        (Analysis.getLiveValue m tlid)
                        obj

            (complete, acCmd) =
              processAutocompleteMods m [ ACSetAvailableVarnames varnames
                                        , ACShowFunctions showFunctions
                                        , ACFilterByLiveValue lv
                                        ]
        in
      ({ m | state = Entering entry, complete = complete
      }) ! ([acCmd, Entry.focusEntry])

      SetToplevels tls tlars ->
        { m | toplevels = tls
            , analysis = tlars
        } ! []

      SetCenter c ->
        { m | center = c } ! []
      Drag tlid offset hasMoved state ->
        { m | state = Dragging tlid offset hasMoved state } ! []
      Deselect -> { m | state = Deselected } ! []
      AutocompleteMod mod ->
        let (complete, cmd) = processAutocompleteMods m [mod]
        in ({ m | complete = complete }
            , cmd)
      -- applied from left to right
      Many mods -> List.foldl (updateMod origm) (m, Cmd.none) mods
  in
    (newm, Cmd.batch [cmd, newcmd])

processAutocompleteMods : Model -> List AutocompleteMod -> (Autocomplete, Cmd Msg)
processAutocompleteMods m mods =
  let complete = List.foldl
        (\mod complete -> AC.update mod complete)
        m.complete
        mods
  in (complete, AC.focusItem complete.index)

-- Figure out from the string and the state whether this '.' means field
-- access.
isFieldAccessDot : State -> String -> Bool
isFieldAccessDot state baseStr =
  -- We know from the fact that this function is called that there has
  -- been a '.' entered. However, it might not be in baseStr, so
  -- canonicalize it first.
  let str = Util.replace "\\.*$" "" baseStr in
  case state of
    Entering _ ->
      if String.startsWith "\"" str
      || Runtime.isInt str
      then False
      else True
    _ -> False

update_ : Msg -> Model -> Modification
update_ msg m =
  case (msg, m.state) of

    (GlobalKeyPress event, state) ->
      if event.ctrlKey && (event.keyCode == Key.Z || event.keyCode == Key.Y)
      then
        case event.keyCode of
          Key.Z -> RPC ([Undo], FocusNothing)
          Key.Y -> RPC ([Redo], FocusNothing)
          _ -> NoChange
      else
        case state of
          Selecting tlid hid ->
            case event.keyCode of
              Key.Backspace ->
                case hid of
                  Nothing -> Many [ RPC ([DeleteTL tlid], FocusNothing), Deselect ]
                  Just i -> Selection.delete m tlid i
              Key.Escape ->
                Deselect
              Key.Enter ->
                if event.shiftKey
                then
                  case TL.getTL m tlid |> .data of
                    TLDB _ ->
                      RPC ([ AddDBCol tlid (gid ()) (gid ())]
                          , FocusNext tlid Nothing)
                    TLHandler _ ->
                      NoChange
                else
                  case hid of
                    Just i -> Selection.enter m tlid i
                    Nothing -> Selection.downLevel m tlid hid
              Key.Up -> Selection.upLevel m tlid hid
              Key.Down -> Selection.downLevel m tlid hid
              Key.Right -> Selection.nextSibling m tlid hid
              Key.Left -> Selection.previousSibling m tlid hid
              Key.Tab -> Selection.nextHole m tlid hid
              Key.O ->
                if event.ctrlKey
                then Selection.upLevel m tlid hid
                else NoChange
              Key.I ->
                if event.ctrlKey
                then Selection.downLevel m tlid hid
                else NoChange
              Key.N ->
                if event.ctrlKey
                then Selection.nextSibling m tlid hid
                else NoChange
              Key.P ->
                if event.ctrlKey
                then Selection.previousSibling m tlid hid
                else NoChange
              _ -> NoChange

          Entering cursor ->
            if event.ctrlKey
            then
              case event.keyCode of
                Key.P -> AutocompleteMod ACSelectUp
                Key.N -> AutocompleteMod ACSelectDown
                Key.Enter ->
                  if AC.isLargeStringEntry m.complete
                  then Entry.submit m cursor Entry.ContinueThread m.complete.value
                  else if AC.isSmallStringEntry m.complete
                  then
                    Many [ AutocompleteMod (ACAppendQuery "\n")
                         , MakeCmd Entry.focusEntry
                         ]
                  else NoChange
                _ -> NoChange
            else if event.shiftKey && event.keyCode == Key.Enter
            then
              case cursor of
                Filling tlid id ->
                  let tl = TL.getTL m tlid in
                  case tl.data of
                    TLDB _ -> NoChange
                    TLHandler h ->
                      let name = AC.getValue m.complete
                      in Entry.submit m cursor Entry.StartThread name
                Creating _ ->
                  let name = AC.getValue m.complete
                  in Entry.submit m cursor Entry.StartThread name
           else
              case event.keyCode of
                Key.Enter ->
                  if AC.isLargeStringEntry m.complete
                  then AutocompleteMod (ACSetQuery m.complete.value)
                  else
                    let name = AC.getValue m.complete
                    in Entry.submit m cursor Entry.ContinueThread name

                Key.Unknown c ->
                  if event.key == Just "."
                  && isFieldAccessDot m.state m.complete.value
                  then
                    let name = AC.getValue m.complete
                    in Entry.submit m cursor Entry.ContinueThread ("." ++ name)
                  else NoChange

                Key.Escape ->
                  case cursor of
                    Creating _ -> Many [Deselect, AutocompleteMod ACReset]
                    Filling tlid hid ->
                      let tl = TL.getTL m tlid in
                        case tl.data of
                          TLHandler h ->
                            let replacement = AST.closeThread h.ast in
                            if replacement == h.ast
                            then
                              Many [ Select tlid (Just hid)
                                   , AutocompleteMod ACReset]
                            else
                              RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                                  , FocusNext tl.id Nothing)
                          _ ->
                            Many [ Select tlid (Just hid)
                                 , AutocompleteMod ACReset]

                Key.Up -> AutocompleteMod ACSelectUp
                Key.Down -> Many [ AutocompleteMod (ACOpen True)
                                 , AutocompleteMod ACSelectDown]
                Key.Right ->
                  let sp = AC.sharedPrefix m.complete in
                  if sp == "" then NoChange
                  else
                    AutocompleteMod <| ACSetQuery sp

                key ->
                  NoChange


          Deselected ->
            case event.keyCode of
              Key.Enter -> Entry.createFindSpace m
              Key.Up -> SetCenter <| Viewport.moveUp m.center
              Key.Down -> SetCenter <| Viewport.moveDown m.center
              Key.Left -> SetCenter <| Viewport.moveLeft m.center
              Key.Right -> SetCenter <| Viewport.moveRight m.center
              _ -> NoChange

          Dragging _ _ _ _ -> NoChange


    ------------------------
    -- entry node
    ------------------------
    (EntryInputMsg target, _) ->
      -- don't process the autocomplete for '.', as nothing will match
      -- and it will reset the order, losing our spot. The '.' will be
      -- processed by
      if String.endsWith "." target
      && isFieldAccessDot m.state target
      then
        NoChange
      else
        Many [ AutocompleteMod <| ACSetQuery target
             , MakeCmd Entry.focusEntry
             ]

    (EntrySubmitMsg, _) ->
      NoChange -- just keep this here to prevent the page from loading


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
                , Enter (Creating (Viewport.toAbsolute m event.pos))]
      else NoChange

    (ToplevelClickDown tl event, _) ->
      if event.button == Defaults.leftButton
      then Drag tl.id event.pos False m.state
      else NoChange

    (DragToplevel id mousePos, _) ->
      case m.state of
        Dragging tlid startVPos _ origState ->
          let xDiff = mousePos.x-startVPos.vx
              yDiff = mousePos.y-startVPos.vy
              m2 = TL.move tlid xDiff yDiff m in
          Many [ SetToplevels m2.toplevels m2.analysis
               , Drag tlid {vx=mousePos.x, vy=mousePos.y} True origState ]
        _ -> NoChange

    (ToplevelClickUp tlid event, _) ->
      if event.button == Defaults.leftButton
      then
        case m.state of
          Dragging tlid startVPos hasMoved origState ->
            let xDiff = event.pos.vx-startVPos.vx
                yDiff = event.pos.vy-startVPos.vy
                m2 = TL.move tlid xDiff yDiff m
                tl = TL.getTL m2 tlid
            in
            if hasMoved
            then Many
                  [ SetState origState
                  , RPC ([MoveTL tl.id tl.pos], FocusSame)]
            else Select tlid Nothing
          _ -> Debug.crash "it can never not be dragging"
      else NoChange


    -----------------
    -- Buttons
    -----------------
    (ClearGraph, _) ->
      Many [ RPC ([DeleteAll], FocusNothing), Deselect]

    (SaveTestButton, _) ->
      MakeCmd saveTest

    (FinishIntegrationTest, _) ->
      EndIntegrationTest

    -- (AddRandom, _) ->
    --   Many [ RandomGraph.makeRandomChange m, Deselect]
    --
    (RPCCallBack focus extraMod calls (Ok (toplevels, analysis)), _) ->
      let m2 = { m | toplevels = toplevels }
          newState =
            case focus of
              FocusNext tlid pred ->
                let tl = TL.getTL m2 tlid
                    nh = TL.getNextHole tl pred
                in
                    case nh of
                      Just h -> Enter (Filling tlid h)
                      Nothing -> Select tlid Nothing
              FocusExact tlid next ->
                let tl = TL.getTL m2 tlid
                    ht = TL.holeType tl next
                in
                case ht of
                  NotAHole -> Select tlid (Just next)
                  _ -> Enter (Filling tlid next)
              _  -> NoChange
      in Many [ SetToplevels toplevels analysis
              , AutocompleteMod ACReset
              , ClearError
              , newState
              , extraMod -- for testing, maybe more
              ]

    (SaveTestCallBack (Ok msg), _) ->
      Error <| "Success! " ++ msg


    ------------------------
    -- plumbing
    ------------------------
    (RPCCallBack _ _ _ (Err (Http.BadStatus error)), _) ->
      Error <| "Error: " ++ error.body

    (RPCCallBack _ _ _ (Err (Http.NetworkError)), _) ->
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
            [ Mouse.moves (DragToplevel id)]
          _ -> []
  in Sub.batch
    (List.concat [keySubs, dragSubs])


