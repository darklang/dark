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
import PageVisibility
-- import List.Extra as LE
import String.Extra as SE
import Time
import Task
import Window

-- dark
import RPC
import Types exposing (..)
import View
import Clipboard
import Defaults
import Editor
import Runtime as RT
import Entry
import Autocomplete as AC
import Viewport
import Window.Events exposing (onWindow)
import VariantTesting exposing (parseVariantTestsFromQueryString)
import Util
import Pointer as P
import AST
import Selection
import Runtime
import Toplevel as TL
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
parseLocation : Navigation.Location -> Maybe Pos
parseLocation loc =
  let removeHash = String.dropLeft 1 loc.hash in -- remove "#"
  case String.split "&" removeHash of -- split on delimiter
    [xpart, ypart] ->
      let trimmedx = String.dropLeft 1 xpart -- remove 'x'
          trimmedy = String.dropLeft 1 ypart -- remove 'y'
      in
      case (String.toInt trimmedx, String.toInt trimmedy) of
        (Ok x, Ok y) ->
          let newPosition = { x = x, y = y } in
          Just newPosition
        _ -> Nothing
    _ -> Nothing

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
  , infix = fn.infix
  }

init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init {editorState, complete} location =
  let editor = case editorState of
            Just e -> e
            Nothing -> Defaults.defaultEditor
      tests = case parseVariantTestsFromQueryString location.search of
                  Just t  -> t
                  Nothing -> []
      m = Editor.editor2model editor

      visibilityTask =
        Task.perform PageVisibilityChange PageVisibility.visibility
      center =
        case parseLocation location of
          Nothing -> m.center
          Just c -> c
      m2 = { m | complete = AC.init (List.map flag2function complete)
               , tests = tests
               , toplevels = []
               , center = center
           }
      shouldRunIntegrationTest =
        "/admin/integration_test" == location.pathname
      integrationTestName =
        location.hostname
        |> SE.replace ".localhost" ""
        |> SE.replace ".integration-tests" ""
        |> SE.replace ".dark-dev" ""
        |> SE.replace ".dark-local-gcp" ""
        |> SE.replace ":8000" ""
        |> SE.replace ":9000" ""
  in
    if shouldRunIntegrationTest
    then m2 ! [RPC.integrationRPC m integrationTestName, visibilityTask]
    else m2 ! [RPC.rpc m FocusNothing [], visibilityTask]


-----------------------
-- ports, save Editor state in LocalStorage
-----------------------
port setStorage : Editor -> Cmd a

-----------------------
-- updates
-----------------------

processFocus : Model -> Focus -> Modification
processFocus m focus =
  case focus of
    FocusNext tlid pred ->
      let tl = TL.getTL m tlid
          next = TL.getNextBlank tl pred in
      case next of
        Just p -> Enter (Filling tlid p)
        Nothing -> Select tlid Nothing
    FocusExact tlid p ->
      case p of
        PFilled _ _ -> Select tlid (Just p)
        PBlank _ _ -> Enter (Filling tlid p)
    Refocus tlid ->
      Select tlid Nothing
    FocusFirstAST tlid ->
      let tl = TL.getTL m tlid
          next = TL.getFirstASTBlank tl in
      case next of
        Just p -> Enter (Filling tlid p)
        Nothing -> Select tlid Nothing
    FocusSame ->
      case unwrapState m.state of
        Selecting tlid mp ->
          case (TL.get m tlid, mp) of
            (Just tl, Just p) ->
                if TL.isValidPointer tl p
                then NoChange
                else Deselect
            _ -> Deselect
        Entering (Filling tlid p) ->
          case TL.get m tlid of
            Just tl ->
              if TL.isValidPointer tl p
              then NoChange
              else Deselect
            _ -> Deselect
        _ -> NoChange
    FocusNothing -> Deselect
    -- used instead of focussame when we've already done the focus
    FocusNoChange -> NoChange



update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
  let mods = update_ msg m
      (newm, newc) = updateMod mods (m, Cmd.none)
  in
    ({ newm | lastMsg = msg
            , lastMod = mods}
     , Cmd.batch [newc, m |> Editor.model2editor |> setStorage])

updateMod : Modification -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMod mod (m, cmd) =
  let _ = if m.integrationTestState /= NoIntegrationTest
          then Debug.log "mod update" mod
          else mod
      closeThreads newM =
        -- close open threads in the previous TL
        m.state
        |> tlidOf
        |> Maybe.andThen (TL.get m)
        |> Maybe.map (\tl ->
            case tl.data of
              TLHandler h ->
                let replacement = AST.closeThread h.ast in
                if replacement == h.ast
                then []
                else
                  let newH = { h | ast = replacement }
                      calls = [ SetHandler tl.id tl.pos newH]
                  -- call RPC on the new model
                  in [RPC.rpc newM FocusSame calls]
              _ -> [])
        |> Maybe.withDefault []
        |> \rpc -> if tlidOf newM.state == tlidOf m.state
                   then []
                   else rpc
  in
  let (newm, newcmd) =
    case mod of
      Error e -> { m | error = Just e} ! []
      ClearError -> { m | error = Nothing} ! []

      RPC (calls, focus) ->
        -- immediately update the model based on SetHandler and focus, if
        -- possible
        let hasNonHandlers =
              List.any (\c -> case c of
                                SetHandler _ _ _ ->
                                  False
                                _ -> True) calls

        -- Set to true to disable for now. There is a bug where
        -- autocomplete results no longer work. It's related to the ID
        -- of the toplevel (and also the lower-level exprs/blanks/etc)
        -- changing ID on every tick (until something happens like
        -- deselecting). Haven't figured it out, so just disable to fix
        -- for now.
        -- Update: turns out the situation without this is unbearable,
        -- and the breakage is worth it.
        in if hasNonHandlers
           then
             m ! [RPC.rpc m focus calls]
           else
             let localM =
                   List.foldl (\call m ->
                     case call of
                       SetHandler tlid pos h ->
                         TL.upsert m
                           {id = tlid, pos = pos, data = TLHandler h, cursor = 0 }
                       _ -> m) m calls

                 (withFocus, wfCmd) =
                   updateMod (Many [ AutocompleteMod ACReset
                                   , processFocus localM focus
                                   ])
                             (localM, Cmd.none)
              in withFocus ! [wfCmd, RPC.rpc withFocus FocusNoChange calls]

      GetAnalysisRPC ->
        m ! [RPC.getAnalysisRPC]

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

      Select tlid p ->
        let newM = { m | state = Selecting tlid p } in
        newM ! closeThreads newM

      Deselect ->
        let newM = { m | state = Deselected }
        in newM ! (closeThreads newM)

      Enter entry ->
        let target =
              case entry of
                Creating _ -> Nothing
                Filling tlid p -> Just (tlid, p)

            (complete, acCmd) =
              processAutocompleteMods m [ ACSetTarget target ]
            newM = { m | state = Entering entry, complete = complete }
        in
        newM ! (closeThreads newM ++ [acCmd, Entry.focusEntry newM])


      SetToplevels tls tlars globals ->
        let m2 = { m | toplevels = tls
                     , analysis = tlars
                     , globals = globals }
            (complete, acCmd) =
              processAutocompleteMods m2 [ ACRegenerate ]
        in
        { m2 | complete = complete } ! [acCmd]

      SetCenter c ->
        { m | center = c } ! []
      SetHover p ->
        let nhovering = (p :: m.hovering) in
        { m | hovering = nhovering } ! []
      ClearHover p ->
        let nhovering = List.filter (\m -> m /= p) m.hovering in
        { m | hovering = nhovering } ! []
      SetCursor tlid cur ->
        let newM = TL.update m tlid (\tl -> { tl | cursor = cur })
        in
            newM ! []
      CopyToClipboard clipboard ->
        let newM = { m | clipboard = clipboard } in
        newM ! [setStorage (Editor.model2editor newM)]
      SetStorage editorState ->
        m ! [setStorage editorState]
      Drag tlid offset hasMoved state ->
        { m | state = Dragging tlid offset hasMoved state } ! []
      TweakModel fn ->
        fn m ! []
      AutocompleteMod mod ->
        let (complete, cmd) = processAutocompleteMods m [mod]
        in ({ m | complete = complete }
            , cmd)
      -- applied from left to right
      Many mods -> List.foldl updateMod (m, Cmd.none) mods
  in
    (newm, Cmd.batch [cmd, newcmd])

processAutocompleteMods : Model -> List AutocompleteMod -> (Autocomplete, Cmd Msg)
processAutocompleteMods m mods =
  let _ = if m.integrationTestState /= NoIntegrationTest
          then Debug.log "autocompletemod update" mods
          else mods
      complete = List.foldl
        (\mod complete -> AC.update m mod complete)
        m.complete
        mods
      focus = case unwrapState m.state of
                Entering _ -> AC.focusItem complete.index
                _ -> Cmd.none
      _ = if m.integrationTestState /= NoIntegrationTest
          then
            let i = complete.index
                val = AC.getValue complete
            in Debug.log "autocompletemod result: "
                (toString complete.index ++ " => " ++ val)
          else ""
  in (complete, focus)

-- Figure out from the string and the state whether this '.' means field
-- access.
isFieldAccessDot : State -> String -> Bool
isFieldAccessDot state baseStr =
  -- We know from the fact that this function is called that there has
  -- been a '.' entered. However, it might not be in baseStr, so
  -- canonicalize it first.
  let str = Util.replace "\\.*$" "" baseStr
      intOrString = String.startsWith "\"" str || Runtime.isInt str
  in
  case state of
    Entering (Creating _) -> not intOrString
    Entering (Filling tlid p) ->
      (P.typeOf p == Expr
       || P.typeOf p == Field)
      && not intOrString
    _ -> False

update_ : Msg -> Model -> Modification
update_ msg m =
  let _ = if m.integrationTestState /= NoIntegrationTest
          then Debug.log "msg update" msg
          else msg in
  case (msg, m.state) of

    (GlobalKeyPress event, state) ->
      if event.ctrlKey && (event.keyCode == Key.Z || event.keyCode == Key.Y)
      then
        case event.keyCode of
          Key.Z -> RPC ([Undo], FocusSame)
          Key.Y -> RPC ([Redo], FocusSame)
          _ -> NoChange
      else
        case state of
          Selecting tlid p ->
            case event.keyCode of
              Key.Delete ->
                case p of
                  Nothing ->
                    Many [ RPC ([DeleteTL tlid], FocusNothing), Deselect ]
                  Just i -> Selection.delete m tlid i
              Key.Backspace ->
                case p of
                  Nothing ->
                    Many [ RPC ([DeleteTL tlid], FocusNothing), Deselect ]
                  Just i -> Selection.delete m tlid i
              Key.Escape ->
                case p of
                  -- if we're selecting an expression,
                  -- go 'up' to selecting the toplevel only
                  Just p ->
                    Select tlid Nothing
                  -- if we're selecting a toplevel only, deselect.
                  Nothing ->
                    Deselect
              Key.Enter ->
                if event.shiftKey
                then
                  let tl = TL.getTL m tlid in
                  case tl.data of
                    TLDB _ ->
                      RPC ([ AddDBCol tlid (gid ()) (gid ())]
                          , FocusNext tlid Nothing)
                    TLHandler h ->
                      case p of
                        Just p ->
                          let replacement = AST.addThreadBlank (P.idOf p) h.ast in
                          RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                              , FocusNext tlid (Just p))
                        Nothing -> NoChange
                else
                  case p of
                    Just i -> Selection.enter m tlid i
                    Nothing -> Selection.selectDownLevel m tlid p
              Key.Up -> Selection.selectUpLevel m tlid p
              Key.Down -> Selection.selectDownLevel m tlid p
              Key.Right ->
                if event.altKey
                then Selection.moveCursorForwardInTime m tlid
                else Selection.selectNextSibling m tlid p
              Key.Left ->
                if event.altKey
                then Selection.moveCursorBackInTime m tlid
                else Selection.selectPreviousSibling m tlid p
              Key.Tab ->
                case p of
                  Just pp ->
                    if event.shiftKey
                    then Selection.selectPrevBlank m tlid p
                    else Selection.selectNextBlank m tlid p
                  Nothing ->
                    if event.shiftKey
                    then Selection.selectPrevToplevel m (Just tlid)
                    else Selection.selectNextToplevel m (Just tlid)
              Key.O ->
                if event.ctrlKey
                then Selection.selectUpLevel m tlid p
                else NoChange
              Key.I ->
                if event.ctrlKey
                then Selection.selectDownLevel m tlid p
                else NoChange
              Key.N ->
                if event.ctrlKey
                then Selection.selectNextSibling m tlid p
                else NoChange
              Key.P ->
                if event.ctrlKey
                then Selection.selectPreviousSibling m tlid p
                else NoChange
              Key.C ->
                if event.ctrlKey
                then
                  let tl = TL.getTL m tlid
                  in
                      Clipboard.copy m tl p
                else NoChange
              Key.V ->
                if event.ctrlKey
                then
                  let tl = TL.getTL m tlid in
                  case p of
                    Nothing ->
                      case TL.rootOf tl of
                        Just i -> Clipboard.paste m tl i
                        Nothing -> NoChange
                    Just i ->
                      Clipboard.paste m tl i
                else NoChange
              Key.X ->
                if event.ctrlKey
                then
                  case p of
                    Nothing -> NoChange
                    Just i ->
                      let tl = TL.getTL m tlid in
                      Clipboard.cut m tl i
                else NoChange
              _ -> NoChange

          Entering cursor ->
            if event.ctrlKey
            then
              case event.keyCode of
                Key.P -> AutocompleteMod ACSelectUp
                Key.N -> AutocompleteMod ACSelectDown
                Key.V ->
                  case cursor of
                    Creating pos -> Clipboard.newFromClipboard m pos
                    Filling tlid p ->
                      let tl = TL.getTL m tlid in
                      Clipboard.paste m tl p
                Key.Enter ->
                  if AC.isLargeStringEntry m.complete
                  then Entry.submit m cursor Entry.ContinueThread m.complete.value
                  else if AC.isSmallStringEntry m.complete
                  then
                    Many [ AutocompleteMod (ACAppendQuery "\n")
                         , MakeCmd (Entry.focusEntry m)
                         ]
                  else NoChange
                _ -> NoChange
            else if event.shiftKey && event.keyCode == Key.Enter
            then
              case cursor of
                Filling tlid p ->
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
                Key.Spacebar ->

                  -- if we're trying to create a database via our magic
                  -- incantation, then we should be able to do that without
                  -- submitting
                  if String.startsWith "DB" m.complete.value
                  || m.complete.value == "="
                  || AC.isStringEntry m.complete
                  then
                    -- TODO: appending isnt right when we're editing, we want
                    -- to put this wherever the cursor is. We need to allow the
                    -- inputbox to do it's thing.
                    AutocompleteMod <| ACAppendQuery " "
                  else
                    let name = AC.getValue m.complete
                    in Entry.submit m cursor Entry.ContinueThread name

                Key.Enter ->
                  if AC.isLargeStringEntry m.complete
                  then AutocompleteMod (ACSetQuery m.complete.value)
                  else
                    let name = AC.getValue m.complete
                    in Entry.submit m cursor Entry.ContinueThread name

                Key.Tab ->
                  case cursor of
                    Filling tlid p ->
                      if event.shiftKey
                      then
                        Selection.enterPrevBlank m tlid (Just p)
                      else
                        Selection.enterNextBlank m tlid (Just p)
                    Creating _ ->
                      NoChange

                Key.Unknown c ->
                  if event.key == Just "."
                  && isFieldAccessDot m.state m.complete.value
                  then
                    let name = AC.getValue m.complete
                    in Entry.submit m cursor Entry.ContinueThread (name ++ ".")
                  else NoChange

                Key.Escape ->
                  case cursor of
                    Creating _ -> Many [Deselect, AutocompleteMod ACReset]
                    Filling tlid p ->
                      let tl = TL.getTL m tlid in
                        case tl.data of
                          TLHandler h ->
                            let replacement = AST.closeThread h.ast in
                            if replacement == h.ast
                            then
                              Many [ Select tlid (Just p)
                                   , AutocompleteMod ACReset]
                            else
                              RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                                  , FocusNext tl.id Nothing)
                          _ ->
                            Many [ Select tlid (Just p)
                                 , AutocompleteMod ACReset]

                Key.Up -> AutocompleteMod ACSelectUp
                Key.Down -> AutocompleteMod ACSelectDown
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

              Key.A ->
                if event.ctrlKey
                then Viewport.pageLeft m.center
                else NoChange
              Key.E ->
                if event.ctrlKey
                then Viewport.pageRight m.center
                else NoChange
              Key.F ->
                if event.ctrlKey
                then Viewport.pageDown m.center
                else NoChange
              Key.B ->
                if event.ctrlKey
                then Viewport.pageUp m.center
                else NoChange

              Key.PageUp -> Viewport.pageUp m.center
              Key.PageDown -> Viewport.pageDown m.center

              Key.Up -> Viewport.moveUp m.center
              Key.Down -> Viewport.moveDown m.center
              Key.Left -> Viewport.moveLeft m.center
              Key.Right -> Viewport.moveRight m.center

              Key.Zero -> Viewport.moveTo { x=0, y=0 }

              Key.Tab -> Selection.selectNextToplevel m Nothing
              _ -> NoChange

          Dragging _ _ _ _ -> NoChange


    ------------------------
    -- entry node
    ------------------------
    (EntryInputMsg target, _) ->
      -- There are functions to convert strings to and from quoted
      -- strings, but they don't get to run until later, so hack
      -- around the problem here.
      let query = if target == "\""
                  then "\"\""
                  else target in
      -- don't process the autocomplete for '.', as nothing will match
      -- and it will reset the order, losing our spot. The '.' will be
      -- processed by
      if String.endsWith "." query
      && isFieldAccessDot m.state query
      then
        NoChange
      else
        Many [ AutocompleteMod <| ACSetQuery query
             , MakeCmd (Entry.focusEntry m)
             ]

    (EntrySubmitMsg, _) ->
      NoChange -- just keep this here to prevent the page from loading


    ------------------------
    -- mouse
    ------------------------

    -- The interaction between the different mouse states is a little
    -- tricky. We use stopPropagating a lot of ensure the interactions
    -- work, but also combine multiple interactions into single
    -- handlers to make it easier to choose between the desired
    -- interactions (esp ToplevelClickUp)

    (GlobalClick event, _) ->
      if event.button == Defaults.leftButton
      then Many [ AutocompleteMod ACReset
                , Enter (Creating (Viewport.toAbsolute m event.pos))]
      else NoChange


    (AutocompleteClick value, state) ->
      case unwrapState m.state of
        Entering cursor ->
          Entry.submit m cursor Entry.ContinueThread value
        _ -> NoChange


    ------------------------
    -- dragging
    ------------------------
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
          Many [ SetToplevels m2.toplevels m2.analysis m2.globals
               , Drag tlid {vx=mousePos.x, vy=mousePos.y} True origState ]
        _ -> NoChange

    (ToplevelClickUp tlid mPointer event, _) ->
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
            -- this is where we select toplevels
            else Select tlid mPointer
          _ ->
            -- if we stopPropagative the TopleveClickDown
            NoChange
      else NoChange

    (MouseEnter id _, _) ->
      SetHover id

    (MouseLeave p _, _) ->
      ClearHover p

    -----------------
    -- Buttons
    -----------------
    (ClearGraph, _) ->
      Many [ RPC ([DeleteAll], FocusNothing), Deselect]

    (ToggleSync, _) ->
      TweakModel (\m -> { m | syncEnabled = not m.syncEnabled })

    (SaveTestButton, _) ->
      MakeCmd RPC.saveTestRPC

    (FinishIntegrationTest, _) ->
      EndIntegrationTest

    -----------------
    -- URL stuff
    -----------------
    (NavigateTo url, _) ->
      MakeCmd (Navigation.newUrl url)

    (RPCCallback focus extraMod calls (Ok (toplevels, analysis, globals)), _) ->
      if focus == FocusNoChange
      then
        Many [ SetToplevels toplevels analysis globals
             , extraMod -- for testing, maybe more
             , MakeCmd (Entry.focusEntry m)
             ]
      else
        let m2 = { m | toplevels = toplevels }
            newState = processFocus m2 focus
        -- TODO: can make this much faster by only receiving things that have
        -- been updated
        in Many [ SetToplevels toplevels analysis globals
                , AutocompleteMod ACReset
                , ClearError
                , newState
                , extraMod -- for testing, maybe more
                ]

    (SaveTestRPCCallback (Ok msg), _) ->
      Error <| "Success! " ++ msg

    (GetAnalysisRPCCallback (Ok (analysis, globals)), _) ->
      SetToplevels m.toplevels analysis globals

    ------------------------
    -- plumbing
    ------------------------
    (RPCCallback _ _ _ (Err (Http.BadStatus error)), _) ->
      Error <| "Error: " ++ error.body

    (RPCCallback _ _ _ (Err (Http.NetworkError)), _) ->
      Error <| "Network error: is the server running?"

    (RPCCallback _ _ _ _, _) as t ->
      Error <| "Dark Client Error: unknown error: " ++ (toString t)

    (SaveTestRPCCallback (Err err), _) ->
      Error <| "Error: " ++ (toString err)

    (GetAnalysisRPCCallback (Err (Http.NetworkError)), _) ->
      NoChange

    (GetAnalysisRPCCallback (Err err), _) as t ->
      Error <| "Dark Client GetAnalysis Error: unknown error: " ++ (toString t)

    (WindowResize x y, _) ->
      -- just receiving the subscription will cause a redraw, which uses
      -- the native sizing function.
      NoChange

    (FocusEntry _, _) ->
      NoChange

    (NothingClick _, _) ->
      NoChange

    (FocusAutocompleteItem _, _) ->
      NoChange

    (LocationChange loc, _) ->
      case (parseLocation loc) of
        Nothing -> NoChange
        Just c -> SetCenter c

    (ClockTick action time, _) ->
      case action of
        RefreshAnalyses ->
          GetAnalysisRPC

    (Initialization, _) ->
      NoChange

    (AddRandom, _) ->
      NoChange

    (PageVisibilityChange vis, _) ->
      TweakModel (\m -> { m | visibility = vis })

    (PageFocusChange vis, _) ->
      TweakModel (\m -> { m | visibility = vis })

-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let keySubs =
        [onWindow "keydown"
           (JSD.map GlobalKeyPress Keyboard.Event.decodeKeyboardEvent)]
      resizes = [Window.resizes (\{height,width} ->
                                    WindowResize height width)]
      dragSubs =
        case m.state of
          -- we use IDs here because the node will change
          -- before they're triggered
          Dragging id offset _ _ ->
            [ Mouse.moves (DragToplevel id)]
          _ -> []
      timers =
        case m.visibility of
          PageVisibility.Hidden -> []
          PageVisibility.Visible ->
            if m.syncEnabled
            then [ Time.every Time.second (ClockTick RefreshAnalyses) ]
            else []

      visibility =
        [ PageVisibility.visibilityChanges PageVisibilityChange
        , onWindow "focus" (JSD.succeed (PageFocusChange PageVisibility.Visible))
        , onWindow "blur" (JSD.succeed (PageFocusChange PageVisibility.Hidden))]
  in Sub.batch
    (List.concat [keySubs, dragSubs, resizes, timers, visibility])


