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
import Refactor
import Runtime as RT
import Entry
import Autocomplete as AC
import Viewport
import FeatureFlags
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
  let savedEditor = Editor.fromString editorState

      m0 = Editor.editor2model savedEditor
      savedCursorState = m0.cursorState
      m = { m0 | cursorState = Deselected}

      tests = case parseVariantTestsFromQueryString location.search of
                  Just t  -> t
                  Nothing -> []

      center =
        case parseLocation location of
          Nothing -> m.center
          Just c -> c

      visibilityTask =
        Task.perform PageVisibilityChange PageVisibility.visibility


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

      m2 = { m | complete = AC.init (List.map flag2function complete)
               , tests = tests
               , toplevels = []
               , center = center
           }

  in
    if shouldRunIntegrationTest
    then m2 ! [RPC.integrationRPC m integrationTestName, visibilityTask]
    else m2 ! [RPC.rpc m (FocusCursorState savedCursorState) []
              , visibilityTask]


-----------------------
-- ports, save Editor state in LocalStorage
-----------------------
port setStorage : String -> Cmd a
port recordError : (String -> msg) -> Sub msg

-----------------------
-- updates
-----------------------

processFocus : Model -> Focus -> Modification
processFocus m focus =
  case focus of
    FocusNext tlid pred ->
      let tl = TL.getTL m tlid
          pd = Maybe.andThen (TL.find tl) pred
          next = TL.getNextBlank tl pd in
      case next of
        Just pd -> Enter (Filling tlid (P.toID pd))
        Nothing -> Select tlid Nothing
    FocusExact tlid id ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id in
          if P.isBlank pd
          then Enter (Filling tlid id)
          else Select tlid (Just id)
    FocusSame ->
      case unwrapCursorState m.cursorState of
        Selecting tlid mId ->
          case (TL.get m tlid, mId) of
            (Just tl, Just id) ->
                if TL.isValidID tl id
                then NoChange
                else Deselect
            _ -> Deselect
        Entering (Filling tlid id) ->
          case TL.get m tlid of
            Just tl ->
              if TL.isValidID tl id
              then NoChange
              else Deselect
            _ -> Deselect
        _ -> NoChange
    FocusCursorState cs ->
      let setCS = SetCursorState cs in
      case cs of
        Selecting tlid mId ->
          case (TL.get m tlid, mId) of
            (Just tl, Just id) ->
                if TL.isValidID tl id
                then setCS
                else Deselect
            (Just tl, Nothing) -> setCS
            _ -> Deselect
        Entering (Filling tlid id) ->
          case TL.get m tlid of
            Just tl ->
              if TL.isValidID tl id
              then setCS
              else Deselect
            _ -> Deselect
        _ -> setCS
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
     , Cmd.batch [newc, m
                        |> Editor.model2editor
                        |> Editor.toString
                        |> setStorage])

updateMod : Modification -> (Model, Cmd Msg) -> (Model, Cmd Msg)
updateMod mod (m, cmd) =
  let _ = if m.integrationTestState /= NoIntegrationTest
          then Debug.log "mod update" mod
          else mod
      closeThreads newM =
        -- close open threads in the previous TL
        m.cursorState
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
        |> \rpc -> if tlidOf newM.cursorState == tlidOf m.cursorState
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
      SetCursorState cursorState ->
        -- DOES NOT RECALCULATE VIEW
        { m | cursorState = cursorState } ! []

      Select tlid p ->
        let newM = { m | cursorState = Selecting tlid p } in
        newM ! closeThreads newM

      Deselect ->
        let newM = { m | cursorState = Deselected }
        in newM ! (closeThreads newM)

      Enter entry ->
        let target =
              case entry of
                Creating _ -> Nothing
                Filling tlid id ->
                  let tl = TL.getTL m tlid
                      pd = TL.findExn tl id
                  in
                  Just (tlid, pd)

            (complete, acCmd) =
              processAutocompleteMods m [ ACSetTarget target ]
            newM = { m | cursorState = Entering entry, complete = complete }
        in
        newM ! (closeThreads newM ++ [acCmd, Entry.focusEntry newM])


      SetToplevels tls tlars globals userFuncs ->
        let m2 = { m | toplevels = tls
                     , analysis = tlars
                     , globals = globals
                     , userFunctions = userFuncs
                 }
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
        { m | clipboard = clipboard } ! []
      Drag tlid offset hasMoved state ->
        { m | cursorState = Dragging tlid offset hasMoved state } ! []
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
      focus = case unwrapCursorState m.cursorState of
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
isFieldAccessDot : Model -> String -> Bool
isFieldAccessDot m baseStr =
  -- We know from the fact that this function is called that there has
  -- been a '.' entered. However, it might not be in baseStr, so
  -- canonicalize it first.
  let str = Util.replace "\\.*$" "" baseStr
      intOrString = String.startsWith "\"" str || Runtime.isInt str
  in
  case m.cursorState of
    Entering (Creating _) -> not intOrString
    Entering (Filling tlid id) ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id in
      (P.typeOf pd == Expr
       || P.typeOf pd == Field)
      && not intOrString
    _ -> False

update_ : Msg -> Model -> Modification
update_ msg m =
  let _ = if m.integrationTestState /= NoIntegrationTest
          then Debug.log "msg update" msg
          else msg in
  case msg of

    GlobalKeyPress event ->
      if event.ctrlKey && (event.keyCode == Key.Z || event.keyCode == Key.Y)
      then
        case event.keyCode of
          Key.Z -> RPC ([Undo], FocusSame)
          Key.Y -> RPC ([Redo], FocusSame)
          _ -> NoChange
      else
        case m.cursorState of
          Selecting tlid mId ->
            case event.keyCode of
              Key.Delete -> Selection.delete m tlid mId
              Key.Backspace -> Selection.delete m tlid mId
              Key.Escape ->
                case mId of
                  -- if we're selecting an expression,
                  -- go 'up' to selecting the toplevel only
                  Just _ ->
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
                      case mId of
                        Just id ->
                          let replacement = AST.addThreadBlank id h.ast in
                          RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                              , FocusNext tlid (Just id))
                        Nothing -> NoChange
                    TLFunc f ->
                      case mId of
                        Just id ->
                          let replacement = AST.addThreadBlank id f.ast in
                          RPC ( [ SetFunction { f | ast = replacement}]
                              , FocusNext tlid (Just id))
                        Nothing -> NoChange
                else
                  case mId of
                    Just id -> Selection.enter m tlid id
                    Nothing -> Selection.selectDownLevel m tlid mId
              Key.Up -> Selection.selectUpLevel m tlid mId
              Key.Down -> Selection.selectDownLevel m tlid mId
              Key.Right ->
                if event.altKey
                then Selection.moveCursorForwardInTime m tlid
                else Selection.selectNextSibling m tlid mId
              Key.Left ->
                if event.altKey
                then Selection.moveCursorBackInTime m tlid
                else Selection.selectPreviousSibling m tlid mId
              Key.Tab ->
                case mId of
                  Just id ->
                    if event.shiftKey
                    then Selection.selectPrevBlank m tlid mId
                    else Selection.selectNextBlank m tlid mId
                  Nothing ->
                    if event.shiftKey
                    then Selection.selectPrevToplevel m (Just tlid)
                    else Selection.selectNextToplevel m (Just tlid)
              Key.O ->
                if event.ctrlKey
                then Selection.selectUpLevel m tlid mId
                else NoChange
              Key.I ->
                if event.ctrlKey
                then Selection.selectDownLevel m tlid mId
                else NoChange
              Key.N ->
                if event.ctrlKey
                then Selection.selectNextSibling m tlid mId
                else NoChange
              Key.P ->
                if event.ctrlKey
                then Selection.selectPreviousSibling m tlid mId
                else NoChange
              Key.C ->
                if event.ctrlKey
                then
                  let tl = TL.getTL m tlid
                      mPd = Maybe.map (TL.findExn tl) mId
                  in
                  Clipboard.copy m tl mPd
                else NoChange
              Key.V ->
                if event.ctrlKey
                then
                  let tl = TL.getTL m tlid in
                  case mId of
                    Nothing ->
                      case TL.rootOf tl of
                        Just pd -> Clipboard.paste m tl (P.toID pd)
                        Nothing -> NoChange
                    Just id ->
                      Clipboard.paste m tl id
                else NoChange
              Key.X ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id
                      in
                      Clipboard.cut m tl pd
                else NoChange
              Key.F ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id
                      in
                      Refactor.extractFunction m tl pd
                else
                  NoChange
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
                    TLFunc f -> NoChange
                Creating _ ->
                  let name = AC.getValue m.complete
                  in Entry.submit m cursor Entry.StartThread name
            else
              case event.keyCode of
                Key.Spacebar ->
                  -- if we're trying to create a database via our magic
                  -- incantation, then we should be able to do that
                  -- without submitting
                  if String.startsWith "DB" m.complete.value
                  || m.complete.value == "="
                  || AC.isStringEntry m.complete
                  then
                    -- TODO: appending isnt right when we're editing, we
                    -- want to put this wherever the cursor is. We need
                    -- to allow the inputbox to do it's thing.
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
                  && isFieldAccessDot m m.complete.value
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
    EntryInputMsg target ->
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
      && isFieldAccessDot m query
      then
        NoChange
      else
        Many [ AutocompleteMod <| ACSetQuery query
             , MakeCmd (Entry.focusEntry m)
             ]

    EntrySubmitMsg ->
      NoChange -- just keep this here to prevent the page from loading


    ------------------------
    -- mouse
    ------------------------

    -- The interaction between the different mouse states is a little
    -- tricky. We use stopPropagating a lot of ensure the interactions
    -- work, but also combine multiple interactions into single
    -- handlers to make it easier to choose between the desired
    -- interactions (esp ToplevelMouseUp)

    AutocompleteClick value ->
      case unwrapCursorState m.cursorState of
        Entering cursor ->
          Entry.submit m cursor Entry.ContinueThread value
        _ -> NoChange


    GlobalClick event ->
      if event.button == Defaults.leftButton && m.currentPage == Toplevels
      then Many [ AutocompleteMod ACReset
                , Enter (Creating (Viewport.toAbsolute m event.pos))]
      else NoChange


    BlankOrMouseEnter _ id _ ->
      SetHover id


    BlankOrMouseLeave _ id _ ->
      ClearHover id


    ------------------------
    -- dragging
    ------------------------
    DragToplevel _ mousePos ->
      case m.cursorState of
        Dragging draggingTLID startVPos _ origCursorState ->
          let xDiff = mousePos.x-startVPos.vx
              yDiff = mousePos.y-startVPos.vy
              m2 = TL.move draggingTLID xDiff yDiff m in
          Many [ SetToplevels m2.toplevels m2.analysis m2.globals m2.userFunctions
               , Drag draggingTLID {vx=mousePos.x, vy=mousePos.y} True origCursorState
               ]
        _ -> NoChange


    ToplevelMouseDown targetTLID event ->
      if event.button == Defaults.leftButton
      then
        let tl = TL.getTL m targetTLID in
            case tl.data of
              TLFunc _ -> NoChange
              _ -> Drag targetTLID event.pos False m.cursorState
      else NoChange


    ToplevelMouseUp targetTLID event ->
      if event.button == Defaults.leftButton
      then
        case m.cursorState of
          Dragging draggingTLID startVPos hasMoved origCursorState ->
            if hasMoved
            then
              let tl = TL.getTL m draggingTLID in
              -- We've been updating tl.pos as mouse moves,
              -- now want to report last pos to server
              -- NB: do *not* stop dragging here because we're using
              --     the dragging state in `ToplevelClick` coming up next
              RPC ([MoveTL draggingTLID tl.pos], FocusNoChange)
            else
              SetCursorState origCursorState
          _ ->
            NoChange
      else NoChange

    ------------------------
    -- clicking
    ------------------------
    BlankOrClick targetTLID targetID _ ->
      case m.cursorState of
        Deselected ->
          Select targetTLID (Just targetID)
        Dragging _ _ _ origCursorState ->
          SetCursorState origCursorState
        Entering cursor ->
          case cursor of
            Filling _ fillingID ->
              if fillingID == targetID
              then
                NoChange
              else
                Select targetTLID (Just targetID)
            _ ->
              Select targetTLID (Just targetID)
        Selecting _ maybeSelectingID ->
          case maybeSelectingID of
            Just selectingID ->
              if selectingID == targetID
              then
                NoChange
              else
                Select targetTLID (Just targetID)
            Nothing ->
              Select targetTLID (Just targetID)


    BlankOrDoubleClick targetTLID targetID _ ->
      Selection.enter m targetTLID targetID


    ToplevelClick targetTLID _ ->
      case m.cursorState of
        Dragging _ _ _ origCursorState ->
          SetCursorState origCursorState
        Selecting selectingTLID _ ->
          if targetTLID == selectingTLID
          then
            Deselect
          else
            Select targetTLID Nothing
        Deselected ->
          Select targetTLID Nothing
        _ ->
          NoChange


    -----------------
    -- Buttons
    -----------------
    ClearGraph ->
      Many [ RPC ([DeleteAll], FocusNothing), Deselect]

    ToggleSync ->
      TweakModel (\m -> { m | syncEnabled = not m.syncEnabled })

    SaveTestButton ->
      MakeCmd RPC.saveTestRPC

    FinishIntegrationTest ->
      EndIntegrationTest

    ------------------------
    -- feature flags
    ------------------------
    StartFeatureFlag ->
      FeatureFlags.start m

    SliderChange id value ->
      FeatureFlags.updateSlider m id value


    -----------------
    -- URL stuff
    -----------------
    NavigateTo url ->
      MakeCmd (Navigation.newUrl url)

    RPCCallback focus extraMod calls (Ok (toplevels, analysis, globals, userFuncs)) ->
      if focus == FocusNoChange
      then
        Many [ SetToplevels toplevels analysis globals userFuncs
             , extraMod -- for testing, maybe more
             , MakeCmd (Entry.focusEntry m)
             ]
      else
        let m2 = { m | toplevels = toplevels }
            newState = processFocus m2 focus
        -- TODO: can make this much faster by only receiving things that have
        -- been updated
        in Many [ SetToplevels toplevels analysis globals userFuncs
                , AutocompleteMod ACReset
                , ClearError
                , newState
                , extraMod -- for testing, maybe more
                ]

    SaveTestRPCCallback (Ok msg) ->
      Error <| "Success! " ++ msg

    GetAnalysisRPCCallback (Ok (analysis, globals)) ->
      SetToplevels m.toplevels analysis globals m.userFunctions

    ------------------------
    -- plumbing
    ------------------------
    RPCCallback _ _ _ (Err (Http.BadStatus error)) ->
      Error <| "Error: " ++ error.body

    RPCCallback _ _ _ (Err (Http.NetworkError)) ->
      Error <| "Network error: is the server running?"

    RPCCallback _ _ _ (Err (Http.BadPayload err response)) ->
      let { body } = response in
      Error <| "RPC decoding error: " ++ err ++ " in " ++ body

    (RPCCallback _ _ _ _) as t ->
      Error <| "Dark Client Error: unknown error: " ++ (toString t)

    SaveTestRPCCallback (Err err) ->
      Error <| "Error: " ++ (toString err)

    GetAnalysisRPCCallback (Err (Http.NetworkError)) ->
      NoChange

    GetAnalysisRPCCallback (Err err) as t ->
      Error <| "Dark Client GetAnalysis Error: unknown error: " ++ (toString t)

    JSError msg ->
      let _ = Debug.log "adasd" msg in
      Error ("Error in JS: " ++ msg)

    WindowResize x y ->
      -- just receiving the subscription will cause a redraw, which uses
      -- the native sizing function.
      NoChange

    FocusEntry _ ->
      NoChange

    NothingClick _ ->
      NoChange

    FocusAutocompleteItem _ ->
      NoChange

    LocationChange loc ->
      case parseLocation loc of
        Nothing -> NoChange
        Just c -> SetCenter c

    ClockTick action time  ->
      case action of
        RefreshAnalyses ->
          GetAnalysisRPC

    Initialization ->
      NoChange

    AddRandom ->
      NoChange

    PageVisibilityChange vis ->
      TweakModel (\m -> { m | visibility = vis })

    PageFocusChange vis ->
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
        case m.cursorState of
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
      onError = [recordError JSError]

      visibility =
        [ PageVisibility.visibilityChanges PageVisibilityChange
        , onWindow "focus" (JSD.succeed (PageFocusChange PageVisibility.Visible))
        , onWindow "blur" (JSD.succeed (PageFocusChange PageVisibility.Hidden))]
  in Sub.batch
    (List.concat [keySubs, dragSubs, resizes, timers, visibility, onError])
