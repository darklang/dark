port module Main exposing (..)


-- builtins
import Maybe

-- lib
import Json.Decode as JSD
import Http
import Keyboard.Key as Key
import Navigation
import Mouse
import PageVisibility
import Dict
import String
-- import List.Extra as LE
import String.Extra as SE
import Time
import Task
import Window

-- dark
import RPC
import Types exposing (..)
import Prelude exposing (..)
import View
import Clipboard
import Defaults
import Editor
import DarkKeyboard
import Refactor exposing (WrapLoc(..))
import Runtime as RT
import Entry
import Autocomplete as AC
import Viewport
import FeatureFlags
import Functions
import Window.Events exposing (onWindow)
import VariantTesting exposing (parseVariantTestsFromQueryString)
import Util
import Pointer as P
import Blank as B
import AST
import Selection
import Sync
import DB
import Runtime
import Toplevel as TL
import Util
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
parseLocation : Navigation.Location -> UrlFragmentData
parseLocation loc =
  let unstructured = loc.hash
                   |> String.dropLeft 1 -- remove "#"
                   |> String.split "&"
                   |> List.map (String.split "=")
                   |> List.filterMap
                      (\arr ->
                        case arr of
                          a :: b :: [] -> Just (String.toLower a, b)
                          _ -> Nothing)
                   |> Dict.fromList
      center =
        case (Dict.get "x" unstructured, Dict.get "y" unstructured) of
          (Just x, Just y) ->
            case (String.toInt x, String.toInt y) of
              (Ok x, Ok y) -> Just { x = x, y = y }
              _  -> Nothing
          _ -> Nothing
      editedFn =
        case (Dict.get "fn" unstructured) of
          Just sid ->
            case String.toInt sid of
              Ok id -> Just <| TLID id
              _ -> Nothing
          _ -> Nothing
  in
      { center = center, editedFn = editedFn }

flag2function : FlagFunction -> Function
flag2function fn =
  { name = fn.name
  , description = fn.description
  , returnTipe = RT.str2tipe fn.return_type
  , parameters = List.map (\p -> { name = p.name
                                 , tipe = RT.str2tipe p.tipe
                                 , block_args = p.block_args
                                 , optional = p.optional
                                 , description = p.description})
                          fn.parameters
  , infix = fn.infix
  , previewExecutionSafe = fn.preview_execution_safe
  }

init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init {editorState, complete} location =
  let savedEditor = Editor.fromString editorState

      m0 = Editor.editor2model savedEditor
      savedCursorState = m0.cursorState
      savedCurrentPage = m0.currentPage
      m = { m0 | cursorState = Deselected, currentPage = Toplevels }

      tests =
        case parseVariantTestsFromQueryString location.search of
          Just t  -> t
          Nothing -> []

      urlFragmentData =
        parseLocation location

      center =
        case urlFragmentData.center of
          Nothing -> m.center
          Just c -> c

      nextPage =
        case urlFragmentData.editedFn of
          Nothing -> savedCurrentPage
          Just tlid -> Fn tlid

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

      builtins =
        List.map flag2function complete

      m2 = { m | builtInFunctions = builtins
               , complete = AC.init builtins
               , tests = tests
               , toplevels = []
               , center = center
           }

  in
    if shouldRunIntegrationTest
    then m2 ! [RPC.integrationRPC m integrationTestName, visibilityTask]
    else m2 ! [RPC.rpc m (FocusPageAndCursor nextPage savedCursorState) RPC.emptyParams, visibilityTask]


-----------------------
-- ports, save Editor state in LocalStorage
-----------------------
port mousewheel : ((List Int) -> msg) -> Sub msg
port recordError : (String -> msg) -> Sub msg
port setStorage : String -> Cmd a

-----------------------
-- updates
-----------------------

processFocus : Model -> Focus -> Modification
processFocus m focus =
  case focus of
    FocusNext tlid pred ->
      let tl = TL.getTL m tlid
          predPd = Maybe.andThen (TL.find tl) pred
          next = TL.getNextBlank tl predPd in
      case next of
        Just pd -> Enter (Filling tlid (P.toID pd))
        Nothing -> Select tlid pred
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
                else Select tlid Nothing
            (Just tl, Nothing) -> Select tlid Nothing
            _ -> Deselect
        Entering (Filling tlid id) ->
          case TL.get m tlid of
            Just tl ->
              if TL.isValidID tl id
              then NoChange
              else Select tlid Nothing
            _ -> Deselect
        _ -> NoChange
    FocusPageAndCursor page cs ->
      let setCS = SetCursorState cs
          noTarget = ACSetTarget Nothing
          target tuple = ACSetTarget (Just tuple)
          tlOnPage tl =
            case page of
              Toplevels ->
                case tl.data of
                  TLHandler _ -> True
                  TLDB _ -> True
                  TLFunc _ -> False
              Fn id ->
                tl.id == id
          (nextCursor, acTarget) =
            case cs of
              Selecting tlid mId ->
                case (TL.get m tlid, mId) of
                  (Just tl, Just id) ->
                      if TL.isValidID tl id && (tlOnPage tl)
                      then (setCS, target (tlid, TL.findExn tl id))
                      else (Deselect, noTarget)
                  (Just tl, Nothing) -> (setCS, noTarget)
                  _ -> (Deselect, noTarget)
              Entering (Filling tlid id) ->
                case TL.get m tlid of
                  Just tl ->
                    if TL.isValidID tl id && (tlOnPage tl)
                    then (setCS, target (tlid, TL.findExn tl id))
                    else (Deselect, noTarget)
                  _ -> (Deselect, noTarget)
              Dragging tlid _ _ _  ->
                case TL.get m tlid of
                  Just tl ->
                    if tlOnPage tl
                    then (setCS, noTarget)
                    else (Deselect, noTarget)
                  _ -> (Deselect, noTarget)
              _ -> (Deselect, noTarget)
      in
          Many [ SetCurrentPage page
               , nextCursor
               , AutocompleteMod acTarget
               ]
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
                      ops = [ SetHandler tl.id tl.pos newH]
                      params = RPC.opsParams ops
                  -- call RPC on the new model
                  in [RPC.rpc newM FocusSame params]
              _ -> [])
        |> Maybe.withDefault []
        |> \rpc -> if tlidOf newM.cursorState == tlidOf m.cursorState
                   then []
                   else rpc
  in
  let (newm, newcmd) =
    let handleRPC params focus =
          -- immediately update the model based on SetHandler and focus, if
          -- possible
          let hasNonHandlers =
                List.any (\c -> case c of
                                  SetHandler _ _ _ ->
                                    False
                                  _ -> True) params.ops

          in
          if hasNonHandlers
          then
            m ! [RPC.rpc m focus params]
          else
            let localM =
                  List.foldl (\call m ->
                    case call of
                      SetHandler tlid pos h ->
                        TL.upsert m
                          { id = tlid
                          , pos = pos
                          , data = TLHandler h
                          , cursor = 0
                          }
                      _ -> m) m params.ops

                (withFocus, wfCmd) =
                  updateMod (Many [ AutocompleteMod ACReset
                                  , processFocus localM focus
                                  ])
                            (localM, Cmd.none)
             in
             withFocus ! [wfCmd, RPC.rpc withFocus FocusNoChange params]

    in
    case mod of
      Error e -> { m | error = Just e} ! []
      ClearError -> { m | error = Nothing} ! []

      RPC (ops, focus) ->
        handleRPC (RPC.opsParams ops) focus
      RPCFull (params, focus) ->
        handleRPC params focus

      GetAnalysisRPC ->
        Sync.fetch m

      NoChange -> m ! []
      TriggerIntegrationTest name ->
        let expect = IntegrationTest.trigger name in
        { m | integrationTestState = expect } ! []
      EndIntegrationTest ->
        let expectationFn =
            case m.integrationTestState of
              IntegrationTestExpectation fn -> fn
              IntegrationTestFinished _ ->
                impossible "Attempted to end integration test but one ran + was already finished"
              NoIntegrationTest ->
                impossible "Attempted to end integration test but none was running"
            result = expectationFn m
        in
        { m | integrationTestState = IntegrationTestFinished result } ! []

      MakeCmd cmd -> m ! [cmd]

      SetCursorState cursorState ->
        let newM = { m | cursorState = cursorState } in
        newM ! [Entry.focusEntry newM]

      SetCurrentPage page ->
        if m.currentPage == page
        then m ! []
        else
          let newM = { m | currentPage = page, cursorState = Deselected }
          in newM ! closeThreads newM

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


      SetToplevels tls tlars globals userFuncs f404s unlocked updateCurrentTL ->
        let m2 = { m | toplevels = tls
                     , analysis = tlars
                     , globals = globals
                     , userFunctions = userFuncs
                     , f404s = f404s
                     , unlockedDBs = unlocked
                 }
            -- If the server is slow, we don't a jittery experience.
            m3 = case tlidOf m.cursorState of
                   Just tlid ->
                     if updateCurrentTL
                     then m2
                     else
                       let tl = TL.getTL m tlid in
                       case tl.data of
                         TLDB _ -> TL.upsert m2 tl
                         TLHandler _ -> TL.upsert m2 tl
                         TLFunc _ -> m2
                   Nothing ->
                     m2

            (complete, acCmd) =
              processAutocompleteMods m3 [ ACRegenerate ]
        in
        { m3 | complete = complete } ! [acCmd]

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

    GlobalKeyPress devent ->
      let event = devent.standard in
      if event.metaKey && event.keyCode == Key.Z
      then
        case tlidOf m.cursorState of
          Just tlid ->
            let undo = if event.shiftKey
                       then RPC ([RedoTL tlid], FocusSame)
                       else RPC ([UndoTL tlid], FocusSame)
            in
            case TL.getTL m tlid |> TL.asDB of
              Just db ->
                if DB.isLocked m tlid
                -- We could do it on the server but it's really hard
                -- atm. To do it on the server, efficiently, we'd create
                -- a canvas with almost all the ops, check if the tlid
                -- is a DB, then recreate the canvas with all the ops
                -- (such that preprocess with the DB works). That way we
                -- load from disk/db once, but still check server side.
                then Error "Cannot undo/redo in locked DBs"
                else undo
              Nothing -> undo
          Nothing -> NoChange
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
                          case (TL.findExn tl id) of
                            PExpr _ ->
                              let replacement = AST.addThreadBlank id f.ast
                              in
                                  RPC ( [ SetFunction { f | ast = replacement}]
                                      , FocusNext tlid (Just id))
                            PParamTipe _ ->
                              let replacement = Functions.extend f
                                  newCalls = Refactor.addNewFunctionParameter m f
                              in
                                  RPC ( [SetFunction replacement] ++ newCalls, FocusNext tlid (Just id))
                            PParamName _ ->
                              let replacement = Functions.extend f
                                  newCalls = Refactor.addNewFunctionParameter m f
                              in
                                  RPC ( [SetFunction replacement] ++ newCalls, FocusNext tlid (Just id))
                            PFnName _ ->
                              let replacement = Functions.extend f
                                  newCalls = Refactor.addNewFunctionParameter m f
                              in
                                  RPC ( [SetFunction replacement] ++ newCalls, FocusNext tlid (Just id))
                            _ ->
                              NoChange
                        Nothing -> NoChange

                else
                  case mId of
                    Just id -> Selection.enter m tlid id
                    Nothing -> Selection.selectDownLevel m tlid mId
              Key.Up -> -- NB: see `stopKeys` in ui.html
                if event.shiftKey
                then Selection.selectUpLevel m tlid mId
                else Selection.moveUp m tlid mId
              Key.Down -> -- NB: see `stopKeys` in ui.html
                if event.shiftKey
                then Selection.selectDownLevel m tlid mId
                else Selection.moveDown m tlid mId
              Key.Right ->
                if event.altKey
                then Selection.moveCursorBackInTime m tlid
                else Selection.moveRight m tlid mId
              Key.Left ->
                if event.altKey
                then Selection.moveCursorForwardInTime m tlid
                else Selection.moveLeft m tlid mId
              Key.Tab ->  -- NB: see `stopKeys` in ui.html
                if event.shiftKey
                then Selection.selectPrevBlank m tlid mId
                else Selection.selectNextBlank m tlid mId
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
                else if False
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id
                      in
                      Refactor.wrap m tl pd WIfCond
                else
                  NoChange
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
              Key.B ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id
                      in
                      Refactor.wrap m tl pd WLetBody
                else
                  NoChange
              Key.L ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id
                      in
                      Refactor.wrap m tl pd WLetRHS
                else
                  NoChange
              Key.T ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id
                      in
                      Refactor.wrap m tl pd WIfThen
                else
                  NoChange
              Key.E ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id
                      in
                      Refactor.wrap m tl pd WIfElse
                else
                  NoChange
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
                         , MakeCmd (Entry.focusEntry m)
                         ]
                  else NoChange
                _ -> NoChange
            else if event.ctrlKey
            then
              case event.keyCode of
                Key.V ->
                  case cursor of
                    Creating pos -> Clipboard.newFromClipboard m pos
                    Filling tlid p ->
                      let tl = TL.getTL m tlid in
                      Clipboard.paste m tl p
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
                Key.Spacebar ->  -- NB: see `stopKeys` in ui.html
                  -- without submitting
                  if m.complete.value == "="
                  || AC.isStringEntry m.complete
                  then
                    NoChange
                  else
                    let name = AC.getValue m.complete
                    in Entry.submit m cursor Entry.ContinueThread name

                Key.Enter ->
                  if AC.isLargeStringEntry m.complete
                  then AutocompleteMod (ACSetQuery m.complete.value)
                  else
                    case cursor of
                      Creating pos ->
                        case AC.highlighted m.complete of
                          Just (ACOmniAction action) ->
                            Entry.submitOmniAction m pos action
                          _ ->
                            let name = AC.getValue m.complete
                            in Entry.submit m cursor Entry.ContinueThread name
                      Filling _ _ ->
                        let name = AC.getValue m.complete
                        in Entry.submit m cursor Entry.ContinueThread name
                Key.Tab ->  -- NB: see `stopKeys` in ui.html
                  case cursor of
                    Filling tlid p ->
                      if AC.isLargeStringEntry m.complete
                      then
                        case devent.selectionStart of
                          Just idx ->
                            let newQ = SE.insertAt "\t" (idx + 1) m.complete.value in
                            AutocompleteMod <| ACSetQuery newQ
                          Nothing -> NoChange
                      else
                        let content = AC.getValue m.complete
                            hasContent = content
                                       |> String.length
                                       |> (<) 0
                        in
                        if event.shiftKey
                        then
                          if hasContent
                          then NoChange
                          else Selection.enterPrevBlank m tlid (Just p)
                        else
                          if hasContent
                          then
                            Entry.submit m cursor Entry.ContinueThread content
                          else
                            Selection.enterNextBlank m tlid (Just p)
                    Creating _ -> NoChange

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

                Key.Up -> AutocompleteMod ACSelectUp -- NB: see `stopKeys` in ui.html
                Key.Down -> AutocompleteMod ACSelectDown -- NB: see `stopKeys` in ui.html
                Key.Right ->
                  let sp = AC.sharedPrefix m.complete in
                  if sp == "" then NoChange
                  else
                    AutocompleteMod <| ACSetQuery sp

                key ->
                  NoChange


          Deselected ->
            -- Don't move viewport when editing fns
            if not (m.currentPage == Toplevels)
            then NoChange
            else
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

              Key.Up -> Viewport.moveUp m.center -- NB: see `stopKeys` in ui.html
              Key.Down -> Viewport.moveDown m.center -- NB: see `stopKeys` in ui.html
              Key.Left -> Viewport.moveLeft m.center
              Key.Right -> Viewport.moveRight m.center

              Key.Zero -> Viewport.moveTo { x=0, y=0 }

              Key.Tab -> Selection.selectNextToplevel m Nothing -- NB: see `stopKeys` in ui.html
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


    MouseWheel deltaCoords ->
      let delta = case deltaCoords of
            x::y::_ -> { x=x, y=y }
            _ -> { x=0, y=0 }
          destination = { x= m.center.x + delta.x, y= m.center.y + delta.y }
      in
          case m.currentPage of
            Toplevels -> Viewport.moveTo destination
            Fn _ -> NoChange

    DataMouseEnter tlid idx _ ->
      SetHover <| tlCursorID tlid idx

    DataMouseLeave tlid idx _ ->
      ClearHover <| tlCursorID tlid idx


    ------------------------
    -- dragging
    ------------------------
    DragToplevel _ mousePos ->
      case m.cursorState of
        Dragging draggingTLID startVPos _ origCursorState ->
          let xDiff = mousePos.x-startVPos.vx
              yDiff = mousePos.y-startVPos.vy
              m2 = TL.move draggingTLID xDiff yDiff m in
          Many [ SetToplevels
                   m2.toplevels
                   m2.analysis
                   m2.globals
                   m2.userFunctions
                   m2.f404s
                   m2.unlockedDBs
                   True
               , Drag
                   draggingTLID
                   { vx=mousePos.x
                   , vy=mousePos.y}
                   True
                   origCursorState
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

    ExecuteFunctionButton tlid id ->
      let tl = TL.getTL m tlid in
      RPCFull ({ ops = []
               , executableFns = [(tlid, id, tl.cursor)]
               }, FocusNoChange)

    DataClick tlid idx _ ->
      case m.cursorState of
        Dragging _ _ _ origCursorState ->
          SetCursorState origCursorState
        _ -> SetCursor tlid idx


    -----------------
    -- Buttons
    -----------------
    ToggleSync ->
      TweakModel Sync.toggle

    SaveTestButton ->
      MakeCmd RPC.saveTestRPC

    FinishIntegrationTest ->
      EndIntegrationTest

    ------------------------
    -- feature flags
    ------------------------
    StartFeatureFlag ->
      FeatureFlags.start m

    EndFeatureFlag ffID ->
      FeatureFlags.end m ffID

    SliderChange id ->
      Many [ FeatureFlags.commitSlider m id
           , TweakModel Sync.enable ]

    SliderMoving id value ->
      Many [ TweakModel Sync.disable
           , FeatureFlags.updateSlider m id value
           ]

    ------------------------
    -- Functions
    ------------------------
    EditFunction ->
      Selection.startEditingFn m

    ReturnToMainCanvas ->
      MakeCmd (Navigation.modifyUrl "/admin/ui")

    -----------------
    -- URL stuff
    -----------------
    NavigateTo url ->
      MakeCmd (Navigation.newUrl url)

    RPCCallback focus extraMod calls
      (Ok (toplevels, analysis, globals, userFuncs, f404s, unlocked)) ->
      if focus == FocusNoChange
      then
        Many [ SetToplevels toplevels analysis globals userFuncs f404s unlocked False
             , extraMod -- for testing, maybe more
             , MakeCmd (Entry.focusEntry m)
             ]
      else
        let m2 = { m | toplevels = toplevels, userFunctions = userFuncs }
            newState = processFocus m2 focus
        -- TODO: can make this much faster by only receiving things that have
        -- been updated
        in Many [ SetToplevels toplevels analysis globals userFuncs f404s unlocked True
                , AutocompleteMod ACReset
                , ClearError
                , newState
                , extraMod -- for testing, maybe more
                ]

    SaveTestRPCCallback (Ok msg) ->
      Error <| "Success! " ++ msg

    GetAnalysisRPCCallback (Ok (analysis, globals, f404s, unlocked)) ->
      if m.syncState.enabled
      then
        Many [ TweakModel Sync.markResponseInModel
             , SetToplevels m.toplevels analysis globals m.userFunctions f404s unlocked False
             ]
      else
        NoChange

    ------------------------
    -- plumbing
    ------------------------
    RPCCallback _ _ _ (Err (Http.BadStatus error)) ->
      Error <| error.body

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
      let urlFragmentData = parseLocation loc
          page =
            case urlFragmentData.editedFn of
              Just id ->
                case Functions.find m id of
                  Just uf -> [SetCurrentPage (Fn uf.tlid)]
                  _ -> [SetCurrentPage Toplevels]
              Nothing -> [SetCurrentPage Toplevels]
          center =
            case urlFragmentData.center of
              Just pos ->
                if pos /= m.center
                then SetCenter pos
                else NoChange
              Nothing -> NoChange

      in
      Many (center :: page)

    TimerFire action time  ->
      case action of
        RefreshAnalyses ->
          GetAnalysisRPC
        CheckUrlHashPosition ->
          Viewport.maybeUpdateUrl m

    Initialization ->
      NoChange

    AddRandom ->
      NoChange

    PageVisibilityChange vis ->
      TweakModel (\m -> { m | visibility = vis })

    PageFocusChange vis ->
      TweakModel (\m -> { m | visibility = vis })

    CreateHandlerFrom404 (space, path, modifier, _) ->
      let anId = gtlid ()
          aPos = m.center
          aHandler =
            { ast = B.new ()
            , spec =
              { module_ = B.newF space
              , name = B.newF path
              , modifier = B.newF modifier
              , types =
                { input = B.new ()
                , output = B.new ()
                }
              }
            }
      in
          RPC ([SetHandler anId aPos aHandler], FocusNothing)


-----------------------
-- SUBSCRIPTIONS
-----------------------
subscriptions : Model -> Sub Msg
subscriptions m =
  let keySubs =
        [onWindow "keydown"
           (JSD.map GlobalKeyPress DarkKeyboard.decodeDarkKeyboardEvent)]
      resizes = [Window.resizes (\{height,width} ->
                                    WindowResize height width)]
      dragSubs =
        case m.cursorState of
          -- we use IDs here because the node will change
          -- before they're triggered
          Dragging id offset _ _ ->
            [ Mouse.moves (DragToplevel id)]
          _ -> []

      syncTimer =
        case m.visibility of
          PageVisibility.Hidden -> []
          PageVisibility.Visible ->
            if m.syncState.enabled
            then [ Time.every Time.second (TimerFire RefreshAnalyses) ]
            else []

      urlTimer =
        [Time.every Time.second (TimerFire CheckUrlHashPosition)]

      onError = [recordError JSError]

      visibility =
        [ PageVisibility.visibilityChanges PageVisibilityChange
        , onWindow "focus" (JSD.succeed (PageFocusChange PageVisibility.Visible))
        , onWindow "blur" (JSD.succeed (PageFocusChange PageVisibility.Hidden))]

      mousewheelSubs = [mousewheel MouseWheel]

  in Sub.batch
    (List.concat [ keySubs
                 , dragSubs
                 , resizes
                 , syncTimer
                 , urlTimer
                 , visibility
                 , onError
                 , mousewheelSubs])
