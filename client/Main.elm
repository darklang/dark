port module Main exposing (..)


-- builtins
import Maybe

-- lib
import Json.Decode as JSD
import Json.Encode as JSE
import Json.Encode.Extra as JSEE
import Maybe.Extra as ME
import Http
import Keyboard.Key as Key
import Navigation
import Mouse
import PageVisibility
import String
import Nineteen.String as String
-- import List.Extra as LE
import String.Extra as SE
import Time
import Task
import Window

-- dark
import Analysis
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
import Commands
import Autocomplete as AC
import Viewport
import FeatureFlags
import Functions
import Toplevel
import Window.Events exposing (onWindow)
import VariantTesting exposing (parseVariantTestsFromQueryString)
import Util
import Pointer as P
import Blank as B
import AST
import JSON
import Selection
import Sync
import DB
import Runtime
import Toplevel as TL
import Util
import Url
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
                                 , description = p.description})
                          fn.parameters
  , infix = fn.infix
  , previewExecutionSafe = fn.preview_execution_safe
  , deprecated = fn.deprecated
  }

init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init {editorState, complete} location =
  let savedEditor = Editor.fromString editorState

      m0 = Editor.editor2model savedEditor

      -- these saved values may not be valid yet
      savedCursorState = m0.cursorState
      savedCurrentPage = m0.currentPage
      m = { m0 | cursorState = Deselected
               , currentPage = Defaults.defaultModel |> .currentPage}

      tests =
        case parseVariantTestsFromQueryString location.search of
          Just t  -> t
          Nothing -> []

      page =
        Url.parseLocation m location
        |> Maybe.withDefault m.currentPage

      canvas = m.canvas
      newCanvas =
        case page of
          Toplevels pos -> { canvas | offset = pos }
          Fn _ pos -> { canvas | fnOffset = pos }

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

      isAdmin = False

      builtins =
        List.map flag2function complete

      m2 = { m | builtInFunctions = builtins
               , complete = AC.init builtins isAdmin
               , tests = tests
               , toplevels = []
               , currentPage = page
               , canvas = newCanvas
           }

  in
    if shouldRunIntegrationTest
    then (m2, Cmd.batch [ RPC.integrationRPC m integrationTestName
              , visibilityTask])
    else (m2, Cmd.batch [ RPC.initialLoadRPC
                  (FocusPageAndCursor page savedCursorState)
              -- load the analysis even if the timers are off
              , RPC.getAnalysisRPC []
              , visibilityTask])


-----------------------
-- ports
-----------------------
port mousewheel : ((List Int) -> msg) -> Sub msg
port displayError : (String -> msg) -> Sub msg
port setStorage : String -> Cmd a
port sendRollbar : JSD.Value -> Cmd a
port requestAnalysis : JSE.Value -> Cmd msg
port receiveAnalysis : (String -> msg) -> Sub msg

-----------------------
-- updates
-----------------------

sendTask : Msg -> Cmd Msg
sendTask t =
  Task.succeed t
  |> Task.perform identity

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
          pd = TL.findExn tl id
      in
          if P.isBlank pd || P.toContent pd == Just ""
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
              Toplevels _ ->
                case tl.data of
                  TLHandler _ -> True
                  TLDB _ -> True
                  TLFunc _ -> False
              Fn id _ ->
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
          Many [ SetPage page
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
      closeBlanks newM =
        -- close open threads in the previous TL
        m.cursorState
        |> tlidOf
        |> Maybe.andThen (TL.get m)
        |> Maybe.map (\tl ->
            case tl.data of
              TLHandler h ->
                let replacement = AST.closeBlanks h.ast in
                if replacement == h.ast
                then []
                else
                  let newH = { h | ast = replacement }
                      ops = [ SetHandler tl.id tl.pos newH]
                      params = RPC.opsParams ops
                  -- call RPC on the new model
                  in [RPC.rpc newM FocusSame params]
              TLFunc f ->
                let replacement = AST.closeBlanks f.ast in
                if replacement == f.ast
                then []
                else
                  let newF = { f | ast = replacement }
                      ops = [ SetFunction newF ]
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
                                  SetFunction _ ->
                                    False
                                  _ -> True) params.ops

          in
          if hasNonHandlers
          then
            (m , RPC.rpc m focus params)
          else
            let localM =
                  List.foldl (\call m ->
                    case call of
                      SetHandler tlid pos h ->
                        TL.upsert m
                          { id = tlid
                          , pos = pos
                          , data = TLHandler h
                          }
                      SetFunction f ->
                        Functions.upsert m f
                      _ -> m) m params.ops

                (withFocus, wfCmd) =
                  updateMod (Many [ AutocompleteMod ACReset
                                  , processFocus localM focus
                                  ])
                            (localM, Cmd.none)
             in
             (withFocus, Cmd.batch [wfCmd, RPC.rpc withFocus FocusNoChange params])

    in
    case mod of
      DisplayError e -> ({ m | error = Just e}, Cmd.none)
      DisplayAndReportError e ->
        let json = JSE.object [ ("message", JSE.string e)
                              , ("url", JSE.null)
                              , ("custom", JSE.object [])
                              ]
        in
        ({ m | error = Just e}, sendRollbar json)
      DisplayAndReportHttpError context e ->
        let response =
              case e of
                Http.BadStatus r -> Just r
                Http.BadPayload _ r -> Just r
                _ -> Nothing
            msg =
              case e of
                Http.BadUrl str  -> "Bad url: " ++ str
                Http.Timeout -> "Timeout"
                Http.NetworkError -> "Network error - is the server running?"
                Http.BadStatus response -> "Bad status: " ++ response.status.message
                Http.BadPayload msg _ -> "Bad payload (" ++context ++ "): " ++ msg
            url =
              case e of
                Http.BadUrl str  -> Just str
                Http.Timeout -> Nothing
                Http.NetworkError -> Nothing
                Http.BadStatus response -> Just response.url
                Http.BadPayload _ response -> Just response.url
            shouldRollbar = e /= Http.NetworkError
            json = JSE.object [ ("message"
                                , JSE.string
                                    (msg ++ " (" ++ context ++ ")"))
                              , ("url", JSEE.maybe JSE.string url)
                              , ("custom", JSON.encodeHttpError e)
                              ]
            cmds = if shouldRollbar then [sendRollbar json] else []
        in
        ({ m | error = Just msg } , Cmd.batch cmds)

      ClearError -> ({ m | error = Nothing} , Cmd.none)

      RPC (ops, focus) ->
        handleRPC (RPC.opsParams ops) focus
      RPCFull (params, focus) ->
        handleRPC params focus

      GetAnalysisRPC ->
        Sync.fetch m

      NoChange -> (m, Cmd.none)
      TriggerIntegrationTest name ->
        let expect = IntegrationTest.trigger name in
        ({ m | integrationTestState = expect }, Cmd.none)
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
        ({ m | integrationTestState = IntegrationTestFinished result }, Cmd.none)

      MakeCmd cmd -> (m, cmd)

      SetCursorState cursorState ->
        let newM = { m | cursorState = cursorState } in
        (newM, Entry.focusEntry newM)

      SetPage page ->
        if m.currentPage == page
        then (m, Cmd.none)
        else
          let canvas = m.canvas
          in case (page, m.currentPage) of
            (Toplevels pos2, Toplevels _) ->
              -- scrolling
              ({ m |
                currentPage = page
                , urlState = UrlState pos2
                , canvas = { canvas | offset = pos2 }
                }, Cmd.none)
            (Fn _ pos2, _) ->
              ({ m |
                currentPage = page
                , cursorState = Deselected
                , urlState = UrlState pos2
                , canvas = { canvas | fnOffset = pos2 }
                }, Cmd.none)
            _ ->
              let newM =
                { m |
                  currentPage = page
                  , cursorState = Deselected
                }
              in (newM, Cmd.batch (closeBlanks newM))

      SetCenter center ->
        case m.currentPage of
          Toplevels pos ->
            ({ m | currentPage = Toplevels center }, Cmd.none)
          Fn id pos ->
            ({ m | currentPage = Fn id center }, Cmd.none)


      Select tlid p ->
        let newM = { m | cursorState = Selecting tlid p } in
        (newM , Cmd.batch (closeBlanks newM))

      Deselect ->
        let newM = { m | cursorState = Deselected }
        in (newM, Cmd.batch (closeBlanks newM))

      Enter entry ->
        let target =
              case entry of
                Creating _ -> Nothing
                Filling tlid id ->
                  let tl = TL.getTL m tlid
                      pd = TL.findExn tl id
                  in
                  Just (tlid, pd)

            (m2, acCmd) =
              processAutocompleteMods m [ ACSetTarget target ]
            m3 = { m2 | cursorState = Entering entry }
        in
        (m3, Cmd.batch (closeBlanks m3 ++ [acCmd, Entry.focusEntry m3]))

      SelectCommand tlid id ->
        let m2 = { m | cursorState = SelectingCommand tlid id }
            (m3, acCmd) = processAutocompleteMods m2
                            [ ACEnableCommandMode
                            , ACRegenerate ]
        in
        (m3, Cmd.batch (closeBlanks m3 ++ [acCmd, Entry.focusEntry m3]))


      SetGlobalVariables globals ->
        let m2 = { m | globals = globals } in
        processAutocompleteMods m2 [ ACRegenerate ]

      RemoveToplevel tl ->
        (Toplevel.remove m tl, Cmd.none)

      SetToplevels tls updateCurrent ->
        let m2 = { m | toplevels = tls }
            -- Bring back the TL being edited, so we don't lose work
            -- done since the API call.
            m3 = case tlidOf m.cursorState of
                   Just tlid ->
                     if updateCurrent
                     then m2
                     else
                       let tl = TL.getTL m tlid in
                       case tl.data of
                         TLDB _ -> TL.upsert m2 tl
                         TLHandler _ -> TL.upsert m2 tl
                         TLFunc f -> m2
                   Nothing ->
                     m2
            m4 = { m3 | deletedToplevels =
                          TL.removeByTLID m3.deletedToplevels tls }
        in
        processAutocompleteMods m4 [ ACRegenerate ]

      UpdateToplevels tls updateCurrent ->
        let m2 = TL.upsertAll m tls
            -- Bring back the TL being edited, so we don't lose work
            -- done since the API call.
            m3 = case tlidOf m.cursorState of
                   Just tlid ->
                     if updateCurrent
                     then m2
                     else
                       let tl = TL.getTL m tlid in
                       case tl.data of
                         TLDB _ -> TL.upsert m2 tl
                         TLHandler _ -> TL.upsert m2 tl
                         TLFunc f -> m2
                   Nothing ->
                     m2
            m4 = { m3 | deletedToplevels =
                          TL.removeByTLID m3.deletedToplevels tls }
        in
        processAutocompleteMods m4 [ ACRegenerate ]

      UpdateDeletedToplevels dtls ->
        let m2 = { m | deletedToplevels =
                         TL.upsertAllByTLID m.deletedToplevels dtls
                     , toplevels = TL.removeByTLID m.toplevels dtls
                 }
        in
        processAutocompleteMods m2 [ ACRegenerate ]

      SetDeletedToplevels dtls ->
        let m2 = { m | deletedToplevels = dtls
                     , toplevels = TL.removeByTLID m.toplevels dtls
                 }
        in
        processAutocompleteMods m2 [ ACRegenerate ]


      RequestAnalysis tls ->
        let handlers = TL.handlers tls
            dbs = TL.dbs tls
            userFns = m.userFunctions

            req h =
              let trace = Analysis.getCurrentTrace m h.tlid
                  param t =
                    JSE.object [ ( "handler" , RPC.encodeHandler h)
                               , ( "trace" , RPC.encodeTrace t)
                               , ( "dbs", JSON.encodeList RPC.encodeDB dbs)
                               , ( "user_fns"
                                 , JSON.encodeList RPC.encodeUserFunction userFns)
                               ]
              in
              trace
              |> Maybe.map
                   (\t -> requestAnalysis (param t))
              |> ME.toList

        in
        (m, Cmd.batch
              (handlers
                 |> List.map req
                 |> List.concat))

      UpdateAnalysis id analysis ->
        let m2 = { m | analyses = Analysis.record m.analyses id analysis } in
        processAutocompleteMods m2 [ ACRegenerate ]

      UpdateTraces traces ->
        let m2 = { m | traces = traces } in
        processAutocompleteMods m2 [ ACRegenerate ]

      UpdateTraceFunctionResult tlid traceID callerID fnName hash dval ->
        let m2 =
              Analysis.replaceFunctionResult m tlid traceID callerID fnName hash dval
        in
        processAutocompleteMods m2 [ ACRegenerate ]

      SetUserFunctions userFuncs updateCurrent ->
        let m2 = { m | userFunctions = userFuncs }
            -- Bring back the TL being edited, so we don't lose work
            -- done since the API call.
            m3 = case tlidOf m.cursorState of
                   Just tlid ->
                     if updateCurrent
                     then m2
                     else
                       let tl = TL.getTL m tlid in
                       case tl.data of
                         TLFunc f -> Functions.upsert m2 f
                         TLDB _ -> m2
                         TLHandler _ -> m2
                   Nothing ->
                     m2
        in
        processAutocompleteMods m3 [ ACRegenerate ]

      SetUnlockedDBs unlockedDBs ->
        ({ m | unlockedDBs = unlockedDBs }, Cmd.none)

      Set404s f404s ->
        ({ m | f404s = f404s }, Cmd.none)

      SetHover p ->
        let nhovering = (p :: m.hovering) in
        ({ m | hovering = nhovering }, Cmd.none)
      ClearHover p ->
        let nhovering = List.filter (\m -> m /= p) m.hovering in
        ({ m | hovering = nhovering }, Cmd.none)
      SetCursor tlid cur ->
        let m2 = Analysis.setCursor m tlid cur in
        (m2, Cmd.none)
      CopyToClipboard clipboard ->
        ({ m | clipboard = clipboard }, Cmd.none)
      Drag tlid offset hasMoved state ->
        ({ m | cursorState = Dragging tlid offset hasMoved state }, Cmd.none)
      ExecutingFunctionBegan tlid id ->
        let nexecutingFunctions = m.executingFunctions ++ [(tlid, id)] in
        ({ m | executingFunctions = nexecutingFunctions }, Cmd.none)
      ExecutingFunctionRPC tlid id name ->
        let trace = Analysis.getCurrentTrace m tlid in
        case trace of
          Nothing -> m ! [sendTask (ExecuteFunctionCancel tlid id)]
          Just t ->
            let traceID = t.id
                args = []
                params = { tlid = tlid
                         , callerID = id
                         , traceID = traceID
                         , fnName = name
                         , args = args
                         }
            in
            (m, RPC.executeFunctionRPC params)
      ExecutingFunctionComplete targets ->
        let isComplete target = not <| List.member target targets
            nexecutingFunctions = List.filter isComplete m.executingFunctions in
        ({ m | executingFunctions = nexecutingFunctions }, Cmd.none)
      SetLockedHandlers locked ->
        ({ m | lockedHandlers = locked }, Cmd.none)
      MoveCanvasTo canvas page pos ->
        let canvas2 =
          case page of
            Toplevels _ -> { canvas | offset = pos }
            Fn _ _ -> { canvas | fnOffset = pos }
        in ({ m | canvas = canvas2 }, Cmd.none)
      TweakModel fn ->
        (fn m, Cmd.none)
      AutocompleteMod mod ->
        processAutocompleteMods m [mod]
      -- applied from left to right
      Many mods -> List.foldl updateMod (m, Cmd.none) mods

  in
    (newm, Cmd.batch [cmd, newcmd])

processAutocompleteMods : Model -> List AutocompleteMod -> (Model, Cmd Msg)
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
                SelectingCommand _ _ -> AC.focusItem complete.index
                _ -> Cmd.none
      _ = if m.integrationTestState /= NoIntegrationTest
          then
            let i = complete.index
                val = AC.getValue complete
            in Debug.log "autocompletemod result: "
                (String.fromInt complete.index ++ " => " ++ val)
          else ""
  in ({m | complete = complete}, focus)

-- Figure out from the string and the state whether this '.' means field
-- access.
isFieldAccessDot : Model -> String -> Bool
isFieldAccessDot m baseStr =
  -- We know from the fact that this function is called that there has
  -- been a '.' entered. However, it might not be in baseStr, so
  -- canonicalize it first.
  let str = Util.replace "\\.*$" "" baseStr
      intOrString = String.startsWith "\"" str
                    || RPC.typeOfLiteralString str == TInt
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
      if (event.metaKey || event.ctrlKey) && event.keyCode == Key.Z
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
                then DisplayError "Cannot undo/redo in locked DBs"
                else undo
              Nothing -> undo
          Nothing -> NoChange
      else
        case m.cursorState of
          Selecting tlid mId ->
            let tl = TL.getTL m tlid in
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
                  case tl.data of
                    TLDB _ ->
                      RPC ([ AddDBCol tlid (gid ()) (gid ())]
                          , FocusNext tlid Nothing)
                    TLHandler h ->
                      case mId of
                        Just id ->
                          case (TL.findExn tl id) of
                            PExpr _ ->
                              let replacement = AST.addThreadBlank id h.ast in
                              RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                                  , FocusNext tlid (Just id))
                            PVarBind _ ->
                              case AST.parentOf_ id h.ast of
                                Just (F _ (Lambda _ _)) ->
                                  let replacement = AST.addLambdaBlank id h.ast
                                  in
                                      RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                                          , FocusNext tlid (Just id))
                                _ ->
                                  NoChange
                            PKey _ ->
                              let (nextid, _, replacement) = AST.addObjectLiteralBlanks id h.ast in
                              RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                                  , FocusExact tlid nextid)
                            _ ->
                              NoChange

                        Nothing -> NoChange
                    TLFunc f ->
                      case mId of
                        Just id ->
                          case (TL.findExn tl id) of
                            PExpr _ ->
                              let replacement = AST.addThreadBlank id f.ast in
                              RPC ( [ SetFunction { f | ast = replacement}]
                                  , FocusNext tlid (Just id))
                            PVarBind _ ->
                              case AST.parentOf_ id f.ast of
                                Just (F _ (Lambda _ _)) ->
                                  let replacement = AST.addLambdaBlank id f.ast in
                                  RPC ( [ SetFunction { f | ast = replacement}]
                                      , FocusNext tlid (Just id))
                                _ ->
                                  NoChange
                            PKey _ ->
                              let (nextid, _, replacement) = AST.addObjectLiteralBlanks id f.ast in
                              RPC ( [ SetFunction { f | ast = replacement}]
                                  , FocusExact tlid nextid)
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
              -- Disabled to make room for Windows keyboards
              -- Key.O ->
              --   if event.ctrlKey
              --   then Selection.selectUpLevel m tlid mId
              --   else NoChange
              -- Key.I ->
              --   if event.ctrlKey
              --   then Selection.selectDownLevel m tlid mId
              --   else NoChange
              Key.N ->
                if event.ctrlKey
                then Selection.selectNextSibling m tlid mId
                else NoChange
              Key.P ->
                if event.ctrlKey
                then Selection.selectPreviousSibling m tlid mId
                else NoChange
              Key.C ->
                if event.ctrlKey && event.altKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WIfCond m tl pd
                else if event.ctrlKey
                then
                  let mPd = Maybe.map (TL.findExn tl) mId in
                  Clipboard.copy m tl mPd

                else
                  NoChange
              Key.V ->
                if event.ctrlKey
                then
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
                      let pd = TL.findExn tl id in
                      Clipboard.cut m tl pd
                else NoChange
              Key.F ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.extractFunction m tl pd
                else
                  NoChange
              Key.B ->
                if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WLetBody m tl pd
                else
                  NoChange
              Key.L ->
                if event.ctrlKey && event.shiftKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.extractVariable m tl pd
                else if event.ctrlKey then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WLetRHS m tl pd
                else
                  NoChange
              Key.I ->
                if event.ctrlKey && event.altKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WIfElse m tl pd
                else if event.ctrlKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WIfThen m tl pd
                else
                  NoChange
              Key.E ->
                if event.altKey
                then
                  case mId of
                    Nothing -> NoChange
                    Just id ->
                      let pd = TL.findExn tl id in
                      Refactor.toggleOnRail m tl pd
                else
                  NoChange
              Key.Unknown c -> -- semicolon
                case mId of
                  Nothing -> NoChange
                  Just id ->
                    if event.key == Just ":"
                    then
                      Many [ SelectCommand tlid id
                           , AutocompleteMod <| ACSetQuery ":"
                           ]
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
                  then
                    Entry.submit m cursor Entry.StayHere
                  else if AC.isSmallStringEntry m.complete
                  then
                    Many [ AutocompleteMod (ACAppendQuery "\n")
                         , MakeCmd (Entry.focusEntry m)
                         ]
                  else NoChange
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
                      Entry.submit m cursor Entry.StartThread
                    TLFunc f ->
                      Entry.submit m cursor Entry.StartThread
                Creating _ ->
                  Entry.submit m cursor Entry.StartThread
            else if event.altKey
            then
              case event.keyCode of
                Key.E ->
                  case cursor of
                    Creating pos -> NoChange
                    Filling tlid id ->
                      let tl = TL.getTL m tlid
                          pd = TL.findExn tl id in
                      Refactor.toggleOnRail m tl pd
                _ -> NoChange
            else
              case event.keyCode of
                Key.Spacebar ->  -- NB: see `stopKeys` in ui.html
                  -- without submitting
                  if m.complete.value == "="
                  || AC.isStringEntry m.complete
                  then
                    NoChange
                  else
                    Entry.submit m cursor Entry.GotoNext

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
                            Entry.submit m cursor Entry.StayHere
                      Filling _ _ ->
                        Entry.submit m cursor Entry.StayHere
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
                          then Entry.submit m cursor Entry.GotoNext
                          else Selection.enterNextBlank m tlid (Just p)
                    Creating _ -> NoChange

                Key.Unknown c ->
                  if event.key == Just "."
                  && isFieldAccessDot m m.complete.value
                  then
                    let c = m.complete
                        -- big hack to for Entry.submit to see field
                        -- access
                        newC = { c | value = AC.getValue c ++ "."
                                           , index = -1}
                        newM = { m | complete = newC }
                    in
                    Entry.submit newM cursor Entry.GotoNext
                  else NoChange

                Key.Escape ->
                  case cursor of
                    Creating _ -> Many [Deselect, AutocompleteMod ACReset]
                    Filling tlid p ->
                      let tl = TL.getTL m tlid in
                        case tl.data of
                          TLHandler h ->
                            let replacement = AST.closeBlanks h.ast in
                            if replacement == h.ast
                            then
                              Many [ Select tlid (Just p)
                                   , AutocompleteMod ACReset]
                            else
                              -- TODO: in this case, when filling a keyname on an object, nothing happens which is unexpected
                              RPC ( [ SetHandler tl.id tl.pos { h | ast = replacement}]
                                  , FocusNext tl.id Nothing)
                          _ ->
                            Many [ Select tlid (Just p)
                                 , AutocompleteMod ACReset]

                Key.Up -> AutocompleteMod ACSelectUp -- NB: see `stopKeys` in ui.html
                Key.Down -> AutocompleteMod ACSelectDown -- NB: see `stopKeys` in ui.html
                Key.Right ->
                  AC.selectSharedPrefix m.complete
                Key.Backspace ->
                  case cursor of
                    Filling tlid id ->
                      if m.complete.value == "\"\""
                      then
                        AutocompleteMod <| ACSetQuery ""
                      else
                        NoChange
                    _ -> NoChange
                key ->
                  NoChange


          Deselected ->
            case m.currentPage of
              Fn _ _ -> NoChange -- Don't move viewport when editing fns
              Toplevels center ->
                case event.keyCode of
                  Key.Enter -> Entry.createFindSpace m

                  Key.A ->
                    if event.ctrlKey
                    then Viewport.pageLeft m
                    else NoChange
                  Key.E ->
                    if event.ctrlKey
                    then Viewport.pageRight m
                    else NoChange
                  Key.F ->
                    if event.ctrlKey
                    then Viewport.pageDown m
                    else NoChange
                  Key.B ->
                    if event.ctrlKey
                    then Viewport.pageUp m
                    else NoChange

                  Key.PageUp -> Viewport.pageUp m
                  Key.PageDown -> Viewport.pageDown m

                  Key.Up -> Viewport.moveUp m -- NB: see `stopKeys` in ui.html
                  Key.Down -> Viewport.moveDown m -- NB: see `stopKeys` in ui.html
                  Key.Left -> Viewport.moveLeft m
                  Key.Right -> Viewport.moveRight m

                  Key.Zero -> Viewport.moveToOrigin m
                  Key.Tab -> Selection.selectNextToplevel m Nothing -- NB: see `stopKeys` in ui.html
                  _ -> NoChange


          SelectingCommand tlid id ->
            case event.keyCode of
              Key.Escape ->
                Commands.endCommandExecution m tlid id
              Key.Enter ->
                Commands.executeCommand m tlid id (AC.highlighted m.complete)
              Key.P ->
                if event.ctrlKey
                then AutocompleteMod ACSelectUp
                else NoChange
              Key.N ->
                if event.ctrlKey
                then AutocompleteMod ACSelectDown
                else NoChange
              Key.Up -> AutocompleteMod ACSelectUp -- NB: see `stopKeys` in ui.html
              Key.Down -> AutocompleteMod ACSelectDown -- NB: see `stopKeys` in ui.html
              Key.Right -> AC.selectSharedPrefix m.complete

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
          let complete = m.complete
              newcomplete = { complete | value = value }
              newm = { m | complete = newcomplete } in
          Entry.submit newm cursor Entry.StayHere
        _ -> NoChange


    GlobalClick event ->
      let _ = Debug.log "globalClick" event in
      case m.currentPage of
        Toplevels _ ->
          if event.button == Defaults.leftButton
          then
            case unwrapCursorState m.cursorState of
              Deselected ->
                Many [ AutocompleteMod ACReset
                     , Enter (Creating (Viewport.toAbsolute m event.pos))]
              _ -> Deselect
          else NoChange
        _ -> NoChange


    BlankOrMouseEnter _ id _ ->
      SetHover id


    BlankOrMouseLeave _ id _ ->
      ClearHover id


    MouseWheel deltaCoords ->
      if m.canvas.enablePan
      then Viewport.moveCanvasBy m deltaCoords
      else NoChange

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
          Many [ SetToplevels m2.toplevels True
               , Drag
                   draggingTLID
                   { vx=mousePos.x
                   , vy=mousePos.y}
                   True
                   origCursorState
               ]
        _ -> NoChange


    ToplevelMouseDown targetTLID event ->
      let _ = Debug.log "tlmousedown" event in
      if event.button == Defaults.leftButton
      then
        let tl = TL.getTL m targetTLID in
            case tl.data of
              TLFunc _ -> NoChange
              _ -> Drag targetTLID event.pos False m.cursorState
      else NoChange


    ToplevelMouseUp targetTLID event ->
      let _ = Debug.log "tlmouseup" event in
      if event.button == Defaults.leftButton
      then
        case m.cursorState of
          Dragging draggingTLID startVPos hasMoved origCursorState ->
            if hasMoved
            then
              let tl = TL.getTL m draggingTLID in
              -- We've been updating tl.pos as mouse moves,
              -- now want to report last pos to server

              -- the SetCursorState here isn't always necessary
              -- because in the happy case we'll also receive
              -- a ToplevelClick event, but it seems that sometimes
              -- we don't, perhaps due to overlapping click handlers
              -- There doesn't seem to be any harm in stopping dragging
              -- here though
              Many [
                SetCursorState origCursorState
                , RPC ([MoveTL draggingTLID tl.pos], FocusNoChange)
                ]
            else
              SetCursorState origCursorState
          _ ->
            NoChange
      else NoChange

    ------------------------
    -- clicking
    ------------------------
    BlankOrClick targetTLID targetID _ ->
      let _ = Debug.log "blankorclick " (targetTLID, targetID) in
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
              then NoChange
              else Select targetTLID (Just targetID)
            Nothing ->
              Select targetTLID (Just targetID)
        SelectingCommand _ selectingID ->
          if selectingID == targetID
          then NoChange
          else Select targetTLID (Just targetID)



    BlankOrDoubleClick targetTLID targetID _ ->
      let _ = Debug.log "blankorDclick " (targetTLID, targetID) in
      Selection.enter m targetTLID targetID


    ToplevelClick targetTLID _ ->
      let _ = Debug.log "tlclick" targetTLID in
      case m.cursorState of
        Dragging _ _ _ origCursorState ->
          SetCursorState origCursorState
        Selecting selectingTLID _ ->
          Select targetTLID Nothing
        SelectingCommand selectingTLID _ ->
          Select targetTLID Nothing
        Deselected ->
          Select targetTLID Nothing
        Entering _ ->
          Select targetTLID Nothing


    ExecuteFunctionButton tlid id name ->
      let tl = TL.getTL m tlid in
      Many [ ExecutingFunctionBegan tlid id
           , ExecutingFunctionRPC tlid id name
           ]


    DataClick tlid idx _ ->
      let _ = Debug.log "dataclick" (tlid, idx) in
      case m.cursorState of
        Dragging _ _ _ origCursorState ->
          SetCursorState origCursorState
        _ -> SetCursor tlid idx


    -----------------
    -- Buttons
    -----------------
    ToggleTimers ->
      TweakModel toggleTimers

    SaveTestButton ->
      MakeCmd RPC.saveTestRPC

    FinishIntegrationTest ->
      EndIntegrationTest

    ------------------------
    -- feature flags
    ------------------------
    StartFeatureFlag ->
      FeatureFlags.start m

    EndFeatureFlag id pick ->
      FeatureFlags.end m id pick

    ToggleFeatureFlag id is ->
      FeatureFlags.toggle m id is

    -------------------------
    -- Function Management
    ------------------------
    ExtractFunction ->
      case m.cursorState of
        Selecting tlid mId ->
          let tl = TL.getTL m tlid in
            case mId of
              Nothing -> NoChange
              Just id ->
                let pd = TL.findExn tl id in
                Refactor.extractFunction m tl pd
        _ -> NoChange

    DeleteUserFunctionParameter uf upf ->
      let replacement = Functions.removeParameter uf upf
          newCalls = Refactor.removeFunctionParameter m uf upf
      in
          RPC ([SetFunction replacement] ++ newCalls, FocusNext uf.tlid Nothing)

    DeleteUserFunction tlid ->
      RPC ([DeleteFunction tlid], FocusNothing)

    RestoreToplevel tlid ->
      RPC ([UndoTL tlid], FocusNext tlid Nothing)

    -----------------
    -- RPCs stuff
    -----------------
    RPCCallback focus calls
      (Ok ( newToplevels
          , newDeletedToplevels
          , newTraces
          , globals
          , userFuncs
          , unlockedDBs)) ->
      if focus == FocusNoChange
      then
        Many [ UpdateToplevels newToplevels False
             , UpdateDeletedToplevels newDeletedToplevels
             , UpdateTraces newTraces
             , SetGlobalVariables globals
             , SetUserFunctions userFuncs False
             , SetUnlockedDBs unlockedDBs
             , RequestAnalysis newToplevels
             , MakeCmd (Entry.focusEntry m)
             ]
      else
        let m2 = TL.upsertAll m newToplevels
            m3 = { m2 | userFunctions = userFuncs }
            newState = processFocus m3 focus
        in Many [ UpdateToplevels newToplevels True
                , UpdateDeletedToplevels newDeletedToplevels
                , UpdateTraces newTraces
                , SetGlobalVariables globals
                , SetUserFunctions userFuncs True
                , SetUnlockedDBs unlockedDBs
                , RequestAnalysis newToplevels
                , AutocompleteMod ACReset
                , ClearError
                , newState
                ]

    InitialLoadRPCCallback focus extraMod
      (Ok ( toplevels
          , deletedToplevels
          , newTraces
          , globals
          , userFuncs
          , unlockedDBs)) ->
      let m2 = { m | toplevels = toplevels, userFunctions = userFuncs }
          newState = processFocus m2 focus
      in Many [ SetToplevels toplevels True
              , SetDeletedToplevels deletedToplevels
              , UpdateTraces newTraces
              , SetGlobalVariables globals
              , SetUserFunctions userFuncs True
              , SetUnlockedDBs unlockedDBs
              , RequestAnalysis toplevels
              , AutocompleteMod ACReset
              , ClearError
              , extraMod -- for integration tests, maybe more
              , newState
              ]

    SaveTestRPCCallback (Ok msg) ->
      DisplayError <| "Success! " ++ msg

    ExecuteFunctionRPCCallback params (Ok (dval, hash)) ->
      Many [ UpdateTraceFunctionResult params.tlid params.traceID params.callerID params.fnName hash dval
           , ExecutingFunctionComplete [(params.tlid, params.callerID)]
           ]

    ExecuteFunctionCancel tlid id ->
      Many [ DisplayError "No trace"
           , ExecutingFunctionComplete [(tlid, id)]
           ]


    GetAnalysisRPCCallback (Ok (newTraces, globals, f404s, unlockedDBs)) ->
      Many [ TweakModel Sync.markResponseInModel
           , UpdateTraces newTraces
           , SetGlobalVariables globals
           , Set404s f404s
           , SetUnlockedDBs unlockedDBs
           , RequestAnalysis m.toplevels
           ]

    ReceiveAnalysis json ->
      let envelope = JSD.decodeString RPC.decodeAnalysisEnvelope json in
      case envelope of
        Ok (id, analysisResults) -> UpdateAnalysis id analysisResults
        Err str -> DisplayError str

    ------------------------
    -- plumbing
    ------------------------
    RPCCallback _ _ (Err err) ->
      DisplayAndReportHttpError "RPC" err

    SaveTestRPCCallback (Err err) ->
      DisplayError <| "Error: " ++ toString err

    ExecuteFunctionRPCCallback _ (Err err) ->
      DisplayAndReportHttpError "ExecuteFunction" err

    InitialLoadRPCCallback _ _ (Err err) ->
      DisplayAndReportHttpError "InitialLoad" err

    GetAnalysisRPCCallback (Err err) ->
      DisplayAndReportHttpError "GetAnalysis" err

    JSError msg ->
      DisplayError ("Error in JS: " ++ msg)

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
      Url.changeLocation m loc

    TimerFire action time  ->
      case action of
        RefreshAnalysis ->
          GetAnalysisRPC
        CheckUrlHashPosition ->
          Url.maybeUpdateScrollUrl m

    Initialization ->
      NoChange

    AddRandom ->
      NoChange

    PageVisibilityChange vis ->
      TweakModel (\m -> { m | visibility = vis })

    PageFocusChange vis ->
      TweakModel (\m -> { m | visibility = vis })

    CreateHandlerFrom404 {space, path, modifier} ->
      let center = findCenter m
          anId = gtlid ()
          aPos = center
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
            , tlid = anId
            }
      in
        RPC ([SetHandler anId aPos aHandler], FocusNothing)
    CreateRouteHandler ->
      let center = findCenter m
        in Entry.submitOmniAction m center NewHTTPHandler
    CreateFunction ->
      let ufun = Refactor.generateEmptyFunction ()
      in
          Many ([RPC ([SetFunction ufun], FocusNothing)
                , MakeCmd (Url.navigateTo (Fn ufun.tlid Viewport.origin))
                ])
    LockHandler tlid isLocked ->
      Editor.updateLockedHandlers tlid isLocked m

    EnablePanning pan ->
      let c = m.canvas
      in TweakModel (\m -> { m | canvas = { c | enablePan = pan } } )

    _ -> NoChange

findCenter : Model -> Pos
findCenter m =
  case m.currentPage of
    Toplevels center -> center
    _ -> Defaults.initialPos

enableTimers : Model -> Model
enableTimers m =
  { m | timersEnabled = True }

disableTimers : Model -> Model
disableTimers m =
  { m | timersEnabled = False }

toggleTimers : Model -> Model
toggleTimers m =
  { m | timersEnabled = not m.timersEnabled }


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
            [ Time.every Time.second (TimerFire RefreshAnalysis) ]

      urlTimer =
        [Time.every Time.second (TimerFire CheckUrlHashPosition)]

      timers = if m.timersEnabled
               then syncTimer ++ urlTimer
               else []


      onError = [displayError JSError]

      visibility =
        [ PageVisibility.visibilityChanges PageVisibilityChange
        , onWindow "focus" (JSD.succeed (PageFocusChange PageVisibility.Visible))
        , onWindow "blur" (JSD.succeed (PageFocusChange PageVisibility.Hidden))]

      mousewheelSubs = [mousewheel MouseWheel]

  in Sub.batch
    (List.concat [ keySubs
                 , dragSubs
                 , resizes
                 , timers
                 , visibility
                 , onError
                 , mousewheelSubs
                 , [receiveAnalysis ReceiveAnalysis]])
