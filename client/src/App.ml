open Tc
open Prelude
open Types

(* Tea *)
module Cmd = Tea.Cmd
module Http = Tea.Http

(* Dark *)
module AC = Autocomplete
module B = Blank
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Key = Keyboard

let init (flagString : string) (location : Web.Location.location) =
  let {Flags.editorState; complete; userContentHost; environment; csrfToken} =
    Flags.fromString flagString
  in
  let savedEditor = Editor.fromString editorState in
  let m0 = Editor.editor2model savedEditor in
  (* these saved values may not be valid yet *)
  let savedCursorState = m0.cursorState in
  let m =
    { m0 with
      cursorState = Deselected
    ; currentPage = (Defaults.defaultModel |> fun x -> x.currentPage)
    ; builtInFunctions = complete }
  in
  let page =
    Url.parseLocation location |> Option.withDefault ~default:m.currentPage
  in
  let canvas = m.canvas in
  let newCanvas =
    match page with
    | Toplevels pos ->
        {canvas with offset = pos}
    | Fn (_, pos) ->
        {canvas with fnOffset = pos}
  in
  let canvasName = Url.parseCanvasName location in
  let integrationTestName = canvasName in
  let m2 =
    { m with
      complete = AC.init m
    ; tests = VariantTesting.enabledVariantTests
    ; toplevels = []
    ; currentPage = page
    ; canvas = newCanvas
    ; canvasName
    ; userContentHost
    ; environment
    ; csrfToken }
  in
  if Url.isIntegrationTest
  then
    ( m2
    , Cmd.batch [RPC.integrationRPC (contextFromModel m2) integrationTestName]
    )
  else
    ( m2
    , Cmd.batch
        [ RPC.initialLoadRPC
            (contextFromModel m2)
            (FocusPageAndCursor (page, savedCursorState))
        ; Sync.fetchAll m2 ] )


let updateError (oldErr : darkError) (newErrMsg : string) : darkError =
  if oldErr.message = Some newErrMsg && not oldErr.showDetails
  then oldErr
  else {message = Some newErrMsg; showDetails = true}


let processFocus (m : model) (focus : focus) : modification =
  match focus with
  | FocusNext (tlid, pred) ->
      let tl = TL.getTL m tlid in
      let predPd = Option.andThen ~f:(TL.find tl) pred in
      let next = TL.getNextBlank tl predPd in
      ( match next with
      | Some pd ->
          Enter (Filling (tlid, P.toID pd))
      | None ->
          Select (tlid, pred) )
  | FocusExact (tlid, id) ->
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      if P.isBlank pd || P.toContent pd = Some ""
      then Enter (Filling (tlid, id))
      else Select (tlid, Some id)
  | FocusSame ->
    ( match unwrapCursorState m.cursorState with
    | Selecting (tlid, mId) ->
      ( match (TL.get m tlid, mId) with
      | Some tl, Some id ->
          if TL.isValidID tl id then NoChange else Select (tlid, None)
      | Some _, None ->
          Select (tlid, None)
      | _ ->
          Deselect )
    | Entering (Filling (tlid, id)) ->
      ( match TL.get m tlid with
      | Some tl ->
          if TL.isValidID tl id then NoChange else Select (tlid, None)
      | _ ->
          Deselect )
    | _ ->
        NoChange )
  | FocusPageAndCursor (page, cs) ->
      let setCS = SetCursorState cs in
      let noTarget = ACSetTarget None in
      let target tuple = ACSetTarget (Some tuple) in
      let tlOnPage tl =
        match page with
        | Toplevels _ ->
          ( match tl.data with
          | TLHandler _ ->
              true
          | TLDB _ ->
              true
          | TLFunc _ ->
              false )
        | Fn (id, _) ->
            tl.id = id
      in
      let setQuery =
        let mTl = cs |> tlidOf |> Option.andThen ~f:(TL.get m) in
        match (mTl, idOf cs) with
        | Some tl, Some id when TL.isValidID tl id ->
            let pd = TL.find tl id in
            AutocompleteMod
              (ACSetQuery
                 ( pd
                 |> Option.andThen ~f:P.toContent
                 |> Option.withDefault ~default:"" ))
        | _ ->
            NoChange
      in
      let nextCursor, acTarget =
        match cs with
        | Selecting (tlid, mId) ->
          ( match (TL.get m tlid, mId) with
          | Some tl, Some id ->
              if TL.isValidID tl id && tlOnPage tl
              then (setCS, target (tlid, TL.findExn tl id))
              else (Deselect, noTarget)
          | Some _, None ->
              (setCS, noTarget)
          | _ ->
              (Deselect, noTarget) )
        | Entering (Filling (tlid, id)) ->
          ( match TL.get m tlid with
          | Some tl ->
              if TL.isValidID tl id && tlOnPage tl
              then (setCS, target (tlid, TL.findExn tl id))
              else (Deselect, noTarget)
          | _ ->
              (Deselect, noTarget) )
        | Dragging (tlid, _, _, _) ->
          ( match TL.get m tlid with
          | Some tl ->
              if tlOnPage tl then (setCS, noTarget) else (Deselect, noTarget)
          | _ ->
              (Deselect, noTarget) )
        | _ ->
            (Deselect, noTarget)
      in
      Many [SetPage page; nextCursor; AutocompleteMod acTarget; setQuery]
  | FocusNothing ->
      Deselect
  (* used instead of focussame when we've already done the focus *)
  | FocusNoChange ->
      NoChange


let processAutocompleteMods (m : model) (mods : autocompleteMod list) :
    model * msg Cmd.t =
  if m.integrationTestState <> NoIntegrationTest
  then
    Debug.loG "autocompletemod update" (show_list ~f:show_autocompleteMod mods) ;
  let complete =
    List.foldl
      ~f:(fun mod_ complete_ -> AC.update m mod_ complete_)
      ~init:m.complete
      mods
  in
  let focus =
    match unwrapCursorState m.cursorState with
    | Entering _ ->
        AC.focusItem complete.index
    | SelectingCommand (_, _) ->
        AC.focusItem complete.index
    | _ ->
        Cmd.none
  in
  ( if m.integrationTestState <> NoIntegrationTest
  then
    let val_ = AC.getValue complete in
    Debug.loG
      "autocompletemod result: "
      (string_of_int complete.index ^ " => '" ^ val_ ^ "'") ) ;
  ({m with complete}, focus)


let rec updateMod (mod_ : modification) ((m, cmd) : model * msg Cmd.t) :
    model * msg Cmd.t =
  if m.integrationTestState <> NoIntegrationTest
  then Debug.loG "mod update" (show_modification mod_) ;
  let closeBlanks newM =
    (* close open threads in the previous TL *)
    m.cursorState
    |> tlidOf
    |> Option.andThen ~f:(TL.get m)
    |> Option.map ~f:(fun tl ->
           match tl.data with
           | TLHandler h ->
               let replacement = AST.closeBlanks h.ast in
               if replacement = h.ast
               then []
               else
                 let newH = {h with ast = replacement} in
                 let ops = [SetHandler (tl.id, tl.pos, newH)] in
                 let params = RPC.opsParams ops in
                 (* call RPC on the new model *)
                 [RPC.rpc (contextFromModel newM) FocusSame params]
           | TLFunc f ->
               let replacement = AST.closeBlanks f.ufAST in
               if replacement = f.ufAST
               then []
               else
                 let newF = {f with ufAST = replacement} in
                 let ops = [SetFunction newF] in
                 let params = RPC.opsParams ops in
                 (* call RPC on the new model *)
                 [RPC.rpc (contextFromModel newM) FocusSame params]
           | _ ->
               [] )
    |> Option.withDefault ~default:[]
    |> fun rpc ->
    if tlidOf newM.cursorState = tlidOf m.cursorState then [] else rpc
  in
  let newm, newcmd =
    let handleRPC params focus =
      (* immediately update the model based on SetHandler and focus, if
         possible *)
      let hasNonHandlers =
        List.any
          ~f:(fun c ->
            match c with
            | SetHandler (_, _, _) ->
                false
            | SetFunction _ ->
                false
            | _ ->
                true )
          params.ops
      in
      if hasNonHandlers
      then (m, RPC.rpc (contextFromModel m) focus params)
      else
        let localM =
          List.foldl
            ~f:(fun call m ->
              match call with
              | SetHandler (tlid, pos, h) ->
                  TL.upsert m {id = tlid; pos; data = TLHandler h}
              | SetFunction f ->
                  Functions.upsert m f
              | _ ->
                  m )
            ~init:m
            params.ops
        in
        let withFocus, wfCmd =
          updateMod
            (Many [AutocompleteMod ACReset; processFocus localM focus])
            (localM, Cmd.none)
        in
        ( withFocus
        , Cmd.batch
            [wfCmd; RPC.rpc (contextFromModel withFocus) FocusNoChange params]
        )
    in
    match mod_ with
    | DisplayError e ->
        ({m with error = updateError m.error e}, Cmd.none)
    | DisplayAndReportError e ->
        ( {m with error = updateError m.error e}
        , Tea.Cmd.call (fun _ -> Native.Rollbar.send e None Js.Json.null) )
    | DisplayAndReportHttpError (context, e) ->
        let body (body : Tea.Http.responseBody) =
          let maybe name m =
            match m with Some s -> ", " ^ name ^ ": " ^ s | None -> ""
          in
          let str =
            match body with
            | NoResponse ->
                "todo-noresponse"
            | StringResponse str ->
                str
            | ArrayBufferResponse _ ->
                "todo-arratbufferresponse"
            | BlobResponse _ ->
                "todo-blobresponse"
            | DocumentResponse _ ->
                "todo-document-response"
            | JsonResponse _ ->
                "todo-jsonresponse"
            | TextResponse str ->
                str
            | RawResponse (str, _) ->
                str
          in
          str
          |> Json_decode_extended.decodeString Decoders.exception_
          |> Result.toOption
          |> Option.map
               ~f:(fun { short
                       ; long
                       ; exceptionTipe
                       ; actual
                       ; actualType
                       ; expected
                       ; result
                       ; resultType
                       ; info
                       ; workarounds }
                  ->
                 " ("
                 ^ exceptionTipe
                 ^ "): "
                 ^ short
                 ^ maybe "message" long
                 ^ maybe "actual value" actual
                 ^ maybe "actual type" actualType
                 ^ maybe "result" result
                 ^ maybe "result type" resultType
                 ^ maybe "expected" expected
                 ^ ( if info = StrDict.empty
                   then ""
                   else ", info: " ^ StrDict.toString info )
                 ^
                 if workarounds = []
                 then ""
                 else ", workarounds: [" ^ String.concat workarounds ^ "]" )
          |> Option.withDefault ~default:str
        in
        let msg =
          match e with
          | Http.BadUrl str ->
              "Bad url: " ^ str
          | Http.Timeout ->
              "Timeout"
          | Http.NetworkError ->
              "Network error - is the server running?"
          | Http.BadStatus response ->
              "Bad status: " ^ response.status.message ^ body response.body
          | Http.BadPayload (msg, _) ->
              "Bad payload (" ^ context ^ "): " ^ msg
          | Http.Aborted ->
              "Request Aborted"
        in
        let url =
          match e with
          | Http.BadUrl str ->
              Some str
          | Http.Timeout ->
              None
          | Http.NetworkError ->
              None
          | Http.BadStatus response ->
              Some response.url
          | Http.BadPayload (_, response) ->
              Some response.url
          | Http.Aborted ->
              None
        in
        let shouldRollbar = e <> Http.NetworkError in
        let msg = msg ^ " (" ^ context ^ ")" in
        let custom = Encoders.httpError e in
        let cmds =
          if shouldRollbar
          then [Tea.Cmd.call (fun _ -> Native.Rollbar.send msg url custom)]
          else []
        in
        ({m with error = updateError m.error msg}, Cmd.batch cmds)
    | ClearError ->
        ({m with error = {message = None; showDetails = false}}, Cmd.none)
    | RPC (ops, focus) ->
        handleRPC (RPC.opsParams ops) focus
    | RPCFull (params, focus) ->
        handleRPC params focus
    | GetAnalysisRPC (ignore404s, ignoreTraces) ->
        Sync.fetch ~ignore404s ~ignoreTraces m
    | NoChange ->
        (m, Cmd.none)
    | TriggerIntegrationTest name ->
        let expect = IntegrationTest.trigger name in
        ({m with integrationTestState = expect}, Cmd.none)
    | EndIntegrationTest ->
        let expectationFn =
          match m.integrationTestState with
          | IntegrationTestExpectation fn ->
              fn
          | IntegrationTestFinished _ ->
              impossible
                "Attempted to end integration test but one ran + was already finished"
          | NoIntegrationTest ->
              impossible
                "Attempted to end integration test but none was running"
        in
        let result = expectationFn m in
        ( {m with integrationTestState = IntegrationTestFinished result}
        , Cmd.none )
    | MakeCmd cmd ->
        (m, cmd)
    | SetCursorState cursorState ->
        let newM = {m with cursorState} in
        (newM, Entry.focusEntry newM)
    | SetPage page ->
        if m.currentPage = page
        then (m, Cmd.none)
        else
          let canvas = m.canvas in
          ( match (page, m.currentPage) with
          | Toplevels pos2, Toplevels _ ->
              (* scrolling *)
              ( { m with
                  currentPage = page
                ; urlState = {lastPos = pos2}
                ; canvas = {canvas with offset = pos2} }
              , Cmd.none )
          | Fn (_, pos2), _ ->
              ( { m with
                  currentPage = page
                ; cursorState = Deselected
                ; urlState = {lastPos = pos2}
                ; canvas = {canvas with fnOffset = pos2} }
              , Cmd.none )
          | _ ->
              let newM =
                {m with currentPage = page; cursorState = Deselected}
              in
              (newM, Cmd.batch (closeBlanks newM)) )
    | SetCenter center ->
      ( match m.currentPage with
      | Toplevels _ ->
          ({m with currentPage = Toplevels center}, Cmd.none)
      | Fn (id, _) ->
          ({m with currentPage = Fn (id, center)}, Cmd.none) )
    | Select (tlid, p) ->
        let newM = {m with cursorState = Selecting (tlid, p)} in
        (newM, Cmd.batch (closeBlanks newM))
    | Deselect ->
        let newM = {m with cursorState = Deselected} in
        (newM, Cmd.batch (closeBlanks newM))
    | Enter entry ->
        let target =
          match entry with
          | Creating _ ->
              None
          | Filling (tlid, id) ->
              let tl = TL.getTL m tlid in
              let pd = TL.findExn tl id in
              Some (tlid, pd)
        in
        let m2, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m3 = {m2 with cursorState = Entering entry} in
        (m3, Cmd.batch (closeBlanks m3 @ [acCmd; Entry.focusEntry m3]))
    | EnterWithOffset (entry, offset) ->
        let target =
          match entry with
          | Creating _ ->
              None
          | Filling (tlid, id) ->
              let tl = TL.getTL m tlid in
              let pd = TL.findExn tl id in
              Some (tlid, pd)
        in
        let m2, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m3 = {m2 with cursorState = Entering entry} in
        ( m3
        , Cmd.batch
            (closeBlanks m3 @ [acCmd; Entry.focusEntryWithOffset m3 offset]) )
    | SelectCommand (tlid, id) ->
        let m2 = {m with cursorState = SelectingCommand (tlid, id)} in
        let m3, acCmd =
          processAutocompleteMods m2 [ACEnableCommandMode; ACRegenerate]
        in
        (m3, Cmd.batch (closeBlanks m3 @ [acCmd; Entry.focusEntry m3]))
    | RemoveToplevel tl ->
        (Toplevel.remove m tl, Cmd.none)
    | SetToplevels (tls, updateCurrent) ->
        let m2 = {m with toplevels = tls} in
        (* Bring back the TL being edited, so we don't lose work done since the
           API call *)
        let m3 =
          match tlidOf m.cursorState with
          | Some tlid ->
              if updateCurrent
              then m2
              else
                let tl = TL.getTL m tlid in
                ( match tl.data with
                | TLDB _ ->
                    TL.upsert m2 tl
                | TLHandler _ ->
                    TL.upsert m2 tl
                | TLFunc _ ->
                    m2 )
          | None ->
              m2
        in
        let m4 =
          { m3 with
            deletedToplevels =
              TL.removeByTLID m3.deletedToplevels ~toBeRemoved:tls }
        in
        let m5 = Refactor.updateUsageCounts m4 in
        processAutocompleteMods m5 [ACRegenerate]
    | UpdateToplevels (tls, updateCurrent) ->
        let m2 = TL.upsertAll m tls in
        updateMod (SetToplevels (m2.toplevels, updateCurrent)) (m, cmd)
    | UpdateDeletedToplevels dtls ->
        let m2 =
          { m with
            deletedToplevels =
              TL.upsertAllByTLID m.deletedToplevels ~newTLs:dtls
          ; toplevels = TL.removeByTLID m.toplevels ~toBeRemoved:dtls }
        in
        processAutocompleteMods m2 [ACRegenerate]
    | SetDeletedToplevels dtls ->
        let m2 =
          { m with
            deletedToplevels = dtls
          ; toplevels = TL.removeByTLID m.toplevels ~toBeRemoved:dtls }
        in
        processAutocompleteMods m2 [ACRegenerate]
    | RequestAnalysis tls ->
        let handlers = TL.handlers tls in
        let dbs = TL.dbs m.toplevels in
        let userFns = m.userFunctions in
        let requestAnalysis s =
          Tea_cmd.call (fun _ -> Analysis.RequestAnalysis.send s)
        in
        let req h =
          let trace = Analysis.getCurrentTrace m h.tlid in
          let param t = {handler = h; trace = t; dbs; userFns} in
          trace
          |> Option.map ~f:(fun t -> requestAnalysis (param t))
          |> Option.toList
        in
        (m, Cmd.batch (handlers |> List.map ~f:req |> List.concat))
    | UpdateAnalysis (id, analysis) ->
        let m2 = {m with analyses = Analysis.record m.analyses id analysis} in
        processAutocompleteMods m2 [ACRegenerate]
    | UpdateTraces traces ->
        let newTraces =
          Belt.Map.String.merge m.traces traces (fun _ maybeOld maybeNew ->
              match (maybeOld, maybeNew) with
              | None, None ->
                  None
              | Some o, None ->
                  Some o
              | None, Some n ->
                  Some n
              | Some _, Some n ->
                  Some n )
        in
        let m2 =
          {m with traces = newTraces; unfetchedTraces = Belt.Map.String.empty}
        in
        processAutocompleteMods m2 [ACRegenerate]
    | UpdateTraceFunctionResult (tlid, traceID, callerID, fnName, hash, dval)
      ->
        let m2 =
          Analysis.replaceFunctionResult
            m
            tlid
            traceID
            callerID
            fnName
            hash
            dval
        in
        processAutocompleteMods m2 [ACRegenerate]
    | AddUnfetchedTrace (tlid, traceID) ->
        if Some tlid = tlidOf m.cursorState
        then
          let unfetchedTraces =
            Belt.Map.String.update
              m.unfetchedTraces
              (deTLID tlid)
              (fun maybeOld ->
                match maybeOld with
                | None ->
                    Some [traceID]
                | Some o ->
                    Some (traceID :: o) )
          in
          Sync.fetch
            ~ignoreTraces:false
            ~ignore404s:true
            {m with unfetchedTraces}
        else (m, Cmd.none)
    | SetUserFunctions (userFuncs, deletedUserFuncs, updateCurrent) ->
        let m2 =
          { m with
            userFunctions =
              Functions.upsertAllByTLID m.userFunctions ~newFns:userFuncs
              |> Functions.removeByTLID ~toBeRemoved:deletedUserFuncs
          ; deletedUserFunctions =
              Functions.upsertAllByTLID
                m.deletedUserFunctions
                ~newFns:deletedUserFuncs
              |> Functions.removeByTLID ~toBeRemoved:userFuncs }
        in
        (* Bring back the TL being edited, so we don't lose work done since the
           API call *)
        let m3 =
          match tlidOf m.cursorState with
          | Some tlid ->
              if updateCurrent
              then m2
              else
                let tl = TL.getTL m tlid in
                ( match tl.data with
                | TLFunc f ->
                    Functions.upsert m2 f
                | TLDB _ ->
                    m2
                | TLHandler _ ->
                    m2 )
          | None ->
              m2
        in
        let m4 = Refactor.updateUsageCounts m3 in
        processAutocompleteMods m4 [ACRegenerate]
    | SetUnlockedDBs unlockedDBs ->
        ({m with unlockedDBs}, Cmd.none)
    | Set404s (f404s, latest404) ->
        ({m with f404s; latest404}, Cmd.none)
    | Delete404 f404 ->
        ({m with f404s = List.filter ~f:(( <> ) f404) m.f404s}, Cmd.none)
    | Append404s (f404s, latest404) ->
        let new404s =
          f404s @ m.f404s
          |> List.uniqueBy ~f:(fun f404 ->
                 f404.space ^ f404.path ^ f404.modifier )
        in
        ({m with f404s = new404s; latest404}, Cmd.none)
    | SetHover p ->
        let nhovering = p :: m.hovering in
        ({m with hovering = nhovering}, Cmd.none)
    | ClearHover p ->
        let nhovering = List.filter ~f:(fun m -> m <> p) m.hovering in
        ({m with hovering = nhovering}, Cmd.none)
    | SetCursor (tlid, cur) ->
        let m2 = Analysis.setCursor m tlid cur in
        (m2, Cmd.none)
    | CopyToClipboard clipboard ->
        ({m with clipboard}, Cmd.none)
    | Drag (tlid, offset, hasMoved, state) ->
        (* Because mouseEvents are not perfectly reliable, we can end up in
         * weird dragging states. If we start dragging, make sure the state
         * we're in before isnt also dragging. *)
        ( { m with
            cursorState =
              Dragging (tlid, offset, hasMoved, Editor.stripDragging state) }
        , Cmd.none )
    | ExecutingFunctionBegan (tlid, id) ->
        let nexecutingFunctions = m.executingFunctions @ [(tlid, id)] in
        ({m with executingFunctions = nexecutingFunctions}, Cmd.none)
    | ExecutingFunctionRPC (tlid, id, name) ->
      ( match Analysis.getCurrentTrace m tlid with
      | Some trace ->
        ( match Analysis.getArguments m tlid trace.traceID id with
        | Some args ->
            let params =
              { efpTLID = tlid
              ; efpCallerID = id
              ; efpTraceID = trace.traceID
              ; efpFnName = name
              ; efpArgs = args }
            in
            (m, RPC.executeFunctionRPC (contextFromModel m) params)
        | None ->
            (m, Cmd.none)
            |> updateMod
                 (DisplayError "Traces are not loaded for this handler")
            |> updateMod (ExecutingFunctionComplete [(tlid, id)]) )
      | None ->
          (m, Cmd.none)
          |> updateMod (DisplayError "Traces are not loaded for this handler")
          |> updateMod (ExecutingFunctionComplete [(tlid, id)]) )
    | ExecutingFunctionComplete targets ->
        let isComplete target = not <| List.member ~value:target targets in
        let nexecutingFunctions =
          List.filter ~f:isComplete m.executingFunctions
        in
        ({m with executingFunctions = nexecutingFunctions}, Cmd.none)
    | SetLockedHandlers locked ->
        ({m with lockedHandlers = locked}, Cmd.none)
    | MoveCanvasTo (canvas, page, pos) ->
        let canvas2 =
          match page with
          | Toplevels _ ->
              {canvas with offset = pos}
          | Fn (_, _) ->
              {canvas with fnOffset = pos}
        in
        ({m with canvas = canvas2}, Cmd.none)
    | TweakModel fn ->
        (fn m, Cmd.none)
    | AutocompleteMod mod_ ->
        processAutocompleteMods m [mod_]
    (* applied from left to right *)
    | Many mods ->
        List.foldl ~f:updateMod ~init:(m, Cmd.none) mods
  in
  (newm, Cmd.batch [cmd; newcmd])


(* Figure out from the string and the state whether this '.' means field
   access. *)
let isFieldAccessDot (m : model) (baseStr : string) : bool =
  (* We know from the fact that this function is called that there has
     been a '.' entered. However, it might not be in baseStr, so
     canonicalize it first. *)
  let str = Regex.replace ~re:"\\.*$" ~repl:"" baseStr in
  let intOrString =
    String.startsWith ~prefix:"\"" str
    || Decoders.typeOfLiteralString str = TInt
  in
  match m.cursorState with
  | Entering (Creating _) ->
      not intOrString
  | Entering (Filling (tlid, id)) ->
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      (P.typeOf pd = Expr || P.typeOf pd = Field) && not intOrString
  | _ ->
      false


let enableTimers (m : model) : model = {m with timersEnabled = true}

let disableTimers (m : model) : model = {m with timersEnabled = false}

let toggleTimers (m : model) : model =
  {m with timersEnabled = not m.timersEnabled}


let findCenter (m : model) : pos =
  match m.currentPage with
  | Toplevels center ->
      Viewport.toCenter center
  | _ ->
      Defaults.centerPos


let update_ (msg : msg) (m : model) : modification =
  if m.integrationTestState <> NoIntegrationTest
  then Debug.loG "msg update" (show_msg msg) ;
  match msg with
  | GlobalKeyPress event ->
      KeyPress.handler event m
  | EntryInputMsg target ->
      let query = if target = "\"" then "\"\"" else target in
      if String.endsWith ~suffix:"." query && isFieldAccessDot m query
      then NoChange
      else
        Many
          [ AutocompleteMod (ACSetQuery query)
          ; AutocompleteMod (ACSetVisible true)
          ; MakeCmd (Entry.focusEntry m) ]
  | EntrySubmitMsg ->
      NoChange
  | AutocompleteClick value ->
    ( match unwrapCursorState m.cursorState with
    | Entering cursor ->
        let complete = m.complete in
        let newcomplete = {complete with value} in
        let newm = {m with complete = newcomplete} in
        Entry.submit newm cursor Entry.StayHere
    | _ ->
        NoChange )
  | GlobalClick event ->
    ( match m.currentPage with
    | Toplevels _ ->
        if event.button = Defaults.leftButton
        then
          match unwrapCursorState m.cursorState with
          | Deselected ->
              Many
                [ AutocompleteMod ACReset
                ; Enter (Creating (Viewport.toAbsolute m event.mePos)) ]
          | _ ->
              Deselect
        else NoChange
    | _ ->
        NoChange )
  | BlankOrMouseEnter (_, id, _) ->
      SetHover id
  | BlankOrMouseLeave (_, id, _) ->
      ClearHover id
  | MouseWheel (x, y) ->
      if m.canvas.enablePan then Viewport.moveCanvasBy m x y else NoChange
  | DataMouseEnter (tlid, idx, _) ->
      SetHover (tlCursorID tlid idx)
  | DataMouseLeave (tlid, idx, _) ->
      ClearHover (tlCursorID tlid idx)
  | DragToplevel (_, mousePos) ->
    ( match m.cursorState with
    | Dragging (draggingTLID, startVPos, _, origCursorState) ->
        let xDiff = mousePos.x - startVPos.vx in
        let yDiff = mousePos.y - startVPos.vy in
        let m2 = TL.move draggingTLID xDiff yDiff m in
        Many
          [ SetToplevels (m2.toplevels, true)
          ; Drag
              ( draggingTLID
              , {vx = mousePos.x; vy = mousePos.y}
              , true
              , origCursorState ) ]
    | _ ->
        NoChange )
  | ToplevelMouseDown (targetTLID, event) ->
      if event.button = Defaults.leftButton
      then
        let tl = TL.getTL m targetTLID in
        match tl.data with
        | TLFunc _ ->
            NoChange
        | _ ->
            Drag (targetTLID, event.mePos, false, m.cursorState)
      else NoChange
  | ToplevelMouseUp (_, event) ->
      if event.button = Defaults.leftButton
      then
        match m.cursorState with
        | Dragging (draggingTLID, _, hasMoved, origCursorState) ->
            if hasMoved
            then
              let tl = TL.getTL m draggingTLID in
              (* We've been updating tl.pos as mouse moves, *)
              (* now want to report last pos to server *)
              
              (* the SetCursorState here isn't always necessary *)
              (* because in the happy case we'll also receive *)
              (* a ToplevelClick event, but it seems that sometimes *)
              (* we don't, perhaps due to overlapping click handlers *)
              (* There doesn't seem to be any harm in stopping dragging *)
              (* here though *)
              Many
                [ SetCursorState origCursorState
                ; RPC ([MoveTL (draggingTLID, tl.pos)], FocusNoChange) ]
            else SetCursorState origCursorState
        | _ ->
            NoChange
      else NoChange
  | BlankOrClick (targetTLID, targetID, event) ->
      (* TODO: switch to ranges to get actual character offset
     * rather than approximating *)
      let offset () =
        match
          Js.Nullable.toOption (Web_document.getElementById (showID targetID))
        with
        | Some elem ->
            let rect = elem##getBoundingClientRect () in
            if event.mePos.vy >= int_of_float rect##top
               && event.mePos.vy <= int_of_float rect##bottom
               && event.mePos.vx >= int_of_float rect##left
               && event.mePos.vx <= int_of_float rect##right
            then Some ((event.mePos.vx - int_of_float rect##left) / 8)
            else None
        | None ->
            None
      in
      let fluidEnterOrSelect m tlid id =
        if VariantTesting.variantIsActive m FluidInputModel
        then
          match offset () with
          | Some offset ->
              Selection.enterWithOffset m tlid id (Some offset)
          | None ->
              Selection.enter m tlid id
        else Select (tlid, Some id)
      in
      ( match m.cursorState with
      | Deselected ->
          fluidEnterOrSelect m targetTLID targetID
      | Dragging (_, _, _, origCursorState) ->
          SetCursorState origCursorState
      | Entering cursor ->
        ( match cursor with
        | Filling (_, fillingID) ->
            if fillingID = targetID
            then NoChange
            else fluidEnterOrSelect m targetTLID targetID
        | _ ->
            fluidEnterOrSelect m targetTLID targetID )
      | Selecting (_, _) ->
          fluidEnterOrSelect m targetTLID targetID
      | SelectingCommand (_, selectingID) ->
          if selectingID = targetID
          then NoChange
          else fluidEnterOrSelect m targetTLID targetID )
  | BlankOrDoubleClick (targetTLID, targetID, _) ->
      Selection.dblclick m targetTLID targetID
  | ToplevelClick (targetTLID, _) ->
    ( match m.cursorState with
    | Dragging (_, _, _, origCursorState) ->
        SetCursorState origCursorState
    | Selecting (_, _) ->
        Select (targetTLID, None)
    | SelectingCommand (_, _) ->
        Select (targetTLID, None)
    | Deselected ->
        Select (targetTLID, None)
    | Entering _ ->
        Select (targetTLID, None) )
  | ExecuteFunctionButton (tlid, id, name) ->
      Many
        [ ExecutingFunctionBegan (tlid, id)
        ; ExecutingFunctionRPC (tlid, id, name) ]
  | DataClick (tlid, idx, _) ->
    ( match m.cursorState with
    | Dragging (_, _, _, origCursorState) ->
        SetCursorState origCursorState
    | Deselected ->
        Many [Select (tlid, None); SetCursor (tlid, idx)]
    | _ ->
        SetCursor (tlid, idx) )
  | StartMigration tlid ->
      let mdb = tlid |> TL.getTL m |> TL.asDB in
      ( match mdb with
      | Some db ->
          DB.startMigration tlid db.cols
      | None ->
          NoChange )
  | AbandonMigration tlid ->
      RPC ([AbandonDBMigration tlid], FocusNothing)
  | DeleteColInDB (tlid, nameId) ->
      let mdb = tlid |> TL.getTL m |> TL.asDB in
      ( match mdb with
      | Some db ->
          if DB.isMigrationCol db nameId
          then RPC ([DeleteColInDBMigration (tlid, nameId)], FocusNothing)
          else RPC ([DeleteDBCol (tlid, nameId)], FocusNothing)
      | None ->
          NoChange )
  | ToggleTimers ->
      TweakModel toggleTimers
  | SaveTestButton ->
      MakeCmd (RPC.saveTestRPC (contextFromModel m))
  | FinishIntegrationTest ->
      EndIntegrationTest
  | StartFeatureFlag ->
      FeatureFlags.start m
  | EndFeatureFlag (id, pick) ->
      FeatureFlags.end_ m id pick
  | ToggleFeatureFlag (id, is) ->
      FeatureFlags.toggle id is
  | ExtractFunction ->
    ( match m.cursorState with
    | Selecting (tlid, mId) ->
        let tl = TL.getTL m tlid in
        ( match mId with
        | None ->
            NoChange
        | Some id ->
            let pd = TL.findExn tl id in
            Refactor.extractFunction m tl pd )
    | _ ->
        NoChange )
  | DeleteUserFunctionParameter (uf, upf) ->
      let replacement = Functions.removeParameter uf upf in
      let newCalls = Refactor.removeFunctionParameter m uf upf in
      RPC ([SetFunction replacement] @ newCalls, FocusNext (uf.ufTLID, None))
  | ToplevelDelete tlid ->
      let tl = TL.getTL m tlid in
      Many [RemoveToplevel tl; RPC ([DeleteTL tl.id], FocusSame)]
  | ToplevelDeleteForever tlid ->
      Many
        [ RPC ([DeleteTLForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              { m with
                deletedToplevels =
                  List.filter ~f:(fun tl -> tl.id <> tlid) m.deletedToplevels
              } ) ]
  | DeleteUserFunction tlid ->
      RPC ([DeleteFunction tlid], FocusSame)
  | RestoreToplevel tlid ->
      RPC ([UndoTL tlid], FocusNext (tlid, None))
  | DeleteUserFunctionForever tlid ->
      Many
        [ RPC ([DeleteFunctionForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              { m with
                deletedUserFunctions =
                  List.filter
                    ~f:(fun uf -> uf.ufTLID <> tlid)
                    m.deletedUserFunctions } ) ]
  | RPCCallback (focus, _, Ok r) ->
      if focus = FocusNoChange
      then
        Many
          [ UpdateToplevels (r.toplevels, false)
          ; UpdateDeletedToplevels r.deletedToplevels
          ; UpdateTraces r.newTraces
          ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, false)
          ; SetUnlockedDBs r.unlockedDBs
          ; RequestAnalysis r.toplevels
          ; MakeCmd (Entry.focusEntry m) ]
      else
        let m2 = TL.upsertAll m r.toplevels in
        let m3 = {m2 with userFunctions = r.userFunctions} in
        let newState = processFocus m3 focus in
        Many
          [ UpdateToplevels (r.toplevels, true)
          ; UpdateDeletedToplevels r.deletedToplevels
          ; UpdateTraces r.newTraces
          ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, true)
          ; SetUnlockedDBs r.unlockedDBs
          ; RequestAnalysis r.toplevels
          ; AutocompleteMod ACReset
          ; ClearError
          ; newState ]
  | InitialLoadRPCCallback
      (focus, extraMod (* for integration tests, maybe more *), Ok r) ->
      let m2 =
        {m with toplevels = r.toplevels; userFunctions = r.userFunctions}
      in
      let newState = processFocus m2 focus in
      Many
        [ SetToplevels (r.toplevels, true)
        ; SetDeletedToplevels r.deletedToplevels
        ; UpdateTraces r.newTraces
        ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, true)
        ; SetUnlockedDBs r.unlockedDBs
        ; RequestAnalysis r.toplevels
        ; AutocompleteMod ACReset
        ; ClearError
        ; extraMod
        ; newState ]
  | SaveTestRPCCallback (Ok msg_) ->
      DisplayError ("Success! " ^ msg_)
  | ExecuteFunctionRPCCallback (params, Ok (dval, hash)) ->
      let tl = TL.getTL m params.efpTLID in
      Many
        [ UpdateTraceFunctionResult
            ( params.efpTLID
            , params.efpTraceID
            , params.efpCallerID
            , params.efpFnName
            , hash
            , dval )
        ; ExecutingFunctionComplete [(params.efpTLID, params.efpCallerID)]
        ; RequestAnalysis [tl] ]
  | GetAnalysisRPCCallback (params, Ok (newTraces, (f404s, ts), unlockedDBs))
    ->
      let analysisTLs =
        List.filter
          ~f:(fun tl -> List.member ~value:tl.id params.tlids)
          m.toplevels
      in
      let maybeUpdateTraces =
        if params.ignoreTraces then NoChange else UpdateTraces newTraces
      in
      let maybeAppend404s =
        if params.ignore404s then NoChange else Append404s (f404s, ts)
      in
      Many
        [ TweakModel Sync.markResponseInModel
        ; maybeUpdateTraces
        ; maybeAppend404s
        ; SetUnlockedDBs unlockedDBs
        ; RequestAnalysis analysisTLs ]
  | NewTracePush (tlid, traceID) ->
      if VariantTesting.variantIsActive m PushAnalysis
      then AddUnfetchedTrace (tlid, traceID)
      else NoChange
  | New404Push (f404, ts) ->
      Append404s ([f404], ts)
  | GetDelete404RPCCallback (Ok (f404s, ts)) ->
      Set404s (f404s, ts)
  | ReceiveAnalysis result ->
    ( match result with
    | Ok (id, analysisResults) ->
        UpdateAnalysis (id, analysisResults)
    | Error (AnalysisExecutionError (_, str)) ->
        DisplayError str
    | Error (AnalysisParseError str) ->
        DisplayError str )
  | ReceiveTraces (TraceFetchFailure str) ->
      DisplayAndReportError str
  | ReceiveTraces (TraceFetchSuccess res) ->
      let newTraces, (f404s, ts), unlockedDBs = res.result in
      let analysisTLs =
        List.filter
          ~f:(fun tl -> List.member ~value:tl.id res.params.tlids)
          m.toplevels
      in
      Many
        [ TweakModel Sync.markResponseInModel
        ; UpdateTraces newTraces
        ; Append404s (f404s, ts)
        ; SetUnlockedDBs unlockedDBs
        ; RequestAnalysis analysisTLs ]
  | RPCCallback (_, _, Error err) ->
      DisplayAndReportHttpError ("RPC", err)
  | SaveTestRPCCallback (Error err) ->
      DisplayError ("Error: " ^ Tea_http.string_of_error err)
  | ExecuteFunctionRPCCallback (_, Error err) ->
      DisplayAndReportHttpError ("ExecuteFunction", err)
  | InitialLoadRPCCallback (_, _, Error err) ->
      DisplayAndReportHttpError ("InitialLoad", err)
  | GetAnalysisRPCCallback (_, Error err) ->
      DisplayAndReportHttpError ("GetAnalysis", err)
  | GetDelete404RPCCallback (Error err) ->
      DisplayAndReportHttpError ("Delete404", err)
  | JSError msg_ ->
      DisplayError ("Error in JS: " ^ msg_)
  | WindowResize (_, _) ->
      (* just receiving the subscription will cause a redraw, which uses *)
      (* the native sizing function. *)
      NoChange
  | FocusEntry _ ->
      NoChange
  | NothingClick _ ->
      NoChange
  | FocusAutocompleteItem _ ->
      NoChange
  | LocationChange loc ->
      Url.changeLocation m loc
  | TimerFire (action, _) ->
    ( match action with
    | RefreshAnalysis ->
        let ignorePushed = VariantTesting.variantIsActive m PushAnalysis in
        GetAnalysisRPC (ignorePushed, ignorePushed)
    | CheckUrlHashPosition ->
        Url.maybeUpdateScrollUrl m )
  | Initialization ->
      NoChange
  | AddRandom ->
      NoChange
  | IgnoreMsg ->
      (* Many times we have to receive a Msg and we don't actually do anything.
       * To lower to conceptual load, we send an IgnoreMsg, rather than a
       * different msg each time that we have to understand. *)
      NoChange
  | PageVisibilityChange vis ->
      TweakModel (fun m_ -> {m_ with visibility = vis})
  | CreateHandlerFrom404 ({space; path; modifier} as fof) ->
      let center = findCenter m in
      let tlid = gtlid () in
      let aPos = center in
      let ast = B.new_ () in
      let aHandler =
        { ast
        ; spec =
            { module_ = B.newF space
            ; name = B.newF path
            ; modifier = B.newF modifier }
        ; tlid }
      in
      Many
        [ RPC
            ([SetHandler (tlid, aPos, aHandler)], FocusExact (tlid, B.toID ast))
        ; Delete404 fof ]
  | Delete404 fof ->
      MakeCmd (RPC.delete404RPC (contextFromModel m) fof)
  | MarkRoutingTableOpen (shouldOpen, key) ->
      TweakModel
        (fun m ->
          { m with
            routingTableOpenDetails =
              ( if shouldOpen
              then StrSet.add ~value:key m.routingTableOpenDetails
              else StrSet.remove ~value:key m.routingTableOpenDetails ) } )
  | CreateRouteHandler space ->
      let center = findCenter m in
      let action =
        match space with
        | Some "HTTP" ->
            NewHTTPHandler None
        | Some spacename ->
            NewEventSpace spacename
        | None ->
            NewHandler None
      in
      Entry.submitOmniAction center action
  | CreateDBTable ->
      let center = findCenter m
      and genName = DB.generateDBName () in
      DB.createDB genName center
  | CreateFunction ->
      let ufun = Refactor.generateEmptyFunction () in
      Many
        [ RPC ([SetFunction ufun], FocusNothing)
        ; MakeCmd (Url.navigateTo (Fn (ufun.ufTLID, Defaults.centerPos))) ]
  | LockHandler (tlid, isLocked) ->
      Editor.updateLockedHandlers tlid isLocked m
  | EnablePanning pan ->
      let c = m.canvas in
      TweakModel (fun m_ -> {m_ with canvas = {c with enablePan = pan}})
  | ShowErrorDetails show ->
      let e = m.error in
      TweakModel (fun m -> {m with error = {e with showDetails = show}})


let update (m : model) (msg : msg) : model * msg Cmd.t =
  let mods = update_ msg m in
  let newm, newc = updateMod mods (m, Cmd.none) in
  let state = m |> Editor.model2editor |> Editor.toString in
  Dom.Storage.setItem
    ("editorState-" ^ m.canvasName)
    state
    Dom.Storage.localStorage ;
  ({newm with lastMsg = msg; lastMod = mods}, newc)


let subscriptions (m : model) : msg Tea.Sub.t =
  let keySubs = [Keyboard.downs (fun x -> GlobalKeyPress x)] in
  let resizes =
    [ Native.Window.OnResize.listen ~key:"window_on_resize" (fun (w, h) ->
          WindowResize (w, h) ) ]
  in
  let dragSubs =
    match m.cursorState with
    (* we use IDs here because the node will change *)
    (* before they're triggered *)
    | Dragging (id, _, _, _) ->
        let listenerKey = "mouse_moves_" ^ deTLID id in
        [ Native.DarkMouse.moves ~key:listenerKey (fun x -> DragToplevel (id, x)
          ) ]
    | _ ->
        []
  in
  let timers =
    if m.timersEnabled
    then
      ( match m.visibility with
      | Hidden ->
          []
      | Visible ->
          [ Patched_tea_time.every
              ~key:"refresh_analysis"
              Tea.Time.second
              (fun f -> TimerFire (RefreshAnalysis, f) ) ] )
      @ [ Patched_tea_time.every
            ~key:"check_url_hash_position"
            Tea.Time.second
            (fun f -> TimerFire (CheckUrlHashPosition, f) ) ]
    else []
  in
  let onError =
    [ Native.DisplayClientError.listen ~key:"display_client_error" (fun s ->
          JSError s ) ]
  in
  let visibility =
    [ Native.Window.OnFocusChange.listen ~key:"window_on_focus_change" (fun v ->
          if v
          then PageVisibilityChange Visible
          else PageVisibilityChange Hidden ) ]
  in
  let mousewheelSubs =
    [ Native.OnWheel.listen ~key:"on_wheel" (fun (dx, dy) -> MouseWheel (dx, dy)
      ) ]
  in
  let analysisSubs =
    [ Analysis.ReceiveAnalysis.listen ~key:"receive_analysis" (fun s ->
          ReceiveAnalysis s )
    ; Analysis.NewTracePush.listen ~key:"new_trace_push" (fun s ->
          NewTracePush s )
    ; Analysis.New404Push.listen ~key:"new_404_push" (fun s -> New404Push s)
    ; Analysis.ReceiveTraces.listen ~key:"receive_traces" (fun s ->
          ReceiveTraces s ) ]
  in
  Tea.Sub.batch
    (List.concat
       [ keySubs
       ; dragSubs
       ; resizes
       ; timers
       ; visibility
       ; onError
       ; mousewheelSubs
       ; analysisSubs ])


let debugging =
  let prog =
    Tea.Debug.debug
      show_msg
      { init = (fun a -> init a (Tea.Navigation.getLocation ()))
      ; view = View.view
      ; update
      ; subscriptions
      ; shutdown = (fun _ -> Cmd.none) }
  in
  let myInit flag _ = prog.init flag in
  Tea.Navigation.navigationProgram
    (fun x -> Tea.Debug.ClientMsg (LocationChange x))
    { init = myInit
    ; update = prog.update
    ; view = prog.view
    ; subscriptions = prog.subscriptions
    ; shutdown = prog.shutdown }


let normal =
  let program : (string, model, msg) Tea.Navigation.navigationProgram =
    { init
    ; view = View.view
    ; update
    ; subscriptions
    ; shutdown = (fun _ -> Cmd.none) }
  in
  Tea.Navigation.navigationProgram (fun x -> LocationChange x) program


let main = normal
