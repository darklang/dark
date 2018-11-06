open Tea
open! Porting
module AC = Autocomplete
module B = Blank
module P = Pointer
module RT = Runtime
module TL = Toplevel
open Prelude
open Types
module JSE = Json_encode_extended
module JSD = Json_decode_extended
module Key = Keyboard

let init (flagString: string) (location : Web.Location.location) : model * msg Cmd.t =
  let {Flags.editorState; complete; userContentHost; environment; csrfToken} =
    Flags.fromString flagString
  in
  let savedEditor = Editor.fromString editorState in
  let m0 = Editor.editor2model savedEditor in
  let _ = "comment" in
  let savedCursorState = m0.cursorState in
  let savedCurrentPage = m0.currentPage in
  let m =
    { m0 with
      cursorState= Deselected
    ; currentPage= (Defaults.defaultModel |> fun x -> x.currentPage) }
  in
  let tests =
    match VariantTesting.parseVariantTestsFromQueryString location.search with
    | Some t -> t
    | None -> []
  in
  let page =
    Url.parseLocation m location |> Option.withDefault m.currentPage
  in
  let canvas = m.canvas in
  let newCanvas =
    match page with
    | Toplevels pos -> {canvas with offset= pos}
    | Fn (_, pos) -> {canvas with fnOffset= pos}
  in
  let shouldRunIntegrationTest =
    String.endsWith "/integration_test" location.pathname
  in
  let isAdmin = false in
  let canvasName = Url.parseCanvasName location in
  let integrationTestName = canvasName in
  let m2 =
    { m with
      builtInFunctions = complete
    ; complete= AC.init complete isAdmin
    ; tests
    ; toplevels= []
    ; currentPage= page
    ; canvas= newCanvas
    ; canvasName
    ; userContentHost
    ; environment
    ; csrfToken
    }
  in
  if shouldRunIntegrationTest then
    ( m2
    , Cmd.batch
        [RPC.integrationRPC m2 (contextFromModel m2) integrationTestName
        ]
    )
  else
    ( m2
    , Cmd.batch
        [ RPC.initialLoadRPC (contextFromModel m2)
            (FocusPageAndCursor (page, savedCursorState))
        ; RPC.getAnalysisRPC (contextFromModel m2) []
        ] )

let updateError (oldErr : darkError) (newErrMsg : string) : darkError =
  if oldErr.message = Some newErrMsg && not oldErr.showDetails then oldErr
  else {message= Some newErrMsg; showDetails= true}

let sendRollbar x : msg Tea.Cmd.t =
  (* TODO: porting *)
  Js.log "TODO: sendRollbar";
  Tea.Cmd.none

(* let sendTask (t : msg) : msg Cmd.t = Task.succeed t |> Task.perform identity *)

let processFocus (m : model) (focus : focus) : modification =
  match focus with
  | FocusNext (tlid, pred) -> (
      let tl = TL.getTL m tlid in
      let predPd = Option.andThen (TL.find tl) pred in
      let next = TL.getNextBlank tl predPd in
      match next with
      | Some pd -> Enter (Filling (tlid, P.toID pd))
      | None -> Select (tlid, pred) )
  | FocusExact (tlid, id) ->
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      if P.isBlank pd || P.toContent pd = Some "" then
        Enter (Filling (tlid, id))
      else Select (tlid, Some id)
  | FocusSame -> (
    match unwrapCursorState m.cursorState with
    | Selecting (tlid, mId) -> (
      match (TL.get m tlid, mId) with
      | Some tl, Some id ->
          if TL.isValidID tl id then NoChange else Select (tlid, None)
      | Some tl, None -> Select (tlid, None)
      | _ -> Deselect )
    | Entering (Filling (tlid, id)) -> (
      match TL.get m tlid with
      | Some tl -> if TL.isValidID tl id then NoChange else Select (tlid, None)
      | _ -> Deselect )
    | _ -> NoChange )
  | FocusPageAndCursor (page, cs) ->
      let setCS = SetCursorState cs in
      let noTarget = ACSetTarget None in
      let target tuple = ACSetTarget (Some tuple) in
      let tlOnPage tl =
        match page with
        | Toplevels _ -> (
          match tl.data with
          | TLHandler _ -> true
          | TLDB _ -> true
          | TLFunc _ -> false )
        | Fn (id, _) -> tl.id = id
      in
      let nextCursor, acTarget =
        match cs with
        | Selecting (tlid, mId) -> (
          match (TL.get m tlid, mId) with
          | Some tl, Some id ->
              if TL.isValidID tl id && tlOnPage tl then
                (setCS, target (tlid, TL.findExn tl id))
              else (Deselect, noTarget)
          | Some tl, None -> (setCS, noTarget)
          | _ -> (Deselect, noTarget) )
        | Entering (Filling (tlid, id)) -> (
          match TL.get m tlid with
          | Some tl ->
              if TL.isValidID tl id && tlOnPage tl then
                (setCS, target (tlid, TL.findExn tl id))
              else (Deselect, noTarget)
          | _ -> (Deselect, noTarget) )
        | Dragging (tlid, _, _, _) -> (
          match TL.get m tlid with
          | Some tl ->
              if tlOnPage tl then (setCS, noTarget) else (Deselect, noTarget)
          | _ -> (Deselect, noTarget) )
        | _ -> (Deselect, noTarget)
      in
      Many [SetPage page; nextCursor; AutocompleteMod acTarget]
  | FocusNothing -> Deselect
  | FocusNoChange -> NoChange

let processAutocompleteMods (m : model) (mods : autocompleteMod list) :
    model * msg Cmd.t =
  if m.integrationTestState <> NoIntegrationTest then
    Debug.loG "autocompletemod update" (show_list show_autocompleteMod mods |> show_clean);
  let complete =
    List.foldl
      (fun mod_ complete_ -> AC.update m mod_ complete_)
      m.complete mods
  in
  let focus =
    match unwrapCursorState m.cursorState with
    | Entering _ -> AC.focusItem complete.index
    | SelectingCommand (_, _) -> AC.focusItem complete.index
    | _ -> Cmd.none
  in

  (if m.integrationTestState <> NoIntegrationTest then
    let i = complete.index in
    let val_ = AC.getValue complete in
    Debug.loG "autocompletemod result: "
      (string_of_int complete.index ^ " => '" ^ val_ ^ "'"));

  ({m with complete}, focus)

let rec updateMod (mod_ : modification) ((m, cmd) : model * msg Cmd.t) :
    model * msg Cmd.t =
  if m.integrationTestState <> NoIntegrationTest then
    Debug.loG "mod update" (show_modification mod_ |> show_clean);

  let closeBlanks newM =
    m.cursorState |> tlidOf
    |> Option.andThen (TL.get m)
    |> Option.map (fun tl ->
           match tl.data with
           | TLHandler h ->
               let replacement = AST.closeBlanks h.ast in
               if replacement = h.ast then []
               else
                 let newH = {h with ast= replacement} in
                 let ops = [SetHandler (tl.id, tl.pos, newH)] in
                 let params = RPC.opsParams ops in
                 let _ = "comment" in
                 [RPC.rpc newM (contextFromModel newM) FocusSame params]
           | TLFunc f ->
               let replacement = AST.closeBlanks f.ufAST in
               if replacement = f.ufAST then []
               else
                 let newF = {f with ufAST= replacement} in
                 let ops = [SetFunction newF] in
                 let params = RPC.opsParams ops in
                 let _ = "comment" in
                 [RPC.rpc newM (contextFromModel newM) FocusSame params]
           | _ -> [] )
    |> Option.withDefault []
    |> fun rpc ->
    if tlidOf newM.cursorState = tlidOf m.cursorState then [] else rpc
  in
  let newm, newcmd =
    let handleRPC params focus =
      let hasNonHandlers =
        List.any
          (fun c ->
            match c with
            | SetHandler (_, _, _) -> false
            | SetFunction _ -> false
            | _ -> true )
          params.ops
      in
      if hasNonHandlers then (m, RPC.rpc m (contextFromModel m) focus params)
      else
        let localM =
          List.foldl
            (fun call m ->
              match call with
              | SetHandler (tlid, pos, h) ->
                  TL.upsert m {id= tlid; pos; data= TLHandler h}
              | SetFunction f -> Functions.upsert m f
              | _ -> m )
            m params.ops
        in
        let withFocus, wfCmd =
          updateMod
            (Many [AutocompleteMod ACReset; processFocus localM focus])
            (localM, Cmd.none)
        in
        ( withFocus
        , Cmd.batch
            [wfCmd; RPC.rpc withFocus (contextFromModel withFocus) FocusNoChange params]
        )
    in
    match mod_ with
    | DisplayError e -> ({m with error= updateError m.error e}, Cmd.none)
    | DisplayAndReportError e ->
        let json =
          JSE.object_
            [ ("message", JSE.string e)
            ; ("url", JSE.null)
            ; ("custom", JSE.object_ []) ]
        in
        ({m with error= updateError m.error e}, sendRollbar json)
    | DisplayAndReportHttpError (context, e) ->
        let response =
          match e with
          | Http.BadStatus r -> Some r
          | Http.BadPayload (_, r) -> Some r
          | _ -> None
        in
        let body str =
          let maybe name m =
            match m with Some s -> ", " ^ name ^ ": " ^ s | None -> ""
          in
          str
          |> JSD.decodeString Decoders.exception_
          |> Result.toOption
          |> Option.map
               (fun { short
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
                 " (" ^ exceptionTipe ^ "): " ^ short ^ maybe "message" long
                 ^ maybe "actual value" actual
                 ^ maybe "actual type" actualType
                 ^ maybe "result" result
                 ^ maybe "result type" resultType
                 ^ maybe "expected" expected
                 ^ ( if info = StrDict.empty then ""
                   else ", info: " ^ toString info )
                 ^
                 if workarounds = [] then ""
                 else ", workarounds: " ^ toString workarounds )
          |> Option.withDefault str
        in
        let msg =
          match e with
          | Http.BadUrl str -> "Bad url: " ^ str
          | Http.Timeout -> "Timeout"
          | Http.NetworkError -> "Network error - is the server running?"
          | Http.BadStatus response ->
              "Bad status: "
              ^ response.status.message
                  (* TODO: PORTING  *)
              (* ^ body response.body *)
              ^ "TODO: decode body response"
          | Http.BadPayload (msg, _) -> "Bad payload (" ^ context ^ "): " ^ msg
          | Http.Aborted -> "Request Aborted"
        in
        let url =
          match e with
          | Http.BadUrl str -> Some str
          | Http.Timeout -> None
          | Http.NetworkError -> None
          | Http.BadStatus response -> Some response.url
          | Http.BadPayload (_, response) -> Some response.url
          | Http.Aborted -> None
        in
        let shouldRollbar = e <> Http.NetworkError in
        let json =
          JSE.object_
            [ ("message", JSE.string (msg ^ " (" ^ context ^ ")"))
            ; ("url", JSE.nullable JSE.string url)
            ; ("custom", Encoders.httpError e) ]
        in
        let cmds = if shouldRollbar then [sendRollbar json] else [] in
        ({m with error= updateError m.error msg}, Cmd.batch cmds)
    | ClearError ->
        ({m with error= {message= None; showDetails= false}}, Cmd.none)
    | RPC (ops, focus) -> handleRPC (RPC.opsParams ops) focus
    | RPCFull (params, focus) -> handleRPC params focus
    | GetAnalysisRPC -> Sync.fetch m
    | NoChange -> (m, Cmd.none)
    | TriggerIntegrationTest name ->
        let expect = IntegrationTest.trigger name in
        ({m with integrationTestState= expect}, Cmd.none)
    | EndIntegrationTest ->
        let expectationFn =
          match m.integrationTestState with
          | IntegrationTestExpectation fn -> fn
          | IntegrationTestFinished _ ->
              impossible
                "Attempted to end integration test but one ran + was already \
                 finished"
          | NoIntegrationTest ->
              impossible
                "Attempted to end integration test but none was running"
        in
        let result = expectationFn m in
        ( {m with integrationTestState= IntegrationTestFinished result}
        , Cmd.none )
    | MakeCmd cmd -> (m, cmd)
    | SetCursorState cursorState ->
        let newM = {m with cursorState} in
        (newM, Entry.focusEntry newM)
    | SetPage page -> (
        if m.currentPage = page then (m, Cmd.none)
        else
          let canvas = m.canvas in
          match (page, m.currentPage) with
          | Toplevels pos2, Toplevels _ ->
              ( { m with
                  currentPage= page
                ; urlState= { lastPos = pos2 }
                ; canvas= {canvas with offset= pos2} }
              , Cmd.none )
          | Fn (_, pos2), _ ->
              ( { m with
                  currentPage= page
                ; cursorState= Deselected
                ; urlState= { lastPos = pos2 }
                ; canvas= {canvas with fnOffset= pos2} }
              , Cmd.none )
          | _ ->
              let newM = {m with currentPage= page; cursorState= Deselected} in
              (newM, Cmd.batch (closeBlanks newM)) )
    | SetCenter center -> (
      match m.currentPage with
      | Toplevels pos -> ({m with currentPage= Toplevels center}, Cmd.none)
      | Fn (id, pos) -> ({m with currentPage= Fn (id, center)}, Cmd.none) )
    | Select (tlid, p) ->
        let newM = {m with cursorState= Selecting (tlid, p)} in
        (newM, Cmd.batch (closeBlanks newM))
    | Deselect ->
        let newM = {m with cursorState= Deselected} in
        (newM, Cmd.batch (closeBlanks newM))
    | Enter entry ->
        let target =
          match entry with
          | Creating _ -> None
          | Filling (tlid, id) ->
              let tl = TL.getTL m tlid in
              let pd = TL.findExn tl id in
              Some (tlid, pd)
        in
        let m2, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m3 = {m2 with cursorState= Entering entry} in
        (m3, Cmd.batch (closeBlanks m3 @ [acCmd; Entry.focusEntry m3]))
    | SelectCommand (tlid, id) ->
        let m2 = {m with cursorState= SelectingCommand (tlid, id)} in
        let m3, acCmd =
          processAutocompleteMods m2 [ACEnableCommandMode; ACRegenerate]
        in
        (m3, Cmd.batch (closeBlanks m3 @ [acCmd; Entry.focusEntry m3]))
    | SetGlobalVariables globals ->
        let m2 = {m with globals} in
        processAutocompleteMods m2 [ACRegenerate]
    | RemoveToplevel tl -> (Toplevel.remove m tl, Cmd.none)
    | SetToplevels (tls, updateCurrent) ->
        let m2 = {m with toplevels= tls} in
        let _ = "comment" in
        let _ = "comment" in
        let m3 =
          match tlidOf m.cursorState with
          | Some tlid -> (
              if updateCurrent then m2
              else
                let tl = TL.getTL m tlid in
                match tl.data with
                | TLDB _ -> TL.upsert m2 tl
                | TLHandler _ -> TL.upsert m2 tl
                | TLFunc f -> m2 )
          | None -> m2
        in
        let m4 =
          {m3 with deletedToplevels= TL.removeByTLID m3.deletedToplevels tls}
        in
        processAutocompleteMods m4 [ACRegenerate]
    | UpdateToplevels (tls, updateCurrent) ->
        let m2 = TL.upsertAll m tls in
        let _ = "comment" in
        let _ = "comment" in
        let m3 =
          match tlidOf m.cursorState with
          | Some tlid -> (
              if updateCurrent then m2
              else
                let tl = TL.getTL m tlid in
                match tl.data with
                | TLDB _ -> TL.upsert m2 tl
                | TLHandler _ -> TL.upsert m2 tl
                | TLFunc f -> m2 )
          | None -> m2
        in
        let m4 =
          {m3 with deletedToplevels= TL.removeByTLID m3.deletedToplevels tls}
        in
        processAutocompleteMods m4 [ACRegenerate]
    | UpdateDeletedToplevels dtls ->
        let m2 =
          { m with
            deletedToplevels= TL.upsertAllByTLID m.deletedToplevels dtls
          ; toplevels= TL.removeByTLID m.toplevels dtls }
        in
        processAutocompleteMods m2 [ACRegenerate]
    | SetDeletedToplevels dtls ->
        let m2 =
          { m with
            deletedToplevels= dtls; toplevels= TL.removeByTLID m.toplevels dtls
          }
        in
        processAutocompleteMods m2 [ACRegenerate]
    | RequestAnalysis tls ->
        let handlers = TL.handlers tls in
        let dbs = TL.dbs tls in
        let userFns = m.userFunctions in
        let requestAnalysis s =
          Tea_cmd.call (fun _ ->
            Analysis.RequestAnalysis.send s
          )
        in

        let req h =
          let trace = Analysis.getCurrentTrace m h.tlid in
          let param t =
            JSE.object_
              [ ("handler", Encoders.handler h)
              ; ("trace", Encoders.trace t)
              ; ("dbs", JSE.list Encoders.db dbs)
              ; ("user_fns" , JSE.list Encoders.userFunction userFns ) ]
            |> Js.Json.stringify
          in
          trace
          |> Option.map (fun t -> requestAnalysis (param t))
          |> Option.toList
        in
        (m, Cmd.batch (handlers |> List.map req |> List.concat))
    | UpdateAnalysis (id, analysis) ->
        let m2 = {m with analyses= Analysis.record m.analyses id analysis} in
        processAutocompleteMods m2 [ACRegenerate]
    | UpdateTraces traces ->
        let m2 = {m with traces} in
        processAutocompleteMods m2 [ACRegenerate]
    | UpdateTraceFunctionResult (tlid, traceID, callerID, fnName, hash, dval)
      ->
        let m2 =
          Analysis.replaceFunctionResult m tlid traceID callerID fnName hash
            dval
        in
        processAutocompleteMods m2 [ACRegenerate]
    | SetUserFunctions (userFuncs, updateCurrent) ->
        let m2 = {m with userFunctions= userFuncs} in
        let _ = "comment" in
        let _ = "comment" in
        let m3 =
          match tlidOf m.cursorState with
          | Some tlid -> (
              if updateCurrent then m2
              else
                let tl = TL.getTL m tlid in
                match tl.data with
                | TLFunc f -> Functions.upsert m2 f
                | TLDB _ -> m2
                | TLHandler _ -> m2 )
          | None -> m2
        in
        processAutocompleteMods m3 [ACRegenerate]
    | SetUnlockedDBs unlockedDBs -> ({m with unlockedDBs}, Cmd.none)
    | Set404s f404s -> ({m with f404s}, Cmd.none)
    | SetHover p ->
        let nhovering = p :: m.hovering in
        ({m with hovering= nhovering}, Cmd.none)
    | ClearHover p ->
        let nhovering = List.filter (fun m -> m <> p) m.hovering in
        ({m with hovering= nhovering}, Cmd.none)
    | SetCursor (tlid, cur) ->
        let m2 = Analysis.setCursor m tlid cur in
        (m2, Cmd.none)
    | CopyToClipboard clipboard -> ({m with clipboard}, Cmd.none)
    | Drag (tlid, offset, hasMoved, state) ->
        ( {m with cursorState= Dragging (tlid, offset, hasMoved, state)}
        , Cmd.none )
    | ExecutingFunctionBegan (tlid, id) ->
        let nexecutingFunctions = m.executingFunctions @ [(tlid, id)] in
        ({m with executingFunctions= nexecutingFunctions}, Cmd.none)
    | ExecutingFunctionRPC (tlid, id, name) -> (
      match Analysis.getCurrentTrace m tlid with
      | Some trace -> (
        match Analysis.getArguments m tlid trace.traceID id with
        | Some args ->
            let params =
              { efpTLID= tlid
              ; efpCallerID= id
              ; efpTraceID= trace.traceID
              ; efpFnName= name
              ; efpArgs= args }
            in
            (m, RPC.executeFunctionRPC (contextFromModel m) params)
        | None -> (m, Cmd.none
                        (* TODO: PORTING *)
                   (* sendTask (ExecuteFunctionCancel (tlid, id)) *)
      ))
      | None -> (m,
                        (* TODO: PORTING *)
                 (* sendTask (ExecuteFunctionCancel (tlid, id)) *)
                 Cmd.none
                ) )
    | ExecutingFunctionComplete targets ->
        let isComplete target = not <| List.member target targets in
        let nexecutingFunctions =
          List.filter isComplete m.executingFunctions
        in
        ({m with executingFunctions= nexecutingFunctions}, Cmd.none)
    | SetLockedHandlers locked -> ({m with lockedHandlers= locked}, Cmd.none)
    | MoveCanvasTo (canvas, page, pos) ->
        let canvas2 =
          match page with
          | Toplevels _ -> {canvas with offset= pos}
          | Fn (_, _) -> {canvas with fnOffset= pos}
        in
        ({m with canvas= canvas2}, Cmd.none)
    | TweakModel fn -> (fn m, Cmd.none)
    | AutocompleteMod mod_ -> processAutocompleteMods m [mod_]
    | Many mods -> List.foldl updateMod (m, Cmd.none) mods
  in
  (newm, Cmd.batch [cmd; newcmd])

let isFieldAccessDot (m : model) (baseStr : string) : bool =
  let str = Regex.replace "\\.*$" "" baseStr in
  let intOrString =
    String.startsWith "\"" str || Decoders.typeOfLiteralString str = TInt
  in
  match m.cursorState with
  | Entering (Creating _) -> not intOrString
  | Entering (Filling (tlid, id)) ->
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      (P.typeOf pd = Expr || P.typeOf pd = Field) && not intOrString
  | _ -> false

let enableTimers (m : model) : model = {m with timersEnabled= true}

let disableTimers (m : model) : model = {m with timersEnabled= false}

let toggleTimers (m : model) : model =
  {m with timersEnabled= not m.timersEnabled}

let findCenter (m : model) : pos =
  match m.currentPage with
  | Toplevels center -> Viewport.toCenter center
  | _ -> Defaults.centerPos

let update_ (msg : msg) (m : model) : modification =
  if m.integrationTestState <> NoIntegrationTest then
    Debug.loG "msg update" (show_msg msg |> show_clean);
  match msg with
  | GlobalKeyPress event -> (
      if (event.metaKey || event.ctrlKey) && event.keyCode = Key.Z then
        match tlidOf m.cursorState with
        | Some tlid -> (
            let undo =
              if event.shiftKey then RPC ([RedoTL tlid], FocusSame)
              else RPC ([UndoTL tlid], FocusSame)
            in
            match TL.getTL m tlid |> TL.asDB with
            | Some db ->
                if DB.isLocked m tlid then
                  DisplayError "Cannot undo/redo in locked DBs"
                else undo
            | None -> undo )
        | None -> NoChange
      else
        match m.cursorState with
        | Selecting (tlid, mId) -> (
            let tl = TL.getTL m tlid in
            match event.keyCode with
            | Key.Delete -> Selection.delete m tlid mId
            | Key.Backspace -> Selection.delete m tlid mId
            | Key.Escape -> (
              match mId with Some _ -> Select (tlid, None) | None -> Deselect )
            | Key.Enter -> (
                if event.shiftKey then
                  match tl.data with
                  | TLDB _ ->
                      let blankid = gid () in
                      RPC
                        ( [AddDBCol (tlid, blankid, gid ())]
                        , FocusExact (tlid, blankid) )
                  | TLHandler h -> (
                    match mId with
                    | Some id -> (
                      match TL.findExn tl id with
                      | PExpr _ ->
                          let blank = B.new_ () in
                          let replacement =
                            AST.addThreadBlank id blank h.ast
                          in
                          if h.ast = replacement then NoChange
                          else
                            RPC
                              ( [ SetHandler
                                    (tl.id, tl.pos, {h with ast= replacement})
                                ]
                              , FocusExact (tlid, B.toID blank) )
                      | PVarBind _ -> (
                        match AST.parentOf_ id h.ast with
                        | Some (F (_, Lambda (_, _))) ->
                            let replacement = AST.addLambdaBlank id h.ast in
                            RPC
                              ( [ SetHandler
                                    (tl.id, tl.pos, {h with ast= replacement})
                                ]
                              , FocusNext (tlid, Some id) )
                        | _ -> NoChange )
                      | PKey _ ->
                          let nextid, _, replacement =
                            AST.addObjectLiteralBlanks id h.ast
                          in
                          RPC
                            ( [ SetHandler
                                  (tl.id, tl.pos, {h with ast= replacement}) ]
                            , FocusExact (tlid, nextid) )
                      | _ -> NoChange )
                    | None -> NoChange )
                  | TLFunc f -> (
                    match mId with
                    | Some id -> (
                      match TL.findExn tl id with
                      | PExpr _ ->
                          let blank = B.new_ () in
                          let replacement =
                            AST.addThreadBlank id blank f.ufAST
                          in
                          if f.ufAST = replacement then NoChange
                          else
                            RPC
                              ( [SetFunction {f with ufAST= replacement}]
                              , FocusExact (tlid, B.toID blank) )
                      | PVarBind _ -> (
                        match AST.parentOf_ id f.ufAST with
                        | Some (F (_, Lambda (_, _))) ->
                            let replacement = AST.addLambdaBlank id f.ufAST in
                            RPC
                              ( [SetFunction {f with ufAST= replacement}]
                              , FocusNext (tlid, Some id) )
                        | _ -> NoChange )
                      | PKey _ ->
                          let nextid, _, replacement =
                            AST.addObjectLiteralBlanks id f.ufAST
                          in
                          RPC
                            ( [SetFunction {f with ufAST= replacement}]
                            , FocusExact (tlid, nextid) )
                      | PParamTipe _ ->
                          let replacement = Functions.extend f in
                          let newCalls =
                            Refactor.addNewFunctionParameter m f
                          in
                          RPC
                            ( [SetFunction replacement] @ newCalls
                            , FocusNext (tlid, Some id) )
                      | PParamName _ ->
                          let replacement = Functions.extend f in
                          let newCalls =
                            Refactor.addNewFunctionParameter m f
                          in
                          RPC
                            ( [SetFunction replacement] @ newCalls
                            , FocusNext (tlid, Some id) )
                      | PFnName _ ->
                          let replacement = Functions.extend f in
                          let newCalls =
                            Refactor.addNewFunctionParameter m f
                          in
                          RPC
                            ( [SetFunction replacement] @ newCalls
                            , FocusNext (tlid, Some id) )
                      | _ -> NoChange )
                    | None -> NoChange )
                else
                  match mId with
                  | Some id -> Selection.enter m tlid id
                  | None -> Selection.selectDownLevel m tlid mId )
            | Key.Up ->
                if event.shiftKey then Selection.selectUpLevel m tlid mId
                else Selection.moveUp m tlid mId
            | Key.Down ->
                if event.shiftKey then Selection.selectDownLevel m tlid mId
                else Selection.moveDown m tlid mId
            | Key.Right ->
                if event.altKey then Selection.moveCursorBackInTime m tlid
                else Selection.moveRight m tlid mId
            | Key.Left ->
                if event.altKey then Selection.moveCursorForwardInTime m tlid
                else Selection.moveLeft m tlid mId
            | Key.Tab ->
                if event.shiftKey then Selection.selectPrevBlank m tlid mId
                else Selection.selectNextBlank m tlid mId
            | Key.N ->
                if event.ctrlKey then Selection.selectNextSibling m tlid mId
                else NoChange
            | Key.P ->
                if event.ctrlKey then
                  Selection.selectPreviousSibling m tlid mId
                else NoChange
            | Key.C ->
                if event.ctrlKey && event.altKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WIfCond m tl pd
                else if event.ctrlKey then
                  let mPd = Option.map (TL.findExn tl) mId in
                  Clipboard.copy m tl mPd
                else NoChange
            | Key.V ->
                if event.ctrlKey then
                  match mId with
                  | None -> (
                    match TL.rootOf tl with
                    | Some pd -> Clipboard.paste m tl (P.toID pd)
                    | None -> NoChange )
                  | Some id -> Clipboard.paste m tl id
                else NoChange
            | Key.X ->
                if event.ctrlKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Clipboard.cut m tl pd
                else NoChange
            | Key.F ->
                if event.ctrlKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.extractFunction m tl pd
                else NoChange
            | Key.B ->
                if event.ctrlKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WLetBody m tl pd
                else NoChange
            | Key.L ->
                if event.ctrlKey && event.shiftKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.extractVariable m tl pd
                else if event.ctrlKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WLetRHS m tl pd
                else NoChange
            | Key.I ->
                if event.ctrlKey && event.altKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WIfElse m tl pd
                else if event.ctrlKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.wrap WIfThen m tl pd
                else NoChange
            | Key.E ->
                if event.altKey then
                  match mId with
                  | None -> NoChange
                  | Some id ->
                      let pd = TL.findExn tl id in
                      Refactor.toggleOnRail m tl pd
                else NoChange
            | Key.Unknown c -> (
              match mId with
              | None -> NoChange
              | Some id ->
                  if event.key = Some ":" then
                    Many
                      [ SelectCommand (tlid, id)
                      ; AutocompleteMod (ACSetQuery ":") ]
                  else NoChange )
            | _ -> NoChange )
        | Entering cursor -> (
            if event.ctrlKey then
              match event.keyCode with
              | Key.P -> AutocompleteMod ACSelectUp
              | Key.N -> AutocompleteMod ACSelectDown
              | Key.Enter ->
                if AC.isSmallStringEntry m.complete then
                  Many
                    [ AutocompleteMod (ACAppendQuery "\n")
                    ; MakeCmd (Entry.focusEntry m) ]
                else if AC.isLargeStringEntry m.complete then
                  Entry.submit m cursor Entry.StayHere
                else NoChange
              | Key.V -> (
                  match cursor with
                  | Creating pos -> Clipboard.newFromClipboard m pos
                  | Filling (tlid, p) ->
                    let tl = TL.getTL m tlid in
                    Clipboard.paste m tl p )
              | _ -> NoChange
            else if event.shiftKey && event.keyCode = Key.Enter then
              match cursor with
              | Filling (tlid, p) -> (
                  let tl = TL.getTL m tlid in
                  match tl.data with
                  | TLDB _ -> NoChange
                  | TLHandler h -> Entry.submit m cursor Entry.StartThread
                  | TLFunc f -> Entry.submit m cursor Entry.StartThread )
              | Creating _ -> Entry.submit m cursor Entry.StartThread

            else if event.altKey then
              match event.keyCode with
              | Key.E -> (
                match cursor with
                | Creating pos -> NoChange
                | Filling (tlid, id) ->
                    let tl = TL.getTL m tlid in
                    let pd = TL.findExn tl id in
                    Refactor.toggleOnRail m tl pd )
              | _ -> NoChange
            else
              match event.keyCode with
              | Key.Spacebar ->
                  if m.complete.value = "=" || AC.isStringEntry m.complete then
                    NoChange
                  else Entry.submit m cursor Entry.GotoNext
              | Key.Enter -> (
                  if AC.isLargeStringEntry m.complete then
                    AutocompleteMod (ACSetQuery m.complete.value)
                  else
                    match cursor with
                    | Creating pos -> (
                      match AC.highlighted m.complete with
                      | Some (ACOmniAction action) ->
                          Entry.submitOmniAction m pos action
                      | _ -> Entry.submit m cursor Entry.StayHere )
                    | Filling (_, _) -> Entry.submit m cursor Entry.StayHere )
              | Key.Tab -> (
                match cursor with
                | Filling (tlid, p) ->
                    if AC.isLargeStringEntry m.complete then
                      match event.targetSelectionStart with
                      | Some idx ->
                          let newQ =
                            String.insertAt "\t" (idx + 1) m.complete.value
                          in
                          AutocompleteMod (ACSetQuery newQ)
                      | None -> NoChange
                    else
                      let content = AC.getValue m.complete in
                      let hasContent = content |> String.length |> ( < ) 0 in
                      if event.shiftKey then
                        if hasContent then NoChange
                        else Selection.enterPrevBlank m tlid (Some p)
                      else if hasContent then Entry.submit m cursor Entry.GotoNext
                      else Selection.enterNextBlank m tlid (Some p)
                | Creating _ -> NoChange )
              | Key.Unknown c ->
                  if
                    event.key = Some "." && isFieldAccessDot m m.complete.value
                  then
                    let c_ = m.complete in
                    let _ = "comment" in
                    let _ = "comment" in
                    let newC =
                      {c_ with value= AC.getValue c_ ^ "."; index= -1}
                    in
                    let newM = {m with complete= newC} in
                    Entry.submit newM cursor Entry.GotoNext
                  else NoChange
              | Key.Escape -> (
                match cursor with
                | Creating _ -> Many [Deselect; AutocompleteMod ACReset]
                | Filling (tlid, p) -> (
                    let tl = TL.getTL m tlid in
                    match tl.data with
                    | TLHandler h ->
                        let replacement = AST.closeBlanks h.ast in
                        if replacement = h.ast then
                          Many [Select (tlid, Some p); AutocompleteMod ACReset]
                        else
                          RPC
                            ( [ SetHandler
                                  (tl.id, tl.pos, {h with ast= replacement}) ]
                            , FocusNext (tl.id, None) )
                    | _ -> Many [Select (tlid, Some p); AutocompleteMod ACReset]
                    ) )
              | Key.Up -> AutocompleteMod ACSelectUp
              | Key.Down -> AutocompleteMod ACSelectDown
              | Key.Right -> AC.selectSharedPrefix m.complete
              | Key.Backspace ->
                  let v =
                    if
                      m.complete.value = "\"\""
                      && String.length m.complete.prevValue <= 2
                    then ""
                    else m.complete.value
                  in
                  Many
                    [ AutocompleteMod (ACSetQuery v)
                    ; MakeCmd (Entry.focusEntry m) ]
              | key -> NoChange )
        | Deselected -> (
          match m.currentPage with
          | Fn (_, _) -> NoChange
          | Toplevels center -> (
            match event.keyCode with
            | Key.Enter -> Entry.createFindSpace m
            | Key.A -> if event.ctrlKey then Viewport.pageLeft m else NoChange
            | Key.E -> if event.ctrlKey then Viewport.pageRight m else NoChange
            | Key.F -> if event.ctrlKey then Viewport.pageDown m else NoChange
            | Key.B -> if event.ctrlKey then Viewport.pageUp m else NoChange
            | Key.PageUp -> Viewport.pageUp m
            | Key.PageDown -> Viewport.pageDown m
            | Key.Up -> Viewport.moveUp m
            | Key.Down -> Viewport.moveDown m
            | Key.Left -> Viewport.moveLeft m
            | Key.Right -> Viewport.moveRight m
            | Key.Zero -> Viewport.moveToOrigin m
            | Key.Tab -> Selection.selectNextToplevel m None
            | _ -> NoChange ) )
        | SelectingCommand (tlid, id) -> (
          match event.keyCode with
          | Key.Escape -> Commands.endCommandExecution m tlid id
          | Key.Enter ->
              Commands.executeCommand m tlid id (AC.highlighted m.complete)
          | Key.P ->
              if event.ctrlKey then AutocompleteMod ACSelectUp else NoChange
          | Key.N ->
              if event.ctrlKey then AutocompleteMod ACSelectDown else NoChange
          | Key.Up -> AutocompleteMod ACSelectUp
          | Key.Down -> AutocompleteMod ACSelectDown
          | Key.Right -> AC.selectSharedPrefix m.complete
          | _ -> NoChange )
        | Dragging (_, _, _, _) -> NoChange )
  | EntryInputMsg target ->
      let query = if target = "\"" then "\"\"" else target in
      if String.endsWith "." query && isFieldAccessDot m query then NoChange
      else
        Many [AutocompleteMod (ACSetQuery query); MakeCmd (Entry.focusEntry m)]
  | EntrySubmitMsg -> NoChange
  | AutocompleteClick value -> (
    match unwrapCursorState m.cursorState with
    | Entering cursor ->
        let complete = m.complete in
        let newcomplete = {complete with value} in
        let newm = {m with complete= newcomplete} in
        Entry.submit newm cursor Entry.StayHere
    | _ -> NoChange )
  | GlobalClick event -> (
    match m.currentPage with
    | Toplevels _ ->
        if event.button = Defaults.leftButton then
          match unwrapCursorState m.cursorState with
          | Deselected ->
              Many
                [ AutocompleteMod ACReset
                ; Enter (Creating (Viewport.toAbsolute m event.mePos)) ]
          | _ -> Deselect
        else NoChange
    | _ -> NoChange )
  | BlankOrMouseEnter (_, id, _) -> SetHover id
  | BlankOrMouseLeave (_, id, _) -> ClearHover id
  | MouseWheel (x, y) ->
      if m.canvas.enablePan then Viewport.moveCanvasBy m x y else NoChange
  | DataMouseEnter (tlid, idx, _) -> SetHover (tlCursorID tlid idx)
  | DataMouseLeave (tlid, idx, _) -> ClearHover (tlCursorID tlid idx)
  | DragToplevel (_, mousePos) -> (
    match m.cursorState with
    | Dragging (draggingTLID, startVPos, _, origCursorState) ->
        let xDiff = mousePos.x - startVPos.vx in
        let yDiff = mousePos.y - startVPos.vy in
        let m2 = TL.move draggingTLID xDiff yDiff m in
        Many
          [ SetToplevels (m2.toplevels, true)
          ; Drag
              ( draggingTLID
              , {vx= mousePos.x; vy= mousePos.y}
              , true
              , origCursorState ) ]
    | _ -> NoChange )
  | ToplevelMouseDown (targetTLID, event) ->
      if event.button = Defaults.leftButton then
        let tl = TL.getTL m targetTLID in
        match tl.data with
        | TLFunc _ -> NoChange
        | _ -> Drag (targetTLID, event.mePos, false, m.cursorState)
      else NoChange
  | ToplevelMouseUp (targetTLID, event) ->
      if event.button = Defaults.leftButton then
        match m.cursorState with
        | Dragging (draggingTLID, startVPos, hasMoved, origCursorState) ->
            if hasMoved then
              let tl = TL.getTL m draggingTLID in
              Many
                [ SetCursorState origCursorState
                ; RPC ([MoveTL (draggingTLID, tl.pos)], FocusNoChange) ]
            else SetCursorState origCursorState
        | _ -> NoChange
      else NoChange
  | BlankOrClick (targetTLID, targetID, _) -> (
    match m.cursorState with
    | Deselected -> Select (targetTLID, Some targetID)
    | Dragging (_, _, _, origCursorState) -> SetCursorState origCursorState
    | Entering cursor -> (
      match cursor with
      | Filling (_, fillingID) ->
          if fillingID = targetID then NoChange
          else Select (targetTLID, Some targetID)
      | _ -> Select (targetTLID, Some targetID) )
    | Selecting (_, maybeSelectingID) -> (
      match maybeSelectingID with
      | Some selectingID ->
          if selectingID = targetID then NoChange
          else Select (targetTLID, Some targetID)
      | None -> Select (targetTLID, Some targetID) )
    | SelectingCommand (_, selectingID) ->
        if selectingID = targetID then NoChange
        else Select (targetTLID, Some targetID) )
  | BlankOrDoubleClick (targetTLID, targetID, _) ->
      Selection.enter m targetTLID targetID
  | ToplevelClick (targetTLID, _) -> (
    match m.cursorState with
    | Dragging (_, _, _, origCursorState) -> SetCursorState origCursorState
    | Selecting (selectingTLID, _) -> Select (targetTLID, None)
    | SelectingCommand (selectingTLID, _) -> Select (targetTLID, None)
    | Deselected -> Select (targetTLID, None)
    | Entering _ -> Select (targetTLID, None) )
  | ExecuteFunctionButton (tlid, id, name) ->
      let tl = TL.getTL m tlid in
      Many
        [ ExecutingFunctionBegan (tlid, id)
        ; ExecutingFunctionRPC (tlid, id, name) ]
  | DataClick (tlid, idx, _) -> (
    match m.cursorState with
    | Dragging (_, _, _, origCursorState) -> SetCursorState origCursorState
    | Deselected -> Many [Select (tlid, None); SetCursor (tlid, idx)]
    | _ -> SetCursor (tlid, idx) )
  | StartMigration tlid -> (
      let mdb = tlid |> TL.getTL m |> TL.asDB in
      match mdb with
      | Some db -> DB.startMigration tlid db.cols
      | None -> NoChange )
  | AbandonMigration tlid -> RPC ([AbandonDBMigration tlid], FocusNothing)
  | DeleteColInDB (tlid, nameId) ->
      RPC ([DeleteColInDBMigration (tlid, nameId)], FocusNothing)
  | ToggleTimers -> TweakModel toggleTimers
  | SaveTestButton -> MakeCmd (RPC.saveTestRPC (contextFromModel m))
  | FinishIntegrationTest -> EndIntegrationTest
  | StartFeatureFlag -> FeatureFlags.start m
  | EndFeatureFlag (id, pick) -> FeatureFlags.end_ m id pick
  | ToggleFeatureFlag (id, is) -> FeatureFlags.toggle m id is
  | ExtractFunction -> (
    match m.cursorState with
    | Selecting (tlid, mId) -> (
        let tl = TL.getTL m tlid in
        match mId with
        | None -> NoChange
        | Some id ->
            let pd = TL.findExn tl id in
            Refactor.extractFunction m tl pd )
    | _ -> NoChange )
  | DeleteUserFunctionParameter (uf, upf) ->
      let replacement = Functions.removeParameter uf upf in
      let newCalls = Refactor.removeFunctionParameter m uf upf in
      RPC ([SetFunction replacement] @ newCalls, FocusNext (uf.ufTLID, None))
  | DeleteUserFunction tlid -> RPC ([DeleteFunction tlid], FocusNothing)
  | RestoreToplevel tlid -> RPC ([UndoTL tlid], FocusNext (tlid, None))
  | RPCCallback
      ( focus
      , calls
      , Ok
          ( newToplevels
          , newDeletedToplevels
          , newTraces
          , globals
          , userFuncs
          , unlockedDBs ) ) ->
      if focus = FocusNoChange then
        Many
          [ UpdateToplevels (newToplevels, false)
          ; UpdateDeletedToplevels newDeletedToplevels
          ; UpdateTraces newTraces
          ; SetGlobalVariables globals
          ; SetUserFunctions (userFuncs, false)
          ; SetUnlockedDBs unlockedDBs
          ; RequestAnalysis newToplevels
          ; MakeCmd (Entry.focusEntry m) ]
      else
        let m2 = TL.upsertAll m newToplevels in
        let m3 = {m2 with userFunctions= userFuncs} in
        let newState = processFocus m3 focus in
        Many
          [ UpdateToplevels (newToplevels, true)
          ; UpdateDeletedToplevels newDeletedToplevels
          ; UpdateTraces newTraces
          ; SetGlobalVariables globals
          ; SetUserFunctions (userFuncs, true)
          ; SetUnlockedDBs unlockedDBs
          ; RequestAnalysis newToplevels
          ; AutocompleteMod ACReset
          ; ClearError
          ; newState ]
  | InitialLoadRPCCallback
      ( focus
      , extraMod
      , Ok
          ( toplevels
          , deletedToplevels
          , newTraces
          , globals
          , userFuncs
          , unlockedDBs ) ) ->
      let m2 = {m with toplevels; userFunctions= userFuncs} in
      let newState = processFocus m2 focus in
      Many
        [ SetToplevels (toplevels, true)
        ; SetDeletedToplevels deletedToplevels
        ; UpdateTraces newTraces
        ; SetGlobalVariables globals
        ; SetUserFunctions (userFuncs, true)
        ; SetUnlockedDBs unlockedDBs
        ; RequestAnalysis toplevels
        ; AutocompleteMod ACReset
        ; ClearError
        ; extraMod
        ; newState ]
  | SaveTestRPCCallback (Ok msg_) -> DisplayError ("Success! " ^ msg_)
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
  | ExecuteFunctionCancel (tlid, id) ->
      Many
        [ DisplayError "Traces are not loaded for this handler"
        ; ExecutingFunctionComplete [(tlid, id)] ]
  | GetAnalysisRPCCallback (Ok (newTraces, globals, f404s, unlockedDBs)) ->
      Many
        [ TweakModel Sync.markResponseInModel
        ; UpdateTraces newTraces
        ; SetGlobalVariables globals
        ; Set404s f404s
        ; SetUnlockedDBs unlockedDBs
        ; RequestAnalysis m.toplevels ]
  | GetDelete404RPCCallback (Ok f404s) -> Set404s f404s
  | ReceiveAnalysis json -> (
      let envelope = JSD.decodeString Decoders.analysisEnvelope json in
      match envelope with
      | Ok (id, analysisResults) -> UpdateAnalysis (id, analysisResults)
      | Error str -> DisplayError str
    )
  | RPCCallback (_, _, Error err) -> DisplayAndReportHttpError ("RPC", err)
  | SaveTestRPCCallback (Error err) -> DisplayError ("Error: " ^ toString err)
  | ExecuteFunctionRPCCallback (_, Error err) ->
      DisplayAndReportHttpError ("ExecuteFunction", err)
  | InitialLoadRPCCallback (_, _, Error err) ->
      DisplayAndReportHttpError ("InitialLoad", err)
  | GetAnalysisRPCCallback (Error err) ->
      DisplayAndReportHttpError ("GetAnalysis", err)
  | JSError msg_ -> DisplayError ("Error in JS: " ^ msg_)
  | WindowResize (w, h) -> NoChange
  | FocusEntry _ -> NoChange
  | NothingClick _ -> NoChange
  | FocusAutocompleteItem _ -> NoChange
  | LocationChange loc -> Url.changeLocation m loc
  | TimerFire (action, time) -> (
    match action with
    | RefreshAnalysis -> GetAnalysisRPC
    | CheckUrlHashPosition -> Url.maybeUpdateScrollUrl m )
  | Initialization -> NoChange
  | AddRandom -> NoChange
  | PageVisibilityChange vis ->
    TweakModel (fun m_ -> {m_ with visibility= vis})
  | CreateHandlerFrom404 {space; path; modifier} ->
      let center = findCenter m in
      let anId = gtlid () in
      let aPos = center in
      let aHandler =
        { ast= B.new_ ()
        ; spec=
            { module_= B.newF space
            ; name= B.newF path
            ; modifier= B.newF modifier }
        ; tlid= anId }
      in
      RPC ([SetHandler (anId, aPos, aHandler)], FocusNothing)
  | Delete404 fof -> MakeCmd (RPC.delete404RPC (contextFromModel m) fof)
  | CreateRouteHandler ->
      let center = findCenter m in
      Entry.submitOmniAction m center NewHTTPHandler
  | CreateFunction ->
      let ufun = Refactor.generateEmptyFunction () in
      Many
        [ RPC ([SetFunction ufun], FocusNothing)
        ; MakeCmd (Url.navigateTo (Fn (ufun.ufTLID, Defaults.centerPos))) ]
  | LockHandler (tlid, isLocked) -> Editor.updateLockedHandlers tlid isLocked m
  | EnablePanning pan ->
      let c = m.canvas in
      TweakModel (fun m_ -> {m_ with canvas= {c with enablePan= pan}})
  | ShowErrorDetails show ->
      let e = m.error in
      TweakModel (fun m -> {m with error= {e with showDetails= show}})
  | _ -> NoChange

let update (m : model) (msg : msg) : model * msg Cmd.t =
  let mods = update_ msg m in
  let newm, newc = updateMod mods (m, Cmd.none) in
  let state = m |> Editor.model2editor |> Editor.toString in
  Dom.Storage.setItem "editorState" state Dom.Storage.localStorage;
  ( {newm with lastMsg= msg; lastMod= mods}
  , newc)

let subscriptions (m : model) : msg Sub.t =
  let keySubs = [Keyboard.downs (fun x -> GlobalKeyPress x)] in
  let resizes =
    [ Window.OnResize.listen
      ~key:"window_on_resize"
      (fun (w, h) -> WindowResize (w, h) )
    ]
  in
  let dragSubs =
    match m.cursorState with
    | Dragging (id, offset, _, _) ->
      let listenerKey = "mouse_moves_" ^ string_of_int (deTLID id) in
      [DarkMouse.moves ~key:listenerKey (fun x -> DragToplevel (id, x))]
    | _ -> []
  in
  let timers =
    if m.timersEnabled
    then
      (match m.visibility with
      | Hidden -> []
      | Visible ->
        [ Patched_tea_time.every
          ~key: "refresh_analysis"
          Tea.Time.second
          (fun f -> TimerFire (RefreshAnalysis, f))
        ]
      ) @
      [ Patched_tea_time.every
        ~key: "check_url_hash_position"
        Time.second
        (fun f -> TimerFire (CheckUrlHashPosition, f))
      ]
    else [] in
  let onError =
    [DisplayClientError.listen ~key:"display_client_error" (fun s -> JSError s)]
  in
  let visibility =
    [Window.OnFocusChange.listen ~key:"window_on_focus_change"
      (fun v ->
        if v
        then PageVisibilityChange Visible
        else PageVisibilityChange Hidden
     )
    ]
  in
  let mousewheelSubs =
    [ OnWheel.listen
      ~key:"on_wheel"
      (fun (dx, dy) -> MouseWheel (dx, dy))
    ]
  in
  let analysisSubs =
    [ Analysis.ReceiveAnalysis.listen
      ~key:"receive_analysis"
      (fun s -> ReceiveAnalysis s)
    ]
  in
  Sub.batch
    (List.concat
       [ keySubs
       ; dragSubs
       ; resizes
       ; timers
       ; visibility
       ; onError
       ; mousewheelSubs
       ; analysisSubs
       ])

let main =
  Navigation.navigationProgram (fun x -> LocationChange x)
    { init
    ; view= View.view
    ; update
    ; subscriptions
    ; shutdown = (fun _ -> Cmd.none)
    }


