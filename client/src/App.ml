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
  let m = editorState |> Editor.fromString |> Editor.editor2model in
  let page =
    Url.parseLocation location
    |> Option.withDefault ~default:Defaults.defaultModel.currentPage
  in
  (* these saved values may not be valid yet *)
  let savedCursorState = m.cursorState in
  let m =
    { m with
      cursorState =
        Deselected
        (* deselect for now as the selected blank isn't available yet *)
    ; currentPage = page
    ; builtInFunctions = complete
    ; complete = AC.init m
    ; tests = VariantTesting.enabledVariantTests
    ; toplevels = []
    ; canvasName = Url.parseCanvasName location
    ; userContentHost
    ; environment
    ; csrfToken }
  in
  if Url.isIntegrationTest
  then (m, Cmd.batch [RPC.integration m m.canvasName])
  else
    ( m
    , Cmd.batch
        [RPC.initialLoad m (FocusPageAndCursor (page, savedCursorState))] )


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
        match tl.data with
        | TLHandler _ ->
            true
        | TLDB _ ->
            true
        | TLFunc _ ->
            false
        | TLTipe _ ->
            false
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
                 [RPC.addOp newM FocusSame params]
           | TLFunc f ->
               let replacement = AST.closeBlanks f.ufAST in
               if replacement = f.ufAST
               then []
               else
                 let newF = {f with ufAST = replacement} in
                 let ops = [SetFunction newF] in
                 let params = RPC.opsParams ops in
                 (* call RPC on the new model *)
                 [RPC.addOp newM FocusSame params]
           | TLDB _ | TLTipe _ ->
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
            | SetType _ ->
                false
            | _ ->
                true )
          params.ops
      in
      if hasNonHandlers
      then (m, RPC.addOp m focus params)
      else
        let localM =
          List.foldl
            ~f:(fun call m ->
              match call with
              | SetHandler (tlid, pos, h) ->
                  TL.upsert m {id = tlid; pos; data = TLHandler h}
              | SetFunction f ->
                  Functions.upsert m f
              | SetType t ->
                  UserTypes.upsert m t
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
        (withFocus, Cmd.batch [wfCmd; RPC.addOp withFocus FocusNoChange params])
    in
    match mod_ with
    | DisplayError e ->
        ({m with error = updateError m.error e}, Cmd.none)
    | DisplayAndReportError (message, url, custom) ->
        let url = match url with Some url -> " (" ^ url ^ ")" | None -> "" in
        let custom = match custom with Some c -> ": " ^ c | None -> "" in
        let error = message ^ url ^ custom in
        (* Reload on bad csrf *)
        if String.contains error ~substring:"Bad CSRF"
        then Native.Location.reload true ;
        ( {m with error = updateError m.error error}
        , Tea.Cmd.call (fun _ -> Rollbar.send error None Js.Json.null) )
    | DisplayAndReportHttpError (context, ignoreCommon, e, params) ->
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
              if response.status.code = 502
              then "502"
              else
                "Bad status: "
                ^ response.status.message
                ^ " - "
                ^ body response.body
          | Http.BadPayload (msg, _) ->
              "Bad payload (" ^ context ^ "): " ^ msg
          | Http.Aborted ->
              "Request Aborted"
        in
        let url =
          match e with
          | Http.BadUrl url ->
              Some url
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
        let displayError =
          match e with
          | Http.BadUrl _ ->
              true
          | Http.Timeout ->
              not ignoreCommon
          | Http.NetworkError ->
              not ignoreCommon
          | Http.BadStatus response ->
              if response.status.code = 502 then not ignoreCommon else true
          | Http.BadPayload _ ->
              true
          | Http.Aborted ->
              not ignoreCommon
        in
        let shouldRollbar =
          match e with
          | Http.BadUrl _ ->
              true
          | Http.Timeout ->
              true
          | Http.NetworkError ->
              (* Don't rollbar if the internet is down *)
              false
          | Http.BadStatus response ->
              (* Don't rollbar if you aren't logged in *)
              if response.status.code = 401 then false else true
          | Http.BadPayload _ ->
              true
          | Http.Aborted ->
              true
        in
        let msg = msg ^ " (" ^ context ^ ")" in
        let custom =
          Json_encode_extended.object_
            [ ("httpResponse", Encoders.httpError e)
            ; ("parameters", params)
            ; ("cursorState", Encoders.cursorState m.cursorState) ]
        in
        (* Reload if it's a CSRF failure. *)
        ( match e with
        | Http.BadStatus response ->
            if response.status.code = 401
               && String.startsWith (body response.body) ~prefix:"Bad CSRF"
            then Native.Location.reload true
        | _ ->
            () ) ;
        let cmds =
          if shouldRollbar
          then [Tea.Cmd.call (fun _ -> Rollbar.send msg url custom)]
          else []
        in
        ( (if displayError then {m with error = updateError m.error msg} else m)
        , Cmd.batch cmds )
    | ClearError ->
        ({m with error = {message = None; showDetails = false}}, Cmd.none)
    | RPC (ops, focus) ->
        handleRPC (RPC.opsParams ops) focus
    | RPCFull (params, focus) ->
        handleRPC params focus
    | GetUnlockedDBsRPC ->
        Sync.attempt ~key:"unlocked" m (RPC.getUnlockedDBs m)
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
        (Url.setPage m m.currentPage page, Cmd.none)
    | Select (tlid, p) ->
        let hashCmd = Url.shouldUpdateHash m tlid in
        let m = {m with cursorState = Selecting (tlid, p)} in
        let m, afCmd = Analysis.analyzeFocused m in
        let commands = hashCmd @ closeBlanks m @ [afCmd] in
        (m, Cmd.batch commands)
    | Deselect ->
        let hashCmd = Url.navigateTo Architecture in
        let m, acCmd = processAutocompleteMods m [ACReset] in
        let m = {m with cursorState = Deselected} in
        let commands = hashCmd :: (closeBlanks m @ [acCmd]) in
        (m, Cmd.batch commands)
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
        let m, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m = {m with cursorState = Entering entry} in
        let m, afCmd = Analysis.analyzeFocused m in
        (m, Cmd.batch (closeBlanks m @ [afCmd; acCmd; Entry.focusEntry m]))
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
        let m, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m = {m with cursorState = Entering entry} in
        let m, afCmd = Analysis.analyzeFocused m in
        ( m
        , Cmd.batch
            ( closeBlanks m
            @ [afCmd; acCmd; Entry.focusEntryWithOffset m offset] ) )
    | SelectCommand (tlid, id) ->
        let m = {m with cursorState = SelectingCommand (tlid, id)} in
        let m, acCmd =
          processAutocompleteMods m [ACEnableCommandMode; ACRegenerate]
        in
        (m, Cmd.batch (closeBlanks m @ [acCmd; Entry.focusEntry m]))
    | RemoveToplevel tl ->
        (Toplevel.remove m tl, Cmd.none)
    | SetToplevels (tls, updateCurrent, updateRefs) ->
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
                let m_ =
                  match tl.data with
                  | TLDB _ ->
                      TL.upsert m2 tl
                  | TLHandler _ ->
                      TL.upsert m2 tl
                  | TLTipe _ ->
                      m2
                  | TLFunc _ ->
                      m2
                in
                if updateRefs
                then
                  let r = Introspect.getReferences tl tls in
                  { m_ with
                    tlReferences =
                      r @ m_.tlReferences
                      |> List.uniqueBy ~f:(fun (TLID fromtl, TLID totl, _) ->
                             fromtl ^ totl ) }
                else m_
          | None ->
              if updateRefs
              then
                { m2 with
                  tlReferences = Introspect.initReferences tls
                ; tlMeta = Introspect.initTLMeta ~init:StrDict.empty tls }
              else m2
        in
        let m4 =
          { m3 with
            deletedToplevels =
              TL.removeByTLID m3.deletedToplevels ~toBeRemoved:tls }
        in
        let m5 = Refactor.updateUsageCounts m4 in
        processAutocompleteMods m5 [ACRegenerate]
    | UpdateToplevels (tls, updateCurrent, updateRefs) ->
        let m = TL.upsertAll m tls in
        let m, acCmd = processAutocompleteMods m [ACRegenerate] in
        updateMod
          (SetToplevels (m.toplevels, updateCurrent, updateRefs))
          (m, Cmd.batch [cmd; acCmd])
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
    | UpdateAnalysis (id, analysis) ->
        let m2 = {m with analyses = Analysis.record m.analyses id analysis} in
        processAutocompleteMods m2 [ACRegenerate]
    | UpdateTraces traces ->
        let newTraces =
          StrDict.merge m.traces traces ~f:(fun _tlid oldList newList ->
              match (oldList, newList) with
              | None, None ->
                  None
              | Some o, None ->
                  Some o
              | None, Some n ->
                  Some n
              | Some o, Some n ->
                  (* merge the lists, updating the trace in the same position
                   * if present, and adding it to the front otherwise. *)
                  Some
                    (List.foldl n ~init:o ~f:(fun (newID, newData) list ->
                         let found = ref false in
                         let updated =
                           List.map list ~f:(fun (oldID, oldData) ->
                               if oldID = newID
                               then (
                                 found := true ;
                                 if newData <> None
                                 then (newID, newData)
                                 else (oldID, oldData) )
                               else (oldID, oldData) )
                         in
                         if !found (* deref, not "not" *)
                         then updated
                         else (newID, newData) :: list )) )
        in
        let m = {m with traces = newTraces} in
        let m, afCmd = Analysis.analyzeFocused m in
        let m, acCmd = processAutocompleteMods m [ACRegenerate] in
        (m, Cmd.batch [afCmd; acCmd])
    | UpdateTraceFunctionResult (tlid, traceID, callerID, fnName, hash, dval)
      ->
        let m =
          Analysis.replaceFunctionResult
            m
            tlid
            traceID
            callerID
            fnName
            hash
            dval
        in
        (* traces could be missing *)
        let m, afCmd = Analysis.analyzeFocused m in
        (* make sure we run the analysis even if the analyzeFocused conditions
         * don't hold, as we have a new result to be analyzed *)
        let reExeCmd = Analysis.requestAnalysis m tlid traceID in
        let m, acCmd = processAutocompleteMods m [ACRegenerate] in
        (m, Cmd.batch [afCmd; acCmd; reExeCmd])
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
                | TLTipe _ | TLDB _ | TLHandler _ ->
                    m2 )
          | None ->
              let asTLs = List.map ~f:TL.ufToTL m2.userFunctions in
              let references =
                Introspect.initReferences
                  ~prev:m2.tlReferences
                  (asTLs @ m2.toplevels)
              in
              let meta = Introspect.initTLMeta ~init:m2.tlMeta asTLs in
              {m2 with tlReferences = references; tlMeta = meta}
        in
        let m4 = Refactor.updateUsageCounts m3 in
        processAutocompleteMods m4 [ACRegenerate]
    | SetTypes (userTipes, deletedUserTipes, updateCurrent) ->
        let m2 =
          { m with
            userTipes =
              UserTypes.upsertAllByTLID m.userTipes ~newTipes:userTipes
              |> UserTypes.removeByTLID ~toBeRemoved:deletedUserTipes
          ; deletedUserTipes =
              UserTypes.upsertAllByTLID
                m.deletedUserTipes
                ~newTipes:deletedUserTipes
              |> UserTypes.removeByTLID ~toBeRemoved:userTipes }
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
                | TLTipe t ->
                    UserTypes.upsert m2 t
                | TLFunc _ ->
                    m2
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
    | AppendUnlockedDBs newDBs ->
        (* You probably don't want to use this, you probably want to wait for the
         * regular unlockedDBs RPC timer to do a full SetUnlockedDBs, but this
         * can be useful if you've done an operation to a DB and you know 100%
         * that it will be in the next unlockedDBs set.
         *
         * If your assumption is wrong, it'll be blown away by the next SetUnlockedDBs
         * operation regardless -- so as long as that interim period doesn't result
         * in the potential for dangerous operations, you should be fine.
        *)
        ({m with unlockedDBs = StrSet.union m.unlockedDBs newDBs}, Cmd.none)
    | Delete404 f404 ->
        ( { m with
            f404s =
              List.filter
                ~f:(fun f ->
                  f.space ^ f.path ^ f.modifier
                  <> f404.space ^ f404.path ^ f404.modifier )
                m.f404s }
        , Cmd.none )
    | Append404s f404s ->
        let new404s =
          f404s @ m.f404s
          |> List.uniqueBy ~f:(fun f404 ->
                 f404.space
                 ^ f404.path
                 ^ f404.modifier
                 ^ f404.timestamp
                 ^ f404.traceID )
        in
        ({m with f404s = new404s}, Cmd.none)
    | AppendStaticDeploy d ->
        ( {m with staticDeploys = DarkStorage.appendDeploy d m.staticDeploys}
        , Cmd.none )
    | SetHover (tlid, id) ->
        let nhovering = (tlid, id) :: m.hovering in
        ({m with hovering = nhovering}, Cmd.none)
    | ClearHover (tlid, id) ->
        let nhovering = List.filter ~f:(fun m -> m <> (tlid, id)) m.hovering in
        ({m with hovering = nhovering}, Cmd.none)
    | SetCursor (tlid, cur) ->
        let m = Analysis.setCursor m tlid cur in
        let m, afCmd = Analysis.analyzeFocused m in
        (m, afCmd)
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
      ( match TL.get m tlid with
      | Some tl ->
        ( match Analysis.getCurrentTrace m tlid with
        | Some (traceID, _) ->
          ( match Analysis.getArguments m tl traceID id with
          | Some args ->
              let params =
                { efpTLID = tlid
                ; efpCallerID = id
                ; efpTraceID = traceID
                ; efpFnName = name
                ; efpArgs = args }
              in
              (m, RPC.executeFunction m params)
          | None ->
              (m, Cmd.none)
              |> updateMod
                   (DisplayError "Traces are not loaded for this handler")
              |> updateMod (ExecutingFunctionComplete [(tlid, id)]) )
        | None ->
            (m, Cmd.none)
            |> updateMod
                 (DisplayError "Traces are not loaded for this handler")
            |> updateMod (ExecutingFunctionComplete [(tlid, id)]) )
      | None ->
          (* Attempted to execute a function in a toplevel that we just deleted! *)
          (m, Cmd.none) |> updateMod (ExecutingFunctionComplete [(tlid, id)])
      )
    | ExecutingFunctionComplete targets ->
        let isComplete target = not <| List.member ~value:target targets in
        let nexecutingFunctions =
          List.filter ~f:isComplete m.executingFunctions
        in
        ({m with executingFunctions = nexecutingFunctions}, Cmd.none)
    | MoveCanvasTo pos ->
        let newCanvasProps = {m.canvasProps with offset = pos} in
        ({m with canvasProps = newCanvasProps}, Cmd.none)
    | CenterCanvasOn tlid ->
        ( { m with
            canvasProps =
              { m.canvasProps with
                offset =
                  Viewport.centerCanvasOn (TL.getTL m tlid) m.canvasProps
              ; panAnimation = true } }
        , Cmd.none )
    | TriggerCronRPC tlid ->
        (m, RPC.triggerCron m {tcpTLID = tlid})
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
  let str = Regex.replace ~re:(Regex.regex "\\.*$") ~repl:"" baseStr in
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


let toggleTimers (m : model) : model =
  {m with timersEnabled = not m.timersEnabled}


let findCenter (m : model) : pos =
  match m.currentPage with
  | Architecture | FocusedHandler _ | FocusedDB _ ->
      Viewport.toCenter m.canvasProps.offset
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
        let newcomplete = AC.setQuery m value m.complete in
        let newm = {m with complete = newcomplete} in
        Entry.submit newm cursor Entry.StayHere
    | _ ->
        NoChange )
  | GlobalClick event ->
    ( match m.currentPage with
    | FocusedFn _ | FocusedType _ ->
        NoChange
    | Architecture | FocusedDB _ | FocusedHandler _ ->
        if event.button = Defaults.leftButton
        then
          match unwrapCursorState m.cursorState with
          | Deselected ->
              Many
                [ AutocompleteMod ACReset
                ; Enter (Creating (Viewport.toAbsolute m event.mePos)) ]
          | _ ->
              Deselect
        else NoChange )
  | BlankOrMouseEnter (tlid, id, _) ->
      SetHover (tlid, id)
  | BlankOrMouseLeave (tlid, id, _) ->
      ClearHover (tlid, id)
  | MouseWheel (x, y) ->
      if m.canvasProps.enablePan then Viewport.moveCanvasBy m x y else NoChange
  | TraceMouseEnter (tlid, traceID, _) ->
      let traceCmd =
        match Analysis.getTrace m tlid traceID with
        | Some (_, None) ->
            let m, cmd = Analysis.requestTrace m tlid traceID in
            [ TweakModel (fun old -> {old with syncState = m.syncState})
            ; MakeCmd cmd ]
        | _ ->
            []
      in
      Many (traceCmd @ [SetHover (tlid, ID traceID)])
  | TraceMouseLeave (tlid, traceID, _) ->
      ClearHover (tlid, ID traceID)
  | TriggerCron tlid ->
      TriggerCronRPC tlid
  | DragToplevel (_, mousePos) ->
    ( match m.cursorState with
    | Dragging (draggingTLID, startVPos, _, origCursorState) ->
        let xDiff = mousePos.x - startVPos.vx in
        let yDiff = mousePos.y - startVPos.vy in
        let m2 = TL.move draggingTLID xDiff yDiff m in
        Many
          [ SetToplevels (m2.toplevels, true, false)
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
        | TLFunc _ | TLTipe _ ->
            NoChange
        | TLHandler _ | TLDB _ ->
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
  | BlankOrClick (targetTLID, targetID, _) ->
    ( match m.cursorState with
    | Deselected ->
        Select (targetTLID, Some targetID)
    | Dragging (_, _, _, origCursorState) ->
        SetCursorState origCursorState
    | Entering cursor ->
      ( match cursor with
      | Filling (_, fillingID) ->
          if fillingID = targetID
          then NoChange
          else Select (targetTLID, Some targetID)
      | _ ->
          Select (targetTLID, Some targetID) )
    | Selecting (_, _) ->
        Select (targetTLID, Some targetID)
    | SelectingCommand (_, scID) ->
        if scID = targetID then NoChange else Select (targetTLID, Some targetID)
    )
  | BlankOrDoubleClick (targetTLID, targetID, event) ->
      (* TODO: switch to ranges to get actual character offset
       * rather than approximating *)
      let offset =
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
      Selection.dblclick m targetTLID targetID offset
  | ToplevelDoubleClick tlid ->
      CenterCanvasOn tlid
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
  | TraceClick (tlid, traceID, _) ->
    ( match m.cursorState with
    | Dragging (_, _, _, origCursorState) ->
        SetCursorState origCursorState
    | Deselected ->
        Many [Select (tlid, None); SetCursor (tlid, traceID)]
    | _ ->
        SetCursor (tlid, traceID) )
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
      MakeCmd (RPC.saveTest m)
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
  | DeleteUserTypeField (tipe, field) ->
      let replacement = UserTypes.removeField tipe field in
      RPC ([SetType replacement], FocusNext (tipe.utTLID, None))
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
  | DeleteUserType tlid ->
      RPC ([DeleteType tlid], FocusSame)
  | DeleteUserTypeForever tlid ->
      Many
        [ RPC ([DeleteTypeForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              { m with
                deletedUserTipes =
                  List.filter
                    ~f:(fun ut -> ut.utTLID <> tlid)
                    m.deletedUserTipes } ) ]
  | AddOpRPCCallback (focus, oparams, Ok r) ->
      let shouldUpdateRefs = Introspect.shouldUpdateReferences oparams.ops in
      if focus = FocusNoChange
      then
        Many
          [ UpdateToplevels (r.toplevels, false, shouldUpdateRefs)
          ; UpdateDeletedToplevels r.deletedToplevels
          ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, false)
          ; SetTypes (r.userTipes, r.deletedUserTipes, false)
          ; MakeCmd (Entry.focusEntry m) ]
      else
        let m2 = TL.upsertAll m r.toplevels in
        let m3 = {m2 with userFunctions = r.userFunctions} in
        let m4 = {m3 with userTipes = r.userTipes} in
        let newState = processFocus m4 focus in
        Many
          [ UpdateToplevels (r.toplevels, true, shouldUpdateRefs)
          ; UpdateDeletedToplevels r.deletedToplevels
          ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, true)
          ; SetTypes (r.userTipes, r.deletedUserTipes, true)
          ; AutocompleteMod ACReset
          ; ClearError
          ; newState ]
  | InitialLoadRPCCallback
      (focus, extraMod (* for integration tests, maybe more *), Ok r) ->
      let pfM =
        { m with
          toplevels = r.toplevels
        ; userFunctions = r.userFunctions
        ; handlerProps = ViewUtils.createHandlerProp r.toplevels }
      in
      let newState = processFocus pfM focus in
      let traces : traces =
        List.foldl r.traces ~init:StrDict.empty ~f:(fun (tlid, traceid) dict ->
            let trace = (traceid, None) in
            StrDict.update dict ~key:(deTLID tlid) ~f:(fun old ->
                match old with
                | Some existing ->
                    Some (existing @ [trace])
                | None ->
                    Some [trace] ) )
      in
      Many
        [ SetToplevels (r.toplevels, true, true)
        ; SetDeletedToplevels r.deletedToplevels
        ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, true)
        ; SetTypes (r.userTipes, r.deletedUserTipes, true)
        ; SetUnlockedDBs r.unlockedDBs
        ; Append404s r.fofs
        ; AppendStaticDeploy r.staticDeploys
        ; AutocompleteMod ACReset
        ; ClearError
        ; extraMod
        ; newState
        ; UpdateTraces traces ]
  | SaveTestRPCCallback (Ok msg_) ->
      DisplayError ("Success! " ^ msg_)
  | ExecuteFunctionRPCCallback (params, Ok (dval, hash)) ->
      Many
        [ UpdateTraceFunctionResult
            ( params.efpTLID
            , params.efpTraceID
            , params.efpCallerID
            , params.efpFnName
            , hash
            , dval )
        ; ExecutingFunctionComplete [(params.efpTLID, params.efpCallerID)] ]
  | TriggerCronRPCCallback (Ok ()) ->
      NoChange
  | GetUnlockedDBsRPCCallback (Ok unlockedDBs) ->
      Many
        [ TweakModel (Sync.markResponseInModel ~key:"unlocked")
        ; SetUnlockedDBs unlockedDBs ]
  | NewTracePush (traceID, tlids) ->
      let traces =
        List.map ~f:(fun tlid -> (deTLID tlid, [(traceID, None)])) tlids
      in
      UpdateTraces (StrDict.fromList traces)
  | New404Push f404 ->
      Append404s [f404]
  | NewStaticDeployPush asset ->
      AppendStaticDeploy [asset]
  | Delete404RPCCallback (f404, Ok ()) ->
      Delete404 f404
  | ReceiveAnalysis result ->
    ( match result with
    | Ok (id, analysisResults) ->
        UpdateAnalysis (id, analysisResults)
    | Error (AnalysisExecutionError (_, str)) ->
        DisplayError str
    | Error (AnalysisParseError str) ->
        DisplayError str )
  | ReceiveTraces (TraceFetchFailure (params, url, error)) ->
      Many
        [ TweakModel
            (Sync.markResponseInModel ~key:("tracefetch-" ^ params.gtdrpTraceID))
        ; DisplayAndReportError ("Error fetching trace", Some url, Some error)
        ]
  | ReceiveTraces (TraceFetchSuccess (params, result)) ->
      let traces =
        StrDict.fromList [(deTLID params.gtdrpTlid, [result.trace])]
      in
      Many
        [ TweakModel
            (Sync.markResponseInModel ~key:("tracefetch-" ^ params.gtdrpTraceID))
        ; UpdateTraces traces ]
  | ReceiveTraces (TraceFetchMissing params) ->
      (* We'll force it so no need to update syncState *)
      let _, cmd =
        Analysis.requestTrace
          ~force:true
          m
          params.gtdrpTlid
          params.gtdrpTraceID
      in
      MakeCmd cmd
  | AddOpRPCCallback (_, params, Error err) ->
      DisplayAndReportHttpError
        ("RPC", false, err, Encoders.addOpRPCParams params)
  | SaveTestRPCCallback (Error err) ->
      DisplayError ("Error: " ^ Tea_http.string_of_error err)
  | ExecuteFunctionRPCCallback (params, Error err) ->
      DisplayAndReportHttpError
        ( "ExecuteFunction"
        , false
        , err
        , Encoders.executeFunctionRPCParams params )
  | TriggerCronRPCCallback (Error err) ->
      DisplayAndReportHttpError ("TriggerCron", false, err, Js.Json.null)
  | InitialLoadRPCCallback (_, _, Error err) ->
      DisplayAndReportHttpError ("InitialLoad", false, err, Js.Json.null)
  | GetUnlockedDBsRPCCallback (Error err) ->
      Many
        [ TweakModel (Sync.markResponseInModel ~key:"unlocked")
        ; DisplayAndReportHttpError ("GetUnlockedDBs", true, err, Js.Json.null)
        ]
  | Delete404RPCCallback (params, Error err) ->
      DisplayAndReportHttpError ("Delete404", false, err, Encoders.fof params)
  | JSError msg_ ->
      DisplayError ("Error in JS: " ^ msg_)
  | WindowResize (w, h) | WindowOnLoad (w, h) ->
      TweakModel
        (fun m_ ->
          {m_ with canvasProps = {m_.canvasProps with viewportSize = {w; h}}}
          )
  | LocationChange loc ->
      Url.changeLocation m loc
  | TimerFire (action, _) ->
    (match action with RefreshAnalysis -> GetUnlockedDBsRPC | _ -> NoChange)
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
      let traces =
        List.foldl
          ~init:[]
          ~f:(fun search acc ->
            if search.space = fof.space
               && search.path = fof.path
               && search.modifier = fof.modifier
            then (search.traceID, None) :: acc
            else acc )
          m.f404s
      in
      let traceMods =
        match List.head traces with
        | Some (first, _) ->
            let traceDict = StrDict.fromList [(deTLID tlid, traces)] in
            [UpdateTraces traceDict; SetCursor (tlid, first)]
        | None ->
            []
      in
      (* It's important that we update the traces + set the cursor _first_
       * -- otherwise both of them will attempt to 'analyzeFocused' on the new
       * TLID which might not have made it to the server yet.
       *
       * By doing them first they'll fire the analysis on whatever the user is
       * currently focused on, which is safe.
       *)
      Many
        ( traceMods
        @ [ RPC
              ( [SetHandler (tlid, aPos, aHandler)]
              , FocusExact (tlid, B.toID ast) )
          ; Delete404 fof ] )
  | Delete404RPC fof ->
      MakeCmd (RPC.delete404 m fof)
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
        ; MakeCmd (Url.navigateTo (FocusedFn ufun.ufTLID)) ]
  | CreateType ->
      let tipe = Refactor.generateEmptyUserType () in
      Many
        [ RPC ([SetType tipe], FocusNothing)
        ; MakeCmd (Url.navigateTo (FocusedType tipe.utTLID)) ]
  | LockHandler (tlid, locked) ->
      TweakModel (Editor.setHandlerLock tlid locked)
  | EnablePanning pan ->
      TweakModel
        (fun m -> {m with canvasProps = {m.canvasProps with enablePan = pan}})
  | ShowErrorDetails show ->
      let e = m.error in
      TweakModel (fun m -> {m with error = {e with showDetails = show}})
  | ClipboardCopyEvent e ->
      ( match Clipboard.copy m with
      | `Text text ->
          e##clipboardData##setData "text/plain" text ;
          e##preventDefault ()
      | `Json json ->
          let data = Json.stringify json in
          e##clipboardData##setData "application/json" data ;
          e##preventDefault ()
      | `None ->
          () ) ;
      NoChange
  | ClipboardPasteEvent e ->
      let json = e##clipboardData##getData "application/json" in
      if json <> ""
      then Clipboard.paste m (`Json (Json.parseOrRaise json))
      else
        let text = e##clipboardData##getData "text/plain" in
        if text <> "" then Clipboard.paste m (`Text text) else NoChange
  | ClipboardCutEvent e ->
      let copyData, mod_ = Clipboard.cut m in
      ( match copyData with
      | `Text text ->
          e##clipboardData##setData "text/plain" text ;
          e##preventDefault ()
      | `Json json ->
          let data = Json.stringify json in
          e##clipboardData##setData "application/json" data ;
          (* this is probably gonna be useful for debugging, but customers
           * shouldn't get used to it *)
          e##clipboardData##setData "text/plain" data ;
          e##preventDefault ()
      | `None ->
          () ) ;
      mod_
  | ClipboardCopyLivevalue lv ->
      Native.Clipboard.copyToClipboard lv ;
      NoChange
  | EventDecoderError (name, key, error) ->
      (* Consider rollbar'ing here, but consider the following before doing so:
       *    - old clients after a deploy
       *    - lots of events using a bad decoder
       *    - rollbar token exhaustion *)
      DisplayError
        ( "INTERNAL: Error decoding js event "
        ^ name
        ^ " with key "
        ^ key
        ^ " got error: \""
        ^ error
        ^ "\"" )
  | UpdateHandlerState (tlid, state) ->
      TweakModel (Editor.setHandlerState tlid state)
  | CanvasPanAnimationEnd ->
      TweakModel
        (fun m ->
          {m with canvasProps = {m.canvasProps with panAnimation = false}} )
  | GoTo page ->
      MakeCmd (Url.navigateTo page)


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
          WindowResize (w, h) )
    ; Native.Window.OnLoad.listen ~key:"window_on_load" (fun (w, h) ->
          WindowOnLoad (w, h) ) ]
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
      match m.visibility with
      | Hidden ->
          []
      | Visible ->
          [ Patched_tea_time.every
              ~key:"refresh_analysis"
              Tea.Time.second
              (fun f -> TimerFire (RefreshAnalysis, f) ) ]
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
    ; DarkStorage.NewStaticDeployPush.listen ~key:"new_static_deploy" (fun s ->
          NewStaticDeployPush s )
    ; Analysis.ReceiveTraces.listen ~key:"receive_traces" (fun s ->
          ReceiveTraces s ) ]
  in
  let clipboardSubs =
    [ Native.Clipboard.copyListener ~key:"copy_event" (fun e ->
          ClipboardCopyEvent e )
    ; Native.Clipboard.cutListener ~key:"cut_event" (fun e ->
          ClipboardCutEvent e )
    ; Native.Clipboard.pasteListener ~key:"paste_event" (fun e ->
          ClipboardPasteEvent e ) ]
  in
  Tea.Sub.batch
    (List.concat
       [ keySubs
       ; clipboardSubs
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
