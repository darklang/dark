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
module Regex = Util.Regex
module TD = TLIDDict

let incOption x = Option.map x ~f:(fun x -> x + 1)

let expireAvatars (avatars : Types.avatar list) : Types.avatar list =
  let fiveMinsAgo : float = Js.Date.now () -. (5.0 *. 60.0 *. 1000.0) in
  List.filter
    ~f:(fun av -> av.serverTime |> Js.Date.valueOf > fiveMinsAgo)
    avatars


(* We do some dropping of ops based on browserId+opCtr to preserve ordering.
 * (opCtr is per-browserId, and inc'd client-side; thus we know, when processing
 * a set of ops whether this is the latest seen so far from a given client, or
 * has come in out of order.) This is initially done server-side, to guard
 * against ops being processed there out of order; but we also need to do this
 * client-side, since messages coming in from Pusher (and stroller) are not
 * guaranteed to be delivered in order.
 *
 * Ordering is determined by model.opCtrs, and we return a model so we can also
 * update the opCtrs map.
 * *)
let filterOpsAndResult
    (m : model) (params : addOpRPCParams) (result : addOpRPCResult option) :
    model * op list * addOpRPCResult option =
  let newOpCtrs =
    (* if the opCtr in params is greater than the one in the map, we'll create
     * an updated map *)
    StrDict.update m.opCtrs ~key:params.browserId ~f:(fun oldCtr ->
        match (oldCtr, params.opCtr) with
        | Some oldCtr, Some paramsOpCtr ->
            Some (max oldCtr paramsOpCtr)
        | _ ->
            params.opCtr )
  in
  let m2 = {m with opCtrs = newOpCtrs} in
  (* if the new opCtrs map was updated by params.opCtr, then this msg was the
   * latest; otherwise, we need to filter out some ops from params *)
  if StrDict.get m2.opCtrs ~key:params.browserId = params.opCtr
  then (m2, params.ops, result)
  else
    let ops =
      params.ops
      (* filter down to only those ops which can be applied out of order without
       * overwriting previous ops' state - eg, if we have SetHandler1 setting a
       * handler's value to "aaa", and then SetHandler2's value is "aa",
       * applying them out of order (SH2, SH1) will result in SH2's update being
       * overwritten *)
      (* NOTE: DO NOT UPDATE WITHOUT UPDATING THE SERVER-SIDE LIST *)
      |> List.filter ~f:(fun op ->
             match op with
             | SetHandler _
             | SetFunction _
             | SetType _
             | MoveTL _
             | SetDBColName _
             | ChangeDBColName _
             | ChangeDBColType _
             | SetExpr _
             | CreateDBMigration _
             | SetDBColNameInDBMigration _
             | SetDBColTypeInDBMigration _
             | RenameDBname _ ->
                 false
             | _ ->
                 true )
    in
    let opTlids = params.ops |> List.map ~f:(fun op -> Encoders.tlidOf op) in
    (* We also want to ignore the result of ops we ignored *)
    let result =
      Option.map result ~f:(fun (result : addOpRPCResult) ->
          { result with
            handlers =
              result.handlers
              |> List.filter ~f:(fun h -> List.member ~value:h.hTLID opTlids)
          ; userFunctions =
              result.userFunctions
              |> List.filter ~f:(fun uf -> List.member ~value:uf.ufTLID opTlids)
          ; userTipes =
              result.userTipes
              |> List.filter ~f:(fun ut -> List.member ~value:ut.utTLID opTlids)
          } )
    in
    (m2, ops, result)


let init (flagString : string) (location : Web.Location.location) =
  let { Flags.editorState
      ; complete
      ; userContentHost
      ; environment
      ; csrfToken
      ; isAdmin
      ; buildHash } =
    Flags.fromString flagString
  in
  let variants = VariantTesting.enabledVariantTests in
  let m = editorState |> Editor.fromString |> Editor.editor2model in
  let page =
    Url.parseLocation location
    |> Option.withDefault ~default:Defaults.defaultModel.currentPage
  in
  (* these saved values may not be valid yet *)
  let savedCursorState = m.cursorState in
  let functions =
    List.filter complete ~f:(fun fn ->
        if String.contains ~substring:"Twitter::" fn.fnName
        then VariantTesting.libtwitterAvailable variants
        else true )
  in
  let m =
    { m with
      cursorState =
        Deselected
        (* deselect for now as the selected blank isn't available yet *)
    ; currentPage = Architecture
    ; builtInFunctions = functions
    ; complete = AC.init m
    ; tests = variants
    ; handlers = TLIDDict.empty
    ; dbs = TLIDDict.empty
    ; canvasName = Url.parseCanvasName location
    ; userContentHost
    ; origin = location.origin
    ; environment
    ; csrfToken
    ; browserId = BsUuid.Uuid.V4.create () |> BsUuid.Uuid.V4.toString
    ; isAdmin
    ; buildHash }
  in
  let timeStamp = Js.Date.now () /. 1000.0 in
  let avMessage : avatarModelMessage =
    { canvasName = m.canvasName
    ; browserId = m.browserId
    ; tlid = None
    ; timestamp = timeStamp }
  in
  let m = {m with fluidState = Fluid.initAC m.fluidState m} in
  if Url.isIntegrationTest
  then (m, Cmd.batch [RPC.integration m m.canvasName])
  else
    ( m
    , Cmd.batch
        [ RPC.initialLoad m (FocusPageAndCursor (page, savedCursorState))
        ; RPC.sendPresence m avMessage ] )


let updateError (oldErr : darkError) (newErrMsg : string) : darkError =
  if oldErr.message = Some newErrMsg && not oldErr.showDetails
  then oldErr
  else {message = Some newErrMsg; showDetails = true}


let processFocus (m : model) (focus : focus) : modification =
  match focus with
  | FocusNext (tlid, pred) ->
      let tl = TL.getExn m tlid in
      let predPd = Option.andThen ~f:(TL.find tl) pred in
      let next = TL.getNextBlank tl predPd in
      ( match next with
      | Some pd ->
          Enter (Filling (tlid, P.toID pd))
      | None ->
          Select (tlid, pred) )
  | FocusExact (tlid, id) ->
      let tl = TL.getExn m tlid in
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
        match tl with
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


let applyOpsToClient updateCurrent (p : addOpRPCParams) (r : addOpRPCResult) :
    Types.modification list =
  [ UpdateToplevels (r.handlers, r.dbs, updateCurrent)
  ; UpdateDeletedToplevels (r.deletedHandlers, r.deletedDBs)
  ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, updateCurrent)
  ; SetTypes (r.userTipes, r.deletedUserTipes, updateCurrent)
  ; RefreshUsages (Introspect.tlidsToUpdateUsage p.ops) ]


let isACOpened (m : model) : bool =
  if VariantTesting.isFluid m.tests
  then
    FluidAutocomplete.isOpened m.fluidState.ac
    || FluidCommands.isOpened m.fluidState.cp
    || AC.isOpened m.complete
  else AC.isOpened m.complete


let rec updateMod (mod_ : modification) ((m, cmd) : model * msg Cmd.t) :
    model * msg Cmd.t =
  if m.integrationTestState <> NoIntegrationTest
  then Debug.loG "mod update" (show_modification mod_) ;
  let closeBlanks newM =
    if VariantTesting.isFluid m.tests
    then []
    else
      (* close open threads in the previous TL *)
      m.cursorState
      |> tlidOf
      |> Option.andThen ~f:(TL.get m)
      |> Option.map ~f:(fun tl ->
             match tl with
             | TLHandler h ->
                 let replacement = AST.closeBlanks h.ast in
                 if replacement = h.ast
                 then []
                 else
                   let newM =
                     {newM with lastOpCtr = incOption newM.lastOpCtr}
                   in
                   let newH = {h with ast = replacement} in
                   let ops = [SetHandler (h.hTLID, h.pos, newH)] in
                   let params =
                     RPC.opsParams ops (incOption m.lastOpCtr) m.browserId
                   in
                   (* call RPC on the new model *)
                   [RPC.addOp newM FocusSame params]
             | TLFunc f ->
                 let replacement = AST.closeBlanks f.ufAST in
                 if replacement = f.ufAST
                 then []
                 else
                   let newM =
                     {newM with lastOpCtr = incOption newM.lastOpCtr}
                   in
                   let newF = {f with ufAST = replacement} in
                   let ops = [SetFunction newF] in
                   let params =
                     RPC.opsParams ops (incOption m.lastOpCtr) m.browserId
                   in
                   (* call RPC on the new model *)
                   [RPC.addOp newM FocusSame params]
             | TLDB _ | TLTipe _ ->
                 [] )
      |> Option.withDefault ~default:[]
      |> fun rpc ->
      if tlidOf newM.cursorState = tlidOf m.cursorState then [] else rpc
  in
  let newm, newcmd =
    let bringBackCurrentTL (oldM : model) (newM : model) : model =
      (* used with updateCurrent - if updateCurrent is false, we want to restore
       * the current TL so we don't lose local changes made since the API call *)
      match tlidOf oldM.cursorState with
      | Some tlid ->
          let tl = TL.getExn oldM tlid in
          ( match tl with
          | TLDB db ->
              DB.upsert newM db
          | TLHandler h ->
              Handlers.upsert newM h
          | TLTipe _ | TLFunc _ ->
              newM )
      | None ->
          newM
    in
    let handleRPC params focus =
      (* immediately update the model based on SetHandler and focus, if
         possible *)
      let m = {m with lastOpCtr = incOption m.lastOpCtr} in
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
              | SetHandler (_tlid, _pos, h) ->
                  Handlers.upsert m h
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
    | HandleAPIError apiError ->
        let now = Js.Date.now () |> Js.Date.fromFloat in
        let shouldReload =
          let buildHashMismatch =
            ApiError.serverVersionOf apiError
            |> Option.map ~f:(fun hash -> hash <> m.buildHash)
            |> Option.withDefault ~default:false
          in
          let reloadAllowed =
            match m.lastReload with
            | Some time ->
                (* if 60 seconds have elapsed *)
                Js.Date.getTime time +. 60000.0 > Js.Date.getTime now
            | None ->
                true
          in
          (* Reload if it's a CSRF failure or the frontend is out of date *)
          ApiError.isCSRF apiError || (buildHashMismatch && reloadAllowed)
        in
        let cmd =
          if shouldReload
          then
            let m = {m with lastReload = Some now} in
            [ Tea_task.nativeBinding (fun _ -> Editor.serialize m)
            ; Tea_task.nativeBinding (fun _ -> Native.Location.reload true) ]
            |> Tea_task.sequence
            (* No callback bc of reload *)
            |> Tea_task.attempt (fun _ -> IgnoreMsg)
          else if ApiError.shouldRollbar apiError
          then Cmd.call (fun _ -> Rollbar.sendApiError m apiError)
          else Cmd.none
        in
        let newM =
          let error =
            if ApiError.shouldDisplayToUser apiError
            then updateError m.error (ApiError.msg apiError)
            else m.error
          in
          let lastReload = if shouldReload then Some now else m.lastReload in
          {m with error; lastReload}
        in
        (newM, cmd)
    | ClearError ->
        ({m with error = {message = None; showDetails = false}}, Cmd.none)
    | RPC (ops, focus) ->
        handleRPC (RPC.opsParams ops (incOption m.lastOpCtr) m.browserId) focus
    | GetUnlockedDBsRPC ->
        Sync.attempt ~key:"unlocked" m (RPC.getUnlockedDBs m)
    | UpdateDBStatsRPC tlid ->
        Analysis.updateDBStats m tlid
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
        let pagePresent =
          match Page.tlidOf page with
          | None ->
              true
          | Some tlid ->
              TL.get m tlid <> None
        in
        if pagePresent
        then
          let avMessage : avatarModelMessage =
            { canvasName = m.canvasName
            ; browserId = m.browserId
            ; tlid = Page.tlidOf page
            ; timestamp = Js.Date.now () /. 1000.0 }
          in
          let cap = Page.capMinimap m.currentPage page in
          let cmds = Cmd.batch (RPC.sendPresence m avMessage :: cap) in
          (Page.setPage m m.currentPage page, cmds)
        else
          ( Page.setPage m m.currentPage Architecture
          , Url.updateUrl Architecture )
    | Select (tlid, p) ->
        let cursorState =
          if VariantTesting.isFluid m.tests
          then
            match p with
            | None ->
                FluidEntering tlid
            | Some id ->
                let tl = TL.getExn m tlid in
                let pd = TL.findExn tl id in
                if P.astOwned (P.typeOf pd)
                then FluidEntering tlid
                else Selecting (tlid, p)
          else Selecting (tlid, p)
        in
        let m, hashcmd =
          if tlidOf m.cursorState <> Some tlid
          then
            let tl = TL.getExn m tlid in
            let page = TL.asPage tl false in
            let m = Page.setPage m m.currentPage page in
            (m, Url.updateUrl page)
          else (m, Cmd.none)
        in
        let m = {m with cursorState} in
        let m, afCmd = Analysis.analyzeFocused m in
        let timeStamp = Js.Date.now () /. 1000.0 in
        let avMessage : avatarModelMessage =
          { canvasName = m.canvasName
          ; browserId = m.browserId
          ; tlid = Some tlid
          ; timestamp = timeStamp }
        in
        let commands =
          [hashcmd] @ closeBlanks m @ [afCmd] @ [RPC.sendPresence m avMessage]
        in
        (m, Cmd.batch commands)
    | Deselect ->
        if m.cursorState <> Deselected
        then
          let m = Editor.closeMenu m in
          let hashcmd = [Url.updateUrl Architecture] in
          let m = Page.setPage m m.currentPage Architecture in
          let m, acCmd = processAutocompleteMods m [ACReset] in
          let m = {m with cursorState = Deselected} in
          let timeStamp = Js.Date.now () /. 1000.0 in
          let avMessage : avatarModelMessage =
            { canvasName = m.canvasName
            ; browserId = m.browserId
            ; tlid = None
            ; timestamp = timeStamp }
          in
          let commands =
            hashcmd @ closeBlanks m @ [acCmd] @ [RPC.sendPresence m avMessage]
          in
          (m, Cmd.batch commands)
        else (m, Cmd.none)
    | Enter entry ->
        let cursorState, target =
          match entry with
          | Creating _ ->
              (Entering entry, None)
          | Filling (tlid, id) ->
              let tl = TL.getExn m tlid in
              let pd = TL.findExn tl id in
              let cs =
                if VariantTesting.isFluid m.tests && P.astOwned (P.typeOf pd)
                then FluidEntering tlid
                else Entering entry
              in
              (cs, Some (tlid, pd))
        in
        let m, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m = {m with cursorState} in
        let m, afCmd = Analysis.analyzeFocused m in
        (m, Cmd.batch (closeBlanks m @ [afCmd; acCmd; Entry.focusEntry m]))
    | EnterWithOffset (entry, offset) ->
        let cursorState, target =
          match entry with
          | Creating _ ->
              (Entering entry, None)
          | Filling (tlid, id) ->
              let tl = TL.getExn m tlid in
              let pd = TL.findExn tl id in
              let cs =
                if VariantTesting.isFluid m.tests && P.astOwned (P.typeOf pd)
                then FluidEntering tlid
                else Entering entry
              in
              (cs, Some (tlid, pd))
        in
        let m, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m = {m with cursorState} in
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
    | SetToplevels (handlers, dbs, updateCurrent) ->
      let oldM = m in
        let m =
          {m with handlers = Handlers.fromList handlers; dbs = DB.fromList dbs}
        in
        (* If updateCurrent = false, bring back the TL being edited, so we don't lose work done since the
           API call *)
        let m = if updateCurrent then m else bringBackCurrentTL oldM m in
        let m =
          let hTLIDs = List.map ~f:(fun h -> h.hTLID) handlers in
          let dbTLIDs = List.map ~f:(fun db -> db.dbTLID) dbs in
          { m with
            deletedHandlers = TD.removeMany m.deletedHandlers ~tlids:hTLIDs
          ; deletedDBs = TD.removeMany m.deletedDBs ~tlids:dbTLIDs }
        in
        let m = Refactor.updateUsageCounts m in
        processAutocompleteMods m [ACRegenerate]
    | UpdateToplevels (handlers, dbs, updateCurrent) ->
      let oldM = m in
        let m =
          { m with
            handlers = TD.mergeRight m.handlers (Handlers.fromList handlers)
          ; dbs = TD.mergeRight m.dbs (DB.fromList dbs) }
        in
        let m, acCmd = processAutocompleteMods m [ACRegenerate] in
        (* If updateCurrent = false, bring back the TL being edited, so we don't lose work done since the
           API call *)
        let m = if updateCurrent then m else bringBackCurrentTL oldM m in
        updateMod
          (SetToplevels (TD.values m.handlers, TD.values m.dbs, updateCurrent))
          (m, Cmd.batch [cmd; acCmd])
    | UpdateDeletedToplevels (dhandlers, ddbs) ->
        let dhandlers =
          TD.mergeRight m.deletedHandlers (Handlers.fromList dhandlers)
        in
        let ddbs = TD.mergeRight m.deletedDBs (DB.fromList ddbs) in
        updateMod
          (SetDeletedToplevels (TD.values dhandlers, TD.values ddbs))
          (m, cmd)
    | SetDeletedToplevels (dhandlers, ddbs) ->
        let hTLIDs = List.map ~f:(fun h -> h.hTLID) dhandlers in
        let dbTLIDs = List.map ~f:(fun db -> db.dbTLID) ddbs in
        let m =
          { m with
            deletedHandlers = Handlers.fromList dhandlers
          ; deletedDBs = DB.fromList ddbs
          ; handlers = TD.removeMany m.handlers ~tlids:hTLIDs
          ; dbs = TD.removeMany m.dbs ~tlids:dbTLIDs }
        in
        processAutocompleteMods m [ACRegenerate]
    | UpdateAnalysis (id, analysis) ->
        let m2 = {m with analyses = Analysis.record m.analyses id analysis} in
        processAutocompleteMods m2 [ACRegenerate]
    | UpdateDBStats statsStore ->
        let newStore =
          StrDict.merge
            ~f:(fun _k v1 v2 ->
              match (v1, v2) with
              | None, None ->
                  None
              | Some l, None ->
                  Some l
              | None, Some r ->
                  Some r
              | Some _, Some r ->
                  Some r )
            m.dbStats
            statsStore
        in
        let m = {m with dbStats = newStore} in
        (m, Cmd.none)
    | UpdateTraces traces ->
        let newTraces =
          Analysis.mergeTraces
            ~onConflict:(fun (oldID, oldData) (newID, newData) ->
              if newData <> None then (newID, newData) else (oldID, oldData) )
            m.traces
            traces
        in
        let m = {m with traces = newTraces} in
        let m, afCmd = Analysis.analyzeFocused m in
        let m, acCmd = processAutocompleteMods m [ACRegenerate] in
        (m, Cmd.batch [afCmd; acCmd])
    | OverrideTraces traces ->
        (* OverrideTraces takes a set of traces and merges it with the model, but if
         * the (tlid, traceID) pair occurs in both, the result will have its data
         * blown away.
         *
         * Use OverrideTraces when a set of traceIDs has been received by the client but
         * some/all of them might represent _mutated_ traces. (ie. a trace where if
         * you re-fetch the trace data you'd get a different set of input values or
         * stored function results *)
        let newTraces =
          Analysis.mergeTraces
            ~onConflict:(fun _old (newID, _) -> (newID, None))
            m.traces
            traces
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
        (* TODO: note: this updates existing, despite not being called update *)
        let m =
          { m with
            userFunctions =
              TD.mergeRight m.userFunctions (Functions.fromList userFuncs)
              |> TD.removeMany
                   ~tlids:(List.map ~f:Functions.toID deletedUserFuncs)
          ; deletedUserFunctions =
              TD.mergeRight
                m.deletedUserFunctions
                (Functions.fromList deletedUserFuncs)
              |> TD.removeMany ~tlids:(List.map ~f:Functions.toID userFuncs) }
        in
        (* Bring back the TL being edited, so we don't lose work done since the
           API call *)
        let m =
          match tlidOf m.cursorState with
          | Some tlid ->
              if updateCurrent
              then m
              else
                let tl = TL.getExn m tlid in
                ( match tl with
                | TLFunc f ->
                    Functions.upsert m f
                | TLTipe _ | TLDB _ | TLHandler _ ->
                    m )
          | None ->
              m
        in
        let m = Refactor.updateUsageCounts m in
        let m = FluidAutocomplete.updateFunctions m in
        processAutocompleteMods m [ACRegenerate]
    | SetTypes (userTipes, deletedUserTipes, updateCurrent) ->
        let m2 =
          { m with
            userTipes =
              TD.mergeRight m.userTipes (UserTypes.fromList userTipes)
              |> TD.removeMany
                   ~tlids:(List.map ~f:UserTypes.toID deletedUserTipes)
          ; deletedUserTipes =
              TD.mergeRight
                m.deletedUserTipes
                (UserTypes.fromList deletedUserTipes)
              |> TD.removeMany ~tlids:(List.map ~f:UserTypes.toID userTipes) }
        in
        (* Bring back the TL being edited, so we don't lose work done since the
           API call *)
        let m3 =
          match tlidOf m.cursorState with
          | Some tlid ->
              if updateCurrent
              then m2
              else
                let tl = TL.getExn m tlid in
                ( match tl with
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
    | SetPermission permission ->
        ({m with permission}, Cmd.none)
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
    | ExpireAvatars ->
        ({m with avatarsList = m.avatarsList |> expireAvatars}, Cmd.none)
    | UpdateAvatarList avatarsList ->
        let updatedAvatars =
          avatarsList
          |> List.filter ~f:(fun (avatar : Types.avatar) ->
                 avatar.browserId != m.browserId )
          |> expireAvatars
        in
        ({m with avatarsList = updatedAvatars}, Cmd.none)
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
                  Viewport.centerCanvasOn (TL.getExn m tlid) m.canvasProps
              ; panAnimation = true } }
        , Cmd.none )
    | TriggerHandlerRPC tlid ->
      ( match Analysis.getCurrentTrace m tlid with
      | Some (traceID, Some traceData) ->
          let handlerProps =
            RT.setHandlerExeState tlid Executing m.handlerProps
          in
          ( {m with handlerProps}
          , RPC.triggerHandler
              m
              {thTLID = tlid; thTraceID = traceID; thInput = traceData.input}
          )
      | _ ->
          (m, Cmd.none) )
    | InitIntrospect tls ->
        (Introspect.refreshUsages m (List.map ~f:TL.id tls), Cmd.none)
    | RefreshUsages tlids ->
        (Introspect.refreshUsages m tlids, Cmd.none)
    | FluidCommandsShow (tlid, token) ->
        let cp = FluidCommands.show (TL.getExn m tlid) token in
        ( {m with fluidState = {m.fluidState with cp}}
        , Tea_html_cmds.focus FluidCommands.filterInputID )
    | FluidCommandsClose ->
        let cp = FluidCommands.reset in
        ({m with fluidState = {m.fluidState with cp}}, Cmd.none)
    | TweakModel fn ->
        (fn m, Cmd.none)
    | AutocompleteMod mod_ ->
        processAutocompleteMods m [mod_]
    (* applied from left to right *)
    | Many mods ->
        List.foldl ~f:updateMod ~init:(m, Cmd.none) mods
  in
  (newm, Cmd.batch [cmd; newcmd])


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
      if String.endsWith ~suffix:"." query && KeyPress.isFieldAccessDot m query
      then NoChange
      else
        Many
          [ AutocompleteMod (ACSetQuery query)
          ; AutocompleteMod (ACSetVisible true)
          ; MakeCmd (Entry.focusEntry m) ]
  | EntrySubmitMsg ->
      NoChange
  | AutocompleteClick index ->
    ( match unwrapCursorState m.cursorState with
    | Entering cursor ->
        let newcomplete = {m.complete with index} in
        let newm = {m with complete = newcomplete} in
        Entry.submit newm cursor Entry.StayHere
    | _ ->
        NoChange )
  | FluidAutocompleteClick item ->
    ( match unwrapCursorState m.cursorState with
    | FluidEntering _ ->
        if VariantTesting.isFluid m.tests
        then Fluid.update m (FluidAutocompleteClick item)
        else NoChange
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
      Viewport.moveCanvasBy m x y
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
  | TriggerHandler tlid ->
      TriggerHandlerRPC tlid
  | DragToplevel (_, mousePos) ->
    ( match m.cursorState with
    | Dragging (draggingTLID, startVPos, _, origCursorState) ->
        let xDiff = mousePos.x - startVPos.vx in
        let yDiff = mousePos.y - startVPos.vy in
        let m2 = TL.move draggingTLID xDiff yDiff m in
        Many
          [ SetToplevels (TD.values m2.handlers, TD.values m2.dbs, true)
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
        let tl = TL.getExn m targetTLID in
        match tl with
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
              let tl = TL.getExn m draggingTLID in
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
                ; RPC ([MoveTL (draggingTLID, TL.pos tl)], FocusNoChange) ]
            else SetCursorState origCursorState
        | _ ->
            NoChange
      else NoChange
  | BlankOrClick (targetExnID, targetID, _) ->
    ( match m.cursorState with
    | Deselected ->
        Select (targetExnID, Some targetID)
    | Dragging (_, _, _, origCursorState) ->
        SetCursorState origCursorState
    | Entering cursor ->
      ( match cursor with
      | Filling (_, fillingID) ->
          if fillingID = targetID
          then NoChange
          else Select (targetExnID, Some targetID)
      | _ ->
          Select (targetExnID, Some targetID) )
    | Selecting (_, _) ->
        Select (targetExnID, Some targetID)
    | SelectingCommand (_, scID) ->
        if scID = targetID
        then NoChange
        else Select (targetExnID, Some targetID)
    | FluidEntering _ ->
        Select (targetExnID, Some targetID) )
  | BlankOrDoubleClick (targetExnID, targetID, event) ->
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
      Selection.dblclick m targetExnID targetID offset
  | ToplevelClick (targetExnID, _) ->
      let click =
        match m.cursorState with
        | Dragging (_, _, _, origCursorState) ->
            SetCursorState origCursorState
        | Selecting (_, _) ->
            Select (targetExnID, None)
        | SelectingCommand (_, _) ->
            Select (targetExnID, None)
        | Deselected ->
            Select (targetExnID, None)
        | Entering _ ->
            Select (targetExnID, None)
        | FluidEntering _ ->
            Select (targetExnID, None)
      in
      let fluid =
        if VariantTesting.isFluid m.tests
        then Fluid.update m (FluidMouseClick targetExnID)
        else NoChange
      in
      Many [click; fluid]
  | ExecuteFunctionButton (tlid, id, name) ->
      Many
        [ ExecutingFunctionBegan (tlid, id)
        ; ExecutingFunctionRPC (tlid, id, name)
        ; Select (tlid, Some id) ]
  | TraceClick (tlid, traceID, _) ->
    ( match m.cursorState with
    | Dragging (_, _, _, origCursorState) ->
        SetCursorState origCursorState
    | Deselected ->
        Many [Select (tlid, None); SetCursor (tlid, traceID)]
    | _ ->
        SetCursor (tlid, traceID) )
  | StartMigration tlid ->
      let mdb = tlid |> TL.getExn m |> TL.asDB in
      ( match mdb with
      | Some db ->
          DB.startMigration tlid db.cols
      | None ->
          NoChange )
  | AbandonMigration tlid ->
      RPC ([AbandonDBMigration tlid], FocusNothing)
  | DeleteColInDB (tlid, nameId) ->
      let mdb = tlid |> TL.getExn m |> TL.asDB in
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
        let tl = TL.getExn m tlid in
        ( match mId with
        | None ->
            NoChange
        | Some id ->
            let pd = TL.findExn tl id in
            Refactor.extractFunction m tl pd )
    | _ ->
        NoChange )
  | DeleteUserFunctionParameter (uftlid, upf) ->
    ( match TL.get m uftlid |> Option.andThen ~f:TL.asUserFunction with
    | Some uf ->
        let replacement = Functions.removeParameter uf upf in
        let newCalls = Refactor.removeFunctionParameter m uf upf in
        RPC ([SetFunction replacement] @ newCalls, FocusNext (uf.ufTLID, None))
    | None ->
        NoChange )
  | AddUserFunctionParameter uftlid ->
    ( match TL.get m uftlid |> Option.andThen ~f:TL.asUserFunction with
    | Some uf ->
        let nextId = Functions.idOfLastBlankor uf in
        Refactor.addFunctionParameter m uf nextId
    | None ->
        NoChange )
  | DeleteUserTypeField (tipetlid, field) ->
    ( match TL.get m tipetlid |> Option.andThen ~f:TL.asUserTipe with
    | Some tipe ->
        let replacement = UserTypes.removeField tipe field in
        RPC ([SetType replacement], FocusNext (tipe.utTLID, None))
    | None ->
        NoChange )
  | ToplevelDelete tlid ->
      let tl = TL.getExn m tlid in
      Many [RemoveToplevel tl; RPC ([DeleteTL (TL.id tl)], FocusSame)]
  | ToplevelDeleteForever tlid ->
      Many
        [ RPC ([DeleteTLForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              { m with
                deletedHandlers = TD.remove ~tlid m.deletedHandlers
              ; deletedDBs = TD.remove ~tlid m.deletedDBs } ) ]
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
                deletedUserFunctions = TD.remove ~tlid m.deletedUserFunctions
              } ) ]
  | DeleteUserType tlid ->
      RPC ([DeleteType tlid], FocusSame)
  | DeleteUserTypeForever tlid ->
      Many
        [ RPC ([DeleteTypeForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              {m with deletedUserTipes = TD.remove ~tlid m.deletedUserTipes} )
        ]
  | AddOpRPCCallback (_, params, Ok _) when params.opCtr = None ->
      HandleAPIError
        (ApiError.make
           ~context:"RPC - old server, no opCtr sent"
           ~importance:ImportantError
           ~requestParams:(Encoders.addOpRPCParams params)
           (* not a great error ... but this is an api error without a
            * corresponding actual http error *)
           Tea.Http.Aborted)
  | AddOpRPCCallback (focus, params, Ok r) ->
      let m, newOps, _ = filterOpsAndResult m params None in
      let params = {params with ops = newOps} in
      let initialMods =
        applyOpsToClient (focus != FocusNoChange) params r.result
      in
      let focusMods =
        if focus = FocusNoChange
        then []
        else
          let m =
            { m with
              lastOpCtr = params.opCtr
            ; handlers =
                TD.mergeRight m.handlers (Handlers.fromList r.result.handlers)
            ; dbs = TD.mergeRight m.dbs (DB.fromList r.result.dbs)
            ; userFunctions =
                TD.mergeRight
                  m.userFunctions
                  (Functions.fromList r.result.userFunctions)
            ; userTipes =
                TD.mergeRight
                  m.userTipes
                  (UserTypes.fromList r.result.userTipes) }
          in
          let newState = processFocus m focus in
          [AutocompleteMod ACReset; ClearError; newState]
      in
      Many (initialMods @ focusMods)
  | AddOpStrollerMsg msg ->
      if msg.params.browserId = m.browserId
      then
        NoChange
        (* msg was sent from this client, we've already handled it
                       in AddOpRPCCallback *)
      else
        let m, newOps, result =
          filterOpsAndResult m msg.params (Some msg.result)
        in
        let params = {msg.params with ops = newOps} in
        let initialMods =
          applyOpsToClient false params (result |> Option.valueExn)
        in
        Many (initialMods @ [MakeCmd (Entry.focusEntry m)])
  | InitialLoadRPCCallback
      (focus, extraMod (* for integration tests, maybe more *), Ok r) ->
      let pfM =
        { m with
          opCtrs = r.opCtrs
        ; handlers = Handlers.fromList r.handlers
        ; dbs = DB.fromList r.dbs
        ; userFunctions = Functions.fromList r.userFunctions
        ; userTipes = UserTypes.fromList r.userTipes
        ; handlerProps = ViewUtils.createHandlerProp r.handlers }
      in
      let newState = processFocus pfM focus in
      let allTLs = TL.all pfM in
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
        [ SetToplevels (r.handlers, r.dbs, true)
        ; SetDeletedToplevels (r.deletedHandlers, r.deletedDBs)
        ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, true)
        ; SetTypes (r.userTipes, r.deletedUserTipes, true)
        ; SetUnlockedDBs r.unlockedDBs
        ; SetPermission r.permission
        ; Append404s r.fofs
        ; AppendStaticDeploy r.staticDeploys
        ; AutocompleteMod ACReset
        ; ClearError
        ; extraMod
        ; newState
        ; UpdateTraces traces
        ; InitIntrospect (TD.values allTLs) ]
  | SaveTestRPCCallback (Ok msg_) ->
      DisplayError ("Success! " ^ msg_)
  | ExecuteFunctionRPCCallback (params, Ok (dval, hash, tlids, unlockedDBs)) ->
      let traces =
        List.map
          ~f:(fun tlid -> (deTLID tlid, [(params.efpTraceID, None)]))
          tlids
      in
      Many
        [ UpdateTraceFunctionResult
            ( params.efpTLID
            , params.efpTraceID
            , params.efpCallerID
            , params.efpFnName
            , hash
            , dval )
        ; ExecutingFunctionComplete [(params.efpTLID, params.efpCallerID)]
        ; OverrideTraces (StrDict.fromList traces)
        ; SetUnlockedDBs unlockedDBs ]
  | TriggerHandlerRPCCallback (params, Ok tlids) ->
      let traces =
        List.map
          ~f:(fun tlid -> (deTLID tlid, [(params.thTraceID, None)]))
          tlids
        |> StrDict.fromList
      in
      Many
        [ OverrideTraces traces
        ; TweakModel
            (fun m ->
              let handlerProps =
                RT.setHandlerExeState params.thTLID Complete m.handlerProps
              in
              {m with handlerProps} ) ]
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
  | NewPresencePush avatarsList ->
      UpdateAvatarList avatarsList
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
  | ReceiveFetch (TraceFetchFailure (params, url, error)) ->
      Many
        [ TweakModel
            (Sync.markResponseInModel ~key:("tracefetch-" ^ params.gtdrpTraceID))
        ; DisplayAndReportError ("Error fetching trace", Some url, Some error)
        ]
  | ReceiveFetch (TraceFetchSuccess (params, result)) ->
      let traces =
        StrDict.fromList [(deTLID params.gtdrpTlid, [result.trace])]
      in
      Many
        [ TweakModel
            (Sync.markResponseInModel ~key:("tracefetch-" ^ params.gtdrpTraceID))
        ; UpdateTraces traces ]
  | ReceiveFetch (TraceFetchMissing params) ->
      (* We'll force it so no need to update syncState *)
      let _, cmd =
        Analysis.requestTrace
          ~force:true
          m
          params.gtdrpTlid
          params.gtdrpTraceID
      in
      MakeCmd cmd
  | ReceiveFetch (DbStatsFetchFailure (params, url, error)) ->
      let key =
        params.dbStatsTlids |> List.map ~f:deTLID |> String.join ~sep:","
      in
      Many
        [ TweakModel (Sync.markResponseInModel ~key:("update-db-stats-" ^ key))
        ; DisplayAndReportError
            ("Error fetching db stats", Some url, Some error) ]
  | ReceiveFetch (DbStatsFetchMissing params) ->
      let key =
        params.dbStatsTlids |> List.map ~f:deTLID |> String.join ~sep:","
      in
      TweakModel (Sync.markResponseInModel ~key:("update-db-stats-" ^ key))
  | ReceiveFetch (DbStatsFetchSuccess (params, result)) ->
      let key =
        params.dbStatsTlids |> List.map ~f:deTLID |> String.join ~sep:","
      in
      Many
        [ TweakModel (Sync.markResponseInModel ~key:("update-db-stats-" ^ key))
        ; UpdateDBStats result ]
  | AddOpRPCCallback (_, params, Error err) ->
      HandleAPIError
        (ApiError.make
           ~context:"RPC"
           ~importance:ImportantError
           ~requestParams:(Encoders.addOpRPCParams params)
           err)
  | SaveTestRPCCallback (Error err) ->
      DisplayError ("Error: " ^ Tea_http.string_of_error err)
  | ExecuteFunctionRPCCallback (params, Error err) ->
      HandleAPIError
        (ApiError.make
           ~context:"ExecuteFunction"
           ~importance:ImportantError
           ~requestParams:(Encoders.executeFunctionRPCParams params)
           err)
  | TriggerHandlerRPCCallback (_, Error err) ->
      HandleAPIError
        (ApiError.make ~context:"TriggerHandler" ~importance:ImportantError err)
  | InitialLoadRPCCallback (_, _, Error err) ->
      HandleAPIError
        (ApiError.make ~context:"InitialLoad" ~importance:ImportantError err)
  | GetUnlockedDBsRPCCallback (Error err) ->
      Many
        [ TweakModel (Sync.markResponseInModel ~key:"unlocked")
        ; HandleAPIError
            (ApiError.make
               ~context:"GetUnlockedDBs"
               ~importance:IgnorableError
               err) ]
  | Delete404RPCCallback (params, Error err) ->
      HandleAPIError
        (ApiError.make
           ~context:"Delete404"
           ~importance:ImportantError
           ~requestParams:(Encoders.fof params)
           err)
  | JSError msg_ ->
      DisplayError ("Error in JS: " ^ msg_)
  | WindowResize (w, h) | WindowOnLoad (w, h) ->
      TweakModel
        (fun m_ ->
          {m_ with canvasProps = {m_.canvasProps with viewportSize = {w; h}}}
          )
  | LocationChange loc ->
      Url.changeLocation loc
  | TimerFire (action, _) ->
    ( match action with
    | RefreshAnalysis ->
      ( match Toplevel.selected m with
      | Some tl when Toplevel.isDB tl ->
          Many [UpdateDBStatsRPC (TL.id tl); GetUnlockedDBsRPC]
      | _ ->
          GetUnlockedDBsRPC )
    | RefreshAvatars ->
        ExpireAvatars
    | _ ->
        NoChange )
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
      let pos = center in
      let ast = B.new_ () in
      let aHandler =
        { ast
        ; spec =
            { space = B.newF space
            ; name = B.newF path
            ; modifier = B.newF modifier }
        ; hTLID = tlid
        ; pos }
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
              ( [SetHandler (tlid, pos, aHandler)]
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
  | ToggleSideBar ->
      TweakModel (fun m -> {m with sidebarOpen = not m.sidebarOpen})
  | CreateRouteHandler action ->
      let center = findCenter m in
      Entry.submitOmniAction m center action
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
      let toast =
        TweakModel
          (fun m ->
            {m with toast = {m.toast with toastMessage = Some "Copied!"}} )
      in
      ( match Clipboard.copy m with
      | `Text text ->
          e##clipboardData##setData "text/plain" text ;
          e##preventDefault () ;
          toast
      | `Json json ->
          let data = Json.stringify json in
          e##clipboardData##setData "application/json" data ;
          e##preventDefault () ;
          toast
      | `None ->
          () ;
          NoChange )
  | ClipboardPasteEvent e ->
      let json = e##clipboardData##getData "application/json" in
      if json <> ""
      then Clipboard.paste m (`Json (Json.parseOrRaise json))
      else
        let text = e##clipboardData##getData "text/plain" in
        if text <> "" then Clipboard.paste m (`Text text) else NoChange
  | ClipboardCutEvent e ->
      let copyData, mod_ = Clipboard.cut m in
      let toast =
        TweakModel
          (fun m ->
            {m with toast = {m.toast with toastMessage = Some "Copied!"}} )
      in
      ( match copyData with
      | `Text text ->
          e##clipboardData##setData "text/plain" text ;
          e##preventDefault () ;
          Many [mod_; toast]
      | `Json json ->
          let data = Json.stringify json in
          e##clipboardData##setData "application/json" data ;
          (* this is probably gonna be useful for debugging, but customers
           * shouldn't get used to it *)
          e##clipboardData##setData "text/plain" data ;
          e##preventDefault () ;
          Many [mod_; toast]
      | `None ->
          () ;
          mod_ )
  | ClipboardCopyLivevalue (lv, pos) ->
      Native.Clipboard.copyToClipboard lv ;
      TweakModel
        (fun m ->
          {m with toast = {toastMessage = Some "Copied!"; toastPos = Some pos}}
          )
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
  | SetHoveringReferences (tlid, ids) ->
      Introspect.setHoveringReferences tlid ids
  | FluidKeyPress _ ->
      Fluid.update m msg
  | TriggerSendPresenceCallback (Ok _) ->
      NoChange
  | TriggerSendPresenceCallback (Error err) ->
      HandleAPIError
        (ApiError.make
           ~context:"TriggerSendPresenceCallback"
           ~importance:IgnorableError
           err)
  | FluidMouseClick _ ->
      impossible "Can never happen"
  | FluidCommandsFilter query ->
      TweakModel
        (fun m ->
          let cp = FluidCommands.filter m query m.fluidState.cp in
          {m with fluidState = {m.fluidState with cp}} )
  | FluidCommandsClick cmd ->
      Many [FluidCommands.runCommand m cmd; FluidCommandsClose]
  | TakeOffErrorRail (tlid, id) ->
      let tl = TL.getExn m tlid in
      let pd = TL.findExn tl id in
      Refactor.takeOffRail m tl pd
  | SetHandlerExeIdle tlid ->
      TweakModel
        (fun m ->
          let handlerProps = RT.setHandlerExeState tlid Idle m.handlerProps in
          {m with handlerProps} )
  | CopyCurl (tlid, pos) ->
      Curl.copyCurlMod m tlid pos
  | SetHandlerActionsMenu (tlid, show) ->
      TweakModel (Editor.setHandlerMenu tlid show)
  | ResetToast ->
      TweakModel (fun m -> {m with toast = Defaults.defaultToast})
  | UpdateMinimap data ->
      TweakModel
        (fun m -> {m with canvasProps = {m.canvasProps with minimap = data}})
  | GoToArchitecturalView ->
      Many
        [ TweakModel
            (fun m ->
              {m with canvasProps = {m.canvasProps with minimap = None}} )
        ; MakeCmd (Url.navigateTo Architecture) ]


let rec filter_read_only (m : model) (modification : modification) =
  if m.permission = Some ReadWrite
  then modification
  else
    match modification with
    | Enter _ | EnterWithOffset _ | RPC _ ->
        NoChange
    | Many ms ->
        Many (List.map ~f:(filter_read_only m) ms)
    | _ ->
        modification


let update (m : model) (msg : msg) : model * msg Cmd.t =
  let mods = update_ msg m |> filter_read_only m in
  let newm, newc = updateMod mods (m, Cmd.none) in
  Editor.serialize m ;
  ({newm with lastMsg = msg; lastMod = mods}, newc)


let subscriptions (m : model) : msg Tea.Sub.t =
  let keySubs =
    [Keyboard.downs (fun x -> GlobalKeyPress x)]
    @
    if VariantTesting.isFluid m.tests
    then
      match m.cursorState with
      | FluidEntering _ ->
          [FluidKeyboard.downs ~key:"fluid" (fun x -> FluidKeyPress x)]
      | _ ->
          []
    else []
  in
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
          @ [ Patched_tea_time.every
                ~key:"refresh_avatars"
                Tea.Time.second
                (fun f -> TimerFire (RefreshAvatars, f) ) ]
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
    if m.canvasProps.enablePan && not (isACOpened m)
    then
      [ Native.OnWheel.listen ~key:"on_wheel" (fun (dx, dy) ->
            MouseWheel (dx, dy) ) ]
    else []
  in
  let analysisSubs =
    [ Analysis.ReceiveAnalysis.listen ~key:"receive_analysis" (fun s ->
          ReceiveAnalysis s )
    ; Analysis.NewTracePush.listen ~key:"new_trace_push" (fun s ->
          NewTracePush s )
    ; Analysis.New404Push.listen ~key:"new_404_push" (fun s -> New404Push s)
    ; DarkStorage.NewStaticDeployPush.listen ~key:"new_static_deploy" (fun s ->
          NewStaticDeployPush s )
    ; Analysis.ReceiveFetch.listen ~key:"receive_fetch" (fun s ->
          ReceiveFetch s )
    ; Analysis.NewPresencePush.listen ~key:"new_presence_push" (fun s ->
          NewPresencePush s )
    ; Analysis.AddOp.listen ~key:"add_op" (fun s -> AddOpStrollerMsg s) ]
  in
  let clipboardSubs =
    [ Native.Clipboard.copyListener ~key:"copy_event" (fun e ->
          ClipboardCopyEvent e )
    ; Native.Clipboard.cutListener ~key:"cut_event" (fun e ->
          ClipboardCutEvent e )
    ; Native.Clipboard.pasteListener ~key:"paste_event" (fun e ->
          e##preventDefault () ;
          ClipboardPasteEvent e ) ]
  in
  let onCaptureView =
    [ Native.OnCaptureView.listen ~key:"capture_view" (fun s ->
          UpdateMinimap (Some s) ) ]
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
       ; analysisSubs
       ; onCaptureView ])


let debugging =
  let prog =
    Tea.Debug.debug
      show_msg
      { init = (fun a -> init a (Tea.Navigation.getLocation ()))
      ; view = View.view
      ; renderCallback = Fluid.renderCallback
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
    ; renderCallback = prog.renderCallback
    ; subscriptions = prog.subscriptions
    ; shutdown = prog.shutdown }


let normal =
  let program : (string, model, msg) Tea.Navigation.navigationProgram =
    { init
    ; view = View.view
    ; update
    ; renderCallback = Fluid.renderCallback
    ; subscriptions
    ; shutdown = (fun _ -> Cmd.none) }
  in
  Tea.Navigation.navigationProgram (fun x -> LocationChange x) program


let main = normal
