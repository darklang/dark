open Prelude

(* Tea *)
module Cmd = Tea.Cmd
module Http = Tea.Http

(* Dark *)
module AC = Autocomplete
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Key = Keyboard
module Regex = Util.Regex
module TD = TLIDDict

let incOpCtr (m : model) : model =
  { m with
    opCtrs =
      StrDict.update m.opCtrs ~key:m.clientOpCtrId ~f:(function
          | Some v ->
              Some (v + 1)
          | None ->
              Some 1) }


let opCtr (m : model) : int =
  match StrDict.get ~key:m.clientOpCtrId m.opCtrs with
  | Some ctr ->
      ctr
  | None ->
      0


let expireAvatars (avatars : Types.avatar list) : Types.avatar list =
  let fiveMinsAgo : float = Js.Date.now () -. (5.0 *. 60.0 *. 1000.0) in
  List.filter
    ~f:(fun av -> av.serverTime |> Js.Date.valueOf > fiveMinsAgo)
    avatars


let createBrowserId : string =
  BsUuid.Uuid.V4.create () |> BsUuid.Uuid.V4.toString


let createClientOpCtrId : string =
  BsUuid.Uuid.V4.create () |> BsUuid.Uuid.V4.toString


let manageBrowserId () : string =
  (* Setting the browser id in session storage so it is stored per tab *)
  match Dom.Storage.getItem "browserId" Dom.Storage.sessionStorage with
  | Some browserId ->
      browserId
  | None ->
      let newBrowserId = createBrowserId in
      Dom.Storage.setItem "browserId" newBrowserId Dom.Storage.sessionStorage ;
      newBrowserId


let init (encodedParamString : string) (location : Web.Location.location) =
  let ({ canvasName
       ; complete
       ; userContentHost
       ; environment
       ; csrfToken
       ; isAdmin
       ; buildHash
       ; username }
        : InitialParameters.t) =
    InitialParameters.fromString encodedParamString
  in
  let variants = VariantTesting.enabledVariantTests () in
  let m = SavedSettings.load canvasName |> SavedSettings.toModel in
  let m = SavedUserSettings.load username |> SavedUserSettings.toModel m in
  let page =
    Url.parseLocation location
    |> Option.withDefault ~default:Defaults.defaultModel.currentPage
  in
  (* these saved values may not be valid yet *)
  let savedCursorState = m.cursorState in
  let functions = complete in
  OldExpr.functions := functions ;
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
    ; canvasName
    ; userContentHost
    ; origin = location.origin
    ; environment
    ; csrfToken
    ; browserId = manageBrowserId ()
    ; clientOpCtrId = createClientOpCtrId
    ; isAdmin
    ; buildHash
    ; username
    ; teaDebuggerEnabled = Url.isDebugging ()
    ; unsupportedBrowser = Entry.unsupportedBrowser () }
  in
  let timeStamp = Js.Date.now () /. 1000.0 in
  let avMessage : avatarModelMessage =
    { canvasName = m.canvasName
    ; browserId = m.browserId
    ; tlid = None
    ; timestamp = timeStamp }
  in
  let m = {m with fluidState = Fluid.initAC m.fluidState m} in
  if Url.isIntegrationTest ()
  then (m, Cmd.batch [API.integration m m.canvasName; API.loadPackages m])
  else
    ( m
    , Cmd.batch
        [ API.loadPackages m
        ; API.initialLoad m (FocusPageAndCursor (page, savedCursorState))
        ; API.sendPresence m avMessage ] )


let processFocus (m : model) (focus : focus) : modification =
  match focus with
  | FocusNext (tlid, pred) ->
    ( match TL.get m tlid with
    | None ->
        NoChange
    | Some tl ->
        let next =
          pred
          |> Option.map ~f:(TL.getNextBlank tl)
          |> Option.withDefault ~default:(TL.firstBlank tl)
        in
        ( match next with
        | Some id ->
            Enter (Filling (tlid, id))
        | None ->
          ( match pred with
          | Some id ->
              Select (tlid, STID id)
          | None ->
              Select (tlid, STTopLevelRoot) ) ) )
  | FocusExact (tlid, id) ->
    ( match TL.getPD m tlid id with
    | Some pd ->
        if P.isBlank pd || P.toContent pd = ""
        then Enter (Filling (tlid, id))
        else Select (tlid, STID id)
    | _ ->
        NoChange )
  | FocusSame ->
    ( match unwrapCursorState m.cursorState with
    | Selecting (tlid, mId) ->
      ( match (TL.get m tlid, mId) with
      | Some tl, Some id ->
          if TL.isValidBlankOrID tl id
          then NoChange
          else Select (tlid, STTopLevelRoot)
      | Some _, None ->
          Select (tlid, STTopLevelRoot)
      | _ ->
          Deselect )
    | Entering (Filling (tlid, id)) ->
      ( match TL.get m tlid with
      | Some tl ->
          if TL.isValidBlankOrID tl id
          then NoChange
          else Select (tlid, STTopLevelRoot)
      | _ ->
          Deselect )
    | _ ->
        NoChange )
  | FocusPageAndCursor (page, cs) ->
      let useCS = Page.tlidOf page = tlidOf cs in
      let tlid, mID =
        (* If they don't match, the URL wins *)
        if useCS then (tlidOf cs, idOf cs) else (Page.tlidOf page, None)
      in
      let mTl = Option.andThen tlid ~f:(TL.get m) in
      let pd = Option.map2 mTl mID ~f:(fun tl id -> TL.find tl id) in
      ( match (mTl, pd) with
      | Some tl, Some (Some pd) when TL.isValidBlankOrID tl (P.toID pd) ->
          let query = AutocompleteMod (ACSetQuery (P.toContent pd)) in
          Many [SetPage page; SetCursorState cs; query]
      | Some _, Some None | Some _, None ->
          Many [SetPage page; SetCursorState cs; AutocompleteMod (ACSetQuery "")]
      | _, _ ->
          NoChange )
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


let applyOpsToClient updateCurrent (p : addOpAPIParams) (r : addOpAPIResult) :
    Types.modification list =
  [ UpdateToplevels (r.handlers, r.dbs, updateCurrent)
  ; UpdateDeletedToplevels (r.deletedHandlers, r.deletedDBs)
  ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, updateCurrent)
  ; SetTypes (r.userTipes, r.deletedUserTipes, updateCurrent)
  ; RefreshUsages (Introspect.tlidsToUpdateUsage p.ops) ]


let isACOpened (m : model) : bool =
  FluidAutocomplete.isOpened m.fluidState.ac
  || FluidCommands.isOpened m.fluidState.cp
  || AC.isOpened m.complete


let rec updateMod (mod_ : modification) ((m, cmd) : model * msg Cmd.t) :
    model * msg Cmd.t =
  if m.integrationTestState <> NoIntegrationTest
  then Debug.loG "mod update" (show_modification mod_) ;
  let newm, newcmd =
    let bringBackCurrentTL (oldM : model) (newM : model) : model =
      (* used with updateCurrent - if updateCurrent is false, we want to restore
       * the current TL so we don't lose local changes made since the API call *)
      match tlidOf oldM.cursorState with
      | Some tlid ->
          let tl = TL.get oldM tlid in
          ( match tl with
          | Some (TLDB db) ->
              DB.upsert newM db
          | Some (TLHandler h) ->
              Handlers.upsert newM h
          | Some (TLFunc func) ->
              UserFunctions.upsert newM func
          | Some (TLGroup _) | Some (TLTipe _) | None ->
              newM )
      | None ->
          newM
    in
    let handleAPI params focus =
      (* immediately update the model based on SetHandler and focus, if
         possible *)
      let m = m |> incOpCtr in
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
                true)
          params.ops
      in
      if hasNonHandlers
      then (m, API.addOp m focus params)
      else
        let localM =
          List.foldl
            ~f:(fun call m ->
              match call with
              | SetHandler (_tlid, _pos, h) ->
                  Handlers.upsert m h
              | SetFunction f ->
                  UserFunctions.upsert m f
              | SetType t ->
                  UserTypes.upsert m t
              | _ ->
                  m)
            ~init:m
            params.ops
        in
        let withFocus, wfCmd =
          updateMod
            (Many [AutocompleteMod ACReset; processFocus localM focus])
            (localM, Cmd.none)
        in
        (withFocus, Cmd.batch [wfCmd; API.addOp withFocus FocusNoChange params])
    in
    match mod_ with
    | DisplayError e ->
        ({m with error = Some e}, Cmd.none)
    | DisplayAndReportError (message, url, custom) ->
        let url = match url with Some url -> " (" ^ url ^ ")" | None -> "" in
        let custom = match custom with Some c -> ": " ^ c | None -> "" in
        let error = message ^ url ^ custom in
        (* Reload on bad csrf *)
        if String.contains error ~substring:"Bad CSRF"
        then Native.Location.reload true ;
        ( {m with error = Some error}
        , Tea.Cmd.call (fun _ -> Rollbar.send error None Js.Json.null) )
    | HandleAPIError apiError ->
        let now = Js.Date.now () |> Js.Date.fromFloat in
        let shouldReload =
          let buildHashMismatch =
            APIError.serverVersionOf apiError
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
          (* Reload if it's an auth failure or the frontend is out of date *)
          APIError.isBadAuth apiError || (buildHashMismatch && reloadAllowed)
        in
        let ignore =
          (* This message is deep in the server code and hard to pull
               * out, so just ignore for now *)
          Js.log "Already at latest redo - ignoring server error" ;
          String.contains
            (APIError.msg apiError)
            ~substring:"(client): Already at latest redo"
        in
        let cmd =
          if shouldReload
          then
            let m = {m with lastReload = Some now} in
            (* Previously, this was two calls to Tea_task.nativeBinding. But
             * only the first got called, unclear why. *)
            Cmd.call (fun _ ->
                SavedSettings.save m ;
                SavedUserSettings.save m ;
                Native.Location.reload true)
          else if (not ignore) && APIError.shouldRollbar apiError
          then Cmd.call (fun _ -> Rollbar.sendAPIError m apiError)
          else Cmd.none
        in
        let newM =
          let error =
            if APIError.shouldDisplayToUser apiError && not ignore
            then Some (APIError.msg apiError)
            else m.error
          in
          let lastReload = if shouldReload then Some now else m.lastReload in
          {m with error; lastReload}
        in
        (newM, cmd)
    | ClearError ->
        ({m with error = None}, Cmd.none)
    | AddOps (ops, focus) ->
        handleAPI (API.opsParams ops ((m |> opCtr) + 1) m.clientOpCtrId) focus
    | GetUnlockedDBsAPICall ->
        Sync.attempt ~key:"unlocked" m (API.getUnlockedDBs m)
    | UpdateDBStatsAPICall tlid ->
        Analysis.updateDBStats m tlid
    | GetWorkerStatsAPICall tlid ->
        Analysis.getWorkerStats m tlid
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
              recover
                "Attempted to end integration test but one ran + was already finished"
                ~debug:m.integrationTestState
                (fun _ -> IntegrationTest.fail "Already finished")
          | NoIntegrationTest ->
              recover
                "Attempted to end integration test but none was running"
                ~debug:m.integrationTestState
                (fun _ -> IntegrationTest.fail "Not running")
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
          let cmds = Cmd.batch (API.sendPresence m avMessage :: cap) in
          (Page.setPage m m.currentPage page, cmds)
        else
          (Page.setPage m m.currentPage Architecture, Url.updateUrl Architecture)
    | Select (tlid, p) ->
        let ( (cursorState : cursorState)
            , (maybeNewFluidState : fluidState option) ) =
          match p with
          | STTopLevelRoot ->
            ( match TL.get m tlid with
            | Some (TLDB _) | Some (TLGroup _) | Some (TLTipe _) ->
                (Selecting (tlid, None), None)
            | Some (TLFunc _) | Some (TLHandler _) ->
                (FluidEntering tlid, None)
            | None ->
                (Deselected, None) )
          | STID id ->
            ( match TL.getPD m tlid id with
            | Some _ ->
                (Selecting (tlid, Some id), None)
            | None ->
                (FluidEntering tlid, None) )
          | STCaret caretTarget ->
              let maybeNewFluidState =
                match Fluid.astAndStateFromTLID m tlid with
                | Some (ast, state) ->
                    Some
                      (Fluid.setPosition
                         state
                         (Fluid.posFromCaretTarget state ast caretTarget))
                | None ->
                    None
              in
              (FluidEntering tlid, maybeNewFluidState)
        in
        let m, hashcmd =
          match TL.get m tlid with
          | Some tl when tlidOf m.cursorState <> Some tlid ->
              let page = TL.asPage tl false in
              let m = Page.setPage m m.currentPage page in
              (m, Url.updateUrl page)
          | _ ->
              (m, Cmd.none)
        in
        let m =
          { m with
            cursorState
          ; fluidState =
              maybeNewFluidState |> Option.withDefault ~default:m.fluidState }
        in
        let m, acCmd =
          (* Note that we want to ensure that when we click out of an entry box that the AC is
           * reset, else we can't scroll. *)
          match p with
          | STTopLevelRoot ->
              processAutocompleteMods m [ACReset]
          | STID _ ->
              processAutocompleteMods m []
          | STCaret _ ->
              processAutocompleteMods m [ACReset]
        in
        let m, afCmd = Analysis.analyzeFocused m in
        let timeStamp = Js.Date.now () /. 1000.0 in
        let avMessage : avatarModelMessage =
          { canvasName = m.canvasName
          ; browserId = m.browserId
          ; tlid = Some tlid
          ; timestamp = timeStamp }
        in
        let commands = [hashcmd; acCmd; afCmd; API.sendPresence m avMessage] in
        (m, Cmd.batch commands)
    | Deselect ->
        if m.cursorState <> Deselected
        then
          let m = TLMenu.closeMenu m in
          let hashcmd = Url.updateUrl Architecture in
          let m = Page.setPage m m.currentPage Architecture in
          let m, acCmd = processAutocompleteMods m [ACReset] in
          let m = {m with cursorState = Deselected} in
          let m =
            {m with fluidState = Fluid.deselectFluidEditor m.fluidState}
          in
          let timeStamp = Js.Date.now () /. 1000.0 in
          let avMessage : avatarModelMessage =
            { canvasName = m.canvasName
            ; browserId = m.browserId
            ; tlid = None
            ; timestamp = timeStamp }
          in
          let commands = [hashcmd; acCmd; API.sendPresence m avMessage] in
          (m, Cmd.batch commands)
        else (m, Cmd.none)
    | Enter entry ->
        let cursorState, target =
          match entry with
          | Creating _ ->
              (Entering entry, None)
          | Filling (tlid, id) ->
            ( match TL.getPD m tlid id with
            | Some pd ->
                (Entering entry, Some (tlid, pd))
            | None ->
                (FluidEntering tlid, None) )
        in
        let m, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m = {m with cursorState} in
        let m, afCmd = Analysis.analyzeFocused m in
        (m, Cmd.batch [afCmd; acCmd; Entry.focusEntry m])
    | EnterWithOffset (entry, offset) ->
        let cursorState, target =
          match entry with
          | Creating _ ->
              (Entering entry, None)
          | Filling (tlid, id) ->
            ( match TL.getPD m tlid id with
            | Some pd ->
                (Entering entry, Some (tlid, pd))
            | None ->
                (FluidEntering tlid, None) )
        in
        let m, acCmd = processAutocompleteMods m [ACSetTarget target] in
        let m = {m with cursorState} in
        let m, afCmd = Analysis.analyzeFocused m in
        (m, Cmd.batch [afCmd; acCmd; Entry.focusEntryWithOffset m offset])
    | RemoveToplevel tl ->
        (Toplevel.remove m tl, Cmd.none)
    | RemoveGroup tl ->
        (Toplevel.remove m tl, Cmd.none)
    | SetToplevels (handlers, dbs, groups, updateCurrent) ->
        let oldM = m in
        let m =
          { m with
            handlers = Handlers.fromList handlers
          ; dbs = DB.fromList dbs
          ; groups = Groups.fromList groups }
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
          (SetToplevels
             ( TD.values m.handlers
             , TD.values m.dbs
             , TD.values m.groups
             , updateCurrent ))
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
    | UpdateAnalysis (traceID, dvals) ->
        let m =
          { m with
            analyses =
              Analysis.record m.analyses traceID (LoadableSuccess dvals) }
        in
        let m, _ =
          updateMod (Fluid.update m FluidUpdateAutocomplete) (m, Cmd.none)
        in
        processAutocompleteMods m [ACRegenerate]
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
                  Some r)
            m.dbStats
            statsStore
        in
        let m = {m with dbStats = newStore} in
        (m, Cmd.none)
    | UpdateWorkerStats (tlid, workerStats) ->
        let newWorkerStats =
          TLIDDict.insert ~tlid ~value:workerStats m.workerStats
        in
        let m = {m with workerStats = newWorkerStats} in
        (m, Cmd.none)
    | UpdateWorkerSchedules schedules ->
        let m = {m with worker_schedules = schedules} in
        (m, Cmd.none)
    | UpdateTraces traces ->
        let newTraces =
          Analysis.mergeTraces
            ~onConflict:(fun (oldID, oldData) (newID, newData) ->
              (* Update if:
               * - new data is ok (successful fetch)
               * - old data is an error, so is new data, but different errors
               * *)
              if Result.isOk newData
                 || ((not (Result.isOk oldData)) && oldData <> newData)
              then (newID, newData)
              else (oldID, oldData))
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
            ~onConflict:(fun _old (newID, _) -> (newID, Error NoneYet))
            m.traces
            traces
        in
        let m = {m with traces = newTraces} in
        let m, afCmd = Analysis.analyzeFocused m in
        let m, acCmd = processAutocompleteMods m [ACRegenerate] in
        (m, Cmd.batch [afCmd; acCmd])
    | UpdateTraceFunctionResult
        (tlid, traceID, callerID, fnName, hash, hashVersion, dval) ->
        let m =
          Analysis.replaceFunctionResult
            m
            tlid
            traceID
            callerID
            fnName
            hash
            hashVersion
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
        let oldM = m in
        let m =
          { m with
            userFunctions =
              TD.mergeRight m.userFunctions (UserFunctions.fromList userFuncs)
              |> TD.removeMany
                   ~tlids:(List.map ~f:UserFunctions.toID deletedUserFuncs)
          ; deletedUserFunctions =
              TD.mergeRight
                m.deletedUserFunctions
                (UserFunctions.fromList deletedUserFuncs)
              |> TD.removeMany ~tlids:(List.map ~f:UserFunctions.toID userFuncs)
          }
        in
        (* Update the functions variable for use in FluidPrinter for fn params *)
        OldExpr.functions := FluidAutocomplete.allFunctions m ;
        (* Bring back the TL being edited, so we don't lose work done since the
           API call *)
        let m = if updateCurrent then m else bringBackCurrentTL oldM m in
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
                TL.get m tlid
                |> Option.andThen ~f:TL.asUserTipe
                |> Option.map ~f:(UserTypes.upsert m2)
                |> Option.withDefault ~default:m2
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
         * regular unlockedDBs API timer to do a full SetUnlockedDBs, but this
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
                  <> f404.space ^ f404.path ^ f404.modifier)
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
                 ^ f404.traceID)
        in
        ({m with f404s = new404s}, Cmd.none)
    | ExpireAvatars ->
        ({m with avatarsList = m.avatarsList |> expireAvatars}, Cmd.none)
    | UpdateAvatarList avatarsList ->
        let updatedAvatars =
          avatarsList
          |> List.filter ~f:(fun (avatar : Types.avatar) ->
                 avatar.browserId != m.browserId)
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
    | AddToGroup (gTLID, tlid) ->
        (* Add to group spec: https://docs.google.com/document/d/19dcGeRZ4c7PW9hYNTJ9A7GsXkS2wggH2h2ABqUw7R6A/edit#heading=h.qw5p3qit4rug *)
        let newMod, newCmd = Groups.addToGroup m gTLID tlid in
        (newMod, newCmd)
    | MoveMemberToNewGroup (gTLID, tlid, newMod) ->
        let newMod, newCmd = Groups.addToGroup newMod gTLID tlid in
        (newMod, newCmd)
    | ShowSaveToast ->
        (* This shows the user that they dont need to hit cmd + s to save. *)
        ( { m with
            toast =
              {toastMessage = Some "Dark saves automatically!"; toastPos = None}
          }
        , Cmd.none )
    | SetTLTraceID (tlid, traceID) ->
        let m = Analysis.setSelectedTraceID m tlid traceID in
        let m, afCmd = Analysis.analyzeFocused m in
        (m, afCmd)
    | DragTL (tlid, offset, hasMoved, state) ->
        (* Because mouseEvents are not perfectly reliable, we can end up in
         * weird dragging states. If we start dragging, make sure the state
         * we're in before isnt also dragging. *)
        ( { m with
            cursorState =
              DraggingTL (tlid, offset, hasMoved, unwrapCursorState state) }
        , Cmd.none )
    | PanCanvas {viewportStart; viewportCurr; prevCursorState} ->
        ( { m with
            cursorState =
              PanningCanvas {viewportStart; viewportCurr; prevCursorState} }
        , Cmd.none )
    | ExecutingFunctionBegan (tlid, id) ->
        let nexecutingFunctions = m.executingFunctions @ [(tlid, id)] in
        ({m with executingFunctions = nexecutingFunctions}, Cmd.none)
    | ExecutingFunctionAPICall (tlid, id, name) ->
      ( match TL.get m tlid with
      | Some tl ->
          let traceID = Analysis.getSelectedTraceID m tlid in
          ( match Option.andThen traceID ~f:(Analysis.getTrace m tlid) with
          | Some (traceID, _) ->
            ( match Analysis.getArguments m tl id traceID with
            | Some args ->
                let params =
                  { efpTLID = tlid
                  ; efpCallerID = id
                  ; efpTraceID = traceID
                  ; efpFnName = name
                  ; efpArgs = args }
                in
                (m, API.executeFunction m params)
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
          (m, Cmd.none) |> updateMod (ExecutingFunctionComplete [(tlid, id)]) )
    | ExecutingFunctionComplete targets ->
        let isComplete target = not <| List.member ~value:target targets in
        let nexecutingFunctions =
          List.filter ~f:isComplete m.executingFunctions
        in
        ({m with executingFunctions = nexecutingFunctions}, Cmd.none)
    | MoveCanvasTo (offset, panAnimation) ->
        let newCanvasProps =
          { m.canvasProps with
            offset
          ; panAnimation
          ; lastOffset = Some m.canvasProps.offset }
        in
        ({m with canvasProps = newCanvasProps}, Cmd.none)
    | CenterCanvasOn tlid ->
      ( match TL.get m tlid with
      | Some tl ->
          ( { m with
              canvasProps =
                { m.canvasProps with
                  offset = Viewport.centerCanvasOn tl
                ; panAnimation = AnimateTransition } }
          , Cmd.none )
      | None ->
          (m, Cmd.none) )
    | TriggerHandlerAPICall tlid ->
        let traceID = Analysis.getSelectedTraceID m tlid in
        ( match Option.andThen traceID ~f:(Analysis.getTrace m tlid) with
        | Some (traceID, Ok traceData) ->
            let handlerProps =
              RT.setHandlerExeState tlid Executing m.handlerProps
            in
            ( {m with handlerProps}
            , API.triggerHandler
                m
                {thTLID = tlid; thTraceID = traceID; thInput = traceData.input}
            )
        | _ ->
            (m, Cmd.none) )
    | InitIntrospect tls ->
        (Introspect.refreshUsages m (List.map ~f:TL.id tls), Cmd.none)
    | RefreshUsages tlids ->
        (Introspect.refreshUsages m tlids, Cmd.none)
    | FluidCommandsShow (tlid, id) ->
        ( FluidCommands.show m tlid id
        , Tea_html_cmds.focus FluidCommands.filterInputID )
    | FluidCommandsClose ->
        let cp = FluidCommands.reset m in
        ( {m with fluidState = {m.fluidState with cp; selectionStart = None}}
        , Cmd.none )
    | AddGroup group ->
        (* This code is temp while we work on FE *)
        let nameAlreadyUsed = Groups.isGroupNameUnique group m.groups in
        if nameAlreadyUsed
        then (m, Cmd.none)
        else
          ( { m with
              groups = TLIDDict.insert ~tlid:group.gTLID ~value:group m.groups
            }
          , Cmd.none )
    | FluidStartClick ->
        ({m with fluidState = {m.fluidState with midClick = true}}, Cmd.none)
    | FluidEndClick ->
        ({m with fluidState = {m.fluidState with midClick = false}}, Cmd.none)
    | TweakModel fn ->
        (fn m, Cmd.none)
    | Apply fn ->
        let mod_ = fn m in
        updateMod mod_ (m, cmd)
    | AutocompleteMod mod_ ->
        processAutocompleteMods m [mod_]
    | UndoGroupDelete (tlid, g) ->
        let newModel = Groups.upsert m g in
        ( {newModel with deletedGroups = TD.remove ~tlid m.deletedGroups}
        , Cmd.none )
    | SetClipboardContents (data, e) ->
        Clipboard.setData data e ;
        (m, Cmd.none)
    | UpdateASTCache (tlid, str) ->
        let searchCache =
          m.searchCache |> TLIDDict.update ~tlid ~f:(fun _ -> Some str)
        in
        ({m with searchCache}, Cmd.none)
    | InitASTCache (handlers, userFunctions) ->
        let hcache =
          handlers
          |> List.foldl ~init:m.searchCache ~f:(fun h cache ->
                 let value = FluidPrinter.eToHumanString h.ast in
                 cache |> TLIDDict.insert ~tlid:h.hTLID ~value)
        in
        let searchCache =
          userFunctions
          |> List.foldl ~init:hcache ~f:(fun f cache ->
                 let value = FluidPrinter.eToHumanString f.ufAST in
                 cache |> TLIDDict.insert ~tlid:f.ufTLID ~value)
        in
        ({m with searchCache}, Cmd.none)
    | FluidSetState fluidState ->
        ({m with fluidState}, Cmd.none)
    | TLMenuUpdate (tlid, msg) ->
        (TLMenu.update m tlid msg, Cmd.none)
    | SettingsViewUpdate msg ->
        SettingsView.update m msg
    (* applied from left to right *)
    | Many mods ->
        List.foldl ~f:updateMod ~init:(m, Cmd.none) mods
  in
  let cmds = Cmd.batch [cmd; newcmd] in
  let newm, modi =
    let mTLID =
      match m.cursorState with
      | FluidEntering tlid when Some tlid <> tlidOf newm.cursorState ->
          Some tlid
      | _ ->
          None
    in
    Fluid.cleanUp newm mTLID
  in
  match modi with NoChange -> (newm, cmds) | _ -> updateMod modi (newm, cmds)


let update_ (msg : msg) (m : model) : modification =
  if m.integrationTestState <> NoIntegrationTest
  then Debug.loG "msg update" (show_msg msg) ;
  match msg with
  | GlobalKeyPress event ->
      KeyPress.handler event m
  | EntryInputMsg query ->
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
  | FluidMsg (FluidUpdateDropdownIndex index) ->
      Fluid.update m (FluidUpdateDropdownIndex index)
  | FluidMsg (FluidAutocompleteClick item) ->
    ( match unwrapCursorState m.cursorState with
    | FluidEntering _ ->
        Fluid.update m (FluidAutocompleteClick item)
    | _ ->
        NoChange )
  | AppMouseDown event ->
      if event.button = Defaults.leftButton
      then
        match m.cursorState with
        | PanningCanvas {prevCursorState; _} ->
            (* In case we are already panning, we got into a weird state;
             * we should stop panning. *)
            SetCursorState prevCursorState
        | _ ->
            PanCanvas
              { viewportStart = event.mePos
              ; viewportCurr = event.mePos
              ; prevCursorState = m.cursorState }
      else NoChange
  | AppMouseDrag mousePos ->
    ( match m.cursorState with
    | PanningCanvas {viewportStart; viewportCurr; prevCursorState} ->
        let viewportNext = {vx = mousePos.x; vy = mousePos.y} in
        let dx = viewportCurr.vx - viewportNext.vx in
        let dy = viewportCurr.vy - viewportNext.vy in
        Many
          [ Viewport.moveCanvasBy m dx dy
          ; PanCanvas
              {viewportStart; viewportCurr = viewportNext; prevCursorState} ]
    | _ ->
        NoChange )
  | WindowMouseUp event | AppMouseUp event ->
      let clickBehavior =
        match m.currentPage with
        | FocusedFn tlid | FocusedType tlid ->
            (* Clicking on the raw canvas should keep you selected to functions/types in their space *)
            let defaultBehaviour = Select (tlid, STTopLevelRoot) in
            ( match unwrapCursorState m.cursorState with
            | Entering (Filling _ as cursor) ->
                (* If we click away from an entry box, commit it before doing the default behaviour *)
                [Entry.commit m cursor; defaultBehaviour]
            | _ ->
                [defaultBehaviour] )
        | Architecture | FocusedDB _ | FocusedHandler _ | FocusedGroup _ ->
            if event.button = Defaults.leftButton
            then
              (* Clicking on the canvas should deselect the current selection on the main canvas *)
              let defaultBehaviour = Deselect in
              match unwrapCursorState m.cursorState with
              | Deselected ->
                  let openAt = Some (Viewport.toAbsolute m event.mePos) in
                  [AutocompleteMod ACReset; Entry.openOmnibox ~openAt ()]
              | Entering (Filling _ as cursor) ->
                  (* If we click away from an entry box, commit it before doing the default behaviour *)
                  [Entry.commit m cursor; defaultBehaviour]
              | _ ->
                  [defaultBehaviour]
            else []
      in
      ( match m.cursorState with
      | PanningCanvas {viewportStart; viewportCurr; prevCursorState} ->
          let distSquared (a : vPos) (b : vPos) : int =
            let dx = b.vx - a.vx in
            let dy = b.vy - a.vy in
            (dx * dx) + (dy * dy)
          in
          let maxSquareDistToConsiderAsClick = 16 in
          if distSquared viewportStart viewportCurr
             <= maxSquareDistToConsiderAsClick
          then Many (SetCursorState prevCursorState :: clickBehavior)
          else SetCursorState prevCursorState
      | DraggingTL (draggingTLID, _, hasMoved, origCursorState) ->
        ( match TL.get m draggingTLID with
        | Some tl ->
            if hasMoved
            then
              (* We've been updating tl.pos as mouse moves, *)
              (* now want to report last pos to server *)
              if not (TL.isGroup tl)
              then
                Many
                  [ SetCursorState origCursorState
                  ; AddOps ([MoveTL (draggingTLID, TL.pos tl)], FocusNoChange)
                  ]
              else SetCursorState origCursorState
            else
              (* if we haven't moved, treat this as a single click and not a attempted drag *)
              let defaultBehaviour = Select (draggingTLID, STTopLevelRoot) in
              ( match origCursorState with
              | Entering (Filling _ as cursor) ->
                  Many [Entry.commit m cursor; defaultBehaviour]
              | _ ->
                  defaultBehaviour )
        | None ->
            SetCursorState origCursorState )
      | _ ->
          NoChange )
  | BlankOrMouseEnter (tlid, id, _) ->
      SetHover (tlid, id)
  | BlankOrMouseLeave (tlid, id, _) ->
      ClearHover (tlid, id)
  | MouseWheel (x, y) ->
      Viewport.moveCanvasBy m x y
  | TraceMouseEnter (tlid, traceID, _) ->
      let traceCmd =
        match Analysis.getTrace m tlid traceID with
        | Some (_, Error _) ->
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
      TriggerHandlerAPICall tlid
  | DragToplevel (_, mousePos) ->
    ( match m.cursorState with
    | DraggingTL (draggingTLID, startVPos, _, origCursorState) ->
        let xDiff = mousePos.x - startVPos.vx in
        let yDiff = mousePos.y - startVPos.vy in
        let m2 = TL.move draggingTLID xDiff yDiff m in
        Many
          [ SetToplevels
              ( TD.values m2.handlers
              , TD.values m2.dbs
              , TD.values m2.groups
              , true )
          ; DragTL
              ( draggingTLID
              , {vx = mousePos.x; vy = mousePos.y}
              , true
              , origCursorState ) ]
    | _ ->
        NoChange )
  | TLDragRegionMouseDown (targetTLID, event) ->
      if event.button = Defaults.leftButton
      then
        let tl = TL.get m targetTLID in
        match tl with
        | Some (TLFunc _) | Some (TLTipe _) | None ->
            NoChange
        | Some (TLHandler _) | Some (TLDB _) | Some (TLGroup _) ->
            DragTL (targetTLID, event.mePos, false, m.cursorState)
      else NoChange
  | TLDragRegionMouseUp (tlid, event) ->
      if event.button = Defaults.leftButton
      then
        match m.cursorState with
        | DraggingTL (draggingTLID, _, hasMoved, origCursorState) ->
          ( match TL.get m draggingTLID with
          | Some tl ->
              if hasMoved
              then
                (* We've been updating tl.pos as mouse moves, *)
                (* now want to report last pos to server *)
                (* the SetCursorState here isn't always necessary *)
                (* because in the happy case we'll also receive *)
                (* a ToplevelClick event, but it seems that sometimes *)
                (* we don't, perhaps due to overlapping click handlers *)
                (* There doesn't seem to be any harm in stopping dragging *)
                (* here though *)
                if not (TL.isGroup tl)
                then
                  (* Check if toplevel landed on top of a group *)
                  (* https://docs.google.com/document/d/19dcGeRZ4c7PW9hYNTJ9A7GsXkS2wggH2h2ABqUw7R6A/edit#heading=h.qw5p3qit4rug *)
                  let gTlid =
                    Groups.landedInGroup draggingTLID m.groups |> List.head
                  in
                  match gTlid with
                  | Some tlid ->
                      Many
                        [ SetCursorState origCursorState
                        ; AddToGroup (tlid, draggingTLID) ]
                  | None ->
                      Many
                        [ SetCursorState origCursorState
                        ; AddOps
                            ([MoveTL (draggingTLID, TL.pos tl)], FocusNoChange)
                        ]
                else SetCursorState origCursorState
              else
                (* if we haven't moved, treat this as a single click and not a attempted drag *)
                let defaultBehaviour = Select (draggingTLID, STTopLevelRoot) in
                ( match origCursorState with
                | Entering (Filling _ as cursor) ->
                    Many [Entry.commit m cursor; defaultBehaviour]
                | _ ->
                    defaultBehaviour )
          | None ->
              SetCursorState origCursorState )
        | Entering (Filling _ as cursor) ->
            Many
              [ Entry.commit m cursor
              ; Select (tlid, STTopLevelRoot)
              ; FluidEndClick ]
        | _ ->
            Many [Select (tlid, STTopLevelRoot); FluidEndClick]
      else NoChange
  | BlankOrClick (targetExnID, targetID, event) ->
      let select id =
        let offset =
          Native.OffsetEstimator.estimateClickOffset (showID id) event
        in
        (* If we're in the Fluid world, we should treat clicking legacy BlankOr inputs
         * as double clicks to automatically enter them. *)
        Selection.dblclick m targetExnID targetID offset
      in
      ( match m.cursorState with
      | Deselected ->
          select targetID
      | DraggingTL (_, _, _, prevCursorState)
      | PanningCanvas {prevCursorState; _} ->
          SetCursorState prevCursorState
      | Entering cursor ->
          let defaultBehaviour = select targetID in
          ( match cursor with
          | Filling (_, fillingID) ->
              if fillingID = targetID
              then
                NoChange
                (* If we click away from an entry box, commit it before doing the default behaviour *)
              else Many [Entry.commit m cursor; defaultBehaviour]
          | _ ->
              defaultBehaviour )
      | Selecting (_, _) ->
          select targetID
      | FluidEntering _ ->
          select targetID )
  | BlankOrDoubleClick (targetExnID, targetID, event) ->
      let offset =
        Native.OffsetEstimator.estimateClickOffset (showID targetID) event
      in
      Selection.dblclick m targetExnID targetID offset
  | ToplevelClick (targetExnID, _) ->
      let defaultBehaviour =
        [ Select (targetExnID, STTopLevelRoot)
        ; Apply
            (fun m ->
              Fluid.update
                m
                (FluidMouseUp
                   {tlid = targetExnID; selection = None; editorIdx = 0})) ]
      in
      ( match m.cursorState with
      (* If we click away from an entry box, commit it before doing the default behaviour *)
      | Entering (Filling _ as cursor) ->
          Many (Entry.commit m cursor :: defaultBehaviour)
      | _ ->
          Many defaultBehaviour )
  | ExecuteFunctionButton (tlid, id, name) ->
      let selectionTarget : tlidSelectTarget =
        (* Note that the intent here is to make the live value visible, which
         * is a side-effect of placing the caret right after the function name
         * in the handler where the function is being called.  We're relying on
         * the length of the function name representing the offset into the
         * tokenized function call node corresponding to this location. Eg:
         * foo|v1 a b *)
        STCaret {astRef = ARFnCall id; offset = String.length name}
      in
      Many
        [ ExecutingFunctionBegan (tlid, id)
        ; ExecutingFunctionAPICall (tlid, id, name)
        ; Select (tlid, selectionTarget) ]
  | ExecuteFunctionFromWithin p ->
      Many
        [ ExecutingFunctionBegan (p.efpTLID, p.efpCallerID)
        ; MakeCmd (API.executeFunction m p) ]
  | TraceClick (tlid, traceID, _) ->
    ( match m.cursorState with
    | DraggingTL (_, _, _, origCursorState) ->
        SetCursorState origCursorState
    | Deselected ->
        Many [Select (tlid, STTopLevelRoot); SetTLTraceID (tlid, traceID)]
    | _ ->
        SetTLTraceID (tlid, traceID) )
  | StartMigration tlid ->
      let mdb = tlid |> TL.get m |> Option.andThen ~f:TL.asDB in
      ( match mdb with
      | Some db ->
          DB.startMigration tlid db.cols
      | None ->
          NoChange )
  | AbandonMigration tlid ->
      AddOps ([AbandonDBMigration tlid], FocusNothing)
  | DeleteColInDB (tlid, nameId) ->
      let mdb = tlid |> TL.get m |> Option.andThen ~f:TL.asDB in
      ( match mdb with
      | Some db ->
          if DB.isMigrationCol db nameId
          then AddOps ([DeleteColInDBMigration (tlid, nameId)], FocusNothing)
          else AddOps ([DeleteDBCol (tlid, nameId)], FocusNothing)
      | None ->
          NoChange )
  | ToggleEditorSetting fn ->
      TweakModel (fun m -> {m with editorSettings = fn m.editorSettings})
  | SaveTestButton ->
      MakeCmd (API.saveTest m)
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
      ( match (TL.get m tlid, mId) with
      | Some tl, Some id ->
          Refactor.extractFunction m tl id
      | _ ->
          NoChange )
    | _ ->
        NoChange )
  | DeleteUserFunctionParameter (uftlid, upf) ->
    ( match TL.get m uftlid |> Option.andThen ~f:TL.asUserFunction with
    | Some uf ->
        let replacement = UserFunctions.removeParameter uf upf in
        let newCalls = Refactor.removeFunctionParameter m uf upf in
        AddOps
          ([SetFunction replacement] @ newCalls, FocusNext (uf.ufTLID, None))
    | None ->
        NoChange )
  | AddUserFunctionParameter uftlid ->
    ( match TL.get m uftlid |> Option.andThen ~f:TL.asUserFunction with
    | Some uf ->
        let nextId = UserFunctions.idOfLastBlankor uf in
        Refactor.addFunctionParameter m uf nextId
    | None ->
        NoChange )
  | DeleteUserTypeField (tipetlid, field) ->
    ( match TL.get m tipetlid |> Option.andThen ~f:TL.asUserTipe with
    | Some tipe ->
        let replacement = UserTypes.removeField tipe field in
        AddOps ([SetType replacement], FocusNext (tipe.utTLID, None))
    | None ->
        NoChange )
  | ToplevelDelete tlid ->
      TL.get m tlid
      |> Option.map ~f:(fun tl ->
             Many
               [RemoveToplevel tl; AddOps ([DeleteTL (TL.id tl)], FocusNothing)])
      |> Option.withDefault ~default:NoChange
  | ToplevelDeleteForever tlid ->
      Many
        [ AddOps ([DeleteTLForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              { m with
                deletedHandlers = TD.remove ~tlid m.deletedHandlers
              ; deletedDBs = TD.remove ~tlid m.deletedDBs }) ]
  | DeleteUserFunction tlid ->
      let page = Page.maybeChangeFromPage tlid m.currentPage in
      Many (AddOps ([DeleteFunction tlid], FocusSame) :: page)
  | RestoreToplevel tlid ->
      (* Temporary check if tlid is a deleted group and add to model manually until groups has a BE *)
      let group = Groups.isFromDeletedGroup m tlid in
      ( match group with
      | Some g ->
          UndoGroupDelete (tlid, g)
      | None ->
          AddOps ([UndoTL tlid], FocusNext (tlid, None)) )
  | DeleteUserFunctionForever tlid ->
      Many
        [ AddOps ([DeleteFunctionForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              { m with
                deletedUserFunctions = TD.remove ~tlid m.deletedUserFunctions })
        ]
  | DeleteUserType tlid ->
      let page = Page.maybeChangeFromPage tlid m.currentPage in
      Many (AddOps ([DeleteType tlid], FocusSame) :: page)
  | DeleteGroup tlid ->
      (* Spec: https://docs.google.com/document/d/19dcGeRZ4c7PW9hYNTJ9A7GsXkS2wggH2h2ABqUw7R6A/edit#heading=h.vv225wwesyqm *)
      TL.get m tlid
      |> Option.map ~f:(fun tl -> Many [RemoveGroup tl])
      |> Option.withDefault ~default:NoChange
  | DragGroupMember (gTLID, tlid, event) ->
      (* Spec: https://docs.google.com/document/d/19dcGeRZ4c7PW9hYNTJ9A7GsXkS2wggH2h2ABqUw7R6A/edit#heading=h.s138ne3frlh0 *)
      let group = TD.get ~tlid:gTLID m.groups in
      ( match group with
      | Some g ->
          let newMembers =
            g.members |> List.filter ~f:(fun member -> member != tlid)
          in
          let newGroup = {g with members = newMembers} in
          let newMod = Groups.upsert m newGroup in
          ( match m.cursorState with
          | DraggingTL (_, _, _, origCursorState) ->
              let mePos = Viewport.toAbsolute m event.mePos in
              let gTlid = Groups.posInGroup mePos m.groups |> List.head in
              (* Check if the new pos is in another group *)
              ( match gTlid with
              | Some gTlid ->
                  Many
                    [ SetCursorState origCursorState
                    ; MoveMemberToNewGroup (gTlid, tlid, newMod) ]
              | None ->
                  (* update the toplevel pos with the curent event position  *)
                  Many
                    [ TweakModel (fun _m -> newMod)
                    ; SetCursorState origCursorState
                    ; AddOps ([MoveTL (tlid, mePos)], FocusNoChange) ] )
          | _ ->
              NoChange )
      | _ ->
          NoChange )
  | UpdateSegment msg ->
      Entry.sendSegmentMessage msg ;
      NoChange
  | CloseWelcomeModal ->
      Entry.sendSegmentMessage WelcomeModal ;
      Many [TweakModel (fun m -> {m with showUserWelcomeModal = false})]
  | DeleteUserTypeForever tlid ->
      Many
        [ AddOps ([DeleteTypeForever tlid], FocusSame)
        ; TweakModel
            (fun m ->
              {m with deletedUserTipes = TD.remove ~tlid m.deletedUserTipes}) ]
  | DeleteGroupForever tlid ->
      (* TODO: Add AddOps *)
      Many
        [ TweakModel
            (fun m -> {m with deletedGroups = TD.remove ~tlid m.deletedGroups})
        ]
  | AddOpsAPICallback (focus, params, Ok (r : addOpAPIResponse)) ->
      let m, newOps, _ = API.filterOpsAndResult m params None in
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
              opCtrs =
                StrDict.insert
                  m.opCtrs
                  ~key:params.clientOpCtrId
                  ~value:params.opCtr
            ; handlers =
                TD.mergeRight m.handlers (Handlers.fromList r.result.handlers)
            ; dbs = TD.mergeRight m.dbs (DB.fromList r.result.dbs)
            ; userFunctions =
                TD.mergeRight
                  m.userFunctions
                  (UserFunctions.fromList r.result.userFunctions)
            ; userTipes =
                TD.mergeRight
                  m.userTipes
                  (UserTypes.fromList r.result.userTipes) }
          in
          let newState = processFocus m focus in
          [AutocompleteMod ACReset; ClearError; newState]
      in
      Many (initialMods @ focusMods)
  | AddOpsStrollerMsg msg ->
      if msg.params.clientOpCtrId = m.clientOpCtrId
      then
        NoChange
        (* msg was sent from this client, we've already handled it
                       in AddOpsAPICallback *)
      else
        let m, newOps, result =
          API.filterOpsAndResult m msg.params (Some msg.result)
        in
        let params = {msg.params with ops = newOps} in
        let initialMods =
          applyOpsToClient false params (result |> Option.valueExn)
        in
        Many (initialMods @ [MakeCmd (Entry.focusEntry m)])
  | FetchAllTracesAPICallback (Ok x) ->
      let traces =
        List.foldl x.traces ~init:StrDict.empty ~f:(fun (tlid, traceid) dict ->
            let trace = (traceid, Error NoneYet) in
            StrDict.update dict ~key:(deTLID tlid) ~f:(function
                | Some existing ->
                    Some (existing @ [trace])
                | None ->
                    Some [trace]))
      in
      UpdateTraces traces
  | FetchAllTracesAPICallback (Error x) ->
      DisplayError ("Failed to load traces: " ^ Tea_http.string_of_error x)
  | InitialLoadAPICallback
      (focus, extraMod (* for integration tests, maybe more *), Ok r) ->
      let pfM =
        { m with
          opCtrs = r.opCtrs
        ; handlers = Handlers.fromList r.handlers
        ; dbs = DB.fromList r.dbs
        ; userFunctions = UserFunctions.fromList r.userFunctions
        ; userTipes = UserTypes.fromList r.userTipes
        ; handlerProps = ViewUtils.createHandlerProp r.handlers
        ; groups = TLIDDict.empty }
      in
      let newState = processFocus pfM focus in
      let allTLs = TL.all pfM in
      Many
        [ TweakModel
            (fun m ->
              { m with
                opCtrs = r.opCtrs
              ; account = r.account
              ; settingsView =
                  { m.settingsView with
                    canvas_list = r.canvas_list
                  ; org_list = r.org_list } })
        ; SetToplevels (r.handlers, r.dbs, r.groups, true)
        ; SetDeletedToplevels (r.deletedHandlers, r.deletedDBs)
        ; SetUserFunctions (r.userFunctions, r.deletedUserFunctions, true)
        ; SetTypes (r.userTipes, r.deletedUserTipes, true)
        ; SetUnlockedDBs r.unlockedDBs
        ; UpdateWorkerSchedules r.worker_schedules
        ; SetPermission r.permission
        ; Append404s r.fofs
        ; AppendStaticDeploy r.staticDeploys
        ; AutocompleteMod ACReset
        ; ClearError
        ; extraMod
        ; newState
        ; MakeCmd (API.fetchAllTraces m)
        ; InitIntrospect (TD.values allTLs)
        ; InitASTCache (r.handlers, r.userFunctions) ]
  | SaveTestAPICallback (Ok msg_) ->
      DisplayError ("Success! " ^ msg_)
  | ExecuteFunctionAPICallback
      (params, Ok (dval, hash, hashVersion, tlids, unlockedDBs)) ->
      let traces =
        List.map
          ~f:(fun tlid -> (deTLID tlid, [(params.efpTraceID, Error NoneYet)]))
          tlids
      in
      Many
        [ UpdateTraceFunctionResult
            ( params.efpTLID
            , params.efpTraceID
            , params.efpCallerID
            , params.efpFnName
            , hash
            , hashVersion
            , dval )
        ; ExecutingFunctionComplete [(params.efpTLID, params.efpCallerID)]
        ; OverrideTraces (StrDict.fromList traces)
        ; SetUnlockedDBs unlockedDBs ]
  | TriggerHandlerAPICallback (params, Ok tlids) ->
      let (traces : Prelude.traces) =
        List.map
          ~f:(fun tlid -> (deTLID tlid, [(params.thTraceID, Error NoneYet)]))
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
              {m with handlerProps}) ]
  | GetUnlockedDBsAPICallback (Ok unlockedDBs) ->
      Many
        [ TweakModel (Sync.markResponseInModel ~key:"unlocked")
        ; SetUnlockedDBs unlockedDBs ]
  | LoadPackagesAPICallback (Ok loadedPackages) ->
      TweakModel
        (fun m ->
          { m with
            packageFns = PackageManager.loadPackages m.packageFns loadedPackages
          })
  | NewTracePush (traceID, tlids) ->
      let traces =
        List.map
          ~f:(fun tlid -> (deTLID tlid, [(traceID, Error NoneYet)]))
          tlids
      in
      UpdateTraces (StrDict.fromList traces)
  | New404Push f404 ->
      Append404s [f404]
  | NewPresencePush avatarsList ->
      UpdateAvatarList avatarsList
  | NewStaticDeployPush asset ->
      AppendStaticDeploy [asset]
  | WorkerStatePush ws ->
      UpdateWorkerSchedules ws
  | Delete404APICall f404 ->
      Many
        [ (* This deletion is speculative *)
          Delete404 f404
        ; MakeCmd (API.delete404 m f404) ]
  | Delete404APICallback (params, result) ->
    ( match result with
    | Ok _ ->
        NoChange
    | Error err ->
        Many
          [ Append404s [params] (* Rollback the speculative deletion *)
          ; HandleAPIError
              (APIError.make
                 ~context:"Delete404"
                 ~importance:ImportantError
                 ~requestParams:(Encoders.fof params)
                 ~reload:false
                 err) ] )
  | ReceiveAnalysis result ->
    ( match result with
    | Ok (id, analysisResults) ->
        UpdateAnalysis (id, analysisResults)
    | Error (AnalysisExecutionError (_, str)) ->
        DisplayError str
    | Error (AnalysisParseError str) ->
        DisplayError str )
  | ReceiveFetch (TraceFetchFailure (params, _, "Bad credentials")) ->
      HandleAPIError
        (APIError.make
           ~context:"TraceFetch"
           ~importance:ImportantError
           ~reload:true
           ~requestParams:(Encoders.getTraceDataAPIParams params)
           (* not a great error ... but this is an api error without a
            * corresponding actual http error *)
           Tea.Http.Aborted)
  | ReceiveFetch (TraceFetchFailure (params, url, error))
    when error
         = "Selected trace too large for the editor to load, maybe try another?"
    ->
      let traces =
        StrDict.fromList
          [ ( deTLID params.gtdrpTlid
            , [(params.gtdrpTraceID, Error MaximumCallStackError)] ) ]
      in
      Many
        [ TweakModel
            (Sync.markResponseInModel
               ~key:("tracefetch-" ^ params.gtdrpTraceID))
        ; UpdateTraces traces
        ; DisplayAndReportError ("Error fetching trace", Some url, Some error)
        ]
  | ReceiveFetch (TraceFetchFailure (params, url, error)) ->
      Many
        [ TweakModel
            (Sync.markResponseInModel
               ~key:("tracefetch-" ^ params.gtdrpTraceID))
        ; DisplayAndReportError ("Error fetching trace", Some url, Some error)
        ]
  | ReceiveFetch (TraceFetchSuccess (params, result)) ->
      let traces =
        StrDict.fromList [(deTLID params.gtdrpTlid, [result.trace])]
      in
      Many
        [ TweakModel
            (Sync.markResponseInModel
               ~key:("tracefetch-" ^ params.gtdrpTraceID))
        ; UpdateTraces traces ]
  | ReceiveFetch (TraceFetchMissing params) ->
      (* We'll force it so no need to update syncState *)
      let _, cmd =
        Analysis.requestTrace ~force:true m params.gtdrpTlid params.gtdrpTraceID
      in
      MakeCmd cmd
  | ReceiveFetch (DbStatsFetchFailure (params, _, "Bad credentials")) ->
      HandleAPIError
        (APIError.make
           ~context:"DbStatsFetch"
           ~importance:ImportantError
           ~reload:true
           ~requestParams:(Encoders.dbStatsAPIParams params)
           (* not a great error ... but this is an api error without a
            * corresponding actual http error *)
           Tea.Http.Aborted)
  | ReceiveFetch (DbStatsFetchFailure (params, url, error)) ->
      let key =
        params.dbStatsTlids |> List.map ~f:deTLID |> String.join ~sep:","
      in
      Many
        [ TweakModel (Sync.markResponseInModel ~key:("update-db-stats-" ^ key))
        ; DisplayAndReportError ("Error fetching db stats", Some url, Some error)
        ]
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
  | ReceiveFetch (WorkerStatsFetchFailure (params, _, "Bad credentials")) ->
      HandleAPIError
        (APIError.make
           ~context:"WorkerStatsFetch"
           ~importance:ImportantError
           ~reload:true
           ~requestParams:(Encoders.workerStatsAPIParams params)
           (* not a great error ... but this is an api error without a
              * corresponding actual http error *)
           Tea.Http.Aborted)
  | ReceiveFetch (WorkerStatsFetchFailure (params, url, error)) ->
      Many
        [ TweakModel
            (Sync.markResponseInModel
               ~key:("get-worker-stats-" ^ deTLID params.workerStatsTlid))
        ; DisplayAndReportError ("Error fetching db stats", Some url, Some error)
        ]
  | ReceiveFetch (WorkerStatsFetchMissing params) ->
      TweakModel
        (Sync.markResponseInModel
           ~key:("get-worker-stats-" ^ deTLID params.workerStatsTlid))
  | ReceiveFetch (WorkerStatsFetchSuccess (params, result)) ->
      Many
        [ TweakModel
            (Sync.markResponseInModel
               ~key:("get-worker-stats-" ^ deTLID params.workerStatsTlid))
        ; UpdateWorkerStats (params.workerStatsTlid, result) ]
  | AddOpsAPICallback (_, params, Error err) ->
      HandleAPIError
        (APIError.make
           ~context:"AddOps"
           ~importance:ImportantError
           ~requestParams:(Encoders.addOpAPIParams params)
           ~reload:false
           err)
  | SaveTestAPICallback (Error err) ->
      DisplayError ("Error: " ^ Tea_http.string_of_error err)
  | ExecuteFunctionAPICallback (params, Error err) ->
      HandleAPIError
        (APIError.make
           ~context:"ExecuteFunction"
           ~importance:ImportantError
           ~requestParams:(Encoders.executeFunctionAPIParams params)
           ~reload:false
           err)
  | TriggerHandlerAPICallback (_, Error err) ->
      HandleAPIError
        (APIError.make
           ~context:"TriggerHandler"
           ~importance:ImportantError
           err
           ~reload:false)
  | InitialLoadAPICallback (_, _, Error err) ->
      HandleAPIError
        (APIError.make
           ~context:"InitialLoad"
           ~importance:ImportantError
           err
           ~reload:false)
  | GetUnlockedDBsAPICallback (Error err) ->
      Many
        [ TweakModel (Sync.markResponseInModel ~key:"unlocked")
        ; HandleAPIError
            (APIError.make
               ~context:"GetUnlockedDBs"
               ~importance:IgnorableError
               ~reload:false
               err) ]
  | LoadPackagesAPICallback (Error err) ->
      HandleAPIError
        (APIError.make
           ~context:"LoadPackages"
           ~importance:ImportantError
           ~reload:false
           err)
  | JSError msg_ ->
      DisplayError ("Error in JS: " ^ msg_)
  | LocationChange loc ->
      Url.changeLocation loc
  | TimerFire (action, _) ->
    ( match action with
    | RefreshAnalysis ->
        let getUnlockedDBs =
          (* Small optimization *)
          if StrDict.count m.dbs > 0 then GetUnlockedDBsAPICall else NoChange
        in
        ( match Toplevel.selected m with
        | Some tl when Toplevel.isDB tl ->
            Many [UpdateDBStatsAPICall (TL.id tl); getUnlockedDBs]
        | Some tl when Toplevel.isWorkerHandler tl ->
            Many [GetWorkerStatsAPICall (TL.id tl); getUnlockedDBs]
        | _ ->
            getUnlockedDBs )
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
  | CreateHandlerFrom404 ({space; path; modifier; _} as fof) ->
      let center = Viewport.findNewPos m in
      let tlid = gtlid () in
      let pos = center in
      let ast = FluidExpression.EBlank (gid ()) in
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
            then (search.traceID, Error NoneYet) :: acc
            else acc)
          m.f404s
      in
      let traceMods =
        match List.head traces with
        | Some (first, _) ->
            let traceDict = StrDict.fromList [(deTLID tlid, traces)] in
            [UpdateTraces traceDict; SetTLTraceID (tlid, first)]
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
        @ [ AddOps
              ( [SetHandler (tlid, pos, aHandler)]
              , FocusExact (tlid, FluidExpression.toID ast) )
          ; Delete404 fof ] )
  | MarkRoutingTableOpen (shouldOpen, key) ->
      TweakModel
        (fun m ->
          { m with
            routingTableOpenDetails =
              ( if shouldOpen
              then StrSet.add ~value:key m.routingTableOpenDetails
              else StrSet.remove ~value:key m.routingTableOpenDetails ) })
  | ToggleSideBar ->
      TweakModel (fun m -> {m with sidebarOpen = not m.sidebarOpen})
  | CreateRouteHandler action ->
      let center = Viewport.findNewPos m in
      Entry.submitOmniAction m center action
  | CreateDBTable ->
      let center = Viewport.findNewPos m
      and genName = DB.generateDBName () in
      Entry.newDB genName center
  | CreateGroup ->
      let center = Viewport.findNewPos m in
      Groups.createEmptyGroup None center
  | CreateFunction ->
      let ufun = Refactor.generateEmptyFunction () in
      Many
        [ AddOps ([SetFunction ufun], FocusNothing)
        ; MakeCmd (Url.navigateTo (FocusedFn ufun.ufTLID)) ]
  | CreateType ->
      let tipe = Refactor.generateEmptyUserType () in
      Many
        [ AddOps ([SetType tipe], FocusNothing)
        ; MakeCmd (Url.navigateTo (FocusedType tipe.utTLID)) ]
  | LockHandler (tlid, locked) ->
      TweakModel (Handlers.setHandlerLock tlid locked)
  | EnablePanning pan ->
      TweakModel
        (fun m -> {m with canvasProps = {m.canvasProps with enablePan = pan}})
  | ClipboardCopyEvent e ->
      let toast =
        TweakModel
          (fun m ->
            {m with toast = {m.toast with toastMessage = Some "Copied!"}})
      in
      let clipboardData = Fluid.getCopySelection m in
      Many [SetClipboardContents (clipboardData, e); toast]
  | ClipboardPasteEvent e ->
      let data = Clipboard.getData e in
      Fluid.update m (FluidPaste data)
  | ClipboardCutEvent e ->
      let toast =
        TweakModel
          (fun m ->
            {m with toast = {m.toast with toastMessage = Some "Copied!"}})
      in
      let copyData, mod_ =
        (Fluid.getCopySelection m, Apply (fun m -> Fluid.update m FluidCut))
      in
      Many [SetClipboardContents (copyData, e); mod_; toast]
  | ClipboardCopyLivevalue (lv, pos) ->
      Native.Clipboard.copyToClipboard lv ;
      TweakModel
        (fun m ->
          {m with toast = {toastMessage = Some "Copied!"; toastPos = Some pos}})
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
      TweakModel (Handlers.setHandlerState tlid state)
  | CanvasPanAnimationEnd ->
      TweakModel
        (fun m ->
          { m with
            canvasProps =
              {m.canvasProps with panAnimation = DontAnimateTransition} })
  | GoTo page ->
      MakeCmd (Url.navigateTo page)
  | SetHoveringReferences (tlid, ids) ->
      Introspect.setHoveringReferences tlid ids
  | TriggerSendPresenceCallback (Ok _) ->
      NoChange
  | TriggerSendPresenceCallback (Error err) ->
      HandleAPIError
        (APIError.make
           ~context:"TriggerSendPresenceCallback"
           ~importance:IgnorableError
           ~reload:false
           err)
  | FluidMsg FluidCut | FluidMsg (FluidPaste _) ->
      recover "Fluid functions should not happen here" ~debug:msg NoChange
  | FluidMsg (FluidCommandsFilter query) ->
      TweakModel
        (fun m ->
          let cp = FluidCommands.filter m query m.fluidState.cp in
          {m with fluidState = {m.fluidState with cp}})
  | FluidMsg (FluidCommandsClick cmd) ->
      Many [FluidCommands.runCommand m cmd; FluidCommandsClose]
  | TakeOffErrorRail (tlid, id) ->
    ( match TL.get m tlid with
    | Some tl ->
        Refactor.takeOffRail m tl id
    | _ ->
        NoChange )
  | UploadFn tlid ->
      TL.get m tlid
      |> Option.andThen ~f:TL.asUserFunction
      |> Option.map ~f:(fun uplFn -> API.uploadFn m {uplFn})
      |> Option.map ~f:(fun cmd -> MakeCmd cmd)
      |> Option.withDefault ~default:(DisplayError "No function to upload")
  | SetHandlerExeIdle tlid ->
      TweakModel
        (fun m ->
          let handlerProps = RT.setHandlerExeState tlid Idle m.handlerProps in
          {m with handlerProps})
  | CopyCurl (tlid, pos) ->
      CurlCommand.copyCurlMod m tlid pos
  | TLMenuMsg (tlid, msg) ->
      TLMenuUpdate (tlid, msg)
  | FluidMsg (FluidMouseDown targetExnID) ->
      let defaultBehaviour =
        [FluidStartClick; Select (targetExnID, STTopLevelRoot)]
      in
      ( match m.cursorState with
      | Entering (Filling _ as cursor) ->
          Many
            (* If we click away from an entry box, commit it before doing the default behaviour *)
            (Entry.commit m cursor :: defaultBehaviour)
      | _ ->
          Many defaultBehaviour )
  | FluidMsg (FluidMouseUp {tlid = targetExnID; _} as msg) ->
      Many
        [ Select (targetExnID, STTopLevelRoot)
        ; Apply (fun m -> Fluid.update m msg) ]
  | FluidMsg msg ->
      (* Handle all other messages *)
      Fluid.update m msg
  | ResetToast ->
      TweakModel (fun m -> {m with toast = Defaults.defaultToast})
  | UpdateMinimap data ->
      TweakModel
        (fun m -> {m with canvasProps = {m.canvasProps with minimap = data}})
  | HideTopbar ->
      TweakModel (fun m -> {m with showTopbar = false})
  | LogoutOfDark ->
      Many
        [ MakeCmd (API.logout m)
        ; TweakModel
            (fun m ->
              {m with editorSettings = {m.editorSettings with runTimers = false}})
        ]
  | LogoutAPICallback ->
      (* For some reason the Tea.Navigation.modifyUrl and .newUrl doesn't work *)
      Native.Ext.redirect "/login" ;
      NoChange
  | GoToArchitecturalView ->
      Many
        [ TweakModel
            (fun m ->
              {m with canvasProps = {m.canvasProps with minimap = None}})
        ; Deselect
        ; MakeCmd (Url.navigateTo Architecture) ]
  | DismissErrorBar ->
      ClearError
  | PauseWorker workerName ->
      MakeCmd (API.updateWorkerSchedule m {workerName; schedule = "pause"})
  | RunWorker workerName ->
      MakeCmd (API.updateWorkerSchedule m {workerName; schedule = "run"})
  | UpdateWorkerScheduleCallback (Ok schedules) ->
      UpdateWorkerSchedules schedules
  | UpdateWorkerScheduleCallback (Error _) ->
      DisplayError "Failed to update worker schedule"
  | NewTabFromTLMenu (url, tlid) ->
      Native.Window.openUrl url "_blank" ;
      TLMenuUpdate (tlid, CloseMenu)
  | SettingsViewMsg (TriggerSendInviteCallback (Error err) as msg) ->
      Many
        [ SettingsViewUpdate msg
        ; HandleAPIError
            (APIError.make
               ~context:"TriggerSendInviteCallback"
               ~importance:IgnorableError
               ~reload:false
               err) ]
  | SettingsViewMsg msg ->
      SettingsViewUpdate msg
  | FnParamMsg msg ->
      FnParams.update m msg
  | UploadFnAPICallback (_, Error err) ->
      HandleAPIError
        (APIError.make
           ~context:"UploadFnAPICallback"
           ~importance:IgnorableError
           ~reload:false
           err)
  | UploadFnAPICallback (_, Ok _) ->
      DisplayError "Successfully uploaded function"


let rec filter_read_only (m : model) (modification : modification) =
  if m.permission = Some ReadWrite
  then modification
  else
    match modification with
    | Enter _ | EnterWithOffset _ | AddOps _ ->
        NoChange
    | Many ms ->
        Many (List.map ~f:(filter_read_only m) ms)
    | _ ->
        modification


let update (m : model) (msg : msg) : model * msg Cmd.t =
  let mods = update_ msg m |> filter_read_only m in
  let newm, newc = updateMod mods (m, Cmd.none) in
  SavedSettings.save m ;
  SavedUserSettings.save m ;
  ({newm with lastMsg = msg; lastMod = mods}, newc)


let subscriptions (m : model) : msg Tea.Sub.t =
  let keySubs = [Keyboard.downs (fun x -> GlobalKeyPress x)] in
  let dragSubs =
    match m.cursorState with
    (* we use IDs here because the node will change *)
    (* before they're triggered *)
    | DraggingTL (id, _, _, _) ->
        let listenerKey = "mouse_moves_" ^ deTLID id in
        [ BrowserListeners.DarkMouse.moves ~key:listenerKey (fun event ->
              DragToplevel (id, event)) ]
    | PanningCanvas _ ->
        let listenerKey = "mouse_drag" in
        [ BrowserListeners.DarkMouse.moves ~key:listenerKey (fun event ->
              AppMouseDrag event) ]
    | _ ->
        []
  in
  let windowMouseSubs =
    [ BrowserListeners.Window.Mouse.ups ~key:"win_mouse_up" (fun event ->
          WindowMouseUp event) ]
  in
  let timers =
    if m.editorSettings.runTimers
    then
      match m.visibility with
      | Hidden ->
          []
      | Visible ->
          [ Tea.Time.every ~key:"refresh_analysis" Tea.Time.second (fun f ->
                TimerFire (RefreshAnalysis, f)) ]
          @ [ Tea.Time.every ~key:"refresh_avatars" Tea.Time.second (fun f ->
                  TimerFire (RefreshAvatars, f)) ]
    else []
  in
  let onError =
    [ BrowserListeners.DisplayClientError.listen
        ~key:"display_client_error"
        (fun s -> JSError s) ]
  in
  let visibility =
    [ BrowserListeners.Window.OnFocusChange.listen
        ~key:"window_on_focus_change"
        (fun v ->
          if v
          then PageVisibilityChange Visible
          else PageVisibilityChange Hidden) ]
  in
  let mousewheelSubs =
    if m.canvasProps.enablePan && not (isACOpened m)
    then
      [ BrowserListeners.OnWheel.listen ~key:"on_wheel" (fun (dx, dy) ->
            MouseWheel (dx, dy)) ]
    else []
  in
  let analysisSubs =
    [ Analysis.ReceiveAnalysis.listen ~key:"receive_analysis" (fun s ->
          ReceiveAnalysis s)
    ; Analysis.NewTracePush.listen ~key:"new_trace_push" (fun s ->
          NewTracePush s)
    ; Analysis.New404Push.listen ~key:"new_404_push" (fun s -> New404Push s)
    ; DarkStorage.NewStaticDeployPush.listen ~key:"new_static_deploy" (fun s ->
          NewStaticDeployPush s)
    ; Analysis.ReceiveFetch.listen ~key:"receive_fetch" (fun s ->
          ReceiveFetch s)
    ; Analysis.NewPresencePush.listen ~key:"new_presence_push" (fun s ->
          NewPresencePush s)
    ; Analysis.AddOps.listen ~key:"add_op" (fun s -> AddOpsStrollerMsg s)
    ; Analysis.WorkerStatePush.listen ~key:"worker_state_push" (fun s ->
          WorkerStatePush s) ]
  in
  let clipboardSubs =
    [ BrowserListeners.Clipboard.copyListener ~key:"copy_event" (fun e ->
          ClipboardCopyEvent e)
    ; BrowserListeners.Clipboard.cutListener ~key:"cut_event" (fun e ->
          ClipboardCutEvent e)
    ; BrowserListeners.Clipboard.pasteListener ~key:"paste_event" (fun e ->
          e##preventDefault () ;
          ClipboardPasteEvent e) ]
  in
  let onCaptureView =
    [ BrowserListeners.OnCaptureView.listen ~key:"capture_view" (fun s ->
          UpdateMinimap (Some s)) ]
  in
  Tea.Sub.batch
    (List.concat
       [ windowMouseSubs
       ; keySubs
       ; clipboardSubs
       ; dragSubs
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
