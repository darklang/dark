open Prelude

let apiCallNoParams
    (m : model)
    ~(decoder : Js.Json.t -> 'resulttype)
    ~(callback : ('resulttype, string Tea.Http.error) Tea.Result.t -> msg)
    (endpoint : string) : msg Tea.Cmd.t =
  let url = "/api/" ^ Tea.Http.encodeUri m.canvasName ^ endpoint in
  let request =
    Tea.Http.request
      { method' = "POST"
      ; headers =
          [ Header ("Content-type", "application/json")
          ; Header ("X-CSRF-Token", m.csrfToken) ]
      ; url
      ; body = Web.XMLHttpRequest.EmptyBody
      ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
      ; timeout = None
      ; withCredentials = false }
  in
  Tea.Http.send callback request


let postJson
    decoder
    ?(withCredentials = false)
    (csrfToken : string)
    (url : string)
    (body : Js.Json.t) =
  Tea.Http.request
    { method' = "POST"
    ; headers =
        [ Header ("Content-type", "application/json")
        ; Header ("X-CSRF-Token", csrfToken) ]
    ; url
    ; body = Web.XMLHttpRequest.StringBody (Json.stringify body)
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials }


let apiCall
    (m : model)
    ~(encoder : 'paramtype -> Js.Json.t)
    ~(params : 'paramtype)
    ~(decoder : Js.Json.t -> 'resulttype)
    ~(callback : ('resulttype, string Tea.Http.error) Tea.Result.t -> msg)
    (endpoint : string) : msg Tea.Cmd.t =
  let request =
    postJson
      decoder
      m.csrfToken
      ("/api/" ^ Tea.Http.encodeUri m.canvasName ^ endpoint)
      (encoder params)
  in
  Tea.Http.send callback request


let opsParams (ops : op list) (opCtr : int) (clientOpCtrId : string) :
    addOpAPIParams =
  {ops; opCtr; clientOpCtrId}


(* ------------- *)
(* API calls *)
(* ------------- *)

let addOp (m : model) (focus : focus) (params : addOpAPIParams) : msg Tea.Cmd.t
    =
  apiCall
    m
    "/add_op"
    ~decoder:Decoders.addOpAPI
    ~encoder:Encoders.addOpAPIParams
    ~params
    ~callback:(fun x -> AddOpsAPICallback (focus, params, x))


let executeFunction (m : model) (params : executeFunctionAPIParams) :
    msg Tea.Cmd.t =
  apiCall
    m
    "/execute_function"
    ~decoder:Decoders.executeFunctionAPIResult
    ~encoder:Encoders.executeFunctionAPIParams
    ~params
    ~callback:(fun x -> ExecuteFunctionAPICallback (params, x))


let uploadFn (m : model) (params : uploadFnAPIParams) : msg Tea.Cmd.t =
  apiCall
    m
    "/packages/upload_function"
    ~decoder:Decoders.uploadFnAPIResult
    ~encoder:Encoders.uploadFnAPIParams
    ~params
    ~callback:(fun x -> UploadFnAPICallback (params, x))


let loadPackages (m : model) : msg Tea.Cmd.t =
  apiCallNoParams
    m
    "/packages"
    ~decoder:Decoders.loadPackagesAPIResult
    ~callback:(fun x -> LoadPackagesAPICallback x)


let triggerHandler (m : model) (params : triggerHandlerAPIParams) :
    msg Tea.Cmd.t =
  apiCall
    m
    "/trigger_handler"
    ~decoder:Decoders.triggerHandlerAPIResult
    ~encoder:Encoders.triggerHandlerAPIParams
    ~params
    ~callback:(fun x -> TriggerHandlerAPICallback (params, x))


let getUnlockedDBs (m : model) : msg Tea.Cmd.t =
  apiCallNoParams
    m
    "/get_unlocked_dbs"
    ~decoder:Decoders.getUnlockedDBsAPIResult
    ~callback:(fun x -> GetUnlockedDBsAPICallback x)


let updateWorkerSchedule (m : model) (params : updateWorkerScheduleAPIParams) :
    msg Tea.Cmd.t =
  apiCall
    m
    "/worker_schedule"
    ~decoder:Decoders.updateWorkerScheduleAPIResult
    ~encoder:Encoders.updateWorkerScheduleAPIParams
    ~params
    ~callback:(fun x -> UpdateWorkerScheduleCallback x)


let delete404 (m : model) (params : delete404APIParams) : msg Tea.Cmd.t =
  apiCall
    m
    "/delete_404"
    ~decoder:(fun _ -> ())
    ~encoder:Encoders.fof
    ~params
    ~callback:(fun x -> Delete404APICallback (params, x))


let initialLoad (m : model) (focus : focus) : msg Tea.Cmd.t =
  apiCallNoParams
    m
    "/initial_load"
    ~decoder:Decoders.initialLoadAPIResult
    ~callback:(fun x -> InitialLoadAPICallback (focus, NoChange, x))


let fetchAllTraces (m : model) : msg Tea.Cmd.t =
  apiCallNoParams
    m
    "/all_traces"
    ~decoder:Decoders.allTracesResult
    ~callback:(fun x -> FetchAllTracesAPICallback x)


let logout (m : model) : msg Tea.Cmd.t =
  apiCallNoParams
    m
    "/logout"
    ~decoder:(fun _ -> ())
    ~callback:(fun _ -> LogoutAPICallback)


let saveTest (m : model) : msg Tea.Cmd.t =
  apiCallNoParams
    m
    "/save_test"
    ~decoder:Decoders.saveTestAPIResult
    ~callback:(fun x -> SaveTestAPICallback x)


let integration (m : model) (name : string) : msg Tea.Cmd.t =
  apiCallNoParams
    m
    "/initial_load"
    ~decoder:Decoders.initialLoadAPIResult
    ~callback:(fun x ->
      InitialLoadAPICallback (FocusNothing, TriggerIntegrationTest name, x))


let sendPresence (m : model) (av : avatarModelMessage) : msg Tea.Cmd.t =
  let url = "https://presence.darklang.com/presence" in
  let request =
    postJson
      ~withCredentials:true
      (fun _ -> ())
      m.csrfToken
      url
      (Encoders.sendPresenceParams av)
  in
  (* If origin is https://darklang.com, then we're in prod (or ngrok, running
   * against prod) and
   * presence.darklang.com's CORS rules will allow this request. If not, we're
   * in local, and both CORS and auth (session, canvas_id) will not work against
   * presence.darklang.com. By putting the conditional here instead of at the
   * beginning of the function, we still exercise the message and request
   * generating code locally. *)
  if m.origin = "https://darklang.com"
  then Tea.Http.send (fun x -> TriggerSendPresenceCallback x) request
  else Tea.Cmd.none


let sendInvite (m : model) (invite : inviteFormMessage) : msg Tea.Cmd.t =
  let url = "https://ops-adduser.builtwithdark.com/sendInvite" in
  let request =
    postJson
      ~withCredentials:true
      (fun _ -> ())
      m.csrfToken
      url
      (Encoders.sendInviteParams invite)
  in
  (* If origin is https://darklang.com, then we're in prod (or ngrok, running
    * against prod) and
    * presence.darklang.com's CORS rules will allow this request. If not, we're
    * in local, and both CORS and auth (session, canvas_id) will not work against
    * presence.darklang.com. By putting the conditional here instead of at the
    * beginning of the function, we still exercise the message and request
    * generating code locally. *)
  if m.origin = "https://darklang.com"
  then
    Tea.Http.send
      (fun x -> SettingsViewMsg (TriggerSendInviteCallback x))
      request
  else Tea.Cmd.none


(* We do some dropping of ops based on clientOpCtrId+opCtr to preserve ordering.
 * (opCtr is per-clientOpCtrId, and inc'd client-side; thus we know, when processing
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
    (m : model) (params : addOpAPIParams) (result : addOpAPIResult option) :
    model * op list * addOpAPIResult option =
  let newOpCtrs =
    (* if the opCtr in params is greater than the one in the map, we'll create
     * an updated map *)
    StrDict.update m.opCtrs ~key:params.clientOpCtrId ~f:(fun oldCtr ->
        match (oldCtr, params.opCtr) with
        | Some oldCtr, paramsOpCtr ->
            Some (max oldCtr paramsOpCtr)
        | _ ->
            Some params.opCtr)
  in
  let m2 = {m with opCtrs = newOpCtrs} in
  (* if the new opCtrs map was updated by params.opCtr, then this msg was the
   * latest; otherwise, we need to filter out some ops from params *)
  (* temporarily _don't_ filter ops *)
  if StrDict.get m2.opCtrs ~key:params.clientOpCtrId = Some params.opCtr
  then (m2, params.ops, result)
  else
    (* filter down to only those ops which can be applied out of order without
       * overwriting previous ops' state - eg, if we have SetHandler1 setting a
       * handler's value to "aaa", and then SetHandler2's value is "aa",
       * applying them out of order (SH2, SH1) will result in SH2's update being
       * overwritten *)
    (* NOTE: DO NOT UPDATE WITHOUT UPDATING THE SERVER-SIDE LIST *)
    let filter_ops_received_out_of_order =
      List.filter ~f:(fun op ->
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
          | UndoTL _
          | RedoTL _
          | TLSavepoint _
          | RenameDBname _ ->
              false
          | CreateDB _
          | AddDBCol _
          | SetDBColType _
          | DeleteTL _
          | DeprecatedInitDbm _
          | DeleteFunction _
          | AddDBColToDBMigration _
          | AbandonDBMigration _
          | DeleteColInDBMigration _
          | DeleteDBCol _
          | CreateDBWithBlankOr _
          | DeleteTLForever _
          | DeleteFunctionForever _
          | DeleteType _
          | DeleteTypeForever _ ->
              true)
    in
    let ops = params.ops |> filter_ops_received_out_of_order in
    let opTlids = ops |> List.map ~f:(fun op -> Encoders.tlidOf op) in
    (* We also want to ignore the result of ops we ignored *)
    let result =
      Option.map result ~f:(fun (result : addOpAPIResult) ->
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
          })
    in
    (m2, ops, result)
