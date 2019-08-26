open Tc
open Types

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


let postEmptyJson decoder (csrfToken : string) (url : string) =
  Tea.Http.request
    { method' = "POST"
    ; headers =
        [ Header ("Content-type", "application/json")
        ; Header ("X-CSRF-Token", csrfToken) ]
    ; url
    ; body = Web.XMLHttpRequest.EmptyBody
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials = false }


let postEmptyString decoder (csrfToken : string) (url : string) =
  Tea.Http.request
    { method' = "POST"
    ; headers = [Header ("X-CSRF-Token", csrfToken)]
    ; url
    ; body = Web.XMLHttpRequest.EmptyBody
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials = false }


let opsParams (ops : op list) (opCtr : int option) (browserId : string) :
    addOpRPCParams =
  {ops; opCtr; browserId}


let addOp (m : model) (focus : focus) (params : addOpRPCParams) : msg Tea.Cmd.t
    =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/add_op"]
  in
  let request =
    postJson
      Decoders.addOpRPCStrollerMsg
      m.csrfToken
      url
      (Encoders.addOpRPCParams params)
  in
  Tea.Http.send (fun x -> AddOpRPCCallback (focus, params, x)) request


let executeFunction (m : model) (params : executeFunctionRPCParams) :
    msg Tea.Cmd.t =
  let url =
    String.concat
      ["/api/"; Tea.Http.encodeUri m.canvasName; "/execute_function"]
  in
  let request =
    postJson
      Decoders.executeFunctionRPCResult
      m.csrfToken
      url
      (Encoders.executeFunctionRPCParams params)
  in
  Tea.Http.send (fun x -> ExecuteFunctionRPCCallback (params, x)) request


let triggerHandler (m : model) (params : triggerHandlerRPCParams) :
    msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/trigger_handler"]
  in
  let request =
    postJson
      Decoders.triggerHandlerRPCResult
      m.csrfToken
      url
      (Encoders.triggerHandlerRPCParams params)
  in
  Tea.Http.send (fun x -> TriggerHandlerRPCCallback (params, x)) request


let getUnlockedDBs (m : model) : msg Tea.Cmd.t =
  let url = "/api/" ^ Tea.Http.encodeUri m.canvasName ^ "/get_unlocked_dbs" in
  let request =
    postEmptyJson Decoders.getUnlockedDBsRPCResult m.csrfToken url
  in
  Tea.Http.send (fun x -> GetUnlockedDBsRPCCallback x) request


let delete404 (m : model) (param : delete404RPCParams) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/delete_404"]
  in
  let request = postJson (fun _ -> ()) m.csrfToken url (Encoders.fof param) in
  Tea.Http.send (fun x -> Delete404RPCCallback (param, x)) request


let initialLoad (m : model) (focus : focus) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPCResult m.csrfToken url in
  Tea.Http.send (fun x -> InitialLoadRPCCallback (focus, NoChange, x)) request


let saveTest (m : model) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/save_test"]
  in
  let request = postEmptyString Decoders.saveTestRPCResult m.csrfToken url in
  Tea.Http.send (fun x -> SaveTestRPCCallback x) request


let integration (m : model) (name : string) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPCResult m.csrfToken url in
  Tea.Http.send
    (fun x ->
      InitialLoadRPCCallback (FocusNothing, TriggerIntegrationTest name, x) )
    request


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
  (* temporarily _don't_ filter ops *)
  if true || StrDict.get m2.opCtrs ~key:params.browserId = params.opCtr
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
          | RenameDBname _ ->
              false
          | CreateDB _
          | AddDBCol _
          | SetDBColType _
          | DeleteTL _
          | DeprecatedInitDbm _
          | TLSavepoint _
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
              true )
    in
    let ops = params.ops |> filter_ops_received_out_of_order in
    let opTlids = ops |> List.map ~f:(fun op -> Encoders.tlidOf op) in
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
