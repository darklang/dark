open Prelude

let apiRoot = "/api/"

let clientVersionHeader = (m): Tea_http.header => Header("x-darklang-client-version", m.buildHash)

let apiCallNoParams = (
  m: model,
  ~decoder: Js.Json.t => 'resulttype,
  ~callback: Tea.Result.t<'resulttype, Tea.Http.error<string>> => msg,
  endpoint: string,
): Tea.Cmd.t<msg> => {
  let url = apiRoot ++ Tea.Http.encodeUri(m.canvasName) ++ endpoint
  let request = Tea.Http.request({
    method': "POST",
    headers: list{
      Header("Content-type", "application/json"),
      Header("X-CSRF-Token", m.csrfToken),
      clientVersionHeader(m),
    },
    url: url,
    body: Web.XMLHttpRequest.EmptyBody,
    expect: Tea.Http.expectStringResponse(Decoders.wrapExpect(decoder)),
    timeout: None,
    withCredentials: false,
  })

  Tea.Http.send(callback, request)
}

let postJson = (
  decoder,
  ~withCredentials=false,
  ~headers=list{},
  csrfToken: string,
  url: string,
  body: Js.Json.t,
) =>
  Tea.Http.request({
    method': "POST",
    headers: list{
      Header("Content-type", "application/json"),
      Header("X-CSRF-Token", csrfToken),
      ...headers,
    },
    url: url,
    body: Web.XMLHttpRequest.StringBody(Json.stringify(body)),
    expect: Tea.Http.expectStringResponse(Decoders.wrapExpect(decoder)),
    timeout: None,
    withCredentials: withCredentials,
  })

let apiCall = (
  m: model,
  ~encoder: 'paramtype => Js.Json.t,
  ~params: 'paramtype,
  ~decoder: Js.Json.t => 'resulttype,
  ~callback: Tea.Result.t<'resulttype, Tea.Http.error<string>> => msg,
  endpoint: string,
): Tea.Cmd.t<msg> => {
  let url = apiRoot ++ Tea.Http.encodeUri(m.canvasName) ++ endpoint
  let request = postJson(
    ~headers=list{clientVersionHeader(m)},
    decoder,
    m.csrfToken,
    url,
    encoder(params),
  )

  Tea.Http.send(callback, request)
}

let opsParams = (ops: list<op>, opCtr: int, clientOpCtrId: string): addOpAPIParams => {
  ops: ops,
  opCtr: opCtr,
  clientOpCtrId: clientOpCtrId,
}

// -------------
// API calls
// -------------

let addOp = (m: model, focus: focus, params: addOpAPIParams): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/add_op",
    ~decoder=Decoders.addOpAPI,
    ~encoder=Encoders.addOpAPIParams,
    ~params,
    ~callback=x => AddOpsAPICallback(focus, params, x),
  )

let executeFunction = (m: model, params: executeFunctionAPIParams): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/execute_function",
    ~decoder=Decoders.executeFunctionAPIResult,
    ~encoder=Encoders.executeFunctionAPIParams,
    ~params,
    ~callback=x => ExecuteFunctionAPICallback(params, x),
  )

let uploadFn = (m: model, params: uploadFnAPIParams): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/packages/upload_function",
    ~decoder=Decoders.uploadFnAPIResult,
    ~encoder=Encoders.uploadFnAPIParams,
    ~params,
    ~callback=x => UploadFnAPICallback(params, x),
  )

let loadPackages = (m: model): Tea.Cmd.t<msg> =>
  apiCallNoParams(
    m,
    "/packages",
    ~decoder=Decoders.loadPackagesAPIResult,
    ~callback=x => LoadPackagesAPICallback(x),
  )

let triggerHandler = (m: model, params: triggerHandlerAPIParams): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/trigger_handler",
    ~decoder=Decoders.triggerHandlerAPIResult,
    ~encoder=Encoders.triggerHandlerAPIParams,
    ~params,
    ~callback=x => TriggerHandlerAPICallback(params, x),
  )

let getUnlockedDBs = (m: model): Tea.Cmd.t<msg> =>
  apiCallNoParams(
    m,
    "/get_unlocked_dbs",
    ~decoder=Decoders.getUnlockedDBsAPIResult,
    ~callback=x => GetUnlockedDBsAPICallback(x),
  )

let updateWorkerSchedule = (m: model, params: updateWorkerScheduleAPIParams): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/worker_schedule",
    ~decoder=Decoders.updateWorkerScheduleAPIResult,
    ~encoder=Encoders.updateWorkerScheduleAPIParams,
    ~params,
    ~callback=x => UpdateWorkerScheduleCallback(x),
  )

let get404s = (m: model): Tea.Cmd.t<msg> =>
  apiCallNoParams(
    m,
    "/get_404s",
    ~decoder=Decoders.get404sAPIResult,
    ~callback=x => Get404sAPICallback(x),
  )

let delete404 = (m: model, params: delete404APIParams): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/delete_404",
    ~decoder=_ => (),
    ~encoder=Encoders.fof,
    ~params,
    ~callback=x => Delete404APICallback(params, x),
  )

let deleteToplevelForever = (m: model, params: deleteToplevelForeverAPIParams): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/delete-toplevel-forever",
    ~decoder=_ => (),
    ~encoder=Encoders.deleteToplevelForeverAPIParams,
    ~params,
    ~callback=x => DeleteToplevelForeverAPICallback(params, x),
  )

let insertSecret = (m: model, params: SecretTypes.t): Tea.Cmd.t<msg> =>
  apiCall(
    m,
    "/insert_secret",
    ~encoder=Encoders.secret,
    ~decoder=Decoders.insertSecretResult,
    ~params,
    ~callback=x => InsertSecretCallback(x),
  )

let initialLoad = (m: model, focus: focus): Tea.Cmd.t<msg> =>
  apiCallNoParams(
    m,
    "/initial_load",
    ~decoder=Decoders.initialLoadAPIResult,
    ~callback=x => InitialLoadAPICallback(focus, NoChange, x),
  )

let fetchAllTraces = (m: model): Tea.Cmd.t<msg> =>
  apiCallNoParams(
    m,
    "/all_traces",
    ~decoder=Decoders.allTracesResult,
    ~callback=x => FetchAllTracesAPICallback(x),
  )

let logout = (m: model): Tea.Cmd.t<msg> =>
  apiCallNoParams(m, "/logout", ~decoder=_ => (), ~callback=_ => LogoutAPICallback)

let saveTest = (m: model): Tea.Cmd.t<msg> =>
  apiCallNoParams(
    m,
    "/save_test",
    ~decoder=Decoders.saveTestAPIResult,
    ~callback=x => SaveTestAPICallback(x),
  )

let integration = (m: model, name: string): Tea.Cmd.t<msg> =>
  apiCallNoParams(
    m,
    "/initial_load",
    ~decoder=Decoders.initialLoadAPIResult,
    ~callback=x => InitialLoadAPICallback(FocusNothing, TriggerIntegrationTest(name), x),
  )

let sendPresence = (m: model, av: avatarModelMessage): Tea.Cmd.t<msg> => {
  let url = "https://presence.darklang.com/presence"
  let request = postJson(
    ~headers=list{clientVersionHeader(m)},
    ~withCredentials=true,
    _ => (),
    m.csrfToken,
    url,
    Encoders.sendPresenceParams(av),
  )

  /* If origin is https://darklang.com, then we're in prod (or ngrok, running
   * against prod) and
   * presence.darklang.com's CORS rules will allow this request. If not, we're
   * in local, and both CORS and auth (session, canvas_id) will not work against
   * presence.darklang.com. By putting the conditional here instead of at the
   * beginning of the function, we still exercise the message and request
   * generating code locally. */
  if m.origin == "https://darklang.com" {
    Tea.Http.send(x => TriggerSendPresenceCallback(x), request)
  } else {
    Tea.Cmd.none
  }
}

let sendInvite = (m: model, invite: SettingsViewTypes.inviteFormMessage): Tea.Cmd.t<msg> => {
  let url = "https://accounts.darklang.com/send-invite"
  let request = postJson(
    ~headers=list{clientVersionHeader(m)},
    ~withCredentials=true,
    _ => (),
    m.csrfToken,
    url,
    Encoders.sendInviteParams(invite),
  )

  /* If origin is https://darklang.com, then we're in prod (or ngrok, running
   * against prod) and
   * ops-adduser.darklang.com's CORS rules will allow this request. If not, we're
   * in local, and both CORS and auth (session, canvas_id) will not work against
   * ops-adduser.darklang.com. By putting the conditional here instead of at the
   * beginning of the function, we still exercise the message and request
   * generating code locally. */
  if m.origin == "https://darklang.com" {
    Tea.Http.send(x => SettingsViewMsg(TriggerSendInviteCallback(x)), request)
  } else {
    Tea.Cmd.none
  }
}

/* We do some dropping of ops based on clientOpCtrId+opCtr to preserve ordering.
 * (opCtr is per-clientOpCtrId, and inc'd client-side; thus we know, when processing
 * a set of ops whether this is the latest seen so far from a given client, or
 * has come in out of order.) This is initially done server-side, to guard
 * against ops being processed there out of order; but we also need to do this
 * client-side, since messages coming in from Pusher are not
 * guaranteed to be delivered in order.
 *
 * Ordering is determined by model.opCtrs, and we return a model so we can also
 * update the opCtrs map.
 * */
let filterOpsAndResult = (m: model, params: addOpAPIParams, result: option<addOpAPIResult>): (
  model,
  list<op>,
  option<addOpAPIResult>,
) => {
  let newOpCtrs = /* if the opCtr in params is greater than the one in the map, we'll create
   * an updated map */
  Map.update(m.opCtrs, ~key=params.clientOpCtrId, ~f=oldCtr =>
    switch (oldCtr, params.opCtr) {
    | (Some(oldCtr), paramsOpCtr) => Some(max(oldCtr, paramsOpCtr))
    | _ => Some(params.opCtr)
    }
  )

  let m2 = {...m, opCtrs: newOpCtrs}
  /* if the new opCtrs map was updated by params.opCtr, then this msg was the
   * latest; otherwise, we need to filter out some ops from params */
  // temporarily _don't_ filter ops
  if Map.get(~key=params.clientOpCtrId, m2.opCtrs) == Some(params.opCtr) {
    (m2, params.ops, result)
  } else {
    /* filter down to only those ops which can be applied out of order without
     * overwriting previous ops' state - eg, if we have SetHandler1 setting a
     * handler's value to "aaa", and then SetHandler2's value is "aa",
     * applying them out of order (SH2, SH1) will result in SH2's update being
     * overwritten */
    // NOTE: DO NOT UPDATE WITHOUT UPDATING THE SERVER-SIDE LIST
    let filter_ops_received_out_of_order = List.filter(~f=op =>
      switch op {
      | SetHandler(_)
      | SetFunction(_)
      | SetType(_)
      | MoveTL(_)
      | SetDBColName(_)
      | ChangeDBColName(_)
      | ChangeDBColType(_)
      | SetExpr(_)
      | UndoTL(_)
      | RedoTL(_)
      | TLSavepoint(_)
      | RenameDBname(_) => false
      | CreateDB(_)
      | AddDBCol(_)
      | SetDBColType(_)
      | DeleteTL(_)
      | DeleteFunction(_)
      | DeleteDBCol(_)
      | CreateDBWithBlankOr(_)
      | DeleteType(_) => true
      }
    )

    let ops = params.ops |> filter_ops_received_out_of_order
    let opTlids = ops |> List.map(~f=op => Encoders.tlidOf(op))
    // We also want to ignore the result of ops we ignored
    let result = Option.map(result, ~f=(result: addOpAPIResult) => {
      ...result,
      handlers: result.handlers |> List.filter(~f=h => List.member(~value=h.hTLID, opTlids)),
      userFunctions: result.userFunctions |> List.filter(~f=(uf: PT.UserFunction.t) =>
        List.member(~value=uf.tlid, opTlids)
      ),
      userTipes: result.userTipes |> List.filter(~f=(ut: PT.UserType.t) =>
        List.member(~value=ut.tlid, opTlids)
      ),
    })

    (m2, ops, result)
  }
}
