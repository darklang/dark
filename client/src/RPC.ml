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


let opsParams (ops : op list) (browserId : string) : addOpRPCParams =
  let tlidsToUpdateMeta = Introspect.tlidsToUpdateMeta ops in
  let tlidsToUpdateUsage = Introspect.tlidsToUpdateUsage ops in
  {ops; browserId; tlidsToUpdateMeta; tlidsToUpdateUsage}


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
  then Tea.Http.send (fun _ -> TriggerSendPresenceCallback (Ok ())) request
  else Tea.Cmd.none
