open Tc
open Types

let postJson decoder (csrfToken : string) (url : string) (body : Js.Json.t) =
  Tea.Http.request
    { method' = "POST"
    ; headers =
        [ Header ("Content-type", "application/json")
        ; Header ("X-CSRF-Token", csrfToken) ]
    ; url
    ; body = Web.XMLHttpRequest.StringBody (Json.stringify body)
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials = false }


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


let opsParams (ops : op list) : addOpRPCParams = {ops}

let addOp (m : model) (focus : focus) (params : addOpRPCParams) : msg Tea.Cmd.t
    =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/add_op"]
  in
  let request =
    postJson
      Decoders.addOpRPCResult
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


let triggerCron (m : model) (params : triggerCronRPCParams) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/trigger_handler"]
  in
  let request =
    postJson
      Decoders.triggerCronRPCResult
      m.csrfToken
      url
      (Encoders.triggerCronRPCParams params)
  in
  Tea.Http.send (fun _ -> TriggerCronRPCCallback (Ok ())) request


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
