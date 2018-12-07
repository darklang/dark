open! Porting
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


let rpc (c : rpcContext) (focus : focus) (params : rpcParams) : msg Tea.Cmd.t =
  let url = String.concat ["/api/"; Tea.Http.encodeUri c.canvasName; "/rpc"] in
  let request =
    postJson Decoders.rpc c.csrfToken url (Encoders.rpcParams params)
  in
  Tea.Http.send (fun x -> RPCCallback (focus, params, x)) request


let executeFunctionRPC (c : rpcContext) (params : executeFunctionRPCParams) :
    msg Tea.Cmd.t =
  let url =
    String.concat
      ["/api/"; Tea.Http.encodeUri c.canvasName; "/execute_function"]
  in
  let request =
    postJson
      Decoders.executeFunctionRPC
      c.csrfToken
      url
      (Encoders.executeFunctionRPCParams params)
  in
  Tea.Http.send (fun x -> ExecuteFunctionRPCCallback (params, x)) request


let getAnalysisRPC (c : rpcContext) (params : analysisParams) : msg Tea.Cmd.t =
  let url = "/api/" ^ Tea.Http.encodeUri c.canvasName ^ "/get_analysis" in
  let request =
    postJson
      Decoders.getAnalysisRPC
      c.csrfToken
      url
      (Encoders.analysisParams params)
  in
  Tea.Http.send (fun x -> GetAnalysisRPCCallback (params, x)) request


let delete404RPC (c : rpcContext) (param : delete404Param) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri c.canvasName; "/delete_404"]
  in
  let request =
    postJson
      (Json_decode_extended.pair
         (Json_decode_extended.list Decoders.fof)
         Json_decode_extended.string)
      c.csrfToken
      url
      (Encoders.fof param)
  in
  Tea.Http.send (fun x -> GetDelete404RPCCallback x) request


let initialLoadRPC (c : rpcContext) (focus : focus) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri c.canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPC c.csrfToken url in
  Tea.Http.send (fun x -> InitialLoadRPCCallback (focus, NoChange, x)) request


let saveTestRPC (c : rpcContext) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri c.canvasName; "/save_test"]
  in
  let request = postEmptyString Json_decode_extended.string c.csrfToken url in
  Tea.Http.send (fun x -> SaveTestRPCCallback x) request


let opsParams (ops : op list) : rpcParams = {ops}

let integrationRPC (c : rpcContext) (name : string) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri c.canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPC c.csrfToken url in
  Tea.Http.send
    (fun x ->
      InitialLoadRPCCallback (FocusNothing, TriggerIntegrationTest name, x) )
    request
