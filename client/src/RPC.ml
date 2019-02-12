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


let rpc (m : model) (focus : focus) (params : rpcParams) : msg Tea.Cmd.t =
  let url = String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/rpc"] in
  let request =
    postJson Decoders.rpc m.csrfToken url (Encoders.rpcParams params)
  in
  Tea.Http.send (fun x -> RPCCallback (focus, params, x)) request


let executeFunctionRPC (m : model) (params : executeFunctionRPCParams) :
    msg Tea.Cmd.t =
  let url =
    String.concat
      ["/api/"; Tea.Http.encodeUri m.canvasName; "/execute_function"]
  in
  let request =
    postJson
      Decoders.executeFunctionRPC
      m.csrfToken
      url
      (Encoders.executeFunctionRPCParams params)
  in
  Tea.Http.send (fun x -> ExecuteFunctionRPCCallback (params, x)) request


let getAnalysisRPC (m : model) (params : getAnalysisParams) : msg Tea.Cmd.t =
  let url = "/api/" ^ Tea.Http.encodeUri m.canvasName ^ "/get_analysis" in
  let request =
    postJson
      Decoders.getAnalysisRPC
      m.csrfToken
      url
      (Encoders.getAnalysisParams params)
  in
  Tea.Http.send (fun x -> GetAnalysisRPCCallback (params, x)) request


let delete404RPC (m : model) (param : delete404Param) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/delete_404"]
  in
  let request =
    postJson
      (Json_decode_extended.pair
         (Json_decode_extended.list Decoders.fof)
         Json_decode_extended.string)
      m.csrfToken
      url
      (Encoders.fof param)
  in
  Tea.Http.send (fun x -> GetDelete404RPCCallback x) request


let initialLoadRPC (m : model) (focus : focus) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPC m.csrfToken url in
  Tea.Http.send (fun x -> InitialLoadRPCCallback (focus, NoChange, x)) request


let saveTestRPC (m : model) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/save_test"]
  in
  let request = postEmptyString Json_decode_extended.string m.csrfToken url in
  Tea.Http.send (fun x -> SaveTestRPCCallback x) request


let opsParams (ops : op list) : rpcParams = {ops}

let integrationRPC (m : model) (name : string) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri m.canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPC m.csrfToken url in
  Tea.Http.send
    (fun x ->
      InitialLoadRPCCallback (FocusNothing, TriggerIntegrationTest name, x) )
    request
