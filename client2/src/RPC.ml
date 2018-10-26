open! Porting
open Types
module JSD = Json_decode_extended


let postJson decoder (url: string) (body: Js.Json.t) =
  Tea.Http.request
    { method' = "POST"
    ; headers = [Header ("Content-type", "application/json")]
    ; url
    ; body = Web.XMLHttpRequest.StringBody (Json.stringify body)
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials = false
    }

let postEmptyJson decoder (url: string) =
  Tea.Http.request
    { method' = "POST"
    ; headers = [Header ("Content-type", "application/json")]
    ; url
    ; body = Web.XMLHttpRequest.EmptyBody
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials = false
    }

let postEmptyString decoder (url: string) =
  Tea.Http.request
    { method' = "POST"
    ; headers = []
    ; url
    ; body = Web.XMLHttpRequest.EmptyBody
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials = false
    }

let rpc (m : model) (canvasName : string) (focus : focus) (params : rpcParams)
    : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/rpc"]
  in
  let request = postJson Decoders.rpc url (Encoders.rpcParams params) in
  Tea.Http.send (fun x -> RPCCallback (focus, params, x)) request

let executeFunctionRPC (canvasName : string)
    (params : executeFunctionRPCParams) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/execute_function"]
  in
  let request =
    postJson
      Decoders.executeFunctionRPC
      url
      (Encoders.executeFunctionRPCParams params)
  in
  Tea.Http.send (fun x -> ExecuteFunctionRPCCallback (params, x)) request

let getAnalysisRPC (canvasName : string) (params : analysisParams) : msg Tea.Cmd.t
    =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/get_analysis"]
  in
  let request =
    postJson Decoders.getAnalysisRPC url (Encoders.analysisParams params)
  in
  Tea.Http.send (fun x -> GetAnalysisRPCCallback x) request

let delete404RPC (canvasName : string) (param : delete404Param) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/delete_404"]
  in
  let request =
    postJson (JSD.list Decoders.fof) url (Encoders.fof param)
  in
  Tea.Http.send (fun x -> GetDelete404RPCCallback x) request

let initialLoadRPC (canvasName : string) (focus : focus) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPC url in
  Tea.Http.send (fun x -> InitialLoadRPCCallback (focus, NoChange, x)) request

let saveTestRPC (canvasName : string) : msg Tea.Cmd.t =
  let url = String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/save_test"] in
  let request = postEmptyString JSD.string url in
  Tea.Http.send (fun x -> SaveTestRPCCallback x) request

let opsParams (ops : op list) : rpcParams = {ops}

let integrationRPC (m : model) (canvasName : string) (name : string) :
    msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/initial_load"]
  in
  let request = postEmptyJson Decoders.initialLoadRPC url in
  Tea.Http.send
    (fun x -> InitialLoadRPCCallback (FocusNothing, TriggerIntegrationTest name, x))
    request
