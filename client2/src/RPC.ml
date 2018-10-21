open! Porting
(* open JSON *)
open Types

(* let rpc_ (m : model) (url : string) *)
(*     (callback : (rpcParams -> (string Http.error, rpcResult) result) -> msg) *)
(*     (params : rpcParams) : msg Cmd.t = *)
(*   let payload = encodeRPCParams params in *)
(*   let json = Http.jsonBody payload in *)
(*   let request = Http.post url json decodeRPC in *)
(*   Http.send (callback params) request *)
(*  *)
(* let postString (url : string) : string Http.request = *)
(*   Http.request *)
(*     { method_= "POST" *)
(*     ; headers= [] *)
(*     ; url *)
(*     ; body= Http.emptyBody *)
(*     ; expect= Http.expectString *)
(*     ; timeout= None *)
(*     ; withCredentials= false } *)
(*  *)
(* let rpc (m : model) (canvasName : string) (focus : focus) (params : rpcParams) *)
(*     : msg Cmd.t = *)
(*   rpc_ m *)
(*     (String.concat ["/api/"; Http.encodeUri canvasName; "/rpc"]) *)
(*     (RPCCallback focus) params *)
(*  *)
(* let executeFunctionRPC (canvasName : string) *)
(*     (params : executeFunctionRPCParams) : msg Cmd.t = *)
(*   let url = *)
(*     String.concat ["/api/"; Http.encodeUri canvasName; "/execute_function"] *)
(*   in *)
(*   let payload = encodeExecuteFunctionRPCParams params in *)
(*   let json = Http.jsonBody payload in *)
(*   let request = Http.post url json decodeExecuteFunctionRPC in *)
(*   Http.send (ExecuteFunctionRPCCallback params) request *)
(*  *)

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

let postEmpty decoder (url: string) =
  Tea.Http.request
    { method' = "POST"
    ; headers = [Header ("Content-type", "application/json")]
    ; url
    ; body = Web.XMLHttpRequest.EmptyBody
    ; expect = Tea.Http.expectStringResponse (Decoders.wrapExpect decoder)
    ; timeout = None
    ; withCredentials = false
    }


let getAnalysisRPC (canvasName : string) (params : analysisParams) : msg Tea.Cmd.t
    =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/get_analysis"]
  in
  let request =
    postJson Decoders.getAnalysisRPC url (Encoders.analysisParams params)
  in
  Tea.Http.send (fun x -> GetAnalysisRPCCallback x) request

(* let delete404RPC (canvasName : string) (param : delete404Param) : msg Cmd.t = *)
(*   let url = *)
(*     String.concat ["/api/"; Http.encodeUri canvasName; "/delete_404"] *)
(*   in *)
(*   let payload = encode404 param in *)
(*   let json = Http.jsonBody payload in *)
(*   let request = Http.post url json (JSD.list decode404) in *)
(*   Http.send GetDelete404RPCCallback request *)
(*  *)
let initialLoadRPC (canvasName : string) (focus : focus) : msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/initial_load"]
  in
  let request = postEmpty Decoders.initialLoadRPC url in
  Tea.Http.send (fun x -> InitialLoadRPCCallback (focus, NoChange, x)) request

(* let saveTestRPC (canvasName : string) : msg Cmd.t = *)
(*   let url = String.concat ["/api/"; Http.encodeUri canvasName; "/save_test"] in *)
(*   let request = postString url in *)
(*   Http.send SaveTestRPCCallback request *)
(*  *)
(* let emptyParams : rpcParams = {ops= []} *)
(*  *)
(* let opsParams (ops : op list) : rpcParams = {ops} *)
(*  *)
let integrationRPC (m : model) (canvasName : string) (name : string) :
    msg Tea.Cmd.t =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/initial_load"]
  in
  let request = postEmpty Decoders.initialLoadRPC url in
  Tea.Http.send
    (fun x -> InitialLoadRPCCallback (FocusNothing, TriggerIntegrationTest name, x))
    request
