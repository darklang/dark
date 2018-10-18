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
let getAnalysisRPC (canvasName : string) (params : analysisParams) : msg Tea.Cmd.t
    =
  let url =
    String.concat ["/api/"; Tea.Http.encodeUri canvasName; "/get_analysis"]
  in
  let request =
    Tea.Http.request
      { method' = "POST"
      ; headers = [Header ("Content-type", "application/json")]
      ; url
      ; body =
          Web.XMLHttpRequest.StringBody
            (Json.stringify (Encoders.analysisParams params))
      ; expect = Tea.Http.expectStringResponse Decoders.(wrapDecoder getAnalysisRPC)
      ; timeout = None
      ; withCredentials = false
      }
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
(* let initialLoadRPC (canvasName : string) (focus : focus) : msg Cmd.t = *)
(*   let url = *)
(*     String.concat ["/api/"; Http.encodeUri canvasName; "/initial_load"] *)
(*   in *)
(*   let request = Http.post url Http.emptyBody decodeInitialLoadRPC in *)
(*   Http.send (InitialLoadRPCCallback (focus, NoChange)) request *)
(*  *)
(* let saveTestRPC (canvasName : string) : msg Cmd.t = *)
(*   let url = String.concat ["/api/"; Http.encodeUri canvasName; "/save_test"] in *)
(*   let request = postString url in *)
(*   Http.send SaveTestRPCCallback request *)
(*  *)
(* let emptyParams : rpcParams = {ops= []} *)
(*  *)
(* let opsParams (ops : op list) : rpcParams = {ops} *)
(*  *)
(* let integrationRPC (m : model) (canvasName : string) (name : string) : *)
(*     msg Cmd.t = *)
(*   let url = *)
(*     String.concat ["/api/"; Http.encodeUri canvasName; "/initial_load"] *)
(*   in *)
(*   let request = Http.post url Http.emptyBody decodeInitialLoadRPC in *)
(*   Http.send *)
(*     (InitialLoadRPCCallback (FocusNothing, TriggerIntegrationTest name)) *)
(*     request *)
