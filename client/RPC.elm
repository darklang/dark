module RPC exposing (..)

-- builtin
import Http
import Json.Decode as JSD
import Result

-- dark
import Types exposing (..)
import JSON exposing (..)

rpc_ : Model -> String ->
 (RPCParams -> Result Http.Error RPCResult -> Msg) ->
 RPCParams -> Cmd Msg
rpc_ m url callback params =
  let payload = encodeRPCParams params
      json = Http.jsonBody payload
      request = Http.post url json decodeRPC
  in Http.send (callback params) request

postString : String -> Http.Request String
postString url =
  Http.request
    { method = "POST"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }

rpc : Model -> String -> Focus -> RPCParams -> Cmd Msg
rpc m canvasName focus params =
  rpc_ m (String.concat ["/api/", Http.encodeUri canvasName, "/rpc" ])
       (RPCCallback focus) params

executeFunctionRPC : String -> ExecuteFunctionRPCParams -> Cmd Msg
executeFunctionRPC canvasName params =
  let url = String.concat ["/api/", Http.encodeUri canvasName, "/execute_function" ]
      payload = encodeExecuteFunctionRPCParams params
      json = Http.jsonBody payload
      request = Http.post url json decodeExecuteFunctionRPC
  in Http.send (ExecuteFunctionRPCCallback params) request

getAnalysisRPC : String -> AnalysisParams -> Cmd Msg
getAnalysisRPC canvasName params =
  let url = String.concat ["/api/", Http.encodeUri canvasName, "/get_analysis" ]
      payload = encodeAnalysisParams params
      json = Http.jsonBody payload
      request = Http.post url json decodeGetAnalysisRPC
  in Http.send GetAnalysisRPCCallback request

delete404RPC : String -> Delete404Param -> Cmd Msg
delete404RPC canvasName param =
  let url = String.concat ["/api/", Http.encodeUri canvasName, "/delete_404" ]
      payload = encode404 param
      json = Http.jsonBody payload
      request = Http.post url json (JSD.list decode404)
  in
  Http.send GetDelete404RPCCallback request

initialLoadRPC : String -> Focus -> Cmd Msg
initialLoadRPC canvasName focus =
  let url = String.concat ["/api/", Http.encodeUri canvasName, "/initial_load" ]
      request = Http.post url Http.emptyBody decodeInitialLoadRPC
  in Http.send (InitialLoadRPCCallback focus NoChange) request

saveTestRPC : String -> Cmd Msg
saveTestRPC canvasName =
  let url = String.concat ["/api/", Http.encodeUri canvasName, "/save_test" ]
      request = postString url
  in Http.send SaveTestRPCCallback request

emptyParams : RPCParams
emptyParams =
  { ops = [] }

opsParams : List Op -> RPCParams
opsParams ops =
  { ops = ops }

integrationRPC : Model -> String -> String -> Cmd Msg
integrationRPC m canvasName name =
  let url = String.concat ["/api/", Http.encodeUri canvasName, "/initial_load" ]
      request = Http.post url Http.emptyBody decodeInitialLoadRPC
  in Http.send
      (InitialLoadRPCCallback FocusNothing (TriggerIntegrationTest name))
      request

