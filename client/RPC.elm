module RPC exposing (..)

-- builtin
import Http
import Json.Decode as JSD
import Result

-- dark
import Types exposing (..)
import JSON exposing (..)

post : String -> String -> Http.Body -> JSD.Decoder a -> Http.Request a
post csrfToken url body decoder =
  Http.request
    { method = "POST"
    , headers = [ Http.header "X-CSRF-Token" csrfToken ]
    , url = url
    , body = body
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

postString : String -> String -> Http.Request String
postString csrfToken url =
  Http.request
    { method = "POST"
    , headers = [Http.header "X-CSRF-Token" csrfToken]
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }

rpc_ : Model -> String ->
 (RPCParams -> Result Http.Error RPCResult -> Msg) ->
 RPCParams -> Cmd Msg
rpc_ m url callback params =
  let payload = encodeRPCParams params
      json = Http.jsonBody payload
      request = post m.csrfToken url json decodeRPC
  in Http.send (callback params) request

rpc : Model -> Focus -> RPCParams -> Cmd Msg
rpc m focus params =
  rpc_ m (String.concat ["/api/", Http.encodeUri m.canvasName, "/rpc" ])
       (RPCCallback focus) params

executeFunctionRPC : RPCContext -> ExecuteFunctionRPCParams -> Cmd Msg
executeFunctionRPC c params =
  let url = String.concat ["/api/", Http.encodeUri c.canvasName, "/execute_function" ]
      payload = encodeExecuteFunctionRPCParams params
      json = Http.jsonBody payload
      request = post c.csrfToken url json decodeExecuteFunctionRPC
  in Http.send (ExecuteFunctionRPCCallback params) request

getAnalysisRPC : RPCContext -> AnalysisParams -> Cmd Msg
getAnalysisRPC c params =
  let url = String.concat ["/api/", Http.encodeUri c.canvasName, "/get_analysis" ]
      payload = encodeAnalysisParams params
      json = Http.jsonBody payload
      request = post c.csrfToken url json decodeGetAnalysisRPC
  in Http.send GetAnalysisRPCCallback request

delete404RPC : RPCContext -> Delete404Param -> Cmd Msg
delete404RPC c param =
  let url = String.concat ["/api/", Http.encodeUri c.canvasName, "/delete_404" ]
      payload = encode404 param
      json = Http.jsonBody payload
      request = post c.csrfToken url json (JSD.list decode404)
  in
  Http.send GetDelete404RPCCallback request

initialLoadRPC : RPCContext -> Focus -> Cmd Msg
initialLoadRPC c focus =
  let url = String.concat ["/api/", Http.encodeUri c.canvasName, "/initial_load" ]
      request = post c.csrfToken url Http.emptyBody decodeInitialLoadRPC
  in Http.send (InitialLoadRPCCallback focus NoChange) request

saveTestRPC : RPCContext -> Cmd Msg
saveTestRPC c =
  let url = String.concat ["/api/", Http.encodeUri c.canvasName, "/save_test" ]
      request = postString c.csrfToken url
  in Http.send SaveTestRPCCallback request

emptyParams : RPCParams
emptyParams =
  { ops = [] }

opsParams : List Op -> RPCParams
opsParams ops =
  { ops = ops }

integrationRPC : Model -> String -> Cmd Msg
integrationRPC m name =
  let url = String.concat ["/api/", Http.encodeUri m.canvasName, "/initial_load" ]
      request = post m.csrfToken url Http.emptyBody decodeInitialLoadRPC
  in Http.send
      (InitialLoadRPCCallback FocusNothing (TriggerIntegrationTest name))
      request
