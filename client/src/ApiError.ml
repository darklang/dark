open Tc
open Types

(* Tea *)
module Cmd = Tea.Cmd
module Http = Tea.Http

let serverVersionOf (e : apiError) : string option =
  match e.originalError with
  | BadUrl _ | Timeout | NetworkError | Aborted ->
      None
  | BadStatus response | BadPayload (_, response) ->
      let module StringMap = Map.Make (Caml.String) in
      response.headers
      |> StringMap.find_first_opt (fun key ->
             String.toLower key = "x-darklang-server-version" )
      |> Option.map ~f:Tuple2.second


let urlOf (e : apiError) : string option =
  match e.originalError with
  | Http.BadUrl url ->
      Some url
  | Http.BadStatus response | Http.BadPayload (_, response) ->
      Some response.url
  | Http.Aborted | Http.Timeout | Http.NetworkError ->
      None


let shouldDisplayToUser (e : apiError) : bool =
  match e.originalError with
  | Http.BadUrl _ | Http.BadPayload _ ->
      true
  | Http.Timeout | Http.NetworkError | Http.Aborted ->
      e.importance = ImportantError
  | Http.BadStatus response ->
      if response.status.code = 502
      then e.importance = ImportantError
      else true


let shouldRollbar (e : apiError) : bool =
  match e.originalError with
  | Http.BadUrl _ | Http.Timeout | Http.BadPayload _ ->
      true
  | Http.NetworkError ->
      (* Don't rollbar if the internet is down *)
      false
  | Http.BadStatus response ->
      (* Don't rollbar if you aren't logged in *)
      response.status.code <> 401
  | Http.Aborted ->
      (* Don't rollbar if the client aborted the request *)
      false


let parseResponse (body : Http.responseBody) : string =
  let maybe name m =
    match m with Some s -> ", " ^ name ^ ": " ^ s | None -> ""
  in
  let str =
    match body with
    | NoResponse ->
        "todo-noresponse"
    | StringResponse str ->
        str
    | ArrayBufferResponse _ ->
        "todo-arratbufferresponse"
    | BlobResponse _ ->
        "todo-blobresponse"
    | DocumentResponse _ ->
        "todo-document-response"
    | JsonResponse _ ->
        "todo-jsonresponse"
    | TextResponse str ->
        str
    | RawResponse (str, _) ->
        str
  in
  str
  |> Json_decode_extended.decodeString Decoders.exception_
  |> Result.toOption
  |> Option.map
       ~f:(fun { short
               ; long
               ; exceptionTipe
               ; actual
               ; actualType
               ; expected
               ; result
               ; resultType
               ; info
               ; workarounds }
          ->
         " ("
         ^ exceptionTipe
         ^ "): "
         ^ short
         ^ maybe "message" long
         ^ maybe "actual value" actual
         ^ maybe "actual type" actualType
         ^ maybe "result" result
         ^ maybe "result type" resultType
         ^ maybe "expected" expected
         ^ ( if info = StrDict.empty
           then ""
           else ", info: " ^ StrDict.toString info )
         ^
         if workarounds = []
         then ""
         else ", workarounds: [" ^ String.concat workarounds ^ "]" )
  |> Option.withDefault ~default:str


let isBadAuth (e : apiError) : bool =
  if e.reload
  then true
  else
    match e.originalError with
    | Http.BadStatus response ->
        response.status.code = 401
    | _ ->
        false


let msg (e : apiError) : string =
  let withoutContext =
    match e.originalError with
    | Http.BadUrl str ->
        "Bad url: " ^ str
    | Http.Timeout ->
        "Timeout"
    | Http.NetworkError ->
        "Network error - is the server running?"
    | Http.BadStatus response ->
        if response.status.code = 502
        then "502"
        else
          "Bad status: "
          ^ response.status.message
          ^ " - "
          ^ parseResponse response.body
    | Http.BadPayload (msg, _) ->
        "Bad payload : " ^ msg
    | Http.Aborted ->
        "Request Aborted"
  in
  withoutContext ^ " (" ^ e.context ^ ")"


let make ?requestParams ~reload ~context ~importance originalError =
  {requestParams; importance; originalError; context; reload}
