open Prelude

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
             String.toLower key = "x-darklang-server-version")
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
      if response.status.code = 502 then e.importance = ImportantError else true


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
  |> Json.Decode.decodeString Decoders.exception_
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
         else ", workarounds: [" ^ String.concat workarounds ^ "]")
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


let customContext (e : apiError) (state : cursorState) : Js.Json.t =
  let parameters = Option.withDefault ~default:Js.Json.null e.requestParams in
  Json_encode_extended.object_
    [ ("httpResponse", Encoders.httpError e.originalError)
    ; ("parameters", parameters)
    ; ("cursorState", Encoders.cursorState state) ]


let rollbar (m : model) (e : apiError) : unit =
  Rollbar.send (msg e) (urlOf e) (customContext e m.cursorState)


let handle (m : model) (apiError : apiError) : model * msg Cmd.t =
  let now = Js.Date.now () |> Js.Date.fromFloat in
  let shouldReload =
    let buildHashMismatch =
      serverVersionOf apiError
      |> Option.map ~f:(fun hash -> hash <> m.buildHash)
      |> Option.withDefault ~default:false
    in
    let reloadAllowed =
      match m.lastReload with
      | Some time ->
          (* if 60 seconds have elapsed *)
          Js.Date.getTime time +. 60000.0 > Js.Date.getTime now
      | None ->
          true
    in
    (* Reload if it's an auth failure or the frontend is out of date *)
    isBadAuth apiError || (buildHashMismatch && reloadAllowed)
  in
  let ignore =
    (* Ignore when using Ngrok *)
    let usingNgrok = VariantTesting.variantIsActive m NgrokVariant in
    (* This message is deep in the server code and hard to pull
          * out, so just ignore for now *)
    Js.log "Already at latest redo - ignoring server error" ;
    let redoError =
      String.contains
        (msg apiError)
        ~substring:"(client): Already at latest redo"
    in
    redoError || usingNgrok
  in
  let cmd =
    if shouldReload
    then
      let m = {m with lastReload = Some now} in
      (* Previously, this was two calls to Tea_task.nativeBinding. But
          * only the first got called, unclear why. *)
      Cmd.call (fun _ ->
          SavedSettings.save m ;
          SavedUserSettings.save m ;
          Native.Location.reload true)
    else if (not ignore) && shouldRollbar apiError
    then Cmd.call (fun _ -> rollbar m apiError)
    else Cmd.none
  in
  let newM =
    let error =
      if shouldDisplayToUser apiError && not ignore
      then Error.set (msg apiError) m.error
      else m.error
    in
    let lastReload = if shouldReload then Some now else m.lastReload in
    {m with error; lastReload}
  in
  (newM, cmd)
