module BackendOnlyStdLib.LibHttpClient5

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Net.Http

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

module DvalReprExternal = LibExecution.DvalReprExternal

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"
let returnTypeOk = TVariable "result"
let returnTypeErr = TVariable "error" // FSTODO
let returnType = TResult(returnTypeOk, returnTypeErr)

let parameters =
  [ Param.make "uri" TStr ""
    Param.make "body" varA ""
    Param.make "query" (TDict TStr) ""
    Param.make "headers" (TDict TStr) "" ]

let parametersNoBody =
  [ Param.make "uri" TStr ""
    Param.make "query" (TDict TStr) ""
    Param.make "headers" (TDict TStr) "" ]


// Header utility functions, deliberately kept separate from the Http
// Middleware, as we want to be able to change them separately.

let hasFormHeader (headers : HttpHeaders.T) : bool =
  headers
  |> HttpHeaders.get "content-type"
  |> Option.map Tablecloth.String.toLowercase = Some
                                                  "application/x-www-form-urlencoded"

let hasJsonHeader (headers : HttpHeaders.T) : bool =
  // CLEANUP: don't use contains for this
  HttpHeaders.get "content-type" headers
  |> Option.map (fun s -> s.Contains "application/json")
  |> Option.defaultValue false

let hasTextHeader (headers : HttpHeaders.T) : bool =
  // CLEANUP: don't use contains for this
  HttpHeaders.get "content-type" headers
  |> Option.map (fun s -> s.Contains "text/plain")
  |> Option.defaultValue false


let guessContentType (body : Dval option) : string =
  match body with
  | Some dv ->
    match dv with
    (* TODO: DBytes? *)
    // Do nothing to strings; users can set the header if they have opinions
    | DStr _ -> "text/plain; charset=utf-8"
    // Otherwise, jsonify (this is the 'easy' API afterall), regardless of
    // headers passed. This makes a little more sense than you might think on
    // first glance, due to the interaction with the above `DStr` case. Note that
    // this handles all non-DStr dvals.
    | _ -> "application/json; charset=utf-8"
  // If we were passed an empty body, we need to ensure a Content-Type was set, or
  // else helpful intermediary load balancers will set the Content-Type to something
  // they've plucked out of the ether, which is distinctfully non-helpful and also
  // non-deterministic *)
  | None -> "text/plain; charset=utf-8"


// Encodes [body] as a UTF-8 string, safe for sending across the internet! Uses
// the `Content-Type` header provided by the user in [headers] to make ~magic~ decisions about
// how to encode said body. Returns a tuple of the encoded body, and the passed headers that
// have potentially had a Content-Type added to them based on the magic decision we've made.
let encodeRequestBody
  (body : Dval option)
  (headers : HttpHeaders.T)
  : HttpClient.Content =
  match body with
  | Some dv ->
    match dv with
    // CLEANUP support DBytes
    | DStr s ->
      // Do nothing to strings, ever. The reasoning here is that users do not
      // expect any magic to happen to their raw strings. It's also the only real
      // way (barring Bytes) to support users doing their _own_ encoding (say,
      // jsonifying themselves and passing the Content-Type header manually).
      //
      // CLEANUP find a place for all the notion links
      // See:
      // https://www.notion.so/darklang/Httpclient-Empty-Body-2020-03-10-5fa468b5de6c4261b5dc81ff243f79d9
      // for more information. *)
      HttpClient.StringContent s
    // CLEANUP if there is a charset here, it uses json encoding
    | DObj _ when hasFormHeader headers ->
      match DvalReprExternal.toFormEncoding dv with
      | Ok content -> HttpClient.FormContent(content)
      | Error msg -> Exception.raiseDeveloper msg
    | dv when hasTextHeader headers ->
      HttpClient.StringContent(DvalReprExternal.toEnduserReadableTextV0 dv)
    | _ -> // hasJsonHeader
      HttpClient.StringContent(DvalReprExternal.toPrettyMachineJsonStringV1 dv)
  | None -> HttpClient.NoContent


let sendRequest
  (uri : string)
  (verb : HttpMethod)
  (reqBody : Dval option)
  (query : Dval)
  (reqHeaders : Dval)
  : Ply<Dval> =
  uply {
    let query = DvalReprExternal.toQuery query |> Exception.unwrapResultDeveloper

    // Headers
    let encodedReqHeaders =
      DvalReprExternal.toStringPairs reqHeaders |> Exception.unwrapResultDeveloper
    let contentType =
      HttpHeaders.get "content-type" encodedReqHeaders
      |> Option.defaultValue (guessContentType reqBody)
    let reqHeaders =
      Map encodedReqHeaders |> Map.add "Content-Type" contentType |> Map.toList
    let encodedReqBody = encodeRequestBody reqBody reqHeaders

    match! HttpClient.httpCall 0 false uri query verb reqHeaders encodedReqBody with
    | Ok response ->
      let parsedResponseBody =
        // CLEANUP: form header never triggers in OCaml due to bug. But is it even needed?
        if false then // HttpHeaders.hasFormHeader response.headers
          try
            DvalReprExternal.ofQueryString response.body
          with
          | _ -> DStr "form decoding error"
        elif hasJsonHeader response.headers then
          try
            DvalReprExternal.unsafeOfUnknownJsonV0 response.body
          with
          | _ -> DStr "json decoding error"
        else
          DStr response.body

      let parsedResponseHeaders =
        response.headers
        |> List.map (fun (k, v) -> (String.trim k, DStr(String.trim v)))
        |> List.filter (fun (k, _) -> String.length k > 0)
        |> Map.ofList
        |> DObj // in old version, this was Dval.obj, however we want to allow duplicates

      let obj =
        Dval.obj [ ("body", parsedResponseBody)
                   ("headers", parsedResponseHeaders)
                   ("raw", DStr response.body)
                   ("code", DInt(int64 response.code))
                   ("error", DStr response.error) ]
      if response.code >= 200 && response.code <= 299 then
        return DResult(Ok obj)
      else
        return DResult(Error obj)
    | Error err -> return DResult(Error(DStr err.error))
  }


let call (method : HttpMethod) =
  (function
  | _, [ DStr uri; body; query; headers ] ->
    sendRequest uri method (Some body) query headers
  | _ -> incorrectArgs ())

let callNoBody (method : HttpMethod) : BuiltInFnSig =
  (function
  | _, [ DStr uri; query; headers ] -> sendRequest uri method None query headers
  | _ -> incorrectArgs ())


let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "post" 5
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = call HttpMethod.Post
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "HttpClient" "put" 5
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = call HttpMethod.Put
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "HttpClient" "get" 5
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = callNoBody HttpMethod.Get
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "HttpClient" "delete" 5
      // https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE the spec
      // says it may have a body
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = callNoBody HttpMethod.Delete
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "HttpClient" "options" 5
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = callNoBody HttpMethod.Options
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "HttpClient" "head" 5
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = callNoBody HttpMethod.Head
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "HttpClient" "patch" 5
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = call HttpMethod.Patch
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
