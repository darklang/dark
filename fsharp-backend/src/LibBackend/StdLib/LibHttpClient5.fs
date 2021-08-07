module LibBackend.StdLib.LibHttpClient5

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus

type HttpMethod = System.Net.Http.HttpMethod

open Prelude
open LibExecution.RuntimeTypes


module HttpClient = LibBackend.HttpClient
module DvalRepr = LibExecution.DvalRepr

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

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
let encodeRequestBody (body : Dval option) (contentType : string) : string option =
  match body with
  | Some dv ->
    let encodedBody =
      match dv with
      (* TODO: DBytes? *)
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
        s
      | DObj _ when contentType = HttpClient.formContentType ->
        HttpClient.dvalToFormEncoding dv
      | dv when contentType = HttpClient.textContentType ->
        DvalRepr.toEnduserReadableTextV0 dv
      | _ -> // when contentType = jsonContentType
        DvalRepr.toPrettyMachineJsonStringV1 dv
    if String.length encodedBody = 0 then None else Some encodedBody

  // If we were passed an empty body, we need to ensure a Content-Type was set, or
  // else helpful intermediary load balancers will set the Content-Type to something
  // they've plucked out of the ether, which is distinctfully non-helpful and also
  // non-deterministic
  | None -> None


let sendRequest
  (uri : string)
  (verb : HttpMethod)
  (requestBody : Dval option)
  (query : Dval)
  (requestHeaders : Dval)
  : Task<Dval> =
  task {
    let encodedQuery = HttpClient.dvalToQuery query

    // Headers
    let encodedRequestHeaders = DvalRepr.toStringPairsExn requestHeaders
    let contentType =
      HttpClient.getHeader "content-type" encodedRequestHeaders
      |> Option.defaultValue (guessContentType requestBody)

    let defaultHeaders =
      [ "Accept", "*/*"
        "Accept-Encoding", "deflate, gzip, br"
        "Content-Type", contentType ]

    let requestHeaders =
      // Prioritize the users' headers over the defaults
      // FSTODO: keep header ordering
      Map.union (Map defaultHeaders) (Map encodedRequestHeaders)
    let encodedRequestBody = encodeRequestBody requestBody contentType
    match! HttpClient.httpCall
             false
             uri
             encodedQuery
             verb
             (Map.toList requestHeaders)
             encodedRequestBody with
    | Ok response ->
      let parsedResponseBody =
        if HttpClient.hasFormHeader response.headers then
          try
            HttpClient.queryToDval response.body
          with
          | _ -> DStr "form decoding error"
        elif HttpClient.hasJsonHeader response.headers then
          try
            DvalRepr.ofUnknownJsonV0 response.body
          with
          | _ -> DStr "json decoding error"
        else
          DStr response.body

      // CLEANUP: For some reason, the OCaml version includes a header with the HTTP
      // status line the response and each redirect.
      // FSTODO: test redirects
      let statusHeader =
        ($"HTTP/{response.httpVersion} {response.code} {response.httpStatusMessage}")

      let parsedResponseHeaders =
        response.headers
        |> debug "Headers"
        |> List.map (fun (k, v) -> (k.Trim(), DStr(v.Trim())))
        |> debug "trimmed"
        |> List.filter (fun (k, _) -> String.length k > 0)
        |> List.append [ statusHeader, DStr "" ]
        |> debug "filtered"
        |> Dval.obj
        |> debug "objed"

      let obj =
        Dval.obj [ ("body", parsedResponseBody)
                   ("headers", parsedResponseHeaders)
                   // FSTODO: what about bad utf8?
                   ("raw", DStr response.body)
                   ("code", DInt(bigint response.code))
                   ("error", DStr response.error) ]
      if response.code >= 200 && response.code <= 299 then
        return DResult(Ok obj)
      else
        return DResult(Error obj)
    | Error err -> return DResult(Error(DStr err.error))
  }

// (* This is deprecated in favor of [encode_basic_auth u p]
// * due to using Unicode_string.append_broken.
// *)
// let encode_basic_auth_broken u p =
//   let input =
//     if Unicode_string.is_substring
//          (Unicode_string.of_string_exn "-")
//          u
//     then error "Username cannot contain a colon"
//     else
//       Unicode_string.append_broken
//         (Unicode_string.append_broken u (Unicode_string.of_string_exn ":"))
//         p
//   in
//   let encoded =
//     Unicode_string.of_string_exn
//       (B64.encode
//          B64.default_alphabet
//          true
//          (Unicode_string.to_string input))
//   in
//   Unicode_string.append_broken (Unicode_string.of_string_exn "Basic ") encoded
//
//
// let encode_basic_auth u p =
//   let input =
//     if Unicode_string.is_substring
//          (Unicode_string.of_string_exn "-")
//          u
//     then error "Username cannot contain a colon"
//     else
//       Unicode_string.append
//         (Unicode_string.append u (Unicode_string.of_string_exn ":"))
//         p
//   in
//   let encoded =
//     Unicode_string.of_string_exn
//       (B64.encode
//          B64.default_alphabet
//          true
//          (Unicode_string.to_string input))
//   in
//   Unicode_string.append (Unicode_string.of_string_exn "Basic ") encoded
//
//
// let call verb =
//     (function
//     | _, [DStr uri; body; query; headers] ->
//         send_request
//           (Unicode_string.to_string uri)
//           verb
//           (Some body)
//           query
//           headers
//     | _ ->
//         incorrectArgs ())
//
//
let callNoBody (method : System.Net.Http.HttpMethod) : BuiltInFnSig =
  (function
  | _, [ DStr uri; query; headers ] ->
    taskv {
      let! response = sendRequest uri method None query headers
      return response
    }
  | _ -> incorrectArgs ())


let fns : List<BuiltInFn> =
  [
    // [// ; { name = fn "HttpClient" "post" 5
//   ; parameters = parameters
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call Httpclient.POST
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "put" 5
//   ; parameters = parameters
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call Httpclient.PUT
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
    { name = fn "HttpClient" "get" 5
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = callNoBody HttpMethod.Get
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
  // ; { name = fn "HttpClient" "delete" 5
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = parametersNoBody
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = callNoBody Httpclient.DELETE
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "options" 5
//   ; parameters = parametersNoBody
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = callNoBody Httpclient.OPTIONS
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "head" 5
//   ; parameters = parametersNoBody
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = callNoBody Httpclient.HEAD
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "HttpClient" "patch" 5
//   ; parameters = parameters
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
//   ; fn = call Httpclient.PATCH
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
   ]
