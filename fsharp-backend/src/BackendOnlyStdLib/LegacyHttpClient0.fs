module BackendOnlyStdLib.LegacyHttpClient0

// HttpClient for LibHttpClient0, LibHttpClient1, and LibHttpClient2

open System.IO
open System.IO.Compression
open System.Net.Http
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibBackend
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

type AspHeaders = System.Net.Http.Headers.HttpHeaders

open LegacyBaseHttpClient

module DvalRepr = LibExecution.DvalReprExternal
module Errors = LibExecution.Errors
module RT = RuntimeTypes

let incorrectArgs = Errors.incorrectArgs


let sendRequest
  (uri : string)
  (verb : HttpMethod)
  (jsonFn : Dval -> string)
  (reqBody : Dval option)
  (query : Dval)
  (reqHeaders : Dval)
  : Ply<Dval> =
  uply {
    let query = DvalRepr.toQuery query |> Exception.unwrapResultDeveloper

    let encodedReqHeaders =
      DvalRepr.toStringPairs reqHeaders |> Exception.unwrapResultDeveloper
    let encodedReqBody = encodeRequestBody jsonFn encodedReqHeaders reqBody

    match! httpCall 0 false uri query verb encodedReqHeaders encodedReqBody with
    | Ok response ->
      match UTF8.ofBytesOpt response.body with
      | None ->
        // Match how OCaml prints this error
        let encodeByte (b : byte) : byte array =
          if b = (byte '\"') then
            [| byte '\\'; byte '"' |]
          else if b = (byte '\\') then
            [| byte '\\'; byte '\\' |]
          else if (b >= (byte ' ') && b <= (byte '~')) then
            // This is where the vast majority of bytes, including all alphanumeric
            // ones, are handled
            [| b |]
          else if b = (byte '\t') then
            [| byte '\\'; byte 't' |]
          else if b = (byte '\n') then
            [| byte '\\'; byte '\n'; byte '\\'; byte 'n' |]
          else if b = (byte '\r') then
            [| byte '\\'; byte 'r' |]
          else if b = (byte '\b') then
            [| byte '\\'; byte 'b' |]
          else
            // 3-digit decimal value, such as \014
            UTF8.toBytes ("\\" + b.ToString("D3"))
        let asStr = response.body |> Array.collect encodeByte |> UTF8.ofBytesUnsafe
        return DError(SourceNone, $"Unknown Err:  \"Invalid UTF-8 string:{asStr}\"")
      | Some body ->
        let parsedResponseBody =
          if ContentType.hasJsonHeader response.headers then
            DvalRepr.unsafeOfUnknownJsonV0 body
          else
            DStr body

        let parsedResponseHeaders =
          response.headers
          |> List.map (fun (k, v) -> (String.trim k, DStr(String.trim v)))
          |> List.filter (fun (k, _) -> String.length k > 0)
          |> Map.ofList
          |> DObj // in old version, this was Dval.obj, however we want to allow duplicates

        let obj =
          Dval.obj [ ("body", parsedResponseBody)
                     ("headers", parsedResponseHeaders)
                     ("raw", DStr body) ]

        if response.code >= 200 && response.code <= 299 then
          return obj
        else
          // The OCaml version of this was Legacy.LibHttpClientv1, which called
          // Legacy.HttpClientv1.http_call, which threw exceptions for non-200 status
          // codes
          return
            Exception.raiseKnownIssue
              $"Bad HTTP response ({response.code}) in call to {uri}"
              []

    // Raise to be caught in the right place
    | Error err -> return Exception.raiseKnownIssue err.error []
  }

let call (method : HttpMethod) jsonFn : BuiltInFnSig =
  (function
  | _, [ DStr uri; body; query; headers ] ->
    sendRequest uri method jsonFn (Some body) query headers
  | _ -> incorrectArgs ())

let callNoBody (method : HttpMethod) jsonFn : BuiltInFnSig =
  (function
  | _, [ DStr uri; query; headers ] ->
    sendRequest uri method jsonFn None query headers
  | _ -> incorrectArgs ())

let callIgnoreBody (method : HttpMethod) jsonFn : BuiltInFnSig =
  (function
  | _, [ DStr uri; _body; query; headers ] ->
    sendRequest uri method jsonFn None query headers
  | _ -> incorrectArgs ())

let wrappedSendRequest
  (uri : string)
  (verb : HttpMethod)
  (jsonFn : Dval -> string)
  (reqBody : Dval option)
  (query : Dval)
  (reqHeaders : Dval)
  =
  uply {
    try
      let! r = sendRequest uri verb jsonFn reqBody query reqHeaders
      return DResult(Ok(r))
    with
    | e -> return DResult(Error(DStr(e.Message)))
  }

let wrappedCall verb jsonFn : BuiltInFnSig =
  function
  | _state, [ DStr uri; body; query; headers ] ->
    wrappedSendRequest uri verb jsonFn (Some body) query headers
  | _ -> incorrectArgs ()

let wrappedCallNoBody verb jsonFn : BuiltInFnSig =
  function
  | _state, [ DStr uri; query; headers ] ->
    wrappedSendRequest uri verb jsonFn None query headers
  | _ -> incorrectArgs ()
