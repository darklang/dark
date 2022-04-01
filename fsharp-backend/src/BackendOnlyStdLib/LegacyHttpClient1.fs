module BackendOnlyStdLib.LegacyHttpClient1

// HttpClient for LibHttpClient3

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
  (reqBody : Dval option)
  (query : Dval)
  (reqHeaders : Dval)
  : Ply<Dval> =
  uply {
    let query = DvalRepr.toQuery query |> Exception.unwrapResultCode

    let encodedReqHeaders =
      DvalRepr.toStringPairs reqHeaders |> Exception.unwrapResultCode
    let encodedReqBody =
      encodeRequestBody
        DvalRepr.toPrettyMachineJsonStringV1
        encodedReqHeaders
        reqBody

    match! httpCall 0 false uri query verb encodedReqHeaders encodedReqBody with
    | Ok response ->
      let body =
        response.body
        |> UTF8.ofBytesOpt
        |> Option.defaultValue "utf-8 decoding error"
      let parsedResponseBody =
        if ContentType.hasJsonHeader response.headers then
          try
            DvalRepr.unsafeOfUnknownJsonV0 body
          with
          | _ -> DStr "json decoding error"
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
                   ("raw", DStr body)
                   ("code", DInt(int64 response.code)) ]
      if response.code >= 200 && response.code <= 299 then
        return DResult(Ok obj)
      else
        // The OCaml version of this was Legacy.LibHttpClientv1, which called
        // Legacy.HttpClientv1.http_call, which threw exceptions for non-200 status
        // codes
        return
          DError(SourceNone, $"Bad HTTP response ({response.code}) in call to {uri}")
    | Error err -> return DError(SourceNone, err.error)
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
