module BackendOnlyStdLib.LegacyHttpClient2

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

let rec httpCall
  (count : int)
  (rawBytes : bool)
  (url : string)
  (queryParams : (string * string list) list)
  (method : HttpMethod)
  (reqHeaders : HttpHeaders.T)
  (reqBody : Content)
  : Task<Result<HttpResult, ClientError>> =
  task {
    // The OCaml version of these functions handled a number of things differently
    // which can only be done with our own redirect logic:
    //   1) when redirecting, it stored all headers along the way
    //   2) when redirecting, it kept the Authorization header (which HTTPClient drops)
    //   3) when redirecting, it failed appropriately when the scheme was not http/https
    // Each of these was a breaking change, and not great, but considering all three
    // it felt worth implementing redirects outselves. Note that we did not use
    // recursion within makeHttpCall, to ensure that the HTTP objects/streams were
    // all cleaned up before the redirect happened. We do keep more data in this case
    // than we would like but they're all redirects and so unlikely to have bodies.
    if (count > 50) then
      return Error { url = url; code = 0; error = "Too many redirects" }
    else
      let! response = makeHttpCall rawBytes url queryParams method reqHeaders reqBody

      match response with
      | Ok result when result.code >= 300 && result.code < 400 ->
        let location =
          result.headers
          |> List.tryFind (fun (k, _) -> String.equalsCaseInsensitive "location" k)
        match location with
        | Some (_, locationUrl) when method <> HttpMethod.Delete ->
          let newCount = count + 1
          // It might be a relative URL. If the location is absolute, the location will win over the last URL
          let newUrl = System.Uri(System.Uri(url), locationUrl).ToString()
          // Unlike HttpClient, do not drop the authorization header
          let! newResponse =
            httpCall newCount rawBytes newUrl queryParams method reqHeaders reqBody
          return
            Result.map
              (fun redirectResult ->
                // Keep all headers along the way, mirroring the OCaml version
                { redirectResult with
                    headers = redirectResult.headers @ result.headers })
              newResponse
        | _ -> return response
      | _ -> return response
  }


let init (serviceName : string) =
  print $"Initing LegacyHttpClient v2 in {serviceName}"
  // Don't add "traceparent" headers in HttpClient calls. It's not necessarily a bad
  // idea, but it's a change (one that breaks all the tests), and so something we
  // should do consciously.
  System.AppContext.SetSwitch("System.Net.Http.EnableActivityPropagation", false)
  print $" Inited LegacyHttpClient v2 in {serviceName}"



// used by v4 Dark fns
let sendRequest
  (uri : string)
  (verb : HttpMethod)
  (reqBody : Dval option)
  (query : Dval)
  (reqHeaders : Dval)
  : Ply<Dval> =
  uply {
    let query = DvalRepr.toQuery query |> Exception.unwrapResultDeveloper

    let encodedReqHeaders =
      DvalRepr.toStringPairs reqHeaders |> Exception.unwrapResultDeveloper
    let encodedReqBody =
      encodeRequestBody
        DvalRepr.toPrettyMachineJsonStringV1
        encodedReqHeaders
        reqBody

    match! httpCall 0 false uri query verb encodedReqHeaders encodedReqBody with
    | Ok response ->
      let parsedResponseBody =
        if ContentType.hasJsonHeader response.headers then
          try
            DvalRepr.unsafeOfUnknownJsonV0 response.body
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
                   ("code", DInt(int64 response.code)) ]
      if response.code >= 200 && response.code <= 299 then
        return DResult(Ok obj)
      else
        return DResult(Error obj)
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
