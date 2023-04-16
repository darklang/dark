/// Thin, bytes-friendly middleware
///
/// This Middleware corresponds with `HTTP` handlers
[<RequireQualifiedAccess>]
module HttpMiddleware.Http

open Prelude
open LibExecution.VendoredTablecloth

module RT = LibExecution.RuntimeTypes
module Telemetry = LibService.Telemetry

let lowercaseHeaderKeys (headers : HttpHeaders.T) =
  headers |> List.map (fun (k, v) -> (String.toLowercase k, v))

module Request =
  let fromRequest
    (uri : string)
    (headers : HttpHeaders.T)
    (body : byte array)
    : RT.Dval =
    let headers =
      headers
      |> lowercaseHeaderKeys
      |> List.map (fun (k, v) -> RT.DTuple(RT.DString(k), RT.DString(v), []))
      |> RT.DList

    [ "body", RT.DBytes body; "headers", headers; "url", RT.DString uri ]
    |> RT.Dval.record


module Response =
  type HttpResponse =
    { statusCode : int
      body : byte array
      headers : HttpHeaders.T }

  let toHttpResponse (result : RT.Dval) : HttpResponse =
    match result with
    // Expected user response
    | RT.DHttpResponse (code, headers, RT.DBytes body) ->
      Telemetry.addTags [ "response-type", "httpResponse response" ]
      { statusCode = int code; headers = lowercaseHeaderKeys headers; body = body }

    // Error responses
    | uncaughtResult ->
      Telemetry.addTags [ "response-type", "error"; "result", uncaughtResult ]
      { statusCode = 500
        headers = [ "Content-Type", "text/plain; charset=utf-8" ]
        body =
          let message =
            $"Application error: expected an http response, got:
{LibExecution.DvalReprDeveloper.dvalTypeName result}: {LibExecution.DvalReprDeveloper.toRepr result}

Consider using `Http.response`, `Http.responseWithHeaders`, or a similar function."

          UTF8.toBytes message }
