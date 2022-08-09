/// Thin, bytes-friendly middleware
///
/// This Middleware corresponds with `HTTPBytes` handlers
[<RequireQualifiedAccess>]
module HttpMiddleware.HttpMiddlewareV1

module Request =
  open Prelude
  open LibExecution.VendoredTablecloth

  module RT = LibExecution.RuntimeTypes
  module HttpQueryEncoding = BackendOnlyStdLib.HttpQueryEncoding

  let private parseHeaders (headers : (string * string) list) =
    headers
    |> List.map (fun (k, v) -> (String.toLowercase k, RT.DStr v))
    |> Map
    |> RT.Dval.DObj

  let fromRequest
    (uri : string)
    (headers : List<string * string>)
    (query : string)
    (body : byte array)
    : RT.Dval =
    [ "body", RT.DBytes body
      "queryParams", RT.DStr query
      "headers", parseHeaders headers
      "url", RT.DStr uri ]
    |> RT.Dval.obj


module Response =
  open Prelude

  module RT = LibExecution.RuntimeTypes

  type HttpResponse =
    { statusCode : int
      body : byte array
      headers : HttpHeaders.T }

  module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
  module Telemetry = LibService.Telemetry

  let toHttpResponse (result : RT.Dval) : HttpResponse =
    match result with
    // Expected user responses
    | RT.DHttpResponse (RT.Redirect str) ->
      Telemetry.addTags [ "response-type", "httpResponse redirect" ]
      { statusCode = int 302; headers = [ "Location", str ]; body = [||] }

    | RT.DHttpResponse (RT.Response (code, headers, RT.DBytes body)) ->
      Telemetry.addTags [ "response-type", "httpResponse response" ]
      { statusCode = int code; headers = headers; body = body }

    | RT.DBytes body ->
      Telemetry.addTags [ "response-type", "httpResponse response" ]
      { statusCode = 200; headers = []; body = body }

    // Error responses
    | uncaughtResult ->
      Telemetry.addTags [ "response-type", "error"; "result", uncaughtResult ]
      { statusCode = 500
        headers = [ "Content-Type", "text/plain; charset=utf-8" ]
        body =
          UTF8.toBytes
            "Application error: the executed code did not result in a redirect or bytes response." }
