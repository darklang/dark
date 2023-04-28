/// Thin, bytes-friendly middleware
///
/// This Middleware corresponds with `HTTP` handlers
[<RequireQualifiedAccess>]
module HttpMiddleware.Http

open Prelude
open LibExecution.VendoredTablecloth

module RT = LibExecution.RuntimeTypes
module DvalReprDeveloper = LibExecution.DvalReprDeveloper

module Telemetry = LibService.Telemetry

let lowercaseHeaderKeys (headers : HttpHeaders.T) =
  headers |> List.map (fun (k, v) -> (String.toLowercase k, v))



module Request =

  let typ = RT.FQTypeName.Stdlib(RT.FQTypeName.stdlibTypeName "Http" "Request" 0)

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
    |> RT.Dval.record typ


module Response =
  type HttpResponse =
    { statusCode : int
      body : byte array
      headers : HttpHeaders.T }

  let toHttpResponse (result : RT.Dval) : HttpResponse =
    match result with
    // Expected user response
    | RT.DRecord (RT.FQTypeName.Stdlib ({ modules = [ "Http" ]
                                          typ = "Response"
                                          version = 0 }),
                  fields) ->
      Telemetry.addTags [ "response-type", "httpResponse response" ]
      let code = Map.get "code" fields
      let headers = Map.get "headers" fields
      let body = Map.get "body" fields
      match code, headers, body with
      | Some (RT.DInt code), Some (RT.DDict headers), Some (RT.DBytes body) ->
        let headers =
          headers
          |> Map.toList
          |> List.fold (Ok []) (fun acc (k, v) ->
            match acc, v with
            | Ok acc, RT.DString str -> Ok((k, str) :: acc)
            | Ok _, v ->
              Error $"Header must be a string, but got {DvalReprDeveloper.toRepr v}"
            | Error _, _ -> acc)
        match headers with
        | Ok headers ->
          { statusCode = int code
            headers = headers |> lowercaseHeaderKeys
            body = body }
        | Error msg ->
          { statusCode = 500
            headers = [ "Content-Type", "text/plain; charset=utf-8" ]
            body = UTF8.toBytes msg }
      // Error responses
      | incorrectFieldTypes ->
        { statusCode = 500
          headers = [ "Content-Type", "text/plain; charset=utf-8" ]
          body =
            UTF8.toBytes
              "Application error: expected a Http.Response_v0, but its fields were the wrong type" }


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
