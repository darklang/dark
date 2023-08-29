/// Thin, bytes-friendly middleware
///
/// This Middleware corresponds with `HTTP` handlers
[<RequireQualifiedAccess>]
module LibHttpMiddleware.Http

open Prelude
open LibExecution.StdLib.Shortcuts

module RT = LibExecution.RuntimeTypes
module Telemetry = LibService.Telemetry

let lowercaseHeaderKeys (headers : List<string * string>) : List<string * string> =
  headers |> List.map (fun (k, v) -> (String.toLowercase k, v))

module Request =

  let typ = RT.TypeName.fqPackage "Darklang" [ "Stdlib"; "Http" ] "Request" 0

  let fromRequest
    (uri : string)
    (headers : List<string * string>)
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
    { statusCode : int; body : byte array; headers : List<string * string> }

  let toHttpResponse (result : RT.Dval) : HttpResponse =
    match result with
    // Expected user response
    | RT.DRecord(RT.FQName.Package { owner = "Darklang"
                                     modules = [ "Stdlib"; "Http" ]
                                     name = RT.TypeName.TypeName "Response"
                                     version = 0 },
                 _,
                 fields) ->
      Telemetry.addTags [ "response-type", "httpResponse response" ]
      let code = Map.get "statusCode" fields
      let headers = Map.get "headers" fields
      let body = Map.get "body" fields
      match code, headers, body with
      | Some(RT.DInt code), Some(RT.DList headers), Some(RT.DBytes body) ->
        let headers =
          headers
          |> List.fold
            (fun acc v ->
              match acc, v with
              | Ok acc, RT.DTuple(RT.DString k, RT.DString v, []) ->
                Ok((k, v) :: acc)
              // Deliberately don't include the header value in the error message as we show it to users
              | Ok _, _ -> Error $"Header must be a string"
              | Error _, _ -> acc)
            (Ok [])
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
      | _incorrectFieldTypes ->
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
          let typeName = LibExecution.DvalReprDeveloper.dvalTypeName result
          let message =
            [ $"Application error: expected a HTTP response, got:"
              $"type {typeName}:"
              $"  {LibExecution.DvalReprDeveloper.toRepr result}"
              "\nHTTP handlers should return results in the form:"
              "  PACKAGE.Darklang.Stdlib.Http.Response {"
              "    statusCode : Int"
              "    headers : List<String*String>"
              "    body : Bytes"
              "  }" ]
          message |> String.concat "\n" |> UTF8.toBytes }
