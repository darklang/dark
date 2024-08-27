/// Thin, bytes-friendly middleware
///
/// This Middleware corresponds with `HTTP` handlers
[<RequireQualifiedAccess>]
module LibHttpMiddleware.Http

open Prelude
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module RT = LibExecution.RuntimeTypes
module Telemetry = LibService.Telemetry
module PackageIDs = LibExecution.PackageIDs


let lowercaseHeaderKeys (headers : List<string * string>) : List<string * string> =
  headers |> List.map (fun (k, v) -> (String.toLowercase k, v))

module Request =
  let typ = RT.FQTypeName.fqPackage PackageIDs.Type.Stdlib.Http.request

  let fromRequest
    (uri : string)
    (headers : List<string * string>)
    (body : byte array)
    : RT.Dval =
    let headerType = RT.KTTuple(RT.ValueType.string, RT.ValueType.string, [])

    let headers =
      headers
      |> lowercaseHeaderKeys
      |> List.map (fun (k, v) -> RT.DTuple(RT.DString(k), RT.DString(v), []))
      |> Dval.list headerType

    let fields =
      [ "body", Dval.byteArrayToDvalList body
        "headers", headers
        "url", RT.DString uri ]
    RT.DRecord(typ, typ, [], Map fields)


module Response =
  type HttpResponse =
    { statusCode : int; body : byte array; headers : List<string * string> }

  let toHttpResponse (result : RT.Dval) : HttpResponse =
    match result with
    // Expected user response
    | RT.DRecord(RT.FQTypeName.Package id, _, [], fields) when
      id = PackageIDs.Type.Stdlib.Http.response
      ->
      Telemetry.addTags [ "response-type", "httpResponse response" ]

      let code = Map.get "statusCode" fields
      let headers = Map.get "headers" fields
      let body = Map.get "body" fields

      match code, headers, body with
      | Some(RT.DInt64 code), Some(RT.DList(_, headers)), Some(RT.DList(_, body)) ->
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
            headers = lowercaseHeaderKeys headers
            body = body |> Dval.dlistToByteArray }
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
              "Application error: expected a Http.Response, but its fields were the wrong type" }

    // Error responses
    | uncaughtResult ->
      Telemetry.addTags [ "response-type", "error"; "result", uncaughtResult ]

      { statusCode = 500
        headers = [ "Content-Type", "text/plain; charset=utf-8" ]
        body =
          let typeName = LibExecution.DvalReprDeveloper.toTypeName result
          let message =
            [ $"Application error: expected a HTTP response, got:"
              $"type {typeName}:"
              $"  {LibExecution.DvalReprDeveloper.toRepr result}"
              "\nHTTP handlers should return results in the form:"
              "  PACKAGE.Darklang.Stdlib.Http.Response {"
              "    statusCode : Int64"
              "    headers : List<String*String>"
              "    body : Bytes"
              "  }" ]
          message |> String.concat "\n" |> UTF8.toBytes }
