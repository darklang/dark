module HttpMiddleware.ResponseV0

open Prelude

module RT = LibExecution.RuntimeTypes

type HttpResponse = { statusCode : int; body : byte array; headers : HttpHeaders.T }

module ContentType = HeadersV0.ContentType
module MediaType = HeadersV0.MediaType
module DvalReprExternal = LibExecution.DvalReprExternal
module Telemetry = LibService.Telemetry


let inferContentTypeHeader (dv : RT.Dval) : ContentType.T =
  match dv with
  | RT.DObj _
  | RT.DList _ -> ContentType.json
  | _ -> ContentType.text


let toHttpResponse (result : RT.Dval) : HttpResponse =
  match result with
  // ----------
  // errors
  // ----------
  | RT.DErrorRail (RT.DOption None)
  | RT.DErrorRail (RT.DResult (Error _)) ->
    // CLEANUP: result should become a 500 error
    Telemetry.addTags [ "response-type", "Common ErrorRail response"
                        "result", result ]
    { statusCode = 404
      headers = [ "Server", "darklang"; ContentType.textHeader ]
      body = UTF8.toBytes "Not found" }

  | RT.DErrorRail _ ->
    Telemetry.addTags [ "response-type", "Unexpected ErrorRail response"
                        "result", result ]
    { statusCode = 500
      headers = [ "Server", "darklang"; ContentType.textHeader ]
      body = UTF8.toBytes "Invalid conversion from errorrail" }

  | RT.DIncomplete _ ->
    Telemetry.addTags [ "response-type", "dincomplete"; "result", result ]
    let message =
      "Application error: the executed code was not complete. This error can be resolved by the application author by completing the incomplete code."
    { statusCode = 500
      headers = [ ContentType.textHeader ]
      body = UTF8.toBytes message }

  | RT.DError _ ->
    Telemetry.addTags [ "response-type", "derror"; "result", result ]
    let message =
      "Application error: the executed program was invalid. This problem can be resolved by the application's author by resolving the invalid code (often a type error)."
    { statusCode = 500
      headers = [ ContentType.textHeader ]
      body = UTF8.toBytes message }

  // ----------
  // expected user responses
  // ----------
  | RT.DHttpResponse (RT.Redirect str) ->
    Telemetry.addTags [ "response-type", "httpResponse redirect" ]
    { statusCode = int 302; headers = [ "Location", str ]; body = [||] }

  | RT.DHttpResponse (RT.Response (code, headers, body)) ->
    Telemetry.addTags [ "response-type", "httpResponse response" ]
    let contentType = HeadersV0.getContentType headers
    // Potential extra header
    let inferredContentTypeHeader =
      if contentType = None then
        [ ContentType.toHttpHeader (inferContentTypeHeader body) ]
      else
        []
    // Encode the body
    let body =
      match body with
      | RT.DBytes b -> b
      | _ ->
        let mediaType =
          match contentType with
          | Some ct -> ContentType.toMediaType ct
          | None -> inferContentTypeHeader body |> ContentType.toMediaType
        match mediaType with
        | Some MediaType.Text
        | Some MediaType.Xml ->
          DvalReprExternal.toEnduserReadableTextV0 body |> UTF8.toBytes
        | Some MediaType.Html ->
          DvalReprExternal.toEnduserReadableTextV0 body |> UTF8.toBytes
        | Some MediaType.Json
        | Some MediaType.Form
        | Some (MediaType.Other _)
        | None -> DvalReprExternal.toPrettyMachineJsonStringV1 body |> UTF8.toBytes
    { statusCode = int code
      headers = headers @ inferredContentTypeHeader
      body = body }

  | dv ->
    Telemetry.addTags [ "response-type", "user value" ]
    // for demonstrations sake, let's return 200 Okay when
    // no HTTP response object is returned
    { statusCode = 200
      headers = [ ContentType.toHttpHeader (inferContentTypeHeader dv) ]
      body = dv |> DvalReprExternal.toPrettyMachineJsonStringV1 |> UTF8.toBytes }
