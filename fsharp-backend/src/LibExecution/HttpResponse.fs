module LibExecution.HttpResponse

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

module RT = RuntimeTypes

type HttpResponse = { statusCode : int; body : byte array; headers : HttpHeaders.T }

module ContentType = HttpHeaders.ContentType
module MediaType = HttpHeaders.MediaType


let inferContentTypeHeader (dv : RT.Dval) : ContentType.T =
  match dv with
  | RT.DObj _
  | RT.DList _ -> ContentType.json
  | _ -> ContentType.text


let toHttpResponse (result : RT.Dval) : HttpResponse =
  match result with
  | RT.DErrorRail (RT.DOption None)
  | RT.DErrorRail (RT.DResult (Error _)) ->
    // CLEANUP: result should become a 500 error
    { statusCode = 404
      headers = [ "Server", "darklang"; ContentType.textHeader ]
      body = UTF8.toBytes "Not found" }
  | RT.DErrorRail _ ->
    { statusCode = 500
      headers = [ "Server", "darklang"; ContentType.textHeader ]
      body = UTF8.toBytes "Invalid conversion from errorrail" }
  | RT.DHttpResponse (RT.Redirect str) ->
    { statusCode = int 302; headers = [ "Location", str ]; body = [||] }
  | RT.DHttpResponse (RT.Response (code, headers, body)) ->
    let contentType = HttpHeaders.getContentType headers
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
        | Some MediaType.Xml -> DvalRepr.toEnduserReadableTextV0 body |> UTF8.toBytes
        | Some MediaType.Html ->
          DvalRepr.toEnduserReadableTextV0 body |> UTF8.toBytes
        | Some MediaType.Json
        | Some MediaType.Form
        | Some (MediaType.Other _)
        | None -> DvalRepr.toPrettyMachineJsonStringV1 body |> UTF8.toBytes
    { statusCode = int code
      headers = headers @ inferredContentTypeHeader
      body = body }
  | RT.DIncomplete _ ->
    let message =
      "Application error: the executed code was not complete. This error can be resolved by the application author by completing the incomplete code."
    { statusCode = 500
      headers = [ HttpHeaders.ContentType.textHeader ]
      body = UTF8.toBytes message }
  | RT.DError _ ->
    let message =
      "Application error: the executed program was invalid. This problem can be resolved by the application's author by resolving the invalid code (often a type error)."
    { statusCode = 500
      headers = [ HttpHeaders.ContentType.textHeader ]
      body = UTF8.toBytes message }
  | dv ->
    // for demonstrations sake, let's return 200 Okay when
    // no HTTP response object is returned
    { statusCode = 200
      headers = [ ContentType.toHttpHeader (inferContentTypeHeader dv) ]
      body = dv |> DvalRepr.toPrettyMachineJsonStringV1 |> UTF8.toBytes }
