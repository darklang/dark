module LibExecution.HttpResponse

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

module RT = RuntimeTypes

type HttpResponse = { statusCode : int; body : byte array; headers : HttpHeaders.T }

module ContentType = HttpHeaders.ContentType
module MediaType = HttpHeaders.MediaType


let inferContentTypeHeader (dv : RT.Dval) : HttpHeaders.Header =
  match dv with
  | RT.DObj _
  | RT.DList _ -> ContentType.jsonHeader
  | _ -> ContentType.textHeader


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
    let inferredContentTypeHeader =
      match contentType with
      | None -> [ inferContentTypeHeader body ]
      | Some _ -> []
    let body =
      match body with
      | RT.DBytes b -> b
      | _ ->
        match Option.bind ContentType.toMediaType contentType with
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
      headers = [ inferContentTypeHeader dv ]
      body = dv |> DvalRepr.toPrettyMachineJsonStringV1 |> UTF8.toBytes }
