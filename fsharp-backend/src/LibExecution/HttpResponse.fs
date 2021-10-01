module LibExecution.HttpResponse

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

module RT = RuntimeTypes

type HttpResponse = { statusCode : int; body : byte array; headers : HttpHeaders.T }

let textPlain = ("Content-type", "text/plain; charset=utf-8")

let toHttpResponse (result : RT.Dval) : HttpResponse =
  match result with
  | RT.DErrorRail (RT.DOption None)
  | RT.DErrorRail (RT.DResult (Error _)) ->
    // CLEANUP: result should become a 500 error
    { statusCode = 404
      headers =
        [ "Content-Length", "9"
          "Server", "darklang"
          "Content-Type", "text/plain; charset=utf-8" ]
      body = UTF8.toBytes "Not found" }
  | RT.DErrorRail _ ->
    { statusCode = 500
      headers =
        [ "Content-Length", "33"
          "Server", "darklang"
          "Content-Type", "text/plain; charset=utf-8" ]
      body = UTF8.toBytes "Invalid conversion from errorrail" }
  | RT.DHttpResponse (RT.Redirect str) ->
    { statusCode = int 302; headers = [ "Location", str ]; body = [||] }
  | RT.DHttpResponse (RT.Response (code, headers, body)) ->
    let body =
      match body with
      | RT.DBytes b -> b
      | other ->
        match HttpHeaders.getContentType headers with
        | Some "text/plain"
        | Some "application/xml" ->
          DvalRepr.toEnduserReadableTextV0 body |> UTF8.toBytes
        | Some "text/html" -> DvalRepr.toEnduserReadableTextV0 body |> UTF8.toBytes
        | Some "application/json"
        | _ -> DvalRepr.toPrettyMachineJsonStringV1 body |> UTF8.toBytes

    { statusCode = int code; headers = headers; body = body }
  | RT.DIncomplete _ ->
    let message =
      "Application error: the executed code was not complete. This error can be resolved by the application author by completing the incomplete code."
    { statusCode = 500; headers = [ textPlain ]; body = UTF8.toBytes message }
  | RT.DError _ ->
    let message =
      "Application error: the executed program was invalid. This problem can be resolved by the application's author by resolving the invalid code (often a type error)."
    { statusCode = 500; headers = [ textPlain ]; body = UTF8.toBytes message }


  | dv ->
    // for demonstrations sake, let's return 200 Okay when
    // no HTTP response object is returned
    { statusCode = 200
      headers = []
      body = dv |> DvalRepr.toPrettyMachineJsonStringV1 |> UTF8.toBytes }
