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

  let private parseQueryString (query : string) : RT.Dval =
    // Drop leading ?
    let query = if query.Length > 0 then String.dropLeft 1 query else query
    HttpQueryEncoding.ofFormEncoding query

  let private parseHeaders (headers : (string * string) list) =
    headers
    |> List.map (fun (k, v) -> (String.toLowercase k, RT.DStr v))
    |> Map
    |> RT.Dval.DObj

  let private parseCookies (cookies : string) : RT.Dval =
    let decode = System.Web.HttpUtility.UrlDecode
    cookies
    |> String.split ";"
    |> List.map String.trim
    |> List.map (fun s -> s.Split("=", 2) |> Array.toList)
    |> List.map (fun cookie ->
      match cookie with
      | [] -> ("", RT.DNull) // skip empty rows
      | [ _ ] -> ("", RT.DNull) // skip rows with only a key
      | k :: v :: _ -> (decode k, RT.DStr(decode v)))
    |> RT.Dval.obj

  let private cookies (headers : HttpHeaders.T) : RT.Dval =
    HttpHeaders.get "cookie" headers
    |> Option.map parseCookies
    |> Option.defaultValue (RT.DObj Map.empty)


  let private url (headers : List<string * string>) (uri : string) =
    // .NET doesn't url-encode the query like we expect, so we're going to do it
    let parsed = System.UriBuilder(uri)
    // FSTODO test this somehow (probably fuzz against old)
    parsed.Query <- urlEncodeExcept "*$@!:()~?/.,&-_=\\" parsed.Query
    // Set the scheme if it's passed by the load balancer
    headers
    |> List.find (fun (k, _) -> String.toLowercase k = "x-forwarded-proto")
    |> Option.map (fun (_, v) -> parsed.Scheme <- v)
    |> ignore<Option<unit>>
    // Use .Uri or it will slip in a port number
    RT.DStr(string parsed.Uri)

  // -------------------------
  // Exported
  // -------------------------

  // If allow_unparsed is true, we fall back to DNull; this allows us to create a
  // 404 for a request with an unparseable body
  let fromRequest
    (uri : string)
    (headers : List<string * string>)
    (query : string)
    (body : byte array)
    : RT.Dval =
    let body =
      try
        UTF8.ofBytesUnsafe body |> RT.DStr
      with
      | _ -> RT.DError(RT.SourceNone, "Invalid UTF8 input")

    let parts =
      [ "body", body
        "queryParams", parseQueryString query
        "headers", parseHeaders headers
        "cookies", cookies headers
        "url", url headers uri ]

    RT.Dval.obj parts


module private Headers =
  open Prelude
  open LibExecution.VendoredTablecloth

  module MediaType =
    type T =
      | Json
      | Text
      | Other of string

      override this.ToString() : string =
        match this with
        | Json -> "application/json"
        | Text -> "text/plain"
        | Other s -> s

    let parse (str : string) : T =
      match String.trim str with
      | "application/json" -> Json
      | "text/plain" -> Text
      | _ -> Other str

  module Charset =
    type T =
      | Utf8
      | NotUtf8 of string

      override this.ToString() : string =
        match this with
        | Utf8 -> "utf-8"
        | NotUtf8 s -> s

    let parse (str : string) : T =
      match String.trim str with
      | "utf-8" -> Utf8
      | _ -> NotUtf8 str


  module ContentType =
    type T =
      | Known of MediaType.T * Charset.T
      | KnownNoCharset of MediaType.T
      | Unknown of string // don't parse out charset or boundary or anything

      override this.ToString() : string =
        match this with
        | Known (mt, cs) -> $"{mt}; charset={cs}"
        | KnownNoCharset (mt) -> string mt
        | Unknown s -> s

    let toMediaType (ct : T) : Option<MediaType.T> =
      match ct with
      | Known (mt, _) -> Some mt
      | KnownNoCharset (mt) -> Some mt
      | Unknown s -> None

    let toHttpHeader (ct : T) : HttpHeaders.Header = "Content-Type", string ct

    let text = (Known(MediaType.Text, Charset.Utf8))
    let json = (Known(MediaType.Json, Charset.Utf8))

    let textHeader : HttpHeaders.Header = toHttpHeader text


module Response =
  open Prelude

  module RT = LibExecution.RuntimeTypes

  type HttpResponse =
    { statusCode : int
      body : byte array
      headers : HttpHeaders.T }

  module ContentType = Headers.ContentType
  module MediaType = Headers.MediaType
  module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
  module Telemetry = LibService.Telemetry


  let toHttpResponse (result : RT.Dval) : HttpResponse =
    // HttpBytesTODO: should we do less, or different things, for these error cases?
    // maybe we really just leave all of this up to the user, and yell if we have anything other than DBytes?
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

      let body =
        match body with
        | RT.DBytes b -> b
        | _ ->
          // HttpBytesTODO: what should we do here?
          // If the user indicates some specific content-type, should we do something?
          DvalReprLegacyExternal.toPrettyMachineJsonStringV1 body |> UTF8.toBytes

      { statusCode = int code; headers = headers; body = body }

    | dv ->
      Telemetry.addTags [ "response-type", "user value" ]
      // HttpBytesTODO: what should we do here?

      // for demonstrations sake, let's return 200 Okay when
      // no HTTP response object is returned
      { statusCode = 200
        headers = [ ContentType.toHttpHeader (ContentType.json) ]
        body =
          dv |> DvalReprLegacyExternal.toPrettyMachineJsonStringV1 |> UTF8.toBytes }


module Cors =
  open Prelude
  open Tablecloth

  module Req = Request
  module Resp = Response

  let private inferCorsOriginHeader (headers : HttpHeaders.T) : string option =
    let originHeader = HttpHeaders.get "Origin" headers

    let defaultOrigins =
      [ "http://localhost:3000"; "http://localhost:5000"; "http://localhost:8000" ]

    let header =
      match originHeader with
      // if there's no explicit canvas setting, allow common localhosts
      | Some origin when List.contains origin defaultOrigins -> Some origin

      // if there's no supplied origin, don't set the header at all.
      | None -> None

      // Otherwise: there was a supplied origin and it's not in the setting.
      // return "null" explicitly
      | Some _ -> Some "null"

    header

  let addCorsHeaders
    (reqHeaders : HttpHeaders.T)
    (response : Resp.HttpResponse)
    : Resp.HttpResponse =
    inferCorsOriginHeader reqHeaders
    |> Option.map (fun origin ->
      { response with
          // these are added in order, so make sure the user's setting wins
          headers = [ "Access-Control-Allow-Origin", origin ] @ response.headers })
    |> Option.defaultValue response


  let optionsResponse (reqHeaders : HttpHeaders.T) : Option<Resp.HttpResponse> =
    // When javascript in a browser tries to make an unusual cross-origin
    // request (for example, a POST with a weird content-type or something with
    // weird headers), the browser first makes an OPTIONS request to the
    // server in order to get its permission to make that request. It includes
    // "origin", the originating origin, and "access-control-request-headers",
    // which is the list of headers the javascript would like to use.

    // (Ordinary GETs and some POSTs get handled in addCorsHeaders, above,
    // without an OPTIONS).

    // Our strategy here is: if it's from an allowed origin (i.e., in the canvas
    // cors_setting) to:
    // - return an Access-Control-Allow-Origin header for that origin
    // - return Access-Control-Allow-Headers with the requested headers
    // - return Access-Control-Allow-Methods for all of the methods we think
    //   might be useful.

    let acReqHeaders = HttpHeaders.get "access-control-request-headers" reqHeaders
    let allowHeaders = Option.defaultValue "*" acReqHeaders

    (inferCorsOriginHeader reqHeaders)
    |> Option.map (fun origin ->
      { statusCode = 200
        body = [||]
        headers =
          [ "Access-Control-Allow-Headers", allowHeaders
            // CLEANUP: if the origin is null here, we probably shouldn't add the other headers
            "Access-Control-Allow-Origin", origin
            "Access-Control-Allow-Methods", "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS" ] })
