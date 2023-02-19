/// Original Middleware - doesn't work with bytes requests, and prevents user
/// control of HTTP pipelines more than we'd like.
///
/// This middleware corresponds with `HTTP` handlers
[<RequireQualifiedAccess>]
module HttpMiddleware.HttpLegacy

/// <summary>
/// Header utilities for HttpMiddleware
/// </summary>
///
/// <remarks>
/// Deliberately kept separate from HttpClientHeaders,
/// which may need to work differently at some point
/// </remarks>
module private Headers =
  open Prelude
  open LibExecution.VendoredTablecloth

  module MediaType =
    type T =
      | Form
      | Xml
      | Json
      | Text
      | Html
      | Other of string

      override this.ToString() : string =
        match this with
        | Form -> "application/x-www-form-urlencoded"
        | Xml -> "application/xml"
        | Json -> "application/json"
        | Text -> "text/plain"
        | Html -> "text/html"
        | Other s -> s

    let parse (str : string) : T =
      match String.trim str with
      | "application/x-www-form-urlencoded" -> Form
      | "application/xml" -> Xml
      | "application/json" -> Json
      | "text/plain" -> Text
      | "text/html" -> Html
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

    let parse (str : string) : T =
      match String.split ";" str |> List.map String.trim with
      | [ mt; cs ] ->
        match String.split "=" cs |> List.map String.trim with
        | [ "charset"; cs ] -> Known(MediaType.parse mt, Charset.parse cs)
        | _ -> Unknown(str)
      | [ mt ] -> KnownNoCharset(MediaType.parse mt)
      | _ -> Unknown str

    let text = (Known(MediaType.Text, Charset.Utf8))
    let json = (Known(MediaType.Json, Charset.Utf8))

    let textHeader : HttpHeaders.Header = toHttpHeader text


  let getContentType (headers : HttpHeaders.T) : Option<ContentType.T> =
    headers |> HttpHeaders.get "Content-type" |> Option.map ContentType.parse

  let getMediaType (headers : HttpHeaders.T) : Option<MediaType.T> =
    headers
    |> HttpHeaders.get "Content-type"
    |> Option.map ContentType.parse
    |> Option.bind ContentType.toMediaType


module Request =
  open System.Threading.Tasks
  open FSharp.Control.Tasks

  open Prelude
  open LibExecution.VendoredTablecloth

  module RT = LibExecution.RuntimeTypes
  module ContentType = Headers.ContentType
  module MediaType = Headers.MediaType
  module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
  module HttpQueryEncoding = BackendOnlyStdLib.HttpQueryEncoding


  // Internal invariant, _must_ be a DObj
  type T = RT.Dval

  // -------------------------
  // Internal
  // -------------------------

  let private parse (p : Option<MediaType.T>) (body : byte array) : RT.Dval =
    match p with
    | Some (MediaType.Json _) ->
      (try
        body |> UTF8.ofBytesUnsafe |> DvalReprLegacyExternal.unsafeOfUnknownJsonV0
       with
       | e ->
         Exception.raiseGrandUser $"Invalid json: {UTF8.ofBytesWithReplacement body}")
    | Some MediaType.Form ->
      body |> UTF8.ofBytesUnsafe |> HttpQueryEncoding.ofFormEncoding
    // CLEANUP: text should just be text
    | Some (MediaType.Text _)
    | Some (MediaType.Xml _)
    | Some (MediaType.Html _)
    | Some (MediaType.Other _)
    | None ->
      (try
        body |> UTF8.ofBytesUnsafe |> DvalReprLegacyExternal.unsafeOfUnknownJsonV0
       with
       | e ->
         Exception.raiseGrandUser
           "Unknown Content-type -- we assumed application/json but invalid JSON was sent")

  let private parseBody (headers : List<string * string>) (reqbody : byte array) =
    if reqbody.Length = 0 then
      RT.DNull
    else
      let mt = Headers.getContentType headers |> Option.bind ContentType.toMediaType
      parse mt reqbody

  let private parseQueryString (query : string) : RT.Dval =
    // Drop leading ?
    let query = if query.Length > 0 then String.dropLeft 1 query else query
    BackendOnlyStdLib.HttpQueryEncoding.ofFormEncoding query

  let private parseHeaders (headers : (string * string) list) =
    headers
    |> List.map (fun (k, v) -> (String.toLowercase k, RT.DStr v))
    |> Map
    |> RT.Dval.DObj

  let private parseUsing
    (fmt : MediaType.T)
    (headers : HttpHeaders.T)
    (body : byte array)
    : RT.Dval =
    if body.Length = 0 || Some fmt <> Headers.getMediaType headers then
      RT.DNull
    else
      parse (Some fmt) body


  let private parseJsonBody headers body = parseUsing MediaType.Json headers body
  let private parseFormBody headers body = parseUsing MediaType.Form headers body

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
    (allowUnparseable : bool)
    (uri : string)
    (headers : List<string * string>)
    (query : string)
    (body : byte array)
    : RT.Dval =
    let parseBody =
      try
        parseBody headers body
      with
      | _ -> if allowUnparseable then RT.DNull else reraise ()

    let jsonBody =
      try
        parseJsonBody headers body
      with
      | _ -> if allowUnparseable then RT.DNull else reraise ()

    let formBody =
      try
        parseFormBody headers body
      with
      | _ -> if allowUnparseable then RT.DNull else reraise ()

    let fullBody =
      try
        UTF8.ofBytesUnsafe body |> RT.DStr
      with
      | _ -> RT.DError(RT.SourceNone, "Invalid UTF8 input")

    let parts =
      [ "body", parseBody
        "jsonBody", jsonBody
        "formBody", formBody
        "queryParams", parseQueryString query
        "headers", parseHeaders headers
        "fullBody", fullBody
        "cookies", cookies headers
        "url", url headers uri ]

    RT.Dval.obj parts

  let toDval (self : T) : RT.Dval = self


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


  let private inferContentTypeHeader (dv : RT.Dval) : ContentType.T =
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
      let contentType = Headers.getContentType headers
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
            DvalReprLegacyExternal.toEnduserReadableTextV0 body |> UTF8.toBytes
          | Some MediaType.Html ->
            DvalReprLegacyExternal.toEnduserReadableTextV0 body |> UTF8.toBytes
          | Some MediaType.Json
          | Some MediaType.Form
          | Some (MediaType.Other _)
          | None ->
            DvalReprLegacyExternal.toPrettyMachineJsonStringV1 body |> UTF8.toBytes
      { statusCode = int code
        headers = headers @ inferredContentTypeHeader
        body = body }

    | dv ->
      Telemetry.addTags [ "response-type", "user value" ]
      // for demonstrations sake, let's return 200 Okay when
      // no HTTP response object is returned
      { statusCode = 200
        headers = [ ContentType.toHttpHeader (inferContentTypeHeader dv) ]
        body =
          dv |> DvalReprLegacyExternal.toPrettyMachineJsonStringV1 |> UTF8.toBytes }


module Cors =
  open FSharp.Control.Tasks
  open System.Threading.Tasks

  open Prelude
  open Tablecloth

  module Interpreter = LibExecution.Interpreter
  module Req = Request
  module Resp = Response

  // TODO: Remove access to LibBackend from here
  module Canvas = LibBackend.Canvas

  type CorsSetting =
    | AllOrigins
    | Origins of List<string>

  /// Many tests have the cors feature enabled. This module exists so that test
  /// code may add their canvases to the list of ones with the corsSetting set.
  module Test =
    type ConcurrentDictionary<'k, 'v> =
      System.Collections.Concurrent.ConcurrentDictionary<'k, 'v>

    let mutable corsSettings : ConcurrentDictionary<string, CorsSetting> = null

    let initialize () : unit = corsSettings <- ConcurrentDictionary()

    let addAllOrigins (canvasName : CanvasName.T) : unit =
      corsSettings[string canvasName] <- AllOrigins

    let addOrigins (canvasName : CanvasName.T) (origins : List<string>) : unit =
      corsSettings[string canvasName] <- Origins origins

  /// We used to have a feature where we'd set the cors setting on the canvas. It's
  /// much better to do this in middleware. These canvases are the remaining user
  /// canvases while we had a setting. We can remove all this once this is gone.
  let private corsSettingForCanvas
    (canvasName : CanvasName.T)
    : Option<CorsSetting> =
    match string canvasName with
    | "ops-presence" ->
      Some(Origins [ "localhost"; "darklang.localhost"; "https://darklang.com" ])
    | "ops-adduser" -> Some(Origins [ "https://darklang.com" ])
    | "listo" ->
      Some(
        Origins(
          [ "http://localhost:8000"
            "https://usealtitude.com"
            "https://app.usealtitude.com"
            "http://localhost:3000"
            "https://elegant-galileo.netlify.com" ]
        )
      )
    | canvasName ->
      // There's actually a lot of tests for this. I don't want to slow things down by
      // adding every test canvas here, so instead there's a nullable dictionary.
      if isNull Test.corsSettings then
        None
      else
        let mutable result : CorsSetting = AllOrigins
        let success = Test.corsSettings.TryGetValue(string canvasName, &result)
        if success then Some result else None

  // ---------------
  // CORS
  // ---------------
  let private inferCorsOriginHeader
    (canvasName : CanvasName.T)
    (headers : HttpHeaders.T)
    : string option =
    let corsSetting = corsSettingForCanvas canvasName
    let originHeader = HttpHeaders.get "Origin" headers

    let defaultOrigins =
      [ "http://localhost:3000"; "http://localhost:5000"; "http://localhost:8000" ]

    let header =
      match originHeader, corsSetting with

      // if there's no explicit canvas setting, allow common localhosts
      | Some origin, None when List.contains origin defaultOrigins -> Some origin

      // if there's no explicit canvas setting and no default match, fall back to "*"
      | _, None -> Some "*"

      // If there's a "*" in the setting, always use it.
      // This is helpful as a debugging aid since users will always see
      // Access-Control-Allow-Origin: * in their browsers, even if the
      // request has no Origin.
      | _, Some AllOrigins -> Some "*"

      // if there's no supplied origin, don't set the header at all.
      | None, _ -> None

      // Return the origin if and only if it's in the setting
      | Some origin, Some (Origins origins) when List.contains origin origins ->
        Some origin

      // Otherwise: there was a supplied origin and it's not in the setting.
      // return "null" explicitly
      | Some _, Some _ -> Some "null"

    header

  let addCorsHeaders
    (reqHeaders : HttpHeaders.T)
    (canvasName : CanvasName.T)
    (response : Resp.HttpResponse)
    : Resp.HttpResponse =
    inferCorsOriginHeader canvasName reqHeaders
    |> Option.map (fun origin ->
      { response with
          // these are added in order, so make sure the user's setting wins
          headers = [ "Access-Control-Allow-Origin", origin ] @ response.headers })
    |> Option.defaultValue response


  let optionsResponse
    (reqHeaders : HttpHeaders.T)
    (canvasName : CanvasName.T)
    : Option<Resp.HttpResponse> =
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

    (inferCorsOriginHeader canvasName reqHeaders)
    |> Option.map (fun origin ->
      { statusCode = 200
        body = [||]
        headers =
          [ "Access-Control-Allow-Headers", allowHeaders
            // CLEANUP: if the origin is null here, we probably shouldn't add the other headers
            "Access-Control-Allow-Origin", origin
            "Access-Control-Allow-Methods", "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS" ] })
