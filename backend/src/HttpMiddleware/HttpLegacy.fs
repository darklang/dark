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
