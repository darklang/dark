module HttpMiddleware.RequestV0

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.VendoredTablecloth

module RT = LibExecution.RuntimeTypes
module ContentType = HeadersV0.ContentType
module MediaType = HeadersV0.MediaType
module DvalReprExternal = LibExecution.DvalReprExternal
module HttpQueryEncoding = BackendOnlyStdLib.HttpQueryEncoding


// Internal invariant, _must_ be a DObj
type T = RT.Dval

// -------------------------
// Internal
// -------------------------

let parse (p : Option<MediaType.T>) (body : byte array) : RT.Dval =
  match p with
  | Some (MediaType.Json _) ->
    (try
      body |> UTF8.ofBytesUnsafe |> DvalReprExternal.unsafeOfUnknownJsonV0
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
      body |> UTF8.ofBytesUnsafe |> DvalReprExternal.unsafeOfUnknownJsonV0
     with
     | e ->
       Exception.raiseGrandUser
         "Unknown Content-type -- we assumed application/json but invalid JSON was sent")


let parseBody (headers : List<string * string>) (reqbody : byte array) =
  if reqbody.Length = 0 then
    RT.DNull
  else
    let mt = HeadersV0.getContentType headers |> Option.bind ContentType.toMediaType
    parse mt reqbody


let parseQueryString (query : string) : RT.Dval =
  // Drop leading ?
  let query = if query.Length > 0 then String.dropLeft 1 query else query
  BackendOnlyStdLib.HttpQueryEncoding.ofFormEncoding query

let parseHeaders (headers : (string * string) list) =
  headers
  |> List.map (fun (k, v) -> (String.toLowercase k, RT.DStr v))
  |> Map
  |> RT.Dval.DObj

let parseUsing
  (fmt : MediaType.T)
  (headers : HttpHeaders.T)
  (body : byte array)
  : RT.Dval =
  if body.Length = 0 || Some fmt <> HeadersV0.getMediaType headers then
    RT.DNull
  else
    parse (Some fmt) body


let parseJsonBody headers body = parseUsing MediaType.Json headers body
let parseFormBody headers body = parseUsing MediaType.Form headers body

let parseCookies (cookies : string) : RT.Dval =
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

let cookies (headers : HttpHeaders.T) : RT.Dval =
  HttpHeaders.get "cookie" headers
  |> Option.map parseCookies
  |> Option.defaultValue (RT.DObj Map.empty)


let url (headers : List<string * string>) (uri : string) =
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
  let parts =
    [ "body", parseBody
      "jsonBody", jsonBody
      "formBody", formBody
      "queryParams", parseQueryString query
      "headers", parseHeaders headers
      "fullBody", RT.DStr(UTF8.ofBytesUnsafe body)
      "cookies", cookies headers
      "url", url headers uri ]
  RT.Dval.obj parts

let toDval (self : T) : RT.Dval = self
