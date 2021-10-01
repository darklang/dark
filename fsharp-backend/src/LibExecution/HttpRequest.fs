module LibExecution.HttpRequest

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth

module RT = RuntimeTypes
module ContentType = HttpHeaders.ContentType
module MediaType = HttpHeaders.MediaType
module Charset = HttpHeaders.Charset

// Internal invariant, _must_ be a DObj
type T = RT.Dval

// -------------------------
// Internal
// -------------------------

let parse (p : Option<MediaType.T>) (body : byte array) : RT.Dval =
  match p with
  | Some (MediaType.Json _) ->
    (try
      body |> UTF8.ofBytesUnsafe |> DvalRepr.ofUnknownJsonV0
     with
     | e -> failwith $"Invalid json")
  | Some MediaType.Form -> body |> UTF8.ofBytesUnsafe |> DvalRepr.ofFormEncoding
  // CLEANUP: text should just be text
  | Some (MediaType.Text _)
  | Some (MediaType.Xml _)
  | Some (MediaType.Html _)
  | Some (MediaType.Other _)
  | None ->
    (try
      body |> UTF8.ofBytesUnsafe |> DvalRepr.ofUnknownJsonV0
     with
     | e ->
       failwith
         "Unknown Content-type -- we assumed application/json but invalid JSON was sent")


let parseBody (headers : List<string * string>) (reqbody : byte array) =
  if reqbody.Length = 0 then
    RT.DNull
  else
    let mt =
      HttpHeaders.getContentType headers |> Option.bind ContentType.toMediaType
    parse mt reqbody


let parseQueryString (queryvals : List<string * List<string>>) =
  DvalRepr.ofQuery queryvals


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
  if body.Length = 0 || Some fmt <> HttpHeaders.getMediaType headers then
    RT.DNull
  else
    parse (Some fmt) body


let parseJsonBody headers body = parseUsing MediaType.Json headers body
let parseFormBody headers body = parseUsing MediaType.Form headers body

// let parsed_cookies cookies =
//   cookies
//   |> String.split ~on:';'
//   |> List.map ~f:String.strip
//   |> List.map ~f:(String.lsplit2 ~on:'=')
//   |> List.filter_opt
//   |> List.map ~f:(fun (k, v) ->
//          (Uri.pct_decode k, DStr (Uri.pct_decode v)))
//   |> Dval.to_dobj_exn
//
//
// let cookies (headers : (string * string) list) =
//   List.Assoc.find ~equal:( = ) headers "cookie"
//   |> Option.map ~f:parsed_cookies
//   |> Option.value ~default:(Dval.to_dobj_exn [])
//   |> fun x -> Dval.to_dobj_exn [("cookies", x)]
//
//
let url (uri : string) = RT.Dval.obj [ ("url", RT.DStr uri) ]


// -------------------------
// Exported *)
// -------------------------

// If allow_unparsed is true, we fall back to DNull; this allows us to create a
// 404 for a request with an unparseable body
let fromRequest
  (allowUnparseable : bool)
  (uri : string)
  (headers : List<string * string>)
  (query : List<string * List<string>>)
  (body : byte array)
  : RT.Dval =
  let parseBody =
    try
      parseBody headers body
    with
    | e -> if allowUnparseable then RT.DNull else raise e
  let jsonBody =
    try
      parseJsonBody headers body
    with
    | e -> if allowUnparseable then RT.DNull else raise e
  let formBody =
    try
      parseFormBody headers body
    with
    | e -> if allowUnparseable then RT.DNull else raise e
  let parts =
    [ "body", parseBody
      "jsonBody", jsonBody
      "formBody", formBody
      "queryParams", parseQueryString query
      "headers", parseHeaders headers
      "fullBody", RT.DStr(UTF8.ofBytesUnsafe body)
      // cookies headers
      "url", RT.DStr uri ]
  RT.Dval.obj parts

let toDval (self : T) : RT.Dval = self
