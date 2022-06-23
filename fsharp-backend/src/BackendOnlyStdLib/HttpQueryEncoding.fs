module BackendOnlyStdLib.HttpQueryEncoding

/// Parsing and generating HTTP query strings.
///
/// CLEANUP: This file is used by both HttpClients and also the Http framework. We
/// prefer not to share code like this as we may need to mutate one and don't want to
/// change how other parts of the framework and standard library work. If we do
/// further work on any of this, we may want to split functionality out in some way,
/// including dupliucating code.
///
/// CLEANUP: this isn't a great place for this file, at time of writing we don't have
/// a better idea.

type AspHeaders = System.Net.Http.Headers.HttpHeaders

open Prelude
open LibExecution
open LibBackend
open VendoredTablecloth


module RT = RuntimeTypes

// -------------------------
// URLs and queryStrings
// -------------------------

// For putting into URLs as query params
let rec toUrlString (dv : RT.Dval) : string =
  let r = toUrlString
  match dv with
  | RT.DFnVal _ ->
    // See docs/dblock-serialization.ml
    "<block>"
  | RT.DIncomplete _ -> "<incomplete>"
  | RT.DPassword _ -> "<password>"
  | RT.DInt i -> string i
  | RT.DBool true -> "true"
  | RT.DBool false -> "false"
  | RT.DStr s -> s
  | RT.DFloat f -> DvalReprLegacyExternal.ocamlStringOfFloat f
  | RT.DChar c -> c
  | RT.DNull -> "null"
  | RT.DDate d -> RT.DDateTime.toIsoString d
  | RT.DDB dbname -> dbname
  | RT.DErrorRail d -> r d
  | RT.DError _ -> "error="
  | RT.DUuid uuid -> string uuid
  | RT.DHttpResponse (RT.Redirect _) -> "null"
  | RT.DHttpResponse (RT.Response (_, _, hdv)) -> r hdv
  | RT.DList l -> "[ " + String.concat ", " (List.map r l) + " ]"
  | RT.DObj o ->
    let strs = Map.fold [] (fun l key value -> (key + ": " + r value) :: l) o
    "{ " + (String.concat ", " strs) + " }"
  | RT.DOption None -> "none"
  | RT.DOption (Some v) -> r v
  | RT.DResult (Error v) -> "error=" + r v
  | RT.DResult (Ok v) -> r v
  | RT.DBytes bytes -> Base64.defaultEncodeToString bytes

/// Convert strings into queryParams.
/// Note that keys and values use slightly different encodings
let queryToEncodedString_ (queryParams : (List<string * List<string>>)) : string =
  match queryParams with
  | [ key, [] ] -> urlEncodeKey key
  | _ ->
    queryParams
    |> List.map (fun (k, vs) ->
      let k = k |> urlEncodeKey
      vs
      |> List.map urlEncodeValue
      |> fun vs ->
           if vs = [] then
             k
           else
             let vs = String.concat "," vs
             $"{k}={vs}")
    |> String.concat "&"

let toQuery (dv : RT.Dval) : Result<List<string * List<string>>, string> =
  match dv with
  | RT.DObj kvs ->
    kvs
    |> Map.toList
    |> List.map (fun (k, value) ->
      match value with
      | RT.DNull -> Ok(k, [])
      | RT.DList l -> Ok(k, List.map toUrlString l)
      | _ -> Ok(k, [ toUrlString value ]))
    |> Tablecloth.Result.values
  | _ -> Error "attempting to use non-object as query param" // CODE exception

let ofQuery (query : List<string * List<string>>) : RT.Dval =
  query
  |> List.map (fun (k, v) ->
    match v with
    | [] -> k, RT.DNull
    | [ "" ] -> k, RT.DNull // CLEANUP this should be a string
    | [ v ] -> k, RT.DStr v
    | list -> k, RT.DList(List.map RT.DStr list))
  |> Map
  |> RT.DObj

let parseQueryString_ (queryString : string) : List<string * List<string>> =
  queryString
  |> String.split "&"
  |> List.filterMap (fun kvPair ->
    match String.split "=" kvPair with
    | [ k; v ] -> Some(k, [ v ])
    | [ k ] -> Some(k, [])
    | k :: vs -> Some(k, vs)
    | [] -> Some("", []))
  |> List.map (fun (k, vs) ->
    let urlDecode = System.Web.HttpUtility.UrlDecode
    let v =
      if vs = [ "" ] then
        vs
      else
        vs |> String.concat "=" |> String.split "," |> List.map urlDecode
    urlDecode k, v)


let createQueryString
  (existingQuery : string)
  (queryParams : List<string * List<string>>)
  : string =
  (queryParams @ parseQueryString_ existingQuery) |> queryToEncodedString_

// -------------------------
// Forms
// -------------------------

let toFormEncoding (dv : RT.Dval) : Result<string, string> =
  toQuery dv |> Result.map queryToEncodedString_

let ofFormEncoding (f : string) : RT.Dval = f |> parseQueryString_ |> ofQuery
