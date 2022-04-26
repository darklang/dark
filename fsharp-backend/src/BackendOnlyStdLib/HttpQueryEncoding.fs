module BackendOnlyStdLib.HttpQueryEncoding


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
    (* See docs/dblock-serialization.ml *)
    "<block>"
  | RT.DIncomplete _ -> "<incomplete>"
  | RT.DPassword _ -> "<password>"
  | RT.DInt i -> string i
  | RT.DBool true -> "true"
  | RT.DBool false -> "false"
  | RT.DStr s -> s
  | RT.DFloat f -> LibExecution.DvalReprExternal.ocamlStringOfFloat f
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

// Convert strings into queryParams. This matches the OCaml Uri.query function. Note that keys and values use slightly different encodings
let queryToEncodedString (queryParams : (List<string * List<string>>)) : string =
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

let parseQueryString (queryString : string) : List<string * List<string>> =
  // This will eat any intended question mark, so add one
  let nvc = System.Web.HttpUtility.ParseQueryString("?" + queryString)
  nvc.AllKeys
  |> Array.map (fun key ->
    let values = nvc.GetValues key
    let split =
      values[values.Length - 1] |> FSharpPlus.String.split [| "," |] |> Seq.toList

    if isNull key then
      // All the values with no key are by GetValues, so make each one a value
      values |> Array.toList |> List.map (fun k -> (k, []))
    else
      [ (key, split) ])
  |> List.concat




let ofQueryString (queryString : string) : RT.Dval =
  queryString |> parseQueryString |> ofQuery

// -------------------------
// Forms
// -------------------------

let toFormEncoding (dv : RT.Dval) : Result<string, string> =
  toQuery dv |> Result.map queryToEncodedString

let ofFormEncoding (f : string) : RT.Dval = f |> parseQueryString |> ofQuery


let createQueryString
  (existingQuery : string)
  (queryParams : List<string * List<string>>)
  : string =
  // Remove leading '?'
  let queryString = if existingQuery = "" then "" else existingQuery.Substring 1
  (queryParams @ parseQueryString queryString) |> queryToEncodedString
