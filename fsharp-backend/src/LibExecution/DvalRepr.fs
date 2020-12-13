module LibExecution.DvalRepr

// Printing Dvals is more complicated than you'd expect. Different situations
// have different constaints, such as develop-focused representation showing
// explicitly what the value is, vs an API-based representation which does
// something clever with Option/Result types. There is also versioning, as not
// all changes are going to be backward compatible.

// Note: we inline a lot of code which could be reused. This is deliberate: it
// allows us reason more easily about what changes are going to be safe. In
// general, we should avoid general purpose or reusable functions in this file.

open Prelude
open RuntimeTypes

module J = Prelude.Json

let rec toNestedString (reprfn : Dval -> string) (dv : Dval) : string =
  let rec inner (indent : int) (dv : Dval) : string =
    let nl = "\n" + String.replicate indent " "
    let inl = "\n" + String.replicate (indent + 2) " "
    let indent = indent + 2
    let recurse = inner indent
    match dv with
    | DList l ->
        if l = [] then
          "[]"
        else
          "[ " + inl + String.concat ", " (List.map recurse l) + nl + "]"
    | DObj o ->
        if o = Map.empty then
          "{}"
        else
          let strs =
            Map.fold (fun l key value -> (key + ": " + recurse value) :: l) [] o

          "{ " + inl + String.concat ("," + inl) strs + nl + "}"
    | _ -> reprfn dv

  inner 0 dv



let toEnduserReadableTextV0 (dval : Dval) : string =
  let rec nestedreprfn dv =
    (* If nesting inside an object or a list, wrap strings in quotes *)
    match dv with
    | DStr _
    | DUuid _
    | DChar _ -> "\"" + reprfn dv + "\""
    | _ -> reprfn dv

  and reprfn dv =
    match dv with
    | DInt i -> i.ToString()
    | DBool true -> "true"
    | DBool false -> "false"
    | DStr s -> s
    | DFloat f -> f.ToString()
    | DChar c -> c
    | DNull -> "null"
    // | DDate d ->
    //     Util.isostring_of_date d
    | DUuid uuid -> uuid.ToString()
    | DDB dbname -> $"<DB: {dbname}>"
    | DFakeVal (DError _) ->
        // FSTODO make this a string again
        "Error: TODO: print message"
    | DFakeVal (DIncomplete _) -> "<Incomplete>"
    | DFnVal _ ->
        // See docs/dblock-serialization.ml
        "<Block>"
    // | DPassword _ ->
    //     (* redacting, do not unredact *)
    //     "<Password>"
    | DObj _
    | DList _ -> toNestedString nestedreprfn dv
    | DFakeVal (DErrorRail d) ->
        // We don't print error here, because the errorrail value will know
        // whether it's an error or not.
        reprfn d
    | DHttpResponse (status, headers, body) ->
        // FSTODO
        // match d with
        // | Redirect url -> "302 " ^ url
        // | Response (c, hs) ->
        let headersString =
          headers
          |> List.map (fun (k, v) -> k + ": " + v)
          |> String.concat ","
          |> fun s -> "{ " + s + " }"

        (status.ToString()) + " " + headersString + "\n" + nestedreprfn dv + ""
    | DResult (Ok d) -> reprfn d
    | DResult (Error d) -> "Error: " + reprfn d
    | DOption (Some d) -> reprfn d
    | DOption None -> "Nothing"
    | DBytes bytes -> System.BitConverter.ToString bytes

  reprfn dval

let toPrettyMachineJsonValueV1 dval : FSharp.Data.JsonValue =
  let rec r dv =
    match dv with
    (* basic types *)
    | DInt i -> J.bigint i
    | DFloat f -> J.float f
    | DBool b -> J.bool b
    | DNull -> J.nil
    | DStr s -> J.string s
    | DList l -> J.list (List.map r l)
    | DObj o -> o |> Map.toList |> List.map (fun (k, v) -> (k, r v)) |> J.object
    | DFnVal _ ->
        (* See docs/dblock-serialization.ml *)
        J.nil
    | DFakeVal (DIncomplete _) -> J.nil
    | DChar c -> J.string c
    | DFakeVal (DError _) ->
        // FSTODO
        J.object [ "Error", J.string "TODO: error" ]
    | DHttpResponse (code, headers, response) -> r response
    | DDB dbname -> J.string dbname
    // | DDate date ->
    //     `String (Util.isostring_of_date date)
    // | DPassword hashed ->
    //     `Assoc [("Error", `String "Password is redacted")]
    | DUuid uuid -> J.string (uuid.ToString())
    | DOption opt -> Option.map r opt |> Option.defaultValue J.nil
    | DFakeVal (DErrorRail dv) -> r dv
    | DResult res ->
        (match res with
         | Ok dv -> r dv
         | Error dv -> J.object [ "Error", r dv ])
    | DBytes bytes ->
        // FSTODO is this the right b64 encoding
        bytes |> System.Convert.ToBase64String |> J.string

  r dval


let toPrettyMachineJsonV1 dval : string =
  dval |> toPrettyMachineJsonValueV1 |> Json.toPrettyString
