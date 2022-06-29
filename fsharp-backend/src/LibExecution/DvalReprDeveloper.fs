/// Ways of converting Dvals to strings, intended for developers to read
module LibExecution.DvalReprDeveloper

open Prelude
open Tablecloth

open RuntimeTypes

// CLEANUP: make these clearer/better messages for developers.
// They don't need to be backwards-compatible.

let rec typeName (t : DType) : string =
  match t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TNull -> "Null"
  | TChar -> "Character"
  | TStr -> "Str" // CLEANUP change to String
  | TList _ -> "List"
  | TDict _ -> "Dict"
  | TRecord _ -> "Dict"
  | TFn _ -> "Block"
  | TVariable varname -> "Any"
  | TIncomplete -> "Incomplete"
  | TError -> "Error"
  | THttpResponse _ -> "Response"
  | TDB _ -> "Datastore"
  | TDate -> "Date" // CLEANUP Dates should be DateTimes
  | TPassword -> "Password"
  | TUuid -> "UUID"
  | TOption _ -> "Option"
  | TErrorRail -> "ErrorRail"
  | TResult _ -> "Result"
  | TUserType (name, _) -> name
  | TBytes -> "Bytes"

let dvalTypeName (dv : Dval) : string = dv |> Dval.toType |> typeName


let private ocamlStringOfFloat (f : float) : string =
  if System.Double.IsPositiveInfinity f then
    "inf"
  else if System.Double.IsNegativeInfinity f then
    "-inf"
  else if System.Double.IsNaN f then
    "nan"
  else
    let result = sprintf "%.12g" f
    if result.Contains "." then result else $"{result}."

// SERIALIZER_DEF Custom DvalReprLegacyExternal.toDeveloperReprV0
/// For printing something for the developer to read, as a live-value, error
/// message, etc. Redacts passwords.
///
/// Customers should not come to rely on this format. Do not use in stdlib fns
/// or other places a developer could rely on it (i.e. telemery and error
/// messages are OK)
let rec toRepr (dv : Dval) : string =
  let rec toRepr_ (indent : int) (dv : Dval) : string =
    let makeSpaces len = "".PadRight(len, ' ')
    let nl = "\n" + makeSpaces indent
    let inl = "\n" + makeSpaces (indent + 2)
    let indent = indent + 2
    let typename = dvalTypeName dv
    let wrap str = $"<{typename}: {str}>"
    let justtipe = $"<{typename}>"

    match dv with
    | DPassword _ -> "<password>"
    | DStr s -> $"\"{s}\""
    | DChar c -> $"'{c}'"
    | DInt i -> string i
    | DBool true -> "true"
    | DBool false -> "false"
    | DFloat f -> ocamlStringOfFloat f
    | DNull -> "null"
    | DFnVal _ ->
      (* See docs/dblock-serialization.ml *)
      justtipe
    | DIncomplete _ -> justtipe
    | DError _ -> "<error>"
    | DDate d -> wrap (DDateTime.toIsoString d)
    | DDB name -> wrap name
    | DUuid uuid -> wrap (string uuid)
    | DHttpResponse h ->
      match h with
      | Redirect url -> $"302 {url}" + nl + toRepr_ indent DNull
      | Response (code, headers, hdv) ->
        let headerString =
          headers
          |> List.map (fun (k, v) -> k + ": " + v)
          |> String.concat ","
          |> fun s -> "{ " + s + " }"

        $"{code} {headerString}" + nl + toRepr_ indent hdv
    | DList l ->
      if List.isEmpty l then
        "[]"
      else
        let elems = String.concat ", " (List.map (toRepr_ indent) l)
        // CLEANUP: this space makes no sense
        $"[ {inl}{elems}{nl}]"
    | DObj o ->
      if Map.isEmpty o then
        "{}"
      else
        let strs =
          Map.fold [] (fun l key value -> ($"{key}: {toRepr_ indent value}") :: l) o

        let elems = String.concat $",{inl}" strs
        // CLEANUP: this space makes no sense
        "{ " + $"{inl}{elems}{nl}" + "}"
    | DOption None -> "Nothing"
    | DOption (Some dv) -> "Just " + toRepr_ indent dv
    | DResult (Ok dv) -> "Ok " + toRepr_ indent dv
    | DResult (Error dv) -> "Error " + toRepr_ indent dv
    | DErrorRail dv -> "ErrorRail: " + toRepr_ indent dv
    | DBytes bytes -> Base64.defaultEncodeToString bytes

  toRepr_ 0 dv
