/// Ways of converting Dvals to strings, intended for developers to read
module LibExecution.DvalReprDeveloper

open Prelude
open Tablecloth

open RuntimeTypes

let rec typeName (t : TypeReference) : string =
  match t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TUnit -> "Unit"
  | TChar -> "Char"
  | TString -> "String"
  | TList nested -> $"List<{typeName nested}>"
  | TTuple (n1, n2, rest) ->
    let nested = (n1 :: n2 :: rest) |> List.map typeName |> String.concat ", "
    $"({nested})"
  | TDict nested -> $"Dict<{typeName nested}>"
  | TFn _ -> "Function"
  | TVariable varname -> $"'{varname}"
  | TDB _ -> "Datastore"
  | TDateTime -> "DateTime"
  | TPassword -> "Password"
  | TUuid -> "Uuid"
  | TOption nested -> $"Option<{typeName nested}>"
  | TResult (ok, err) -> $"Result<{typeName ok}, {typeName err}>"
  | TCustomType (t, typeArgs) ->
    let typeArgsPortion =
      match typeArgs with
      | [] -> ""
      | args ->
        args
        |> List.map (fun t -> typeName t)
        |> String.concat ", "
        |> fun betweenBrackets -> "<" + betweenBrackets + ">"
    FQTypeName.toString t + typeArgsPortion
  | TBytes -> "Bytes"

let rec dvalTypeName (dv : Dval) : string =
  match dv with
  | DIncomplete _ -> "Incomplete"
  | DError _ -> "Error"
  | DInt _ -> "Int"
  | DFloat _ -> "Float"
  | DBool _ -> "Bool"
  | DUnit -> "Unit"
  | DChar _ -> "Char"
  | DString _ -> "String"
  | DList [] -> "List<'a>"
  | DList (v :: _) -> "List<" + dvalTypeName v + ">"
  | DDict _ -> "Dict"
  | DFnVal (Lambda _) -> "Lambda"
  | DDB _ -> "Datastore"
  | DDateTime _ -> "DateTime"
  | DPassword _ -> "Password"
  | DUuid _ -> "Uuid"
  | DOption (Some v) -> "Option<" + dvalTypeName v + ">"
  | DOption None -> "Option<'a>"
  | DResult (Ok v) -> "Result<" + dvalTypeName v + ", 'err>"
  | DResult (Error v) -> "Result<'ok, " + dvalTypeName v + ">"
  | DTuple (t1, t2, trest) ->
    "(" + (t1 :: t2 :: trest |> List.map dvalTypeName |> String.concat ", ") + ")"
  | DBytes _ -> "Bytes"
  | DRecord (typeName, _) -> FQTypeName.toString typeName
  | DEnum (typeName, _, _) -> FQTypeName.toString typeName


// SERIALIZER_DEF Custom DvalReprDeveloper.toRepr
/// For printing something for the developer to read, as a live-value, error
/// message, etc. Redacts passwords.
///
/// Customers should not come to rely on this format. Do not use in stdlib fns
/// or other places a developer could rely on it (i.e. telemery and error
/// messages are OK)
///
/// This should be kept sync with client Runtime.toRepr
let toRepr (dv : Dval) : string =
  let rec toRepr_ (indent : int) (dv : Dval) : string =
    let makeSpaces len = "".PadRight(len, ' ')
    let nl = "\n" + makeSpaces indent
    let inl = "\n" + makeSpaces (indent + 2)
    let indent = indent + 2
    let typename = dvalTypeName dv
    let wrap str = $"<{typename}: {str}>"
    let justType = $"<{typename}>"

    match dv with
    | DPassword _ -> "<password>"
    | DString s -> $"\"{s}\""
    | DChar c -> $"'{c}'"
    | DInt i -> string i
    | DBool true -> "true"
    | DBool false -> "false"
    | DFloat f ->
      if System.Double.IsPositiveInfinity f then
        "Infinity"
      else if System.Double.IsNegativeInfinity f then
        "-Infinity"
      else if System.Double.IsNaN f then
        "NaN"
      else
        let result = sprintf "%.12g" f
        if result.Contains "." then result else $"{result}.0"
    | DUnit -> "unit"
    | DFnVal _ ->
      // TODO: we should print this, as this use case is safe
      // See docs/dblock-serialization.ml
      justType
    | DIncomplete _ -> justType
    | DError (_, msg) -> $"<error: {msg}>"
    | DDateTime d -> wrap (DarkDateTime.toIsoString d)
    | DDB name -> wrap name
    | DUuid uuid -> wrap (string uuid)
    | DList l ->
      if List.isEmpty l then
        "[]"
      else
        let elems = String.concat ", " (List.map (toRepr_ indent) l)
        $"[{inl}{elems}{nl}]"
    | DTuple (first, second, theRest) ->
      let l = [ first; second ] @ theRest
      let elems = String.concat ", " (List.map (toRepr_ indent) l)
      $"({inl}{elems}{nl})"
    | DRecord (typeName, o) ->
      let strs =
        o
        |> Map.toList
        |> List.map (fun (key, value) -> ($"{key}: {toRepr_ indent value}"))

      let elems = String.concat $",{inl}" strs
      let typeStr = FQTypeName.toString typeName
      $"{typeStr} {{" + $"{inl}{elems}{nl}" + "}"
    | DDict o ->
      if Map.isEmpty o then
        "{}"
      else
        let strs =
          o
          |> Map.toList
          |> List.map (fun (key, value) -> ($"{key}: {toRepr_ indent value}"))

        let elems = String.concat $",{inl}" strs
        "{" + $"{inl}{elems}{nl}" + "}"
    | DOption None -> "Nothing"
    | DOption (Some dv) -> "Just " + toRepr_ indent dv
    | DResult (Ok dv) -> "Ok " + toRepr_ indent dv
    | DResult (Error dv) -> "Error " + toRepr_ indent dv
    | DBytes bytes -> Base64.defaultEncodeToString bytes
    | DEnum (typeName, caseName, fields) ->
      let fieldStr =
        fields |> List.map (fun value -> toRepr_ indent value) |> String.concat ", "

      let typeStr = FQTypeName.toString typeName
      $"{typeStr}.{caseName}({fieldStr})"


  toRepr_ 0 dv
