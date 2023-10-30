/// Ways of converting Dvals to strings, intended for developers to read
module LibExecution.DvalReprDeveloper

open Prelude

open RuntimeTypes

let rec typeName (t : TypeReference) : string =
  match t with
  | TInt -> "Int"
  | TInt8 -> "Int8"
  | TUInt8 -> "UInt8"
  | TInt16 -> "Int16"
  | TUInt16 -> "UInt16"
  | TInt128 -> "Int128"
  | TUInt128 -> "UInt128"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TUnit -> "Unit"
  | TChar -> "Char"
  | TString -> "String"
  | TList nested -> $"List<{typeName nested}>"
  | TTuple(n1, n2, rest) ->
    let nested = (n1 :: n2 :: rest) |> List.map typeName |> String.concat ", "
    $"({nested})"
  | TDict nested -> $"Dict<{typeName nested}>"
  | TFn _ -> "Function"
  | TVariable varname -> $"'{varname}"
  | TDB _ -> "Datastore"
  | TDateTime -> "DateTime"
  | TUuid -> "Uuid"
  | TCustomType(Ok t, typeArgs) ->
    let typeArgsPortion =
      match typeArgs with
      | [] -> ""
      | args ->
        args
        |> List.map (fun t -> typeName t)
        |> String.concat ", "
        |> fun betweenBrackets -> "<" + betweenBrackets + ">"
    TypeName.toString t + typeArgsPortion
  | TCustomType(Error _nre, _) -> "(Error during function resolution)"

  | TBytes -> "Bytes"

let rec knownTypeName (vt : KnownType) : string =
  match vt with
  | KTInt -> "Int"
  | KTInt8 -> "Int8"
  | KTUInt8 -> "UInt8"
  | KTInt16 -> "Int16"
  | KTUInt16 -> "UInt16"
  | KTInt128 -> "Int128"
  | KTUInt128 -> "UInt128"
  | KTFloat -> "Float"
  | KTBool -> "Bool"
  | KTUnit -> "Unit"
  | KTChar -> "Char"
  | KTString -> "String"
  | KTDateTime -> "DateTime"
  | KTUuid -> "Uuid"
  | KTBytes -> "Bytes"

  | KTList typ -> $"List<{valueTypeName typ}>"
  | KTDict typ -> $"Dict<{valueTypeName typ}>"
  | KTDB typ -> $"Datastore<{valueTypeName typ}>"

  | KTFn(argTypes, retType) ->
    (NEList.toList argTypes) @ [ retType ]
    |> List.map valueTypeName
    |> String.concat " -> "

  | KTTuple(t1, t2, trest) ->
    t1 :: t2 :: trest
    |> List.map valueTypeName
    |> String.concat ", "
    |> fun s -> $"({s})"

  | KTCustomType(name, typeArgs) ->
    let typeArgsPortion =
      match typeArgs with
      | [] -> ""
      | args ->
        args
        |> List.map (fun t -> valueTypeName t)
        |> String.concat ", "
        |> fun betweenBrackets -> "<" + betweenBrackets + ">"

    TypeName.toString name + typeArgsPortion

and valueTypeName (typ : ValueType) : string =
  match typ with
  | ValueType.Known typ -> knownTypeName typ
  | ValueType.Unknown -> "_"


let toTypeName (dv : Dval) : string = dv |> Dval.toValueType |> valueTypeName


// SERIALIZER_DEF Custom DvalReprDeveloper.toRepr
/// For printing something for the developer to read, as a live-value, error
/// message, etc.
///
/// Customers should not come to rely on this format. Do not use in stdlib fns
/// or other places a developer could rely on it (i.e. telemetry and error
/// messages are OK)
let toRepr (dv : Dval) : string =
  let rec toRepr_ (indent : int) (dv : Dval) : string =
    let makeSpaces len = "".PadRight(len, ' ')
    let nl = "\n" + makeSpaces indent
    let inl = "\n" + makeSpaces (indent + 2)
    let indent = indent + 2
    let typename = toTypeName dv
    let wrap str = $"<{typename}: {str}>"
    let justType = $"<{typename}>"

    match dv with
    | DString s -> $"\"{s}\""
    | DChar c -> $"'{c}'"
    | DInt i -> string i
    | DInt8 i -> string i
    | DUInt8 i -> string i
    | DInt16 i -> string i
    | DUInt16 i -> string i
    | DInt128 i -> string i
    | DUInt128 i -> string i
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
    | DUnit -> "()"
    | DFnVal _ ->
      // TODO: we should print this, as this use case is safe
      // See docs/dblock-serialization.md
      justType
    | DDateTime d -> wrap (DarkDateTime.toIsoString d)
    | DDB name -> wrap name
    | DUuid uuid -> wrap (string uuid)
    | DList(_, l) ->
      if List.isEmpty l then
        wrap "[]"
      else
        let elems = String.concat ", " (List.map (toRepr_ indent) l)
        $"[{inl}{elems}{nl}]"
    | DTuple(first, second, theRest) ->
      let l = [ first; second ] @ theRest
      let short = String.concat ", " (List.map (toRepr_ indent) l)

      if String.length short <= 80 then
        $"({short})"
      else
        let long = String.concat $"{inl}, " (List.map (toRepr_ indent) l)
        $"({inl}{long}{nl})"

    | DRecord(_, typeName, _typeArgsTODO, fields) ->
      let fields =
        fields
        |> Map.toList
        |> List.map (fun (key, value) -> ($"{key}: {toRepr_ indent value}"))

      let elems = String.concat $",{inl}" fields
      let typeStr = TypeName.toString typeName
      $"{typeStr} {{" + $"{inl}{elems}{nl}" + "}"

    | DDict(_valueTypeTODO, o) ->
      if Map.isEmpty o then
        "{}"
      else
        let strs =
          o
          |> Map.toList
          |> List.map (fun (key, value) -> ($"{key}: {toRepr_ indent value}"))

        let elems = String.concat $",{inl}" strs
        "{" + $"{inl}{elems}{nl}" + "}"
    | DBytes bytes -> Base64.defaultEncodeToString bytes
    | DEnum(_, typeName, typeArgs, caseName, fields) ->
      let typeArgsPart =
        match typeArgs with
        | [] -> ""
        | typeArgs ->
          typeArgs
          |> List.map ValueType.toString
          |> String.concat ", "
          |> fun parts -> $"<{parts}>"

      let short =
        let fieldStr =
          fields
          |> List.map (fun value -> toRepr_ indent value)
          |> String.concat ", "

        let fieldStr = if fieldStr = "" then "" else $"({fieldStr})"

        let typeStr = TypeName.toString typeName
        $"{typeStr}{typeArgsPart}.{caseName}{fieldStr}"

      if String.length short <= 80 then
        short
      else
        let fieldStr =
          fields
          |> List.map (fun value -> toRepr_ indent value)
          |> String.concat $",{inl}"

        let fieldStr = if fieldStr = "" then "" else $"({inl}{fieldStr}{nl})"

        let typeStr = TypeName.toString typeName
        $"{typeStr}{typeArgsPart}.{caseName}{fieldStr}"


  toRepr_ 0 dv
