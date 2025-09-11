module DvalReprDeveloper

open Prelude

open LibExecution.RuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PM = LibPackageManager.PackageManager

// CLEANUP: avoid using `Async.RunSynchronously` and try to handle the pretty printing in dark
let private fqTypeNameToString (typeName : FQTypeName.FQTypeName) : string =
  let getFQName fields =
    match Map.tryFind "name" fields with
    | Some(DRecord(_, _, _, nameFields)) ->
      let modules =
        nameFields
        |> Map.tryFind "modules"
        |> Option.bind (function
          | DList(_, mods) -> Some mods
          | _ -> None)
        |> Option.defaultValue []
        |> List.choose (function
          | DString s -> Some s
          | _ -> None)
        |> String.concat "."

      let name =
        nameFields
        |> Map.tryFind "name"
        |> Option.bind (function
          | DString n -> Some n
          | _ -> None)
        |> Option.defaultValue ""

      if modules = "" then name else modules + "." + name

    | _ -> ""

  let result =
    async {
      match typeName with
      | FQTypeName.Package hash ->
        let! typeOption = PM.pt.getType hash |> Ply.toTask |> Async.AwaitTask

        match typeOption with
        | Some packageType ->
          match PT2DT.PackageType.toDT packageType with
          | DRecord(_, _, _, fields) -> return getFQName fields
          | _ -> return $"Package {hash}"
        | None -> return $"Package {hash}"
    }
  Async.RunSynchronously result



let rec typeReference (t : TypeReference) : string =
  match t with
  | TUnit -> "Unit"
  | TBool -> "Bool"
  | TInt8 -> "Int8"
  | TUInt8 -> "UInt8"
  | TInt16 -> "Int16"
  | TUInt16 -> "UInt16"
  | TInt32 -> "Int32"
  | TUInt32 -> "UInt32"
  | TInt64 -> "Int64"
  | TUInt64 -> "UInt64"
  | TInt128 -> "Int128"
  | TUInt128 -> "UInt128"
  | TFloat -> "Float"
  | TChar -> "Char"
  | TString -> "String"
  | TDateTime -> "DateTime"
  | TUuid -> "Uuid"

  | TTuple(n1, n2, rest) ->
    (n1 :: n2 :: rest)
    |> List.map typeReference
    |> String.concat ", "
    |> fun s -> $"({s})"
  | TList nested -> $"List<{typeReference nested}>"
  | TDict nested -> $"Dict<{typeReference nested}>"

  | TFn _ -> "Function"

  | TCustomType(Error _nre, _) -> "(Error during function resolution)"
  | TCustomType(Ok t, typeArgs) ->
    let typeArgsPortion =
      match typeArgs with
      | [] -> ""
      | args ->
        args
        |> List.map (fun t -> typeReference t)
        |> String.concat ", "
        |> fun betweenBrackets -> "<" + betweenBrackets + ">"
    fqTypeNameToString t + typeArgsPortion

  | TDB _ -> "Datastore"
  | TVariable varname -> $"'{varname}"


let rec private knownType (vt : KnownType) : string =
  match vt with
  | KTUnit -> "Unit"
  | KTBool -> "Bool"
  | KTInt8 -> "Int8"
  | KTUInt8 -> "UInt8"
  | KTInt16 -> "Int16"
  | KTUInt16 -> "UInt16"
  | KTInt32 -> "Int32"
  | KTUInt32 -> "UInt32"
  | KTInt64 -> "Int64"
  | KTUInt64 -> "UInt64"
  | KTInt128 -> "Int128"
  | KTUInt128 -> "UInt128"
  | KTFloat -> "Float"
  | KTChar -> "Char"
  | KTString -> "String"
  | KTDateTime -> "DateTime"
  | KTUuid -> "Uuid"


  | KTTuple(t1, t2, trest) ->
    t1 :: t2 :: trest
    |> List.map valueType
    |> String.concat ", "
    |> fun s -> $"({s})"

  | KTList typ -> $"List<{valueType typ}>"

  | KTDict typ -> $"Dict<{valueType typ}>"

  | KTFn(argTypes, retType) ->
    (NEList.toList argTypes) @ [ retType ]
    |> List.map valueType
    |> String.concat " -> "

  | KTCustomType(name, typeArgs) ->
    let typeArgsPortion =
      match typeArgs with
      | [] -> ""
      | args ->
        args
        |> List.map (fun t -> valueType t)
        |> String.concat ", "
        |> fun betweenBrackets -> "<" + betweenBrackets + ">"

    fqTypeNameToString name + typeArgsPortion

  | KTDB typ -> $"Datastore<{valueType typ}>"


and private valueType (typ : ValueType) : string =
  match typ with
  | ValueType.Known typ -> knownType typ
  | ValueType.Unknown -> "_"


let toTypeName (dv : Dval) : string = dv |> Dval.toValueType |> valueType


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

    match dv with
    | DUnit -> "()"

    | DBool true -> "true"
    | DBool false -> "false"

    | DInt8 i -> string i
    | DUInt8 i -> string i
    | DInt16 i -> string i
    | DUInt16 i -> string i
    | DInt32 i -> string i
    | DUInt32 i -> string i
    | DInt64 i -> string i
    | DUInt64 i -> string i
    | DInt128 i -> string i
    | DUInt128 i -> string i

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

    | DChar c -> $"'{c}'"
    | DString s -> $"\"{s}\""

    | DDateTime d -> wrap (LibExecution.DarkDateTime.toIsoString d)
    | DUuid uuid -> wrap (string uuid)


    | DTuple(first, second, theRest) ->
      let l = [ first; second ] @ theRest
      let short = String.concat ", " (List.map (toRepr_ indent) l)

      if String.length short <= 80 then
        $"({short})"
      else
        let long = String.concat $"{inl}, " (List.map (toRepr_ indent) l)
        $"({inl}{long}{nl})"


    | DList(_, l) ->
      if List.isEmpty l then
        wrap "[]"
      else
        let elems = String.concat ", " (List.map (toRepr_ indent) l)
        $"[{inl}{elems}{nl}]"


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


    | DRecord(_, typeName, _typeArgsTODO, fields) ->
      let fields =
        fields
        |> Map.toList
        |> List.map (fun (key, value) -> ($"{key}: {toRepr_ indent value}"))

      let elems = String.concat $",{inl}" fields
      let typeStr = fqTypeNameToString typeName
      $"{typeStr} {{" + $"{inl}{elems}{nl}" + "}"


    | DEnum(_, typeName, typeArgs, caseName, fields) ->
      let typeArgsPart =
        match typeArgs with
        | [] -> ""
        | typeArgs ->
          typeArgs
          |> List.map valueType
          |> String.concat ", "
          |> fun parts -> $"<{parts}>"

      let short =
        let fieldStr =
          fields
          |> List.map (fun value -> toRepr_ indent value)
          |> String.concat ", "

        let fieldStr = if fieldStr = "" then "" else $"({fieldStr})"

        let typeStr = fqTypeNameToString typeName
        $"{typeStr}{typeArgsPart}.{caseName}{fieldStr}"

      if String.length short <= 80 then
        short
      else
        let fieldStr =
          fields
          |> List.map (fun value -> toRepr_ indent value)
          |> String.concat $",{inl}"

        let fieldStr = if fieldStr = "" then "" else $"({inl}{fieldStr}{nl})"

        let typeStr = fqTypeNameToString typeName
        $"{typeStr}{typeArgsPart}.{caseName}{fieldStr}"


    | DApplicable app ->
      // TODO we can do better here.
      match app with
      | AppLambda _impl -> "<lambda>"
      | AppNamedFn name -> $"<named fn {name.name}>"


    | DDB name -> wrap name

  toRepr_ 0 dv
