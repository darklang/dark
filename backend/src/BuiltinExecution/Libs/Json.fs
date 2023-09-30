module BuiltinExecution.Libs.Json

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module DarkDateTime = LibExecution.DarkDateTime
module VT = ValueType
module Dval = LibExecution.Dval


// parsing
let parseJson (s : string) : JsonElement =
  let mutable options = new JsonDocumentOptions()
  options.CommentHandling <- JsonCommentHandling.Skip
  options.MaxDepth <- System.Int32.MaxValue // infinite

  JsonDocument.Parse(s, options).RootElement


// serialization
let writeJson (f : Utf8JsonWriter -> Ply<unit>) : Ply<string> =
  uply {
    let mutable options = new JsonWriterOptions()
    options.Indented <-
      // TODO: `true` here makes it hard to write tests, because the output is
      // spread across multiple lines.
      //true
      false
    options.SkipValidation <- true
    let encoder =
      System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    options.Encoder <- encoder

    let stream = new System.IO.MemoryStream()
    let w = new Utf8JsonWriter(stream, options)
    do! f w
    w.Flush()

    return UTF8.ofBytesUnsafe (stream.ToArray())
  }

type Utf8JsonWriter with

  member this.writeObject(f : unit -> Ply<unit>) =
    uply {
      this.WriteStartObject()
      do! f ()
      this.WriteEndObject()
    }

  member this.writeArray(f : unit -> Ply<unit>) =
    uply {
      this.WriteStartArray()
      do! f ()
      this.WriteEndArray()
    }


module RuntimeError =
  module RT2DT = LibExecution.RuntimeTypesToDarkTypes
  type Error =
    /// In the future, we will add a trait to indicate types which can be serialized. For
    /// now, we'll raise a RuntimeError instead if any of those types are present.
    /// Helpfully, this allows us keep `serialize` from having to return an Error.
    | UnsupportedType of TypeReference
  let toRuntimeError (e : Error) : Ply<RuntimeError> =
    uply {
      let (caseName, fields) =
        match e with
        | UnsupportedType typ ->
          let typ = RT2DT.TypeReference.toDT typ
          "UnsupportedType", [ typ ]

      let typeName = RuntimeError.name [ "Json" ] "Error" 0
      return!
        Dval.enum typeName typeName (Some []) caseName fields
        |> Ply.map RuntimeError.jsonError
    }

  let raiseUnsupportedType (typ : TypeReference) : Ply<'a> =
    UnsupportedType(typ) |> toRuntimeError |> Ply.map raiseUntargetedRTE


module JsonPath =
  module Part =
    type Part =
      | Root
      | Index of int
      | Field of string
    let typeName =
      TypeName.fqPackage
        "Darklang"
        [ "Stdlib"; "Json"; "ParseError"; "JsonPath"; "Part" ]
        "Part"
        0

    let toDT (part : Part) : Ply<Dval> =
      let (caseName, fields) =
        match part with
        | Root -> "Root", []
        | Index i -> "Index", [ DInt(int64 i) ]
        | Field s -> "Field", [ DString s ]
      Dval.enum typeName typeName (Some []) caseName fields


  type JsonPath = List<Part.Part>

  let toDT (path : JsonPath) : Ply<Dval> =
    path |> Ply.List.mapSequentially Part.toDT |> Ply.map (Dval.list VT.unknownTODO)



let rec serialize
  (types : Types)
  (w : Utf8JsonWriter)
  (typ : TypeReference)
  (dv : Dval)
  : Ply<unit> =
  let r = serialize types w
  uply {
    match typ, dv with
    // basic types
    | TUnit, DUnit -> w.WriteNullValue()

    | TBool, DBool b -> w.WriteBooleanValue b

    | TInt, DInt i ->
      // CLEANUP if the number is outside the range, store as a string?
      w.WriteNumberValue i

    | TFloat, DFloat f ->
      if System.Double.IsNaN f then
        w.WriteStringValue "NaN"
      else if System.Double.IsNegativeInfinity f then
        w.WriteStringValue "-Infinity"
      else if System.Double.IsPositiveInfinity f then
        w.WriteStringValue "Infinity"
      else
        let result = sprintf "%.16g" f
        let result = if result.Contains "." then result else $"{result}.0"
        w.WriteRawValue result

    | TChar, DChar c -> w.WriteStringValue c
    | TString, DString s -> w.WriteStringValue s

    | TDateTime, DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

    | TUuid, DUuid uuid -> w.WriteStringValue(string uuid)

    | TBytes, DBytes bytes ->
      bytes |> Base64.defaultEncodeToString |> w.WriteStringValue


    // Nested types
    | TList ltype, DList(_, l) ->
      do! w.writeArray (fun () -> Ply.List.iterSequentially (r ltype) l)

    | TDict dictType, DDict(_vtTODO, fields) ->
      do!
        w.writeObject (fun () ->
          fields
          |> Map.toList
          |> Ply.List.iterSequentially (fun (k, v) ->
            uply {
              w.WritePropertyName k
              do! r dictType v
            }))

    | TTuple(t1, t2, trest), DTuple(d1, d2, rest) ->
      let zipped = List.zip (t1 :: t2 :: trest) (d1 :: d2 :: rest)
      do!
        w.writeArray (fun () ->
          Ply.List.iterSequentially (fun (t, d) -> r t d) zipped)

    | TCustomType(Ok typeName, typeArgs), dval ->

      match! Types.find typeName types with
      | None -> Exception.raiseInternal "Couldn't find type" [ "typeName", typeName ]
      | Some decl ->

        match decl.definition with
        | TypeDeclaration.Alias typ ->
          let typ = Types.substitute decl.typeParams typeArgs typ
          return! r typ dv

        | TypeDeclaration.Enum cases ->
          match dval with
          | DEnum(dTypeName, _, _typeArgsDEnumTODO, caseName, fields) ->
            let matchingCase =
              cases
              |> NEList.find (fun c -> c.name = caseName)
              |> Exception.unwrapOptionInternal
                "Couldn't find matching case"
                [ "typeName", dTypeName ]

            do!
              w.writeObject (fun () ->
                w.WritePropertyName caseName
                w.writeArray (fun () ->
                  List.zip matchingCase.fields fields
                  |> Ply.List.iterSequentially (fun (fieldType, fieldVal) ->
                    let typ = Types.substitute decl.typeParams typeArgs fieldType
                    r typ fieldVal)))

          | _ -> Exception.raiseInternal "Expected a DEnum but got something else" []

        | TypeDeclaration.Record fields ->
          match dval with
          | DRecord(_, _, _typeArgsTODO, dvalMap) ->
            do!
              w.writeObject (fun () ->
                dvalMap
                |> Map.toList
                |> Ply.List.iterSequentially (fun (fieldName, dval) ->
                  w.WritePropertyName fieldName

                  let matchingFieldDef =
                    fields
                    |> NEList.find (fun def -> def.name = fieldName)
                    |> Exception.unwrapOptionInternal
                      "Couldn't find matching field"
                      [ "fieldName", fieldName ]

                  let typ =
                    Types.substitute decl.typeParams typeArgs matchingFieldDef.typ
                  r typ dval))
          | _ ->
            Exception.raiseInternal
              "Expected a DRecord but got something else"
              [ "actualDval", dval
                "actualType", LibExecution.DvalReprDeveloper.toTypeName dval
                "expectedType", typeName
                "expectedFields", fields ]


    | TCustomType(Error err, _typeArgs), dval -> raiseUntargetedRTE err


    // Not supported
    | TVariable _, _
    | TFn _, _
    | TDB _, _ -> return! RuntimeError.raiseUnsupportedType typ


    // Exhaust the types
    | TUnit, _
    | TBool, _
    | TInt, _
    | TFloat, _
    | TChar, _
    | TString, _
    | TUuid, _
    | TBytes, _
    | TDateTime, _
    | TList _, _
    | TTuple _, _
    | TDB _, _
    | TCustomType _, _
    | TDict _, _ ->
      // Internal error as this shouldn't get past the typechecker
      Exception.raiseInternal
        "Can't serialize this type/value combination"
        [ "value", dv; "type", DString(LibExecution.DvalReprDeveloper.typeName typ) ]
  }

module ParseError =
  module RT2DT = LibExecution.RuntimeTypesToDarkTypes
  let typeName =
    TypeName.fqPackage "Darklang" [ "Stdlib"; "Json"; "ParseError" ] "ParseError" 0
  type ParseError =
    /// The json string can't be parsed as the given type.
    | CantMatchWithType of TypeReference * string * JsonPath.JsonPath
    | EnumExtraField of rawJson : string * JsonPath.JsonPath
    | EnumMissingField of TypeReference * index : int * JsonPath.JsonPath
    /// Provided caseName doesn't match any caseName the enum has
    | EnumInvalidCasename of TypeReference * caseName : string * JsonPath.JsonPath
    /// More than one label in a record expecting one
    | EnumTooManyCases of
      TypeReference *
      caseNames : List<string> *
      JsonPath.JsonPath
    | RecordMissingField of fieldName : string * JsonPath.JsonPath
    | RecordDuplicateField of fieldName : string * JsonPath.JsonPath
    | NotJson

  exception JsonException of ParseError

  let toDT (e : ParseError) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match e with
          | NotJson -> return "NotJson", []
          | CantMatchWithType(typ, json, errorPath) ->
            let typ = RT2DT.TypeReference.toDT typ
            let! errorPath = JsonPath.toDT errorPath
            return "CantMatchWithType", [ typ; DString json; errorPath ]
          | EnumExtraField(json, errorPath) ->
            let! errorPath = JsonPath.toDT errorPath
            return "EnumExtraField", [ DString json; errorPath ]
          | EnumMissingField(typ, fieldCount, errorPath) ->
            let typ = RT2DT.TypeReference.toDT typ
            let! errorPath = JsonPath.toDT errorPath
            return "EnumMissingField", [ typ; DInt(int64 fieldCount); errorPath ]
          | EnumInvalidCasename(typ, caseName, errorPath) ->
            let typ = RT2DT.TypeReference.toDT typ
            let! errorPath = JsonPath.toDT errorPath
            return "EnumInvalidCasename", [ typ; DString caseName; errorPath ]
          | EnumTooManyCases(typ, cases, errorPath) ->
            let typ = RT2DT.TypeReference.toDT typ
            let cases = cases |> List.map (fun s -> DString s) |> Dval.list VT.string
            let! errorPath = JsonPath.toDT errorPath
            return "EnumTooManyCases", [ typ; cases; errorPath ]
          | RecordMissingField(fieldName, errorPath) ->
            let! errorPath = JsonPath.toDT errorPath
            return "RecordMissingField", [ DString fieldName; errorPath ]
          | RecordDuplicateField(fieldName, errorPath) ->
            let! errorPath = JsonPath.toDT errorPath
            return "RecordDuplicateField", [ DString fieldName; errorPath ]
        }

      return! Dval.enum typeName typeName (Some []) caseName fields
    }

let raiseError (e : ParseError.ParseError) : 'a = raise (ParseError.JsonException e)


let raiseCantMatchWithType
  (typ : TypeReference)
  (j : JsonElement)
  (path : JsonPath.JsonPath)
  : 'a =
  ParseError.CantMatchWithType(typ, j.GetRawText(), path) |> raiseError

let raiseEnumMissingField
  (typ : TypeReference)
  (fieldCount : int)
  (path : JsonPath.JsonPath)
  : 'a =
  ParseError.EnumMissingField(typ, fieldCount, path) |> raiseError

let raiseEnumExtraField (j : JsonElement) (path : JsonPath.JsonPath) =
  ParseError.EnumExtraField(j.GetRawText(), path) |> raiseError

let raiseEnumInvalidCasename
  (typ : TypeReference)
  (caseName : string)
  (path : JsonPath.JsonPath)
  : 'a =
  ParseError.EnumInvalidCasename(typ, caseName, path) |> raiseError

let raiseEnumTooManyCases
  (typ : TypeReference)
  (caseNames : List<string>)
  (path : JsonPath.JsonPath)
  : 'a =
  ParseError.EnumTooManyCases(typ, caseNames, path) |> raiseError





let parse
  (types : Types)
  (typ : TypeReference)
  (str : string)
  : Ply<Result<Dval, ParseError.ParseError>> =
  let err = raiseError

  let rec convert
    (typ : TypeReference)
    (pathSoFar : JsonPath.JsonPath)
    (j : JsonElement)
    : Ply<Dval> =
    match typ, j.ValueKind with
    // basic types
    | TUnit, JsonValueKind.Null -> DUnit |> Ply

    | TBool, JsonValueKind.True -> DBool true |> Ply
    | TBool, JsonValueKind.False -> DBool false |> Ply

    | TInt, JsonValueKind.Number ->
      let mutable i64 = 0L
      let mutable ui64 = 0UL
      let mutable d = 0.0
      // dotnet will wrap 9223372036854775808 to be -9223372036854775808 instead, we
      // don't want that and will error instead
      if j.TryGetUInt64(&ui64) then
        if ui64 <= uint64 System.Int64.MaxValue then
          DInt(int64 ui64) |> Ply
        else
          raiseCantMatchWithType TInt j pathSoFar |> Ply
      else if j.TryGetInt64(&i64) then
        DInt i64 |> Ply
      // We allow the user to specify numbers in int or float format (e.g. 1 or 1.0
      // or even 1E+0) -- JSON uses floating point numbers, and the person/API
      // client/server that is creating a field we understand to be an int may choose
      // to print an int in a floating point format.
      else if
        j.TryGetDouble(&d)
        && d <= (float System.Int64.MaxValue)
        && d >= (float System.Int64.MinValue)
        && System.Double.IsInteger d
      then
        int64 d |> DInt |> Ply
      else
        raiseCantMatchWithType TInt j pathSoFar |> Ply

    | TFloat, JsonValueKind.Number -> j.GetDouble() |> DFloat |> Ply
    | TFloat, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat System.Double.NaN
      | "Infinity" -> DFloat System.Double.PositiveInfinity
      | "-Infinity" -> DFloat System.Double.NegativeInfinity
      | _ -> raiseCantMatchWithType TFloat j pathSoFar
      |> Ply

    | TChar, JsonValueKind.String ->
      match String.toEgcChar (j.GetString()) with
      | Some c -> Ply(DChar c)
      | None -> raiseCantMatchWithType TChar j pathSoFar


    | TString, JsonValueKind.String -> DString(j.GetString()) |> Ply

    | TBytes, JsonValueKind.String ->
      j.GetString() |> Base64.decodeFromString |> DBytes |> Ply

    | TUuid, JsonValueKind.String ->
      try
        DUuid(System.Guid(j.GetString())) |> Ply
      with _ ->
        raiseCantMatchWithType TUuid j pathSoFar

    | TDateTime, JsonValueKind.String ->
      try
        j.GetString()
        |> NodaTime.Instant.ofIsoString
        |> DarkDateTime.fromInstant
        |> DDateTime
        |> Ply
      with _ ->
        raiseCantMatchWithType TDateTime j pathSoFar


    // Nested types

    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray()
      |> Seq.mapi (fun i v -> convert nested (JsonPath.Part.Index i :: pathSoFar) v)
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (Dval.list VT.unknownTODO)

    | TTuple(t1, t2, rest), JsonValueKind.Array ->
      let values = j.EnumerateArray() |> Seq.toList
      let types = t1 :: t2 :: rest
      if values.Length <> types.Length then raiseCantMatchWithType typ j pathSoFar

      List.zip types values
      |> List.mapi (fun i (t, v) -> convert t (JsonPath.Part.Index i :: pathSoFar) v)
      |> Ply.List.flatten
      |> Ply.map (fun mapped ->
        match mapped with
        | (d1 :: d2 :: rest) -> DTuple(d1, d2, rest)
        | _ ->
          Exception.raiseInternal "Shouldn't be possible due to length check" [])

    | TDict tDict, JsonValueKind.Object ->
      j.EnumerateObject()
      |> Seq.map (fun jp ->
        uply {
          let! converted =
            convert tDict (JsonPath.Part.Field jp.Name :: pathSoFar) jp.Value
          return (jp.Name, converted)
        })
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (Dval.dict VT.unknownTODO)

    | TCustomType(Ok typeName, typeArgs), jsonValueKind ->
      uply {
        match! Types.find typeName types with
        | None ->
          return
            Exception.raiseInternal "Couldn't find type" [ "typeName", typeName ]

        | Some decl ->
          match decl.definition with
          | TypeDeclaration.Alias alias ->
            let aliasType = Types.substitute decl.typeParams typeArgs alias
            return! convert aliasType pathSoFar j

          | TypeDeclaration.Enum cases ->
            if jsonValueKind <> JsonValueKind.Object then
              do! raiseCantMatchWithType typ j pathSoFar

            let enumerated =
              j.EnumerateObject()
              |> Seq.map (fun jp -> (jp.Name, jp.Value))
              |> Seq.toList

            match enumerated with
            | [ (caseName, j) ] ->
              let matchingCase =
                match cases |> NEList.find (fun c -> c.name = caseName) with
                | Some c -> c
                | None -> raiseEnumInvalidCasename typ caseName pathSoFar

              let j = j.EnumerateArray() |> Seq.toList


              let casePath = JsonPath.Part.Field caseName :: pathSoFar

              // If the field count is off, process whatever fields make sense
              // and then error afterwards
              let expectedFieldCount = List.length matchingCase.fields
              let actualFieldCount = List.length j
              let maxFields = min expectedFieldCount actualFieldCount
              let shortExpectedFields = List.take maxFields matchingCase.fields
              let shortActualFields = List.take maxFields j

              let! fields =
                List.zip shortExpectedFields shortActualFields
                |> List.mapWithIndex (fun i (typ, j) ->
                  let path = JsonPath.Part.Index i :: casePath
                  let typ = Types.substitute decl.typeParams typeArgs typ
                  convert typ path j)
                |> Ply.List.flatten

              if expectedFieldCount > actualFieldCount then
                let index = actualFieldCount // one higher than greatest index
                let expectedType =
                  List.getAt index matchingCase.fields
                  |> Exception.unwrapOptionInternal
                    "Can't find expected field"
                    [ "index", index
                      "expectedFields", matchingCase.fields
                      "actualFields", j ]
                return
                  raiseError (
                    ParseError.EnumMissingField(expectedType, index, casePath)
                  )
              else if expectedFieldCount < actualFieldCount then
                let index = expectedFieldCount // one higher than greatest index
                let fieldJson =
                  List.getAt index j
                  |> Exception.unwrapOptionInternal
                    "Can't find actual field"
                    [ "index", index
                      "expectedFields", matchingCase.fields
                      "actualFields", j ]
                let path = JsonPath.Part.Index index :: casePath
                return raiseEnumExtraField fieldJson path
              else
                return! Dval.enum typeName typeName VT.typeArgsTODO' caseName fields

            | [] -> return raiseCantMatchWithType typ j pathSoFar
            | cases ->
              let caseNames = List.map Tuple2.first cases
              return raiseEnumTooManyCases typ caseNames pathSoFar

          | TypeDeclaration.Record fields ->
            if jsonValueKind <> JsonValueKind.Object then
              do! raiseCantMatchWithType typ j pathSoFar

            let enumerated = j.EnumerateObject() |> Seq.toList

            // We allow the user to add extra fields to a record, but we don't allow
            // fields to be omitted, so use the definition to get the expected fields
            // and then look at the json to match it
            let! fields =
              fields
              |> NEList.toList
              |> List.map (fun def ->
                uply {
                  let correspondingValue =
                    let matchingFieldDef =
                      // TODO: allow Option<>al fields to be omitted
                      enumerated |> List.filter (fun v -> v.Name = def.name)

                    match matchingFieldDef with
                    | [] -> err (ParseError.RecordMissingField(def.name, pathSoFar))
                    | [ matchingFieldDef ] -> matchingFieldDef.Value
                    | _ ->
                      err (ParseError.RecordDuplicateField(def.name, pathSoFar))

                  let typ = Types.substitute decl.typeParams typeArgs def.typ
                  let! converted =
                    convert
                      typ
                      (JsonPath.Part.Field def.name :: pathSoFar)
                      correspondingValue
                  return (def.name, converted)
                })
              |> Ply.List.flatten

            return! Dval.record typeName VT.typeArgsTODO' fields
      }


    // Explicitly not supported
    | TVariable _, _
    | TFn _, _
    | TDB _, _ -> RuntimeError.raiseUnsupportedType typ


    // exhaust TypeReferences
    | TUnit, _
    | TBool, _
    | TInt, _
    | TFloat, _
    | TChar, _
    | TString, _
    | TUuid, _
    | TBytes, _
    | TDateTime, _
    | TList _, _
    | TTuple _, _
    | TCustomType _, _
    | TDict _, _ -> raiseCantMatchWithType typ j pathSoFar

  let parsed =
    try
      Ok(parseJson str)
    with _ex ->
      Error ParseError.NotJson

  match parsed with
  | Error err -> Error err |> Ply
  | Ok parsed ->
    uply {
      try
        let! converted = convert typ [ JsonPath.Part.Root ] parsed
        return Ok converted
      with ParseError.JsonException ex ->
        return Error ex
    }


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Json" ]

let fns : List<BuiltInFn> =
  [ { name = fn "serialize" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "arg" (TVariable "a") "" ]
      returnType = TString
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | state, [ typeToSerializeAs ], [ arg ] ->
          uply {
            // TODO: somehow collect list of TVariable -> TypeReference
            // "'b = Int",
            // so we can Json.serialize<'b>, if 'b is in the surrounding context

            let types = ExecutionState.availableTypes state
            let! response =
              writeJson (fun w -> serialize types w typeToSerializeAs arg)
            return DString response
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "json" TString "" ]
      returnType =
        TypeReference.result
          (TVariable "a")
          (TCustomType(Ok ParseError.typeName, []))
      description =
        "Parses a JSON string <param json> as a Dark value, matching the type <typeParam a>"
      fn =
        let resultOk = Dval.resultOk VT.unknownTODO VT.string
        let resultError =
          Dval.resultError
            VT.unknownTODO
            (VT.known (KTCustomType(ParseError.typeName, [])))
        (function
        | state, [ typeArg ], [ DString arg ] ->
          let types = ExecutionState.availableTypes state
          uply {
            match! parse types typeArg arg with
            | Ok v -> return resultOk v
            | Error e ->
              let! dval = ParseError.toDT e
              return resultError dval
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let contents = (fns, types, constants)
