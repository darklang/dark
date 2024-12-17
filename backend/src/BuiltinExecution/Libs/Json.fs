module BuiltinExecution.Libs.Json

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module DarkDateTime = LibExecution.DarkDateTime
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module PackageIDs = LibExecution.PackageIDs
module RTE = RuntimeError


// parsing
let parseJson (s : string) : JsonElement =
  let options =
    new JsonDocumentOptions(
      CommentHandling = JsonCommentHandling.Skip,
      MaxDepth = System.Int32.MaxValue // infinite
    )

  JsonDocument.Parse(s, options).RootElement


// serialization
let writeJson (f : Utf8JsonWriter -> unit) : string =
  let options =
    new JsonWriterOptions(
      // TODO: `true` here would make it hard to write tests...
      Indented = false,
      SkipValidation = true,
      Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

  let stream = new System.IO.MemoryStream()
  let w = new Utf8JsonWriter(stream, options)
  f w
  w.Flush()

  UTF8.ofBytesUnsafe (stream.ToArray())

type Utf8JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEndObject()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEndArray()



module JsonPath =
  module Part =
    type Part =
      | Root
      | Index of int
      | Field of string

    let typeName =
      FQTypeName.fqPackage PackageIDs.Type.Stdlib.Json.ParseError.JsonPath.part

    let toDT (part : Part) : Dval =
      let (caseName, fields) =
        match part with
        | Root -> "Root", []
        | Index i -> "Index", [ DInt64(int64 i) ]
        | Field s -> "Field", [ DString s ]
      DEnum(typeName, typeName, [], caseName, fields)


  type JsonPath = List<Part.Part>

  let toDT (path : JsonPath) : Dval =
    path |> List.map Part.toDT |> (fun parts -> DList(VT.unknownTODO, parts))



let rec serialize (threadID : ThreadID) (w : Utf8JsonWriter) (dv : Dval) : unit =
  let r = serialize threadID w
  match dv with
  // basic types
  | DUnit -> w.WriteNullValue()

  | DBool b -> w.WriteBooleanValue b

  | DInt8 i -> w.WriteNumberValue i
  | DUInt8 i -> w.WriteNumberValue i
  | DInt16 i -> w.WriteNumberValue i
  | DUInt16 i -> w.WriteNumberValue i
  | DInt32 i -> w.WriteNumberValue i
  | DUInt32 i -> w.WriteNumberValue i
  | DInt64 i -> w.WriteNumberValue i
  | DUInt64 i -> w.WriteNumberValue i
  | DInt128 i -> w.WriteRawValue(i.ToString())
  | DUInt128 i -> w.WriteRawValue(i.ToString())

  | DFloat f ->
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

  | DChar c -> w.WriteStringValue c
  | DString s -> w.WriteStringValue s

  | DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

  | DUuid uuid -> w.WriteStringValue(string uuid)

  // Nested types
  | DTuple(d1, d2, rest) -> w.writeArray (fun () -> List.iter r (d1 :: d2 :: rest))

  | DList(_, items) -> w.writeArray (fun () -> List.iter r items)

  | DDict(_, fields) ->
    w.writeObject (fun () ->
      fields
      |> Map.toList
      |> List.iter (fun (k, v) ->
        w.WritePropertyName k
        r v))

  // Enums and Records
  | DEnum(_, _, _, caseName, fields) ->
    w.writeObject (fun () ->
      w.WritePropertyName caseName
      w.writeArray (fun () -> fields |> List.iter r))

  | DRecord(_, _, _, fields) ->
    w.writeObject (fun () ->
      fields
      |> Map.toList
      |> List.iter (fun (fieldName, dval) ->
        w.WritePropertyName fieldName
        r dval))

  // Not supported
  | DDB _
  | DApplicable _ ->
    (RTE.Jsons.CannotSerializeValue dv) |> RTE.Json |> raiseRTE threadID

module ParseError =
  module RT2DT = LibExecution.RuntimeTypesToDarkTypes
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.Stdlib.Json.ParseError.parseError

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

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | NotJson -> "NotJson", []

      | CantMatchWithType(typ, json, errorPath) ->
        "CantMatchWithType",
        [ RT2DT.TypeReference.toDT typ; DString json; JsonPath.toDT errorPath ]

      | EnumExtraField(json, errorPath) ->
        "EnumExtraField", [ DString json; JsonPath.toDT errorPath ]

      | EnumMissingField(typ, fieldCount, errorPath) ->
        "EnumMissingField",
        [ RT2DT.TypeReference.toDT typ
          DInt64(int64 fieldCount)
          JsonPath.toDT errorPath ]

      | EnumInvalidCasename(typ, caseName, errorPath) ->
        "EnumInvalidCasename",
        [ RT2DT.TypeReference.toDT typ; DString caseName; JsonPath.toDT errorPath ]

      | EnumTooManyCases(typ, cases, errorPath) ->
        "EnumTooManyCases",
        [ RT2DT.TypeReference.toDT typ
          cases |> List.map DString |> Dval.list KTString
          JsonPath.toDT errorPath ]

      | RecordMissingField(fieldName, errorPath) ->
        "RecordMissingField", [ DString fieldName; JsonPath.toDT errorPath ]

      | RecordDuplicateField(fieldName, errorPath) ->
        "RecordDuplicateField", [ DString fieldName; JsonPath.toDT errorPath ]

    DEnum(typeName, typeName, [], caseName, fields)

let raiseError (e : ParseError.ParseError) : 'a = raise (ParseError.JsonException e)

let raiseCantMatchWithType
  (typ : TypeReference)
  (j : JsonElement)
  (path : JsonPath.JsonPath)
  : 'a =
  ParseError.CantMatchWithType(typ, j.GetRawText(), path) |> raiseError



let parse
  (threadID : ThreadID)
  (types : Types)
  (typ : TypeReference)
  (str : string)
  : Ply<Result<Dval, ParseError.ParseError>> =

  let tst = Map.empty // TODO consider passing this in.. somehow?

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

    | TInt64, JsonValueKind.Number ->
      let mutable i64 = 0L
      let mutable ui64 = 0UL
      let mutable d = 0.0
      // dotnet will wrap 9223372036854775808 to be -9223372036854775808 instead, we
      // don't want that and will error instead

      if j.TryGetUInt64(&ui64) then
        if ui64 <= uint64 System.Int64.MaxValue then
          DInt64(int64 ui64) |> Ply
        else
          raiseCantMatchWithType TInt64 j pathSoFar |> Ply
      else if j.TryGetInt64(&i64) then
        DInt64 i64 |> Ply
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
        int64 d |> DInt64 |> Ply
      else
        raiseCantMatchWithType TInt64 j pathSoFar |> Ply

    | TUInt64, JsonValueKind.Number ->
      let mutable ui64 = 0UL
      let mutable d = 0.0
      if j.TryGetUInt64(&ui64) then
        DUInt64 ui64 |> Ply
      else if
        j.TryGetDouble(&d)
        && d <= (float System.UInt64.MaxValue)
        && d >= (float System.UInt64.MinValue)
        && System.Double.IsInteger d
      then
        uint64 d |> DUInt64 |> Ply
      else
        raiseCantMatchWithType TUInt64 j pathSoFar |> Ply

    | TInt8, JsonValueKind.Number ->
      let mutable i64 = 0L
      let mutable ui64 = 0UL
      let mutable d = 0.0
      if
        j.TryGetUInt64(&ui64)
        && ui64 >= uint64 System.SByte.MinValue
        && ui64 <= uint64 System.SByte.MaxValue
      then
        DInt8(int8 ui64) |> Ply

      else if
        j.TryGetInt64(&i64)
        && i64 >= int System.SByte.MinValue
        && i64 <= int System.SByte.MaxValue
      then
        DInt8(int8 i64) |> Ply

      else if
        j.TryGetDouble(&d)
        && d <= (float System.SByte.MaxValue)
        && d >= (float System.SByte.MinValue)
        && System.Double.IsInteger d
      then
        int8 d |> DInt8 |> Ply
      else
        raiseCantMatchWithType TInt8 j pathSoFar |> Ply

    | TUInt8, JsonValueKind.Number ->
      let mutable i64 = 0L
      let mutable ui64 = 0UL
      let mutable d = 0.0

      if j.TryGetUInt64(&ui64) && ui64 <= uint64 System.Byte.MaxValue then
        DUInt8(uint8 ui64) |> Ply

      else if
        j.TryGetInt64(&i64)
        && i64 >= int System.Byte.MinValue
        && i64 <= int System.Byte.MaxValue
      then
        DUInt8(uint8 i64) |> Ply

      else if
        j.TryGetDouble(&d)
        && d <= (float System.Byte.MaxValue)
        && d >= (float System.Byte.MinValue)
        && System.Double.IsInteger d
      then
        uint8 d |> DUInt8 |> Ply
      else
        raiseCantMatchWithType TUInt8 j pathSoFar |> Ply

    | TInt16, JsonValueKind.Number ->
      let mutable i16 = 0s
      let mutable d = 0.0

      if j.TryGetInt16(&i16) then
        DInt16 i16 |> Ply
      else if
        j.TryGetDouble(&d)
        && d <= (float System.Int16.MaxValue)
        && d >= (float System.Int16.MinValue)
        && System.Double.IsInteger d
      then
        int16 d |> DInt16 |> Ply
      else
        raiseCantMatchWithType TInt16 j pathSoFar |> Ply

    | TUInt16, JsonValueKind.Number ->
      let mutable ui16 = 0us
      let mutable d = 0.0

      if j.TryGetUInt16(&ui16) then
        DUInt16 ui16 |> Ply
      else if
        j.TryGetDouble(&d)
        && d <= (float System.UInt16.MaxValue)
        && d >= (float System.UInt16.MinValue)
        && System.Double.IsInteger d
      then
        uint16 d |> DUInt16 |> Ply
      else
        raiseCantMatchWithType TUInt16 j pathSoFar |> Ply

    | TInt32, JsonValueKind.Number ->
      let mutable i32 = 0
      let mutable d = 0.0

      if j.TryGetInt32(&i32) then
        DInt32 i32 |> Ply
      else if
        j.TryGetDouble(&d)
        && d <= (float System.Int32.MaxValue)
        && d >= (float System.Int32.MinValue)
        && System.Double.IsInteger d
      then
        int32 d |> DInt32 |> Ply
      else
        raiseCantMatchWithType TInt32 j pathSoFar |> Ply

    | TUInt32, JsonValueKind.Number ->
      let mutable ui32 = 0ul
      let mutable d = 0.0

      if j.TryGetUInt32(&ui32) then
        DUInt32 ui32 |> Ply
      else if
        j.TryGetDouble(&d)
        && d <= (float System.UInt32.MaxValue)
        && d >= (float System.UInt32.MinValue)
        && System.Double.IsInteger d
      then
        uint32 d |> DUInt32 |> Ply
      else
        raiseCantMatchWithType TUInt32 j pathSoFar |> Ply


    | TInt128, JsonValueKind.Number ->
      let mutable i128 = System.Int128.Zero
      let mutable d = 0.0
      if System.Int128.TryParse(j.GetRawText(), &i128) then
        DInt128 i128 |> Ply
      else if j.TryGetDouble(&d) && System.Double.IsInteger d then
        try
          System.Int128.Parse(
            d.ToString("F0", System.Globalization.CultureInfo.InvariantCulture)
          )
          |> DInt128
          |> Ply
        with :? System.OverflowException ->
          raiseCantMatchWithType TInt128 j pathSoFar |> Ply
      else
        raiseCantMatchWithType TInt128 j pathSoFar |> Ply

    | TUInt128, JsonValueKind.Number ->
      let mutable ui128 = System.UInt128.Zero
      let mutable d = 0.0
      if System.UInt128.TryParse(j.GetRawText(), &ui128) then
        DUInt128 ui128 |> Ply
      else if j.TryGetDouble(&d) && System.Double.IsInteger d then
        try
          System.UInt128.Parse(
            d.ToString("F0", System.Globalization.CultureInfo.InvariantCulture)
          )
          |> DUInt128
          |> Ply
        with :? System.OverflowException ->
          raiseCantMatchWithType TUInt128 j pathSoFar |> Ply
      else
        raiseCantMatchWithType TUInt128 j pathSoFar |> Ply

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
      |> Ply.map (TypeChecker.DvalCreator.list threadID VT.unknownTODO)

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
      |> Ply.map (TypeChecker.DvalCreator.dict threadID VT.unknownTODO)

    | TCustomType(Ok typeName, typeArgs), jsonValueKind ->
      uply {
        let! typeArgsVT =
          typeArgs |> Ply.List.mapSequentially (TypeReference.toVT types tst)

        match! Types.find types typeName with
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
                | None ->
                  raiseError (
                    ParseError.EnumInvalidCasename(typ, caseName, pathSoFar)
                  )

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
                return
                  raiseError (
                    ParseError.EnumExtraField(fieldJson.GetRawText(), path)
                  )
              else
                let! enum =
                  TypeChecker.DvalCreator.enum
                    types
                    threadID
                    tst
                    typeName
                    typeArgsVT
                    caseName
                    fields
                return enum


            | [] -> return raiseCantMatchWithType typ j pathSoFar
            | cases ->
              let caseNames = List.map Tuple2.first cases
              return
                raiseError (ParseError.EnumTooManyCases(typ, caseNames, pathSoFar))

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
                    | [] ->
                      raiseError (
                        ParseError.RecordMissingField(def.name, pathSoFar)
                      )
                    | [ matchingFieldDef ] -> matchingFieldDef.Value
                    | _ ->
                      raiseError (
                        ParseError.RecordDuplicateField(def.name, pathSoFar)
                      )

                  let typ = Types.substitute decl.typeParams typeArgs def.typ
                  let! converted =
                    convert
                      typ
                      (JsonPath.Part.Field def.name :: pathSoFar)
                      correspondingValue
                  return (def.name, converted)
                })
              |> Ply.List.flatten

            let! record =
              TypeChecker.DvalCreator.record
                types
                threadID
                tst
                typeName
                typeArgsVT
                fields
            return record
      }


    // Explicitly not supported
    | TVariable _, _
    | TFn _, _
    | TDB _, _ -> (RTE.Jsons.UnsupportedType typ) |> RTE.Json |> raiseRTE threadID


    // exhaust TypeReferences
    | TUnit, _
    | TBool, _
    | TInt8, _
    | TUInt8, _
    | TInt16, _
    | TUInt16, _
    | TInt32, _
    | TUInt32, _
    | TInt128, _
    | TUInt128, _
    | TInt64, _
    | TUInt64, _
    | TFloat, _
    | TChar, _
    | TString, _
    | TUuid, _
    | TDateTime, _
    | TTuple _, _
    | TList _, _
    | TDict _, _
    | TCustomType _, _ -> raiseCantMatchWithType typ j pathSoFar

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


let fns : List<BuiltInFn> =
  [ { name = fn "jsonSerialize" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "arg" (TVariable "a") "" ]
      returnType = TString
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | _, vm, [ _typeToSerializeAs ], [ arg ] ->
          uply {
            let response = writeJson (fun w -> serialize vm.threadID w arg)
            return DString response
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "jsonParse" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "json" TString "" ]
      returnType =
        TypeReference.result
          (TVariable "a")
          (TCustomType(Ok ParseError.typeName, []))
      description =
        "Parses a JSON string <param json> as a Dark value, matching the type <typeParam a>"
      fn =
        (function
        | exeState, vm, [ typeArg ], [ DString arg ] ->
          let threadID = vm.threadID

          let okType = VT.unknownTODO // "a"
          let errType = KTCustomType(ParseError.typeName, []) |> VT.known
          let resultOk = TypeChecker.DvalCreator.Result.ok threadID okType errType
          let resultError =
            TypeChecker.DvalCreator.Result.error threadID okType errType

          uply {
            match! parse threadID exeState.types typeArg arg with
            | Ok v -> return resultOk v
            | Error e -> return resultError (ParseError.toDT e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
