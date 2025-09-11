module rec LibExecution.RuntimeTypesToDarkTypes

open Prelude

open RuntimeTypes

module VT = ValueType
module D = DvalDecoder
module C2DT = LibExecution.CommonToDarkTypes

// No hash conversion needed - all use Hash now

let ownerField m = m |> D.field "owner" |> D.string
let modulesField m = m |> D.field "modules" |> D.list D.string
let nameField m = m |> D.field "name" |> D.string
let versionField m = m |> D.field "version" |> D.int32


module FQTypeName =
  let typeName =
    FQTypeName.Package
      PackageIDs.Type.LanguageTools.RuntimeTypes.FQTypeName.fqTypeName
  let knownType = KTCustomType(typeName, [])

  module Package =
    let typeName =
      FQTypeName.Package
        PackageIDs.Type.LanguageTools.RuntimeTypes.FQTypeName.package

    let toDT (h : FQTypeName.Package) : Dval =
      let (Hash hashStr) = h
      DString hashStr

    let fromDT (d : Dval) : FQTypeName.Package =
      match d with
      | DString hashStr -> Hash hashStr
      | _ -> Exception.raiseInternal "Invalid FQTypeName.Package" []


  let toDT (u : FQTypeName.FQTypeName) : Dval =
    let (caseName, fields) =
      match u with
      | FQTypeName.Package u -> "Package", [ Package.toDT u ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : FQTypeName.FQTypeName =
    match d with
    | DEnum(_, _, [], "Package", [ u ]) -> FQTypeName.Package(Package.fromDT u)
    | _ -> Exception.raiseInternal "Invalid FQTypeName" []


module FQValueName =
  let typeName =
    FQTypeName.Package
      PackageIDs.Type.LanguageTools.RuntimeTypes.FQValueName.fqValueName
  let knownType = KTCustomType(typeName, [])

  module Builtin =
    let toDT (u : FQValueName.Builtin) : Dval =
      let fields = [ "name", DString u.name; "version", DInt32 u.version ]
      let typeName =
        FQTypeName.Package
          PackageIDs.Type.LanguageTools.RuntimeTypes.FQValueName.builtin
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : FQValueName.Builtin =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = nameField fields; version = versionField fields }
      | _ -> Exception.raiseInternal "Invalid FQValueName.Builtin" []

  module Package =
    let toDT (h : FQValueName.Package) : Dval =
      let (Hash hashStr) = h
      DString hashStr

    let fromDT (d : Dval) : FQValueName.Package =
      match d with
      | DString hashStr -> Hash hashStr
      | _ -> Exception.raiseInternal "Invalid FQValueName.Package" []

  let toDT (u : FQValueName.FQValueName) : Dval =
    let (caseName, fields) =
      match u with
      | FQValueName.Builtin u -> "Builtin", [ Builtin.toDT u ]
      | FQValueName.Package u -> "Package", [ Package.toDT u ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : FQValueName.FQValueName =
    match d with
    | DEnum(_, _, [], "Builtin", [ u ]) -> FQValueName.Builtin(Builtin.fromDT u)
    | DEnum(_, _, [], "Package", [ u ]) -> FQValueName.Package(Package.fromDT u)
    | _ -> Exception.raiseInternal "Invalid FQValueName" []


module FQFnName =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.FQFnName.fqFnName
  let knownType = KTCustomType(typeName, [])

  module Builtin =
    let typeName =
      FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.FQFnName.builtin

    let toDT (u : FQFnName.Builtin) : Dval =
      let fields = [ "name", DString u.name; "version", DInt32 u.version ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : FQFnName.Builtin =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = nameField fields; version = versionField fields }
      | _ -> Exception.raiseInternal "Invalid FQFnName.Builtin" []

  module Package =
    let toDT (h : FQFnName.Package) : Dval =
      let (Hash hashStr) = h
      DString hashStr

    let fromDT (d : Dval) : FQFnName.Package =
      match d with
      | DString hashStr -> Hash hashStr
      | _ -> Exception.raiseInternal "Invalid FQFnName.Package" []


  let toDT (u : FQFnName.FQFnName) : Dval =
    let (caseName, fields) =
      match u with
      | FQFnName.Builtin u -> "Builtin", [ Builtin.toDT u ]
      | FQFnName.Package u -> "Package", [ Package.toDT u ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : FQFnName.FQFnName =
    match d with
    | DEnum(_, _, [], "Builtin", [ u ]) -> FQFnName.Builtin(Builtin.fromDT u)
    | DEnum(_, _, [], "Package", [ u ]) -> FQFnName.Package(Package.fromDT u)
    | _ -> Exception.raiseInternal "Invalid FQFnName" []



module NameResolutionError =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.nameResolutionError

  let toDT (nre : NameResolutionError) : Dval =
    let (caseName, fields) =
      match nre with
      | NotFound names -> "NotFound", [ DList(VT.string, List.map DString names) ]
      | InvalidName names ->
        "InvalidName", [ DList(VT.string, List.map DString names) ]

    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : NameResolutionError =
    match d with
    | DEnum(_, _, [], "NotFound", [ names ]) -> names |> D.list D.string |> NotFound
    | DEnum(_, _, [], "InvalidName", [ names ]) ->
      names |> D.list D.string |> InvalidName
    | _ -> Exception.raiseInternal "Invalid NameResolutionError" []


module NameResolution =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.nameResolution

  let toDT
    (nameValueType : KnownType)
    (f : 'p -> Dval)
    (result : NameResolution<'p>)
    : Dval =
    let errType = KTCustomType(typeName, [])
    C2DT.Result.toDT nameValueType errType result f NameResolutionError.toDT

  let fromDT (f : Dval -> 'a) (d : Dval) : NameResolution<'a> =
    C2DT.Result.fromDT f d NameResolutionError.fromDT


module TypeReference =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.typeReference
  let knownType = KTCustomType(typeName, [])

  let rec toDT (t : TypeReference) : Dval =
    let (caseName, fields) =
      match t with
      | TVariable name -> "TVariable", [ DString name ]

      | TUnit -> "TUnit", []
      | TBool -> "TBool", []
      | TInt8 -> "TInt8", []
      | TUInt8 -> "TUInt8", []
      | TInt16 -> "TInt16", []
      | TUInt16 -> "TUInt16", []
      | TInt32 -> "TInt32", []
      | TUInt32 -> "TUInt32", []
      | TInt64 -> "TInt64", []
      | TUInt64 -> "TUInt64", []
      | TInt128 -> "TInt128", []
      | TUInt128 -> "TUInt128", []
      | TFloat -> "TFloat", []
      | TChar -> "TChar", []
      | TString -> "TString", []
      | TDateTime -> "TDateTime", []
      | TUuid -> "TUuid", []

      | TList inner -> "TList", [ toDT inner ]

      | TTuple(first, second, theRest) ->
        "TTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      | TDict inner -> "TDict", [ toDT inner ]

      | TCustomType(typeName, typeArgs) ->
        "TCustomType",
        [ NameResolution.toDT FQTypeName.knownType FQTypeName.toDT typeName
          DList(VT.known knownType, List.map toDT typeArgs) ]

      | TFn(args, ret) ->
        let args = args |> NEList.toList |> List.map toDT |> Dval.list knownType
        "TFn", [ args; toDT ret ]

      | TDB inner -> "TDB", [ toDT inner ]


    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : TypeReference =
    match d with
    | DEnum(_, _, [], "TVariable", [ DString name ]) -> TVariable(name)

    | DEnum(_, _, [], "TUnit", []) -> TUnit
    | DEnum(_, _, [], "TBool", []) -> TBool
    | DEnum(_, _, [], "TInt8", []) -> TInt8
    | DEnum(_, _, [], "TUInt8", []) -> TUInt8
    | DEnum(_, _, [], "TInt16", []) -> TInt16
    | DEnum(_, _, [], "TUInt16", []) -> TUInt16
    | DEnum(_, _, [], "TInt32", []) -> TInt32
    | DEnum(_, _, [], "TUInt32", []) -> TUInt32
    | DEnum(_, _, [], "TInt64", []) -> TInt64
    | DEnum(_, _, [], "TUInt64", []) -> TUInt64
    | DEnum(_, _, [], "TInt128", []) -> TInt128
    | DEnum(_, _, [], "TUInt128", []) -> TUInt128
    | DEnum(_, _, [], "TFloat", []) -> TFloat
    | DEnum(_, _, [], "TChar", []) -> TChar
    | DEnum(_, _, [], "TString", []) -> TString
    | DEnum(_, _, [], "TDateTime", []) -> TDateTime
    | DEnum(_, _, [], "TUuid", []) -> TUuid

    | DEnum(_, _, [], "TTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "TList", [ inner ]) -> TList(fromDT inner)

    | DEnum(_, _, [], "TDict", [ inner ]) -> TDict(fromDT inner)

    | DEnum(_, _, [], "TCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
      TCustomType(
        NameResolution.fromDT FQTypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, [], "TFn", [ DList(_vtTODO, firstArg :: otherArgs); ret ]) ->
      TFn(NEList.ofList (fromDT firstArg) (List.map fromDT otherArgs), fromDT ret)

    | DEnum(_, _, [], "TDB", [ inner ]) -> TDB(fromDT inner)

    | _ -> Exception.raiseInternal "Invalid TypeReference" [ "typeRef", d ]



module LetPattern =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.letPattern
  let knownType = KTCustomType(typeName, [])

  let rec toDT (p : LetPattern) : Dval =
    let (caseName, fields) =
      match p with
      | LPVariable reg -> "LPVariable", [ DInt32 reg ]
      | LPUnit -> "LPUnit", []
      | LPTuple(first, second, theRest) ->
        "LPTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : LetPattern =
    match d with
    | DEnum(_, _, [], "LPVariable", [ DInt32 reg ]) -> LPVariable(reg)
    | DEnum(_, _, [], "LPUnit", []) -> LPUnit
    | DEnum(_, _, [], "LPTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      LPTuple(fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.matchPattern
  let knownType = KTCustomType(typeName, [])

  let rec toDT (p : MatchPattern) : Dval =
    let (caseName, fields) =
      match p with
      | MPVariable(reg) -> "MPVariable", [ DInt32 reg ]

      | MPUnit -> "MPUnit", []
      | MPBool b -> "MPBool", [ DBool b ]
      | MPInt8 i -> "MPInt8", [ DInt8 i ]
      | MPUInt8 i -> "MPUInt8", [ DUInt8 i ]
      | MPInt16 i -> "MPInt16", [ DInt16 i ]
      | MPUInt16 i -> "MPUInt16", [ DUInt16 i ]
      | MPInt32 i -> "MPInt32", [ DInt32 i ]
      | MPUInt32 i -> "MPUInt32", [ DUInt32 i ]
      | MPInt64 i -> "MPInt64", [ DInt64 i ]
      | MPUInt64 i -> "MPUInt64", [ DUInt64 i ]
      | MPInt128 i -> "MPInt128", [ DInt128 i ]
      | MPUInt128 i -> "MPUInt128", [ DUInt128 i ]
      | MPFloat f -> "MPFloat", [ DFloat f ]
      | MPChar c -> "MPChar", [ DString c ]
      | MPString s -> "MPString", [ DString s ]

      | MPTuple(first, second, theRest) ->
        "MPTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      | MPList inner -> "MPList", [ DList(VT.known knownType, List.map toDT inner) ]
      | MPListCons(head, tail) -> "MPListCons", [ toDT head; toDT tail ]

      | MPEnum(caseName, fieldPats) ->
        "MPEnum",
        [ DString caseName; DList(VT.known knownType, List.map toDT fieldPats) ]

      | MPOr patterns ->
        let patterns = patterns |> NEList.toList |> List.map toDT
        "MPOr", [ DList(VT.known knownType, patterns) ]

    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : MatchPattern =
    match d with
    | DEnum(_, _, [], "MPVariable", [ DInt32 reg ]) -> MPVariable(reg)

    | DEnum(_, _, [], "MPUnit", []) -> MPUnit
    | DEnum(_, _, [], "MPBool", [ DBool b ]) -> MPBool b
    | DEnum(_, _, [], "MPInt8", [ DInt8 i ]) -> MPInt8 i
    | DEnum(_, _, [], "MPUInt8", [ DUInt8 i ]) -> MPUInt8 i
    | DEnum(_, _, [], "MPInt16", [ DInt16 i ]) -> MPInt16 i
    | DEnum(_, _, [], "MPUInt16", [ DUInt16 i ]) -> MPUInt16 i
    | DEnum(_, _, [], "MPInt32", [ DInt32 i ]) -> MPInt32 i
    | DEnum(_, _, [], "MPUInt32", [ DUInt32 i ]) -> MPUInt32 i
    | DEnum(_, _, [], "MPInt64", [ DInt64 i ]) -> MPInt64 i
    | DEnum(_, _, [], "MPUInt64", [ DUInt64 i ]) -> MPUInt64 i
    | DEnum(_, _, [], "MPInt128", [ DInt128 i ]) -> MPInt128 i
    | DEnum(_, _, [], "MPUInt128", [ DUInt128 i ]) -> MPUInt128(i)
    | DEnum(_, _, [], "MPFloat", [ DFloat f ]) -> MPFloat f
    | DEnum(_, _, [], "MPChar", [ DString c ]) -> MPChar c
    | DEnum(_, _, [], "MPString", [ DString s ]) -> MPString s

    | DEnum(_, _, [], "MPTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      MPTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "MPList", [ DList(_vtTODO, inner) ]) ->
      MPList(List.map fromDT inner)
    | DEnum(_, _, [], "MPListCons", [ head; tail ]) ->
      MPListCons(fromDT head, fromDT tail)

    | DEnum(_, _, [], "MPEnum", [ DString caseName; DList(_vtTODO, fieldPats) ]) ->
      MPEnum(caseName, List.map fromDT fieldPats)

    | DEnum(_, _, [], "MPOr", [ DList(_vtTODO, patterns) ]) ->
      let patterns = patterns |> List.map fromDT
      MPOr(NEList.ofList patterns.Head patterns.Tail)

    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module StringSegment =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.stringSegment
  let knownType = KTCustomType(typeName, [])

  let toDT (regToDT : Register -> Dval) (s : StringSegment) : Dval =
    let (caseName, fields) =
      match s with
      | Text text -> "Text", [ DString text ]
      | Interpolated reg -> "Interpolated", [ regToDT reg ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (regFromDT : Dval -> Register) (d : Dval) : StringSegment =
    match d with
    | DEnum(_, _, [], "Text", [ DString text ]) -> Text text
    | DEnum(_, _, [], "Interpolation", [ reg ]) -> Interpolated(regFromDT reg)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


module KnownType =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.knownType

  let toDT (kt : KnownType) : Dval =
    let (caseName, fields) =
      match kt with
      | KTUnit -> "KTUnit", []
      | KTBool -> "KTBool", []
      | KTInt8 -> "KTInt8", []
      | KTUInt8 -> "KTUInt8", []
      | KTInt16 -> "KTInt16", []
      | KTUInt16 -> "KTUInt16", []
      | KTInt32 -> "KTInt32", []
      | KTUInt32 -> "KTUInt32", []
      | KTInt64 -> "KTInt64", []
      | KTUInt64 -> "KTUInt64", []
      | KTInt128 -> "KTInt128", []
      | KTUInt128 -> "KTUInt128", []
      | KTFloat -> "KTFloat", []
      | KTChar -> "KTChar", []
      | KTString -> "KTString", []
      | KTUuid -> "KTUuid", []
      | KTDateTime -> "KTDateTime", []

      | KTTuple(first, second, theRest) ->
        "KTTuple",
        [ ValueType.toDT first
          ValueType.toDT second
          DList(VT.known ValueType.knownType, List.map ValueType.toDT theRest) ]
      | KTList inner -> "KTList", [ ValueType.toDT inner ]
      | KTDict inner -> "KTDict", [ ValueType.toDT inner ]

      | KTCustomType(typeName, typeArgs) ->
        "KTCustomType",
        [ FQTypeName.toDT typeName
          DList(VT.known ValueType.knownType, List.map ValueType.toDT typeArgs) ]

      | KTFn(args, ret) ->
        let args =
          args
          |> NEList.toList
          |> List.map ValueType.toDT
          |> Dval.list ValueType.knownType
        "KTFn", [ args; ValueType.toDT ret ]

      | KTDB d -> "KTDB", [ ValueType.toDT d ]

    DEnum(typeName, typeName, [], caseName, fields)


  let fromDT (d : Dval) : KnownType =
    match d with
    | DEnum(_, _, [], "KTUnit", []) -> KTUnit
    | DEnum(_, _, [], "KTBool", []) -> KTBool
    | DEnum(_, _, [], "KTInt8", []) -> KTInt8
    | DEnum(_, _, [], "KTUInt8", []) -> KTUInt8
    | DEnum(_, _, [], "KTInt16", []) -> KTInt16
    | DEnum(_, _, [], "KTUInt16", []) -> KTUInt16
    | DEnum(_, _, [], "KTInt32", []) -> KTInt32
    | DEnum(_, _, [], "KTUInt32", []) -> KTUInt32
    | DEnum(_, _, [], "KTInt64", []) -> KTInt64
    | DEnum(_, _, [], "KTUInt64", []) -> KTUInt64
    | DEnum(_, _, [], "KTInt128", []) -> KTInt128
    | DEnum(_, _, [], "KTUInt128", []) -> KTUInt128
    | DEnum(_, _, [], "KTFloat", []) -> KTFloat
    | DEnum(_, _, [], "KTChar", []) -> KTChar
    | DEnum(_, _, [], "KTString", []) -> KTString
    | DEnum(_, _, [], "KTUuid", []) -> KTUuid
    | DEnum(_, _, [], "KTDateTime", []) -> KTDateTime

    | DEnum(_, _, [], "KTTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      KTTuple(
        ValueType.fromDT first,
        ValueType.fromDT second,
        List.map ValueType.fromDT theRest
      )
    | DEnum(_, _, [], "KTList", [ inner ]) -> KTList(ValueType.fromDT inner)
    | DEnum(_, _, [], "KTDict", [ inner ]) -> KTDict(ValueType.fromDT inner)

    | DEnum(_, _, [], "KTCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
      KTCustomType(FQTypeName.fromDT typeName, List.map ValueType.fromDT typeArgs)

    | DEnum(_, _, [], "KTFn", [ DList(_vtTODO, firstArg :: otherArgs); ret ]) ->
      KTFn(
        NEList.ofList
          (ValueType.fromDT firstArg)
          (List.map ValueType.fromDT otherArgs),
        ValueType.fromDT ret
      )
    | DEnum(_, _, [], "KTDB", [ inner ]) -> KTDB(ValueType.fromDT inner)

    | _ -> Exception.raiseInternal "Invalid KnownType" []


module ValueType =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.valueType
  let knownType = KTCustomType(typeName, [])

  let toDT (vt : ValueType) : Dval =
    let (caseName, fields) =
      match vt with
      | ValueType.Unknown -> "Unknown", []
      | ValueType.Known kt ->
        let kt = KnownType.toDT kt
        "Known", [ kt ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : ValueType =
    match d with
    | DEnum(_, _, [], "Unknown", []) -> ValueType.Unknown
    | DEnum(_, _, [], "Known", [ kt ]) -> ValueType.Known(KnownType.fromDT kt)
    | _ -> Exception.raiseInternal "Invalid ValueType" []


module ApplicableNamedFn =
  let toDT (namedFn : ApplicableNamedFn) : Dval =
    let typeName =
      FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.applicableNamedFn

    let fields =
      [ "name", FQFnName.toDT namedFn.name
        "typeArgs",
        DList(
          VT.known TypeReference.knownType,
          List.map TypeReference.toDT namedFn.typeArgs
        )
        "argsSoFar",
        DList(VT.known Dval.knownType, List.map Dval.toDT namedFn.argsSoFar) ]

    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : ApplicableNamedFn =
    match d with
    | DRecord(_, _, _, fields) ->
      { name = FQFnName.fromDT (fields |> D.field "name")
        typeSymbolTable = Map.empty // TODO
        typeArgs = fields |> D.field "typeArgs" |> D.list TypeReference.fromDT
        argsSoFar = fields |> D.field "argsSoFar" |> D.list Dval.fromDT }
    | _ -> Exception.raiseInternal "Invalid ApplicableNamedFn" []


module ApplicableLambda =
  let toDT (lambda : ApplicableLambda) : Dval =
    let typeName =
      FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.applicableLambda

    let fields =
      [ ("exprId", DUInt64 lambda.exprId)
        ("closedRegisters",
         DList(
           VT.tuple VT.int32 (VT.known Dval.knownType) [],
           lambda.closedRegisters
           |> List.map (fun (reg, dv) -> DTuple(DInt32 reg, Dval.toDT dv, []))
         ))
        ("typeSymbolTable",
         DDict(
           VT.known ValueType.knownType,
           Map.map ValueType.toDT lambda.typeSymbolTable
         ))
        ("argsSoFar",
         DList(VT.known Dval.knownType, List.map Dval.toDT lambda.argsSoFar)) ]

    DRecord(typeName, typeName, [], Map fields)


  let fromDT (d : Dval) : ApplicableLambda =
    match d with
    | DRecord(_, _, _, fields) ->
      { exprId = fields |> D.field "exprId" |> D.uInt64
        closedRegisters =
          fields
          |> D.field "closedRegisters"
          |> D.list (D.tuple2 D.int32 Dval.fromDT)
        typeSymbolTable =
          fields |> D.field "typeSymbolTable" |> D.dict ValueType.fromDT
        argsSoFar = fields |> D.field "argsSoFar" |> D.list Dval.fromDT }
    | _ -> Exception.raiseInternal "Invalid ApplicableLambda" []


module Applicable =
  let toDT (applicable : Applicable) : Dval =
    let typeName =
      FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.applicable

    let (caseName, fields) =
      match applicable with
      | AppNamedFn namedFn -> "AppNamedFn", [ ApplicableNamedFn.toDT namedFn ]
      | AppLambda lambda -> "AppLambda", [ ApplicableLambda.toDT lambda ]

    DEnum(typeName, typeName, [], caseName, fields)


  let fromDT (d : Dval) : Applicable =
    match d with
    | DEnum(_, _, [], "AppNamedFn", [ namedFn ]) ->
      AppNamedFn(ApplicableNamedFn.fromDT namedFn)
    | DEnum(_, _, [], "AppLambda", [ lambda ]) ->
      AppLambda(ApplicableLambda.fromDT lambda)
    | _ -> Exception.raiseInternal "Invalid Applicable" []



module Dval =
  let typeName = FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.dval
  let knownType = KTCustomType(typeName, [])

  let rec toDT (dv : Dval) : Dval =
    let (caseName, fields) =
      match dv with
      | DUnit -> "DUnit", []
      | DBool b -> "DBool", [ DBool b ]
      | DInt8 i -> "DInt8", [ DInt8 i ]
      | DUInt8 i -> "DUInt8", [ DUInt8 i ]
      | DInt16 i -> "DInt16", [ DInt16 i ]
      | DUInt16 i -> "DUInt16", [ DUInt16 i ]
      | DInt32 i -> "DInt32", [ DInt32 i ]
      | DUInt32 i -> "DUInt32", [ DUInt32 i ]
      | DInt64 i -> "DInt64", [ DInt64 i ]
      | DUInt64 i -> "DUInt64", [ DUInt64 i ]
      | DInt128 i -> "DInt128", [ DInt128 i ]
      | DUInt128 i -> "DUInt128", [ DUInt128 i ]
      | DFloat f -> "DFloat", [ DFloat f ]
      | DChar c -> "DChar", [ DChar c ]
      | DString s -> "DString", [ DString s ]
      | DUuid u -> "DUuid", [ DUuid u ]
      | DDateTime d -> "DDateTime", [ DDateTime d ]

      | DTuple(first, second, theRest) ->
        "DTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      | DList(vt, items) ->
        "DList",
        [ ValueType.toDT vt; DList(VT.known knownType, List.map toDT items) ]

      | DDict(vt, entries) ->
        "DDict",
        [ ValueType.toDT vt; DDict(VT.known knownType, Map.map toDT entries) ]

      | DRecord(runtimeTypeName, sourceTypeName, typeArgs, fields) ->
        "DRecord",
        [ FQTypeName.toDT runtimeTypeName
          FQTypeName.toDT sourceTypeName
          DList(VT.known ValueType.knownType, List.map ValueType.toDT typeArgs)
          DDict(VT.known knownType, Map.map toDT fields) ]

      | DEnum(runtimeTypeName, sourceTypeName, typeArgs, caseName, fields) ->
        "DEnum",
        [ FQTypeName.toDT runtimeTypeName
          FQTypeName.toDT sourceTypeName
          DList(VT.known ValueType.knownType, List.map ValueType.toDT typeArgs)
          DString caseName
          DList(VT.known knownType, List.map toDT fields) ]

      | DApplicable applicable -> "DApplicable", [ Applicable.toDT applicable ]

      | DDB name -> "DDB", [ DString name ]

    DEnum(typeName, typeName, [], caseName, fields)


  let fromDT (d : Dval) : Dval =
    match d with
    | DEnum(_, _, [], "DUnit", []) -> DUnit
    | DEnum(_, _, [], "DBool", [ DBool b ]) -> DBool b
    | DEnum(_, _, [], "DInt8", [ DInt8 i ]) -> DInt8 i
    | DEnum(_, _, [], "DUInt8", [ DUInt8 i ]) -> DUInt8 i
    | DEnum(_, _, [], "DInt16", [ DInt16 i ]) -> DInt16 i
    | DEnum(_, _, [], "DUInt16", [ DUInt16 i ]) -> DUInt16 i
    | DEnum(_, _, [], "DInt32", [ DInt32 i ]) -> DInt32 i
    | DEnum(_, _, [], "DUInt32", [ DUInt32 i ]) -> DUInt32 i
    | DEnum(_, _, [], "DInt64", [ DInt64 i ]) -> DInt64 i
    | DEnum(_, _, [], "DUInt64", [ DUInt64 i ]) -> DUInt64 i
    | DEnum(_, _, [], "DInt128", [ DInt128 i ]) -> DInt128 i
    | DEnum(_, _, [], "DUInt128", [ DUInt128 i ]) -> DUInt128 i
    | DEnum(_, _, [], "DFloat", [ DFloat f ]) -> DFloat f
    | DEnum(_, _, [], "DChar", [ DChar c ]) -> DChar c
    | DEnum(_, _, [], "DString", [ DString s ]) -> DString s
    | DEnum(_, _, [], "DDateTime", [ DDateTime d ]) -> DDateTime d
    | DEnum(_, _, [], "DUuid", [ DUuid u ]) -> DUuid u

    | DEnum(_, _, [], "DTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      DTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "DList", [ vt; DList(_vtTODO, l) ]) ->
      DList(ValueType.fromDT vt, List.map fromDT l)

    | DEnum(_, _, [], "DDict", [ vt; DDict(_vtTODO, map) ]) ->
      DDict(ValueType.fromDT vt, Map.map fromDT map)

    | DEnum(_,
            _,
            [],
            "DRecord",
            [ runtimeTypeName; sourceTypeName; typeArgs; entries ]) ->
      DRecord(
        FQTypeName.fromDT runtimeTypeName,
        FQTypeName.fromDT sourceTypeName,
        typeArgs |> D.list ValueType.fromDT,
        entries |> D.dict fromDT
      )

    | DEnum(_,
            _,
            [],
            "DEnum",
            [ runtimeTypeName; sourceTypeName; typeArgs; caseName; fields ]) ->
      DEnum(
        FQTypeName.fromDT runtimeTypeName,
        FQTypeName.fromDT sourceTypeName,
        typeArgs |> D.list ValueType.fromDT,
        D.string caseName,
        fields |> D.list fromDT
      )

    | DEnum(_, _, [], "DApplicable", [ applicable ]) ->
      DApplicable(Applicable.fromDT applicable)

    | DEnum(_, _, [], "DDB", [ DString name ]) -> DDB name

    | _ -> Exception.raiseInternal "Invalid Dval" []




// TODO: make sure we've tested all of these RTEs
module RuntimeError =
  module Bools =
    let toDT (e : RuntimeError.Bools.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Bools.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Bools.AndOnlySupportsBooleans(gotLeft, gotRight) ->
          "AndOnlySupportsBooleans",
          [ ValueType.toDT gotLeft; ValueType.toDT gotRight ]
        | RuntimeError.Bools.OrOnlySupportsBooleans(gotRight, gotLeft) ->
          "OrOnlySupportsBooleans",
          [ ValueType.toDT gotRight; ValueType.toDT gotLeft ]
        | RuntimeError.Bools.ConditionRequiresBool(actualValueType, actualValue) ->
          "ConditionRequiresBool",
          [ ValueType.toDT actualValueType; Dval.toDT actualValue ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Bools.Error =
      match d with
      | DEnum(_, _, [], "AndOnlySupportsBooleans", [ gotLeft; gotRight ]) ->
        RuntimeError.Bools.AndOnlySupportsBooleans(
          ValueType.fromDT gotLeft,
          ValueType.fromDT gotRight
        )
      | DEnum(_, _, [], "OrOnlySupportsBooleans", [ gotRight; gotLeft ]) ->
        RuntimeError.Bools.OrOnlySupportsBooleans(
          ValueType.fromDT gotRight,
          ValueType.fromDT gotLeft
        )
      | DEnum(_, _, [], "ConditionRequiresBool", [ actualValueType; actualValue ]) ->
        RuntimeError.Bools.ConditionRequiresBool(
          ValueType.fromDT actualValueType,
          Dval.fromDT actualValue
        )
      | _ -> Exception.raiseInternal "Invalid Bools.Error" []

  module Ints =
    let toDT (e : RuntimeError.Ints.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Ints.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Ints.DivideByZeroError -> "DivideByZeroError", []
        | RuntimeError.Ints.OutOfRange -> "OutOfRange", []
        | RuntimeError.Ints.NegativeExponent -> "NegativeExponent", []
        | RuntimeError.Ints.NegativeModulus -> "NegativeModulus", []
        | RuntimeError.Ints.ZeroModulus -> "ZeroModulus", []

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Ints.Error =
      match d with
      | DEnum(_, _, [], "DivideByZeroError", []) ->
        RuntimeError.Ints.DivideByZeroError
      | DEnum(_, _, [], "OutOfRange", []) -> RuntimeError.Ints.OutOfRange
      | DEnum(_, _, [], "NegativeExponent", []) -> RuntimeError.Ints.NegativeExponent
      | DEnum(_, _, [], "NegativeModulus", []) -> RuntimeError.Ints.NegativeModulus
      | DEnum(_, _, [], "ZeroModulus", []) -> RuntimeError.Ints.ZeroModulus
      | _ -> Exception.raiseInternal "Invalid Ints.Error" []

  module Strings =
    let toDT (e : RuntimeError.Strings.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Strings.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Strings.NonStringInInterpolation(vt, dv) ->
          "NonStringInInterpolation", [ ValueType.toDT vt; Dval.toDT dv ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Strings.Error =
      match d with
      | DEnum(_, _, [], "NonStringInInterpolation", [ vt; dv ]) ->
        RuntimeError.Strings.NonStringInInterpolation(
          ValueType.fromDT vt,
          Dval.fromDT dv
        )
      | _ -> Exception.raiseInternal "Invalid Strings.Error" []

  module Lists =
    let toDT (e : RuntimeError.Lists.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Lists.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Lists.TriedToAddMismatchedData(index,
                                                      expectedType,
                                                      actualType,
                                                      actualValue) ->
          "TriedToAddMismatchedData",
          [ DInt32 index
            ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actualValue ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Lists.Error =
      match d with
      | DEnum(_,
              _,
              [],
              "TriedToAddMismatchedData",
              [ index; expectedType; actualType; actualValue ]) ->
        RuntimeError.Lists.TriedToAddMismatchedData(
          D.int32 index,
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actualValue
        )
      | _ -> Exception.raiseInternal "Invalid Lists.Error" []

  module Dicts =
    let toDT (e : RuntimeError.Dicts.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Dicts.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Dicts.TriedToAddKeyAfterAlreadyPresent key ->
          "TriedToAddKeyAfterAlreadyPresent", [ DString key ]
        | RuntimeError.Dicts.TriedToAddMismatchedData(key,
                                                      expectedType,
                                                      actualType,
                                                      actualValue) ->
          "TriedToAddMismatchedData",
          [ DString key
            ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actualValue ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Dicts.Error =
      match d with
      | DEnum(_, _, [], "TriedToAddKeyAfterAlreadyPresent", [ DString key ]) ->
        RuntimeError.Dicts.TriedToAddKeyAfterAlreadyPresent key
      | DEnum(_,
              _,
              [],
              "TriedToAddMismatchedData",
              [ key; expectedType; actualType; actualValue ]) ->
        RuntimeError.Dicts.TriedToAddMismatchedData(
          D.string key,
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actualValue
        )
      | _ -> Exception.raiseInternal "Invalid Dicts.Error" []

  module Lets =
    let toDT (e : RuntimeError.Lets.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Lets.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Lets.PatternDoesNotMatch(dval, pat) ->
          "PatternDoesNotMatch", [ Dval.toDT dval; LetPattern.toDT pat ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Lets.Error =
      match d with
      | DEnum(_, _, [], "PatternDoesNotMatch", [ dval; pat ]) ->
        RuntimeError.Lets.PatternDoesNotMatch(
          Dval.fromDT dval,
          LetPattern.fromDT pat
        )
      | _ -> Exception.raiseInternal "Invalid Lets.Error" []

  module Matches =
    let toDT (e : RuntimeError.Matches.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Matches.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Matches.MatchUnmatched unmatchedValue ->
          "MatchUnmatched", [ Dval.toDT unmatchedValue ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Matches.Error =
      match d with
      | DEnum(_, _, [], "MatchUnmatched", [ unmatchedValue ]) ->
        RuntimeError.Matches.MatchUnmatched(Dval.fromDT unmatchedValue)
      | _ -> Exception.raiseInternal "Invalid Matches.Error" []

  module Records =
    let toDT (e : RuntimeError.Records.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Records.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Records.CreationTypeNotRecord name ->
          "CreationTypeNotRecord", [ FQTypeName.toDT name ]
        | RuntimeError.Records.CreationEmptyKey -> "CreationEmptyKey", []
        | RuntimeError.Records.CreationMissingField fieldName ->
          "CreationMissingField", [ DString fieldName ]
        | RuntimeError.Records.CreationDuplicateField fieldName ->
          "CreationDuplicateField", [ DString fieldName ]
        | RuntimeError.Records.CreationFieldNotExpected fieldName ->
          "CreationFieldNotExpected", [ DString fieldName ]
        | RuntimeError.Records.CreationFieldOfWrongType(fieldName,
                                                        expectedType,
                                                        actualType,
                                                        actual) ->
          "CreationFieldOfWrongType",
          [ DString fieldName
            ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actual ]

        | RuntimeError.Records.UpdateNotRecord actualType ->
          "UpdateNotRecord", [ ValueType.toDT actualType ]
        | RuntimeError.Records.UpdateEmptyKey -> "UpdateEmptyKey", []
        | RuntimeError.Records.UpdateDuplicateField fieldName ->
          "UpdateDuplicateField", [ DString fieldName ]
        | RuntimeError.Records.UpdateFieldNotExpected fieldName ->
          "UpdateFieldNotExpected", [ DString fieldName ]
        | RuntimeError.Records.UpdateFieldOfWrongType(fieldName,
                                                      expectedType,
                                                      actualType,
                                                      actual) ->
          "UpdateFieldOfWrongType",
          [ DString fieldName
            ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actual ]

        | RuntimeError.Records.FieldAccessEmptyFieldName ->
          "FieldAccessEmptyFieldName", []
        | RuntimeError.Records.FieldAccessFieldNotFound fieldName ->
          "FieldAccessFieldNotFound", [ DString fieldName ]
        | RuntimeError.Records.FieldAccessNotRecord actualType ->
          "FieldAccessNotRecord", [ ValueType.toDT actualType ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Records.Error =
      match d with
      | DEnum(_, _, [], "CreationTypeNotRecord", [ name ]) ->
        RuntimeError.Records.CreationTypeNotRecord(FQTypeName.fromDT name)
      | DEnum(_, _, [], "CreationEmptyKey", []) ->
        RuntimeError.Records.CreationEmptyKey
      | DEnum(_, _, [], "CreationMissingField", [ fieldName ]) ->
        RuntimeError.Records.CreationMissingField(D.string fieldName)
      | DEnum(_, _, [], "CreationDuplicateField", [ fieldName ]) ->
        RuntimeError.Records.CreationDuplicateField(D.string fieldName)
      | DEnum(_, _, [], "CreationFieldNotExpected", [ fieldName ]) ->
        RuntimeError.Records.CreationFieldNotExpected(D.string fieldName)
      | DEnum(_,
              _,
              [],
              "CreationFieldOfWrongType",
              [ fieldName; expectedType; actualType; actual ]) ->
        RuntimeError.Records.CreationFieldOfWrongType(
          D.string fieldName,
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actual
        )

      | DEnum(_, _, [], "UpdateNotRecord", [ actualType ]) ->
        RuntimeError.Records.UpdateNotRecord(ValueType.fromDT actualType)
      | DEnum(_, _, [], "UpdateEmptyKey", []) -> RuntimeError.Records.UpdateEmptyKey
      | DEnum(_, _, [], "UpdateDuplicateField", [ fieldName ]) ->
        RuntimeError.Records.UpdateDuplicateField(D.string fieldName)
      | DEnum(_, _, [], "UpdateFieldNotExpected", [ fieldName ]) ->
        RuntimeError.Records.UpdateFieldNotExpected(D.string fieldName)
      | DEnum(_,
              _,
              [],
              "UpdateFieldOfWrongType",
              [ fieldName; expectedType; actualType; actual ]) ->
        RuntimeError.Records.UpdateFieldOfWrongType(
          D.string fieldName,
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actual
        )

      | DEnum(_, _, [], "FieldAccessEmptyFieldName", []) ->
        RuntimeError.Records.FieldAccessEmptyFieldName
      | DEnum(_, _, [], "FieldAccessFieldNotFound", [ fieldName ]) ->
        RuntimeError.Records.FieldAccessFieldNotFound(D.string fieldName)
      | DEnum(_, _, [], "FieldAccessNotRecord", [ actualType ]) ->
        RuntimeError.Records.FieldAccessNotRecord(ValueType.fromDT actualType)
      | _ -> Exception.raiseInternal "Invalid Records.Error" []

  module Enums =
    let toDT (e : RuntimeError.Enums.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Enums.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Enums.ConstructionWrongNumberOfFields(typeName,
                                                             caseName,
                                                             expectedFieldCount,
                                                             actualFieldCount) ->
          "ConstructionWrongNumberOfFields",
          [ FQTypeName.toDT typeName
            DString caseName
            DInt64 expectedFieldCount
            DInt64 actualFieldCount ]
        | RuntimeError.Enums.ConstructionCaseNotFound(typeName, caseName) ->
          "ConstructionCaseNotFound", [ FQTypeName.toDT typeName; DString caseName ]
        | RuntimeError.Enums.ConstructionFieldOfWrongType(caseName,
                                                          fieldIndex,
                                                          expectedType,
                                                          actualType,
                                                          actualValue) ->
          "ConstructionFieldOfWrongType",
          [ DString caseName
            DInt64 fieldIndex
            ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actualValue ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Enums.Error =
      match d with
      | DEnum(_,
              _,
              [],
              "ConstructionWrongNumberOfFields",
              [ typeName; caseName; expectedFieldCount; actualFieldCount ]) ->
        RuntimeError.Enums.ConstructionWrongNumberOfFields(
          FQTypeName.fromDT typeName,
          D.string caseName,
          D.int64 expectedFieldCount,
          D.int64 actualFieldCount
        )
      | DEnum(_, _, [], "ConstructionCaseNotFound", [ typeName; caseName ]) ->
        RuntimeError.Enums.ConstructionCaseNotFound(
          FQTypeName.fromDT typeName,
          D.string caseName
        )
      | DEnum(_,
              _,
              [],
              "ConstructionFieldOfWrongType",
              [ caseName; fieldIndex; expectedType; actualType; actualValue ]) ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          D.string caseName,
          D.int64 fieldIndex,
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actualValue
        )
      | _ -> Exception.raiseInternal "Invalid Enums.Error" []

  module Applications =
    let toDT (e : RuntimeError.Applications.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Applications.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Applications.ExpectedApplicableButNot(actualTyp, actualValue) ->
          "ExpectedApplicableButNot",
          [ ValueType.toDT actualTyp; Dval.toDT actualValue ]

        | RuntimeError.Applications.WrongNumberOfTypeArgsForFn(fn, expected, actual) ->
          "WrongNumberOfTypeArgsForFn",
          [ FQFnName.toDT fn; DInt64 expected; DInt64 actual ]
        | RuntimeError.Applications.CannotApplyTypeArgsMoreThanOnce ->
          "CannotApplyTypeArgsMoreThanOnce", []
        | RuntimeError.Applications.TooManyArgsForFn(fn, expected, actual) ->
          "TooManyArgsForFn", [ FQFnName.toDT fn; DInt64 expected; DInt64 actual ]
        | RuntimeError.Applications.FnParameterNotExpectedType(fnName,
                                                               paramIndex,
                                                               paramName,
                                                               expectedType,
                                                               actualType,
                                                               actualValue) ->
          "FnParameterNotExpectedType",
          [ FQFnName.toDT fnName
            DInt64 paramIndex
            DString paramName
            ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actualValue ]
        | RuntimeError.Applications.FnResultNotExpectedType(fnName,
                                                            expectedType,
                                                            actualType,
                                                            actualValue) ->
          "FnResultNotExpectedType",
          [ FQFnName.toDT fnName
            ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actualValue ]

        | RuntimeError.Applications.CannotApplyTypeArgsToLambda ->
          "CannotApplyTypeArgsToLambda", []

        | RuntimeError.Applications.TooManyArgsForLambda(lambdaExprId,
                                                         expected,
                                                         actual) ->
          "TooManyArgsForLambda",
          [ DUInt64 lambdaExprId; DInt64 expected; DInt64 actual ]


      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Applications.Error =
      match d with
      | DEnum(_, _, [], "ExpectedApplicableButNot", [ actualTyp; actualValue ]) ->
        RuntimeError.Applications.ExpectedApplicableButNot(
          ValueType.fromDT actualTyp,
          Dval.fromDT actualValue
        )

      | DEnum(_, _, [], "WrongNumberOfTypeArgsForFn", [ fn; expected; actual ]) ->
        RuntimeError.Applications.WrongNumberOfTypeArgsForFn(
          FQFnName.fromDT fn,
          D.int64 expected,
          D.int64 actual
        )
      | DEnum(_, _, [], "CannotApplyTypeArgsMoreThanOnce", []) ->
        RuntimeError.Applications.CannotApplyTypeArgsMoreThanOnce
      | DEnum(_, _, [], "TooManyArgsForFn", [ fn; expected; actual ]) ->
        RuntimeError.Applications.TooManyArgsForFn(
          FQFnName.fromDT fn,
          D.int64 expected,
          D.int64 actual
        )
      | DEnum(_,
              _,
              [],
              "FnParameterNotExpectedType",
              [ fnName; paramIndex; paramName; expectedType; actualType; actualValue ]) ->
        RuntimeError.Applications.FnParameterNotExpectedType(
          FQFnName.fromDT fnName,
          D.int64 paramIndex,
          D.string paramName,
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actualValue
        )
      | DEnum(_,
              _,
              [],
              "FnResultNotExpectedType",
              [ fnName; expectedType; actualType; actualValue ]) ->
        RuntimeError.Applications.FnResultNotExpectedType(
          FQFnName.fromDT fnName,
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actualValue
        )

      | DEnum(_, _, [], "CannotApplyTypeArgsToLambda", []) ->
        RuntimeError.Applications.CannotApplyTypeArgsToLambda

      | DEnum(_, _, [], "TooManyArgsForLambda", [ lambdaExprId; expected; actual ]) ->
        RuntimeError.Applications.TooManyArgsForLambda(
          D.uInt64 lambdaExprId,
          D.int64 expected,
          D.int64 actual
        )

      | _ -> Exception.raiseInternal "Invalid Applications.Error" []


  module Statements =
    let toDT (e : RuntimeError.Statements.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Statements.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Statements.FirstExpressionMustBeUnit(expectedType,
                                                            actualType,
                                                            actualValue) ->
          "FirstExpressionMustBeUnit",
          [ ValueType.toDT expectedType
            ValueType.toDT actualType
            Dval.toDT actualValue ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Statements.Error =
      match d with
      | DEnum(_,
              _,
              [],
              "FirstExpressionMustBeUnit",
              [ expectedType; actualType; actualValue ]) ->
        RuntimeError.Statements.FirstExpressionMustBeUnit(
          ValueType.fromDT expectedType,
          ValueType.fromDT actualType,
          Dval.fromDT actualValue
        )
      | _ -> Exception.raiseInternal "Invalid Statements.Error" []


  module Unwraps =
    let toDT (e : RuntimeError.Unwraps.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Unwraps.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Unwraps.GotNone -> "GotNone", []
        | RuntimeError.Unwraps.GotError err -> "GotError", [ Dval.toDT err ]
        | RuntimeError.Unwraps.NonOptionOrResult actual ->
          "NonOptionOrResult", [ Dval.toDT actual ]
        | RuntimeError.Unwraps.MultipleArgs args ->
          "MultipleArgs", [ DList(VT.known Dval.knownType, List.map Dval.toDT args) ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Unwraps.Error =
      match d with
      | DEnum(_, _, [], "GotNone", []) -> RuntimeError.Unwraps.GotNone
      | DEnum(_, _, [], "GotError", [ err ]) ->
        RuntimeError.Unwraps.GotError(Dval.fromDT err)
      | DEnum(_, _, [], "NonOptionOrResult", [ actual ]) ->
        RuntimeError.Unwraps.NonOptionOrResult(Dval.fromDT actual)
      | DEnum(_, _, [], "MultipleArgs", [ args ]) ->
        args |> D.list Dval.fromDT |> RuntimeError.Unwraps.MultipleArgs
      | _ -> Exception.raiseInternal "Invalid Unwraps.Error" []

  module Jsons =
    let toDT (e : RuntimeError.Jsons.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.Jsons.error

      let (caseName, fields) =
        match e with
        | RuntimeError.Jsons.UnsupportedType actual ->
          "UnsupportedType", [ TypeReference.toDT actual ]
        | RuntimeError.Jsons.CannotSerializeValue(actualValue) ->
          "CannotSerializeValue", [ Dval.toDT actualValue ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.Jsons.Error =
      match d with
      | DEnum(_, _, [], "UnsupportedType", [ actual ]) ->
        RuntimeError.Jsons.UnsupportedType(TypeReference.fromDT actual)
      | DEnum(_, _, [], "CannotSerializeValue", [ actualValue ]) ->
        RuntimeError.Jsons.CannotSerializeValue(Dval.fromDT actualValue)
      | _ -> Exception.raiseInternal "Invalid Jsons.Error" []

  module CLIs =
    let toDT (e : RuntimeError.CLIs.Error) : Dval =
      let typeName =
        FQTypeName.fqPackage
          PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.CLIs.error

      let (caseName, fields) =
        match e with
        | RuntimeError.CLIs.NoExpressionsToExecute -> "NoExpressionsToExecute", []
        | RuntimeError.CLIs.NonIntReturned actualReturned ->
          "NonIntReturned", [ Dval.toDT actualReturned ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : RuntimeError.CLIs.Error =
      match d with
      | DEnum(_, _, [], "NoExpressionsToExecute", []) ->
        RuntimeError.CLIs.NoExpressionsToExecute
      | DEnum(_, _, [], "NonIntReturned", [ actualReturned ]) ->
        RuntimeError.CLIs.NonIntReturned(Dval.fromDT actualReturned)
      | _ -> Exception.raiseInternal "Invalid CLIs.Error" []


  let toDT (e : RuntimeError.Error) : Dval =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.error

    let (caseName, fields) =
      match e with
      | RuntimeError.Bool e -> "Bool", [ Bools.toDT e ]
      | RuntimeError.Int e -> "Int", [ Ints.toDT e ]
      | RuntimeError.String e -> "String", [ Strings.toDT e ]
      | RuntimeError.List e -> "List", [ Lists.toDT e ]
      | RuntimeError.Dict e -> "Dict", [ Dicts.toDT e ]
      | RuntimeError.Let e -> "Let", [ Lets.toDT e ]
      | RuntimeError.VariableNotFound attemptedVarName ->
        "VariableNotFound", [ DString attemptedVarName ]
      | RuntimeError.EqualityCheckOnIncompatibleTypes(left, right) ->
        "EqualityCheckOnIncompatibleTypes",
        [ ValueType.toDT left; ValueType.toDT right ]
      | RuntimeError.IfConditionNotBool(actualValue, actualValueType) ->
        "IfConditionNotBool",
        [ Dval.toDT actualValue; ValueType.toDT actualValueType ]
      | RuntimeError.Match e -> "Match", [ Matches.toDT e ]
      | RuntimeError.ParseTimeNameResolution e ->
        "ParseTimeNameResolution", [ NameResolutionError.toDT e ]
      | RuntimeError.TypeNotFound name -> "TypeNotFound", [ FQTypeName.toDT name ]
      | RuntimeError.ValueNotFound name -> "ValueNotFound", [ FQValueName.toDT name ]
      | RuntimeError.FnNotFound name -> "FnNotFound", [ FQFnName.toDT name ]
      | RuntimeError.WrongNumberOfTypeArgsForType(fn, expected, actual) ->
        "WrongNumberOfTypeArgsForType",
        [ FQTypeName.toDT fn; DInt64 expected; DInt64 actual ]
      | RuntimeError.Record e -> "Record", [ Records.toDT e ]
      | RuntimeError.Enum e -> "Enum", [ Enums.toDT e ]
      | RuntimeError.Apply e -> "Apply", [ Applications.toDT e ]
      | RuntimeError.Statement e -> "Statement", [ Statements.toDT e ]
      | RuntimeError.Unwrap e -> "Unwrap", [ Unwraps.toDT e ]
      | RuntimeError.Json e -> "Json", [ Jsons.toDT e ]
      | RuntimeError.CLI e -> "CLI", [ CLIs.toDT e ]
      | RuntimeError.UncaughtException(msg, metadata) ->
        "UncaughtException",
        [ DString msg
          DList(
            VT.tuple VT.string (VT.known Dval.knownType) [],
            metadata |> List.map (fun (k, v) -> DTuple(DString k, Dval.toDT v, []))
          ) ]
      | e -> Exception.raiseInternal "Unhandled RuntimeError.Error" [ "e", e ]

    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : RuntimeError.Error =
    match d with
    | DEnum(_, _, [], "Bool", [ e ]) -> RuntimeError.Bool(Bools.fromDT e)
    | DEnum(_, _, [], "Int", [ e ]) -> RuntimeError.Int(Ints.fromDT e)
    | DEnum(_, _, [], "String", [ e ]) -> RuntimeError.String(Strings.fromDT e)
    | DEnum(_, _, [], "List", [ e ]) -> RuntimeError.List(Lists.fromDT e)
    | DEnum(_, _, [], "Dict", [ e ]) -> RuntimeError.Dict(Dicts.fromDT e)
    | DEnum(_, _, [], "Let", [ e ]) -> RuntimeError.Let(Lets.fromDT e)
    | DEnum(_, _, [], "VariableNotFound", [ DString attemptedVarName ]) ->
      RuntimeError.VariableNotFound attemptedVarName
    | DEnum(_, _, [], "EqualityCheckOnIncompatibleTypes", [ left; right ]) ->
      RuntimeError.EqualityCheckOnIncompatibleTypes(
        ValueType.fromDT left,
        ValueType.fromDT right
      )
    | DEnum(_, _, [], "IfConditionNotBool", [ actualValue; actualValueType ]) ->
      RuntimeError.IfConditionNotBool(
        Dval.fromDT actualValue,
        ValueType.fromDT actualValueType
      )
    | DEnum(_, _, [], "Match", [ e ]) -> RuntimeError.Match(Matches.fromDT e)
    | DEnum(_, _, [], "ParseTimeNameResolution", [ e ]) ->
      RuntimeError.ParseTimeNameResolution(NameResolutionError.fromDT e)
    | DEnum(_, _, [], "TypeNotFound", [ name ]) ->
      RuntimeError.TypeNotFound(FQTypeName.fromDT name)
    | DEnum(_, _, [], "ValueNotFound", [ name ]) ->
      RuntimeError.ValueNotFound(FQValueName.fromDT name)
    | DEnum(_, _, [], "FnNotFound", [ name ]) ->
      RuntimeError.FnNotFound(FQFnName.fromDT name)
    | DEnum(_, _, [], "WrongNumberOfTypeArgsForType", [ fn; expected; actual ]) ->
      RuntimeError.WrongNumberOfTypeArgsForType(
        FQTypeName.fromDT fn,
        D.int64 expected,
        D.int64 actual
      )
    | DEnum(_, _, [], "Record", [ e ]) -> RuntimeError.Record(Records.fromDT e)
    | DEnum(_, _, [], "Enum", [ e ]) -> RuntimeError.Enum(Enums.fromDT e)
    | DEnum(_, _, [], "Apply", [ e ]) -> RuntimeError.Apply(Applications.fromDT e)
    | DEnum(_, _, [], "Statement", [ e ]) ->
      RuntimeError.Statement(Statements.fromDT e)
    | DEnum(_, _, [], "Unwrap", [ e ]) -> RuntimeError.Unwrap(Unwraps.fromDT e)
    | DEnum(_, _, [], "Json", [ e ]) -> RuntimeError.Json(Jsons.fromDT e)
    | DEnum(_, _, [], "CLI", [ e ]) -> RuntimeError.CLI(CLIs.fromDT e)
    | DEnum(_, _, [], "UncaughtException", [ DString msg; DList(_, metadata) ]) ->
      RuntimeError.UncaughtException(
        msg,
        metadata
        |> List.map (fun d ->
          match Dval.fromDT d with
          | DTuple(DString k, v, []) -> (k, v)
          | _ -> Exception.raiseInternal "Invalid metadata" [])
      )
    | _ -> Exception.raiseInternal "Invalid RuntimeError.Error" [ "d", d ]
