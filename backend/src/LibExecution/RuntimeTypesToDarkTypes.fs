module rec LibExecution.RuntimeTypesToDarkTypes

open Prelude

open RuntimeTypes

module VT = ValueType
module D = DvalDecoder
module C2DT = LibExecution.CommonToDarkTypes


// TODO: should these be elsewhere?
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

    let toDT (u : FQTypeName.Package) : Dval = DUuid u

    let fromDT (d : Dval) : FQTypeName.Package =
      match d with
      | DUuid u -> u
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


module FQConstantName =
  let typeName =
    FQTypeName.Package
      PackageIDs.Type.LanguageTools.RuntimeTypes.FQConstantName.fqConstantName
  let knownType = KTCustomType(typeName, [])

  module Builtin =
    let toDT (u : FQConstantName.Builtin) : Dval =
      let fields = [ "name", DString u.name; "version", DInt64 u.version ]
      let typeName =
        FQTypeName.Package
          PackageIDs.Type.LanguageTools.RuntimeTypes.FQConstantName.builtin
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : FQConstantName.Builtin =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = nameField fields; version = versionField fields }
      | _ -> Exception.raiseInternal "Invalid FQConstantName.Builtin" []

  module Package =
    let toDT (u : FQConstantName.Package) : Dval = DUuid u

    let fromDT (d : Dval) : FQConstantName.Package =
      match d with
      | DUuid id -> id
      | _ -> Exception.raiseInternal "Invalid FQConstantName.Package" []

  let toDT (u : FQConstantName.FQConstantName) : Dval =
    let (caseName, fields) =
      match u with
      | FQConstantName.Builtin u -> "Builtin", [ Builtin.toDT u ]
      | FQConstantName.Package u -> "Package", [ Package.toDT u ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : FQConstantName.FQConstantName =
    match d with
    | DEnum(_, _, [], "Builtin", [ u ]) -> FQConstantName.Builtin(Builtin.fromDT u)
    | DEnum(_, _, [], "Package", [ u ]) -> FQConstantName.Package(Package.fromDT u)
    | _ -> Exception.raiseInternal "Invalid FQConstantName" []


module FQFnName =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.FQFnName.fqFnName
  let knownType = KTCustomType(typeName, [])

  module Builtin =
    let typeName =
      FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.FQFnName.builtin

    let toDT (u : FQFnName.Builtin) : Dval =
      let fields = [ "name", DString u.name; "version", DInt64 u.version ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : FQFnName.Builtin =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = nameField fields; version = versionField fields }
      | _ -> Exception.raiseInternal "Invalid FQFnName.Builtin" []

  module Package =
    let toDT (u : FQFnName.Package) : Dval = DUuid u

    let fromDT (d : Dval) : FQFnName.Package =
      match d with
      | DUuid u -> u
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




module NameResolution =
  let typeName =
    FQTypeName.Package
      PackageIDs.Type.LanguageTools.RuntimeError.NameResolution.error

// let toDT
//   (nameValueType : KnownType)
//   (f : 'p -> Dval)
//   (result : NameResolution<'p>)
//   : Dval =
//   let errType = KTCustomType(typeName, [])
//   C2DT.Result.toDT nameValueType errType result f RuntimeError.toDT

// let fromDT (f : Dval -> 'a) (d : Dval) : NameResolution<'a> =
//   C2DT.Result.fromDT f d RuntimeError.fromDT


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
      | TInt64 -> "TInt64", []
      | TUInt64 -> "TUInt64", []
      | TInt8 -> "TInt8", []
      | TUInt8 -> "TUInt8", []
      | TInt16 -> "TInt16", []
      | TUInt16 -> "TUInt16", []
      | TInt32 -> "TInt32", []
      | TUInt32 -> "TUInt32", []
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

      // | TCustomType(typeName, typeArgs) ->
      //   "TCustomType",
      //   [ NameResolution.toDT FQTypeName.knownType FQTypeName.toDT typeName
      //     DList(VT.known knownType, List.map toDT typeArgs) ]

      // | TDB inner -> "TDB", [ toDT inner ]
      | TFn(args, ret) ->
        let args = args |> NEList.toList |> List.map toDT |> Dval.list knownType
        "TFn", [ args; toDT ret ]

      // TODO: remove this
      | _ -> Exception.raiseInternal "Invalid TypeReference" []

    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : TypeReference =
    match d with
    | DEnum(_, _, [], "TVariable", [ DString name ]) -> TVariable(name)

    | DEnum(_, _, [], "TUnit", []) -> TUnit
    | DEnum(_, _, [], "TBool", []) -> TBool
    | DEnum(_, _, [], "TInt64", []) -> TInt64
    | DEnum(_, _, [], "TUInt64", []) -> TUInt64
    | DEnum(_, _, [], "TInt8", []) -> TInt8
    | DEnum(_, _, [], "TUInt8", []) -> TUInt8
    | DEnum(_, _, [], "TInt16", []) -> TInt16
    | DEnum(_, _, [], "TUInt16", []) -> TUInt16
    | DEnum(_, _, [], "TInt32", []) -> TInt32
    | DEnum(_, _, [], "TUInt32", []) -> TUInt32
    | DEnum(_, _, [], "TInt128", []) -> TInt128
    | DEnum(_, _, [], "TUInt128", []) -> TUInt128
    | DEnum(_, _, [], "TFloat", []) -> TFloat
    | DEnum(_, _, [], "TChar", []) -> TChar
    | DEnum(_, _, [], "TString", []) -> TString
    | DEnum(_, _, [], "TDateTime", []) -> TDateTime
    | DEnum(_, _, [], "TUuid", []) -> TUuid

    | DEnum(_, _, [], "TList", [ inner ]) -> TList(fromDT inner)

    | DEnum(_, _, [], "TTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "TDict", [ inner ]) -> TDict(fromDT inner)

    // | DEnum(_, _, [], "TCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
    //   TCustomType(
    //     NameResolution.fromDT FQTypeName.fromDT typeName,
    //     List.map fromDT typeArgs
    //   )

    // | DEnum(_, _, [], "TDB", [ inner ]) -> TDB(fromDT inner)
    | DEnum(_, _, [], "TFn", [ DList(_vtTODO, firstArg :: otherArgs); ret ]) ->
      TFn(NEList.ofList (fromDT firstArg) (List.map fromDT otherArgs), fromDT ret)
    | _ -> Exception.raiseInternal "Invalid TypeReference" [ "typeRef", d ]


module Param =
  let typeName = FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.param

  let toDT (p : Param) : Dval =
    let fields = [ ("name", DString p.name); ("typ", TypeReference.toDT p.typ) ]
    DRecord(typeName, typeName, [], Map fields)


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

      | MPList inner -> "MPList", [ DList(VT.known knownType, List.map toDT inner) ]
      | MPListCons(head, tail) -> "MPListCons", [ toDT head; toDT tail ]

      | MPTuple(first, second, theRest) ->
        "MPTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      | MPEnum(caseName, fieldPats) ->
        "MPEnum",
        [ DString caseName; DList(VT.known knownType, List.map toDT fieldPats) ]

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

    | DEnum(_, _, [], "MPList", [ DList(_vtTODO, inner) ]) ->
      MPList(List.map fromDT inner)
    | DEnum(_, _, [], "MPListCons", [ head; tail ]) ->
      MPListCons(fromDT head, fromDT tail)

    | DEnum(_, _, [], "MPTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      MPTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "MPEnum", [ DString caseName; DList(_vtTODO, fieldPats) ]) ->
      MPEnum(caseName, List.map fromDT fieldPats)

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


// module RuntimeError =
//   let toDT (e : RuntimeError.Error) : Dval =
//     e |> RuntimeTypes.RuntimeError.toDT |> Dval.toDT

//   let fromDT (d : Dval) : RuntimeError.Error =
//     d |> Dval.fromDT |> RuntimeTypes.RuntimeError.fromDT


module KnownType =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.knownType

  let toDT (kt : KnownType) : Dval =
    let (caseName, fields) =
      match kt with
      | KTUnit -> "KTUnit", []
      | KTBool -> "KTBool", []
      | KTInt64 -> "KTInt64", []
      | KTUInt64 -> "KTUInt64", []
      | KTInt8 -> "KTInt8", []
      | KTUInt8 -> "KTUInt8", []
      | KTInt16 -> "KTInt16", []
      | KTUInt16 -> "KTUInt16", []
      | KTInt32 -> "KTInt32", []
      | KTUInt32 -> "KTUInt32", []
      | KTInt128 -> "KTInt128", []
      | KTUInt128 -> "KTUInt128", []
      | KTFloat -> "KTFloat", []
      | KTChar -> "KTChar", []
      | KTString -> "KTString", []
      | KTUuid -> "KTUuid", []
      | KTDateTime -> "KTDateTime", []

      | KTList inner -> "KTList", [ ValueType.toDT inner ]
      | KTTuple(first, second, theRest) ->
        "KTTuple",
        [ ValueType.toDT first
          ValueType.toDT second
          DList(VT.known ValueType.knownType, List.map ValueType.toDT theRest) ]
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

    // | KTDB d -> "KTDB", [ ValueType.toDT d ]

    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : KnownType =
    match d with
    | DEnum(_, _, [], "KTUnit", []) -> KTUnit
    | DEnum(_, _, [], "KTBool", []) -> KTBool
    | DEnum(_, _, [], "KTInt64", []) -> KTInt64
    | DEnum(_, _, [], "KTUInt64", []) -> KTUInt64
    | DEnum(_, _, [], "KTInt8", []) -> KTInt8
    | DEnum(_, _, [], "KTUInt8", []) -> KTUInt8
    | DEnum(_, _, [], "KTInt16", []) -> KTInt16
    | DEnum(_, _, [], "KTUInt16", []) -> KTUInt16
    | DEnum(_, _, [], "KTInt32", []) -> KTInt32
    | DEnum(_, _, [], "KTUInt32", []) -> KTUInt32
    | DEnum(_, _, [], "KTInt128", []) -> KTInt128
    | DEnum(_, _, [], "KTUInt128", []) -> KTUInt128
    | DEnum(_, _, [], "KTFloat", []) -> KTFloat
    | DEnum(_, _, [], "KTChar", []) -> KTChar
    | DEnum(_, _, [], "KTString", []) -> KTString
    | DEnum(_, _, [], "KTUuid", []) -> KTUuid
    | DEnum(_, _, [], "KTDateTime", []) -> KTDateTime

    | DEnum(_, _, [], "KTList", [ inner ]) -> KTList(ValueType.fromDT inner)
    | DEnum(_, _, [], "KTTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      KTTuple(
        ValueType.fromDT first,
        ValueType.fromDT second,
        List.map ValueType.fromDT theRest
      )
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
    // | DEnum(_, _, [], "KTDB", [ inner ]) -> KTDB(ValueType.fromDT inner)

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


// module LambdaImpl =
//   let typeName =
//     FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.lambdaImpl

//   let toDT (l : LambdaImpl) : Dval =
//     let parameters =
//       l.parameters
//       |> NEList.toList
//       |> List.map LetPattern.toDT
//       |> fun p -> DList(VT.tuple VT.int64 VT.string [], p)
//     let fields =
//       [ ("typeSymbolTable",
//          DDict(
//            VT.known TypeReference.knownType,
//            Map.map TypeReference.toDT l.typeSymbolTable
//          ))
//         ("symtable", DDict(VT.known Dval.knownType, Map.map Dval.toDT l.symtable))
//         ("parameters", parameters)
//         ("body", Expr.toDT l.body) ]

//     DRecord(typeName, typeName, [], Map fields)

//   let fromDT (d : Dval) : LambdaImpl =
//     match d with
//     | DRecord(_, _, _, fields) ->
//       { typeSymbolTable =
//           fields |> D.mapField "typeSymbolTable" |> Map.map TypeReference.fromDT

//         symtable = fields |> D.mapField "symtable" |> Map.map Dval.fromDT

//         parameters =
//           fields
//           |> D.listField "parameters"
//           |> List.map LetPattern.fromDT
//           |> NEList.ofListUnsafe
//             "RT2DT.Dval.fromDT expected at least one parameter in LambdaImpl"
//             []

//         body = fields |> D.field "body" |> Expr.fromDT }

//     | _ -> Exception.raiseInternal "Invalid LambdaImpl" []

// module FnValImpl =
//   let typeName =
//     FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.fnValImpl

//   let toDT (fnValImpl : FnValImpl) : Dval =
//     let (caseName, fields) =
//       match fnValImpl with
//       //| Lambda lambda -> "Lambda", [ LambdaImpl.toDT lambda ]
//       | NamedFn fnName -> "NamedFn", [ FQFnName.toDT fnName ]
//     DEnum(typeName, typeName, [], caseName, fields)

//   let fromDT (d : Dval) : FnValImpl =
//     match d with
//     //| DEnum(_, _, [], "Lambda", [ lambda ]) -> Lambda(LambdaImpl.fromDT lambda)
//     | DEnum(_, _, [], "NamedFn", [ fnName ]) -> NamedFn(FQFnName.fromDT fnName)
//     | _ -> Exception.raiseInternal "Invalid FnValImpl" []

module Applicable =
  let toDT (applicable : Applicable) : Dval =
    match applicable with
    | AppLambda _lambda -> DUnit // TODO
    | AppNamedFn fnName -> FQFnName.toDT fnName.name



module Dval =
  let typeName = FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.dval
  let knownType = KTCustomType(typeName, [])

  let rec toDT (dv : Dval) : Dval =
    let (caseName, fields) =
      match dv with
      | DUnit -> "DUnit", []
      | DBool b -> "DBool", [ DBool b ]
      | DInt64 i -> "DInt64", [ DInt64 i ]
      | DUInt64 i -> "DUInt64", [ DUInt64 i ]
      | DInt8 i -> "DInt8", [ DInt8 i ]
      | DUInt8 i -> "DUInt8", [ DUInt8 i ]
      | DInt16 i -> "DInt16", [ DInt16 i ]
      | DUInt16 i -> "DUInt16", [ DUInt16 i ]
      | DInt32 i -> "DInt32", [ DInt32 i ]
      | DUInt32 i -> "DUInt32", [ DUInt32 i ]
      | DInt128 i -> "DInt128", [ DInt128 i ]
      | DUInt128 i -> "DUInt128", [ DUInt128 i ]
      | DFloat f -> "DFloat", [ DFloat f ]
      | DChar c -> "DChar", [ DChar c ]
      | DString s -> "DString", [ DString s ]
      | DUuid u -> "DUuid", [ DUuid u ]
      | DDateTime d -> "DDateTime", [ DDateTime d ]

      | DList(vt, items) ->
        "DList",
        [ ValueType.toDT vt; DList(VT.known knownType, List.map toDT items) ]

      | DTuple(first, second, theRest) ->
        "DTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      // | DDB name -> "DDB", [ DString name ]

      | DApplicable applicable -> "DApplicable", [ Applicable.toDT applicable ]

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

    DEnum(typeName, typeName, [], caseName, fields)


  let fromDT (d : Dval) : Dval =
    match d with
    | DEnum(_, _, [], "DInt64", [ DInt64 i ]) -> DInt64 i
    | DEnum(_, _, [], "DUInt64", [ DUInt64 i ]) -> DUInt64 i
    | DEnum(_, _, [], "DInt8", [ DInt8 i ]) -> DInt8 i
    | DEnum(_, _, [], "DUInt8", [ DUInt8 i ]) -> DUInt8 i
    | DEnum(_, _, [], "DInt16", [ DInt16 i ]) -> DInt16 i
    | DEnum(_, _, [], "DUInt16", [ DUInt16 i ]) -> DUInt16 i
    | DEnum(_, _, [], "DInt32", [ DInt32 i ]) -> DInt32 i
    | DEnum(_, _, [], "DUInt32", [ DUInt32 i ]) -> DUInt32 i
    | DEnum(_, _, [], "DInt128", [ DInt128 i ]) -> DInt128 i
    | DEnum(_, _, [], "DUInt128", [ DUInt128 i ]) -> DUInt128 i
    | DEnum(_, _, [], "DFloat", [ DFloat f ]) -> DFloat f
    | DEnum(_, _, [], "DBool", [ DBool b ]) -> DBool b
    | DEnum(_, _, [], "DUnit", []) -> DUnit
    | DEnum(_, _, [], "DString", [ DString s ]) -> DString s
    | DEnum(_, _, [], "DChar", [ DChar c ]) -> DChar c

    | DEnum(_, _, [], "DList", [ vt; DList(_vtTODO, l) ]) ->
      DList(ValueType.fromDT vt, List.map fromDT l)
    | DEnum(_, _, [], "DTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      DTuple(fromDT first, fromDT second, List.map fromDT theRest)

    // | DEnum(_, _, [], "DDB", [ DString name ]) -> DDB name

    // | DEnum(_, _, [], "DApplicable", [ applicable ]) -> DApplicable(Applicable.fromDT applicable)

    | DEnum(_, _, [], "DDateTime", [ DDateTime d ]) -> DDateTime d
    | DEnum(_, _, [], "DUuid", [ DUuid u ]) -> DUuid u

    | DEnum(_, _, [], "DDict", [ vt; DDict(_vtTODO, map) ]) ->
      DDict(ValueType.fromDT vt, Map.map fromDT map)

    | DEnum(_,
            _,
            [],
            "DRecord",
            [ runtimeTypeName; sourceTypeName; DList(_, typeArgs); DDict(_, entries) ]) ->
      DRecord(
        FQTypeName.fromDT runtimeTypeName,
        FQTypeName.fromDT sourceTypeName,
        List.map ValueType.fromDT typeArgs,
        Map.map fromDT entries
      )
    | DEnum(_,
            _,
            [],
            "DEnum",
            [ runtimeTypeName
              sourceTypeName
              DList(_vtTODO1, typeArgs)
              DString caseName
              DList(_vtTODO2, fields) ]) ->
      DEnum(
        FQTypeName.fromDT runtimeTypeName,
        FQTypeName.fromDT sourceTypeName,
        List.map ValueType.fromDT typeArgs,
        caseName,
        List.map fromDT fields
      )

    | _ -> Exception.raiseInternal "Invalid Dval" []



// module RuntimeError =
//   module RT2DT = LibExecution.RuntimeTypesToDarkTypes

//   type Error =
//     /// In the future, we will add a trait to indicate types which can be serialized. For
//     /// now, we'll raise a RuntimeError instead if any of those types are present.
//     /// Helpfully, this allows us keep `serialize` from having to return an Error.
//     | UnsupportedType of TypeReference

//   let toRuntimeError (e : Error) : RuntimeError =
//     let (caseName, fields) =
//       match e with
//       | UnsupportedType typ -> "UnsupportedType", [ RT2DT.TypeReference.toDT typ ]

//     let typeName =
//       FQTypeName.fqPackage PackageIDs.Type.LanguageTools.RuntimeError.Json.error
//     DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError.jsonError

//   let raiseUnsupportedType (callStack : CallStack) (typ : TypeReference) : 'a =
//     UnsupportedType(typ) |> toRuntimeError |> raiseRTE callStack
