module rec LibExecution.RuntimeTypesToDarkTypes

open Prelude

open RuntimeTypes

module VT = ValueType
module D = DvalDecoder
module C2DT = LibExecution.CommonToDarkTypes


// TODO: should these be elsewhere?
let ownerField m = m |> D.stringField "owner"
let modulesField m = m |> D.stringListField "modules"
let nameField m = m |> D.stringField "name"
let versionField m = m |> D.int32Field "version"


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

  let toDT
    (nameValueType : KnownType)
    (f : 'p -> Dval)
    (result : NameResolution<'p>)
    : Dval =
    let errType = KTCustomType(typeName, [])
    C2DT.Result.toDT nameValueType errType result f RuntimeError.toDT

  let fromDT (f : Dval -> 'a) (d : Dval) : NameResolution<'a> =
    C2DT.Result.fromDT f d RuntimeError.fromDT


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

      | TCustomType(typeName, typeArgs) ->
        "TCustomType",
        [ NameResolution.toDT FQTypeName.knownType FQTypeName.toDT typeName
          DList(VT.known knownType, List.map toDT typeArgs) ]

      | TDB inner -> "TDB", [ toDT inner ]
      | TFn(args, ret) ->
        let args = args |> NEList.toList |> List.map toDT |> Dval.list knownType
        "TFn", [ args; toDT ret ]

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

    | DEnum(_, _, [], "TCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
      TCustomType(
        NameResolution.fromDT FQTypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, [], "TDB", [ inner ]) -> TDB(fromDT inner)
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
      | LPVariable(id, name) -> "LPVariable", [ DInt64(int64 id); DString name ]
      | LPUnit id -> "LPUnit", [ DInt64(int64 id) ]
      | LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt64(int64 id)
          toDT first
          toDT second
          DList(VT.known knownType, List.map toDT theRest) ]

    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : LetPattern =
    match d with
    | DEnum(_, _, [], "LPVariable", [ DInt64 id; DString name ]) ->
      LPVariable(uint64 id, name)
    | DEnum(_, _, [], "LPUnit", [ DInt64 id ]) -> LPUnit(uint64 id)
    | DEnum(_,
            _,
            [],
            "LPTuple",
            [ DInt64 id; first; second; DList(_vtTODO, theRest) ]) ->
      LPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.matchPattern
  let knownType = KTCustomType(typeName, [])

  let rec toDT (p : MatchPattern) : Dval =
    let (caseName, fields) =
      match p with
      | MPVariable(id, name) -> "MPVariable", [ DInt64(int64 id); DString name ]

      | MPUnit id -> "MPUnit", [ DInt64(int64 id) ]
      | MPBool(id, b) -> "MPBool", [ DInt64(int64 id); DBool b ]
      | MPInt8(id, i) -> "MPInt8", [ DInt64(int64 id); DInt8 i ]
      | MPUInt8(id, i) -> "MPUInt8", [ DInt64(int64 id); DUInt8 i ]
      | MPInt16(id, i) -> "MPInt16", [ DInt64(int64 id); DInt16 i ]
      | MPUInt16(id, i) -> "MPUInt16", [ DInt64(int64 id); DUInt16 i ]
      | MPInt32(id, i) -> "MPInt32", [ DInt64(int64 id); DInt32 i ]
      | MPUInt32(id, i) -> "MPUInt32", [ DInt64(int64 id); DUInt32 i ]
      | MPInt64(id, i) -> "MPInt64", [ DInt64(int64 id); DInt64 i ]
      | MPUInt64(id, i) -> "MPUInt64", [ DInt64(int64 id); DUInt64 i ]
      | MPInt128(id, i) -> "MPInt128", [ DInt64(int64 id); DInt128 i ]
      | MPUInt128(id, i) -> "MPUInt128", [ DInt64(int64 id); DUInt128 i ]
      | MPFloat(id, f) -> "MPFloat", [ DInt64(int64 id); DFloat f ]
      | MPChar(id, c) -> "MPChar", [ DInt64(int64 id); DString c ]
      | MPString(id, s) -> "MPString", [ DInt64(int64 id); DString s ]

      | MPList(id, inner) ->
        "MPList",
        [ DInt64(int64 id); DList(VT.known knownType, List.map toDT inner) ]
      | MPListCons(id, head, tail) ->
        "MPListCons", [ DInt64(int64 id); toDT head; toDT tail ]

      | MPTuple(id, first, second, theRest) ->
        "MPTuple",
        [ DInt64(int64 id)
          toDT first
          toDT second
          DList(VT.known knownType, List.map toDT theRest) ]

      | MPEnum(id, caseName, fieldPats) ->
        "MPEnum",
        [ DInt64(int64 id)
          DString caseName
          DList(VT.known knownType, List.map toDT fieldPats) ]

    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : MatchPattern =
    match d with
    | DEnum(_, _, [], "MPVariable", [ DInt64 id; DString name ]) ->
      MPVariable(uint64 id, name)

    | DEnum(_, _, [], "MPUnit", [ DInt64 id ]) -> MPUnit(uint64 id)
    | DEnum(_, _, [], "MPBool", [ DInt64 id; DBool b ]) -> MPBool(uint64 id, b)
    | DEnum(_, _, [], "MPInt8", [ DInt64 id; DInt8 i ]) -> MPInt8(uint64 id, i)
    | DEnum(_, _, [], "MPUInt8", [ DInt64 id; DUInt8 i ]) -> MPUInt8(uint64 id, i)
    | DEnum(_, _, [], "MPInt16", [ DInt64 id; DInt16 i ]) -> MPInt16(uint64 id, i)
    | DEnum(_, _, [], "MPUInt16", [ DInt64 id; DUInt16 i ]) -> MPUInt16(uint64 id, i)
    | DEnum(_, _, [], "MPInt32", [ DInt64 id; DInt32 i ]) -> MPInt32(uint64 id, i)
    | DEnum(_, _, [], "MPUInt32", [ DInt64 id; DUInt32 i ]) -> MPUInt32(uint64 id, i)
    | DEnum(_, _, [], "MPInt64", [ DInt64 id; DInt64 i ]) -> MPInt64(uint64 id, i)
    | DEnum(_, _, [], "MPUInt64", [ DInt64 id; DUInt64 i ]) -> MPUInt64(uint64 id, i)
    | DEnum(_, _, [], "MPInt128", [ DInt64 id; DInt128 i ]) -> MPInt128(uint64 id, i)
    | DEnum(_, _, [], "MPUInt128", [ DInt64 id; DUInt128 i ]) ->
      MPUInt128(uint64 id, i)
    | DEnum(_, _, [], "MPFloat", [ DInt64 id; DFloat f ]) -> MPFloat(uint64 id, f)
    | DEnum(_, _, [], "MPChar", [ DInt64 id; DString c ]) -> MPChar(uint64 id, c)
    | DEnum(_, _, [], "MPString", [ DInt64 id; DString s ]) -> MPString(uint64 id, s)

    | DEnum(_, _, [], "MPList", [ DInt64 id; DList(_vtTODO, inner) ]) ->
      MPList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, [], "MPListCons", [ DInt64 id; head; tail ]) ->
      MPListCons(uint64 id, fromDT head, fromDT tail)

    | DEnum(_,
            _,
            [],
            "MPTuple",
            [ DInt64 id; first; second; DList(_vtTODO, theRest) ]) ->
      MPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_,
            _,
            [],
            "MPEnum",
            [ DInt64 id; DString caseName; DList(_vtTODO, fieldPats) ]) ->
      MPEnum(uint64 id, caseName, List.map fromDT fieldPats)

    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module StringSegment =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.stringSegment
  let knownType = KTCustomType(typeName, [])

  let toDT (exprToDT : Expr -> Dval) (s : StringSegment) : Dval =
    let (caseName, fields) =
      match s with
      | StringText text -> "StringText", [ DString text ]
      | StringInterpolation expr -> "StringInterpolation", [ exprToDT expr ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (exprFromDT : Dval -> Expr) (d : Dval) : StringSegment =
    match d with
    | DEnum(_, _, [], "StringText", [ DString text ]) -> StringText text
    | DEnum(_, _, [], "StringInterpolation", [ expr ]) ->
      StringInterpolation(exprFromDT expr)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


// module Expr =
//   let typeName = FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.expr
//   let knownType = KTCustomType(typeName, [])

//   let rec toDT (e : Expr) : Dval =
//     let (caseName, fields) =
//       match e with
//       | EUnit id -> "EUnit", [ DInt64(int64 id) ]

//       | EBool(id, b) -> "EBool", [ DInt64(int64 id); DBool b ]
//       | EInt64(id, i) -> "EInt64", [ DInt64(int64 id); DInt64 i ]
//       | EUInt64(id, i) -> "EUInt64", [ DInt64(int64 id); DUInt64 i ]
//       | EInt8(id, i) -> "EInt8", [ DInt64(int64 id); DInt8 i ]
//       | EUInt8(id, i) -> "EUInt8", [ DInt64(int64 id); DUInt8 i ]
//       | EInt16(id, i) -> "EInt16", [ DInt64(int64 id); DInt16 i ]
//       | EUInt16(id, i) -> "EUInt16", [ DInt64(int64 id); DUInt16 i ]
//       | EInt32(id, i) -> "EInt32", [ DInt64(int64 id); DInt32 i ]
//       | EUInt32(id, i) -> "EUInt32", [ DInt64(int64 id); DUInt32 i ]
//       | EInt128(id, i) -> "EInt128", [ DInt64(int64 id); DInt128 i ]
//       | EUInt128(id, i) -> "EUInt128", [ DInt64(int64 id); DUInt128 i ]
//       | EFloat(id, f) -> "EFloat", [ DInt64(int64 id); DFloat f ]
//       | EChar(id, c) -> "EChar", [ DInt64(int64 id); DString c ]
//       | EString(id, segments) ->
//         let segments =
//           DList(
//             VT.known StringSegment.knownType,
//             List.map (StringSegment.toDT toDT) segments
//           )
//         "EString", [ DInt64(int64 id); segments ]

//       | EList(id, exprs) ->
//         "EList", [ DInt64(int64 id); Dval.list knownType (List.map toDT exprs) ]

//       | EDict(id, entries) ->
//         let entries =
//           entries
//           |> List.map (fun (k, v) -> DTuple(DString k, toDT v, []))
//           |> fun entries ->
//               DList(VT.tuple VT.string (ValueType.known knownType) [], entries)
//         "EDict", [ DInt64(int64 id); entries ]

//       | ETuple(id, first, second, theRest) ->
//         "ETuple",
//         [ DInt64(int64 id)
//           toDT first
//           toDT second
//           Dval.list knownType (List.map toDT theRest) ]

//       | ERecord(id, typeName, fields) ->
//         let fields =
//           fields
//           |> NEList.toList
//           |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))
//           |> fun fields ->
//               DList(VT.tuple VT.string (ValueType.known knownType) [], fields)
//         "ERecord", [ DInt64(int64 id); FQTypeName.toDT typeName; fields ]

//       | EEnum(id, typeName, caseName, fields) ->
//         "EEnum",
//         [ DInt64(int64 id)
//           FQTypeName.toDT typeName
//           DString caseName
//           Dval.list knownType (List.map toDT fields) ]

//       // declaring and accessing variables
//       | ELet(id, lp, expr, body) ->
//         "ELet", [ DInt64(int64 id); LetPattern.toDT lp; toDT expr; toDT body ]

//       | ERecordFieldAccess(id, expr, fieldName) ->
//         "ERecordFieldAccess", [ DInt64(int64 id); toDT expr; DString fieldName ]

//       | EVariable(id, varName) -> "EVariable", [ DInt64(int64 id); DString varName ]


//       // control flow
//       | EIf(id, cond, thenExpr, elseExpr) ->
//         "EIf",
//         [ DInt64(int64 id)
//           toDT cond
//           toDT thenExpr
//           elseExpr |> Option.map toDT |> Dval.option knownType ]

//       | EMatch(id, arg, cases) ->
//         let matchCaseTypeName =
//           FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.matchCase

//         let cases =
//           cases
//           |> NEList.toList
//           |> List.map (fun case ->
//             let pattern = MatchPattern.toDT case.pat
//             let whenCondition =
//               case.whenCondition |> Option.map toDT |> Dval.option knownType
//             let expr = toDT case.rhs
//             DRecord(
//               matchCaseTypeName,
//               matchCaseTypeName,
//               [],
//               Map
//                 [ ("pat", pattern)
//                   ("whenCondition", whenCondition)
//                   ("rhs", expr) ]
//             ))
//           |> Dval.list (KTCustomType(matchCaseTypeName, []))
//         "EMatch", [ DInt64(int64 id); toDT arg; cases ]


//       | ELambda(id, pats, body) ->
//         let variables =
//           (NEList.toList pats)
//           |> List.map LetPattern.toDT
//           |> Dval.list (KTTuple(VT.int64, VT.string, []))
//         "ELambda", [ DInt64(int64 id); variables; toDT body ]

//       | EConstant(id, name) ->
//         "EConstant", [ DInt64(int64 id); FQConstantName.toDT name ]

//       | EApply(id, expr, typeArgs, args) ->
//         let typeArgs =
//           typeArgs
//           |> List.map TypeReference.toDT
//           |> Dval.list TypeReference.knownType
//         let args =
//           Dval.list TypeReference.knownType (args |> NEList.toList |> List.map toDT)
//         "EApply", [ DInt64(int64 id); toDT expr; typeArgs; args ]

//       | EFnName(id, name) -> "EFnName", [ DInt64(int64 id); FQFnName.toDT name ]

//       | ERecordUpdate(id, record, updates) ->
//         let updates =
//           NEList.toList updates
//           |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))
//           |> Dval.list (KTTuple(VT.string, VT.known knownType, []))
//         "ERecordUpdate", [ DInt64(int64 id); toDT record; updates ]

//       | EAnd(id, left, right) -> "EAnd", [ DInt64(int64 id); toDT left; toDT right ]

//       | EOr(id, left, right) -> "EOr", [ DInt64(int64 id); toDT left; toDT right ]

//       // Let the error straight through
//       | EError(id, rtError, exprs) ->
//         "EError",
//         [ DInt64(int64 id)
//           RuntimeTypes.RuntimeError.toDT rtError
//           Dval.list knownType (List.map toDT exprs) ]


//     DEnum(typeName, typeName, [], caseName, fields)

//   let rec fromDT (d : Dval) : Expr =
//     match d with
//     | DEnum(_, _, [], "EUnit", [ DInt64 id ]) -> EUnit(uint64 id)

//     | DEnum(_, _, [], "EBool", [ DInt64 id; DBool b ]) -> EBool(uint64 id, b)
//     | DEnum(_, _, [], "EInt64", [ DInt64 id; DInt64 i ]) -> EInt64(uint64 id, i)
//     | DEnum(_, _, [], "EUInt64", [ DInt64 id; DUInt64 i ]) -> EUInt64(uint64 id, i)
//     | DEnum(_, _, [], "EInt8", [ DInt64 id; DInt8 i ]) -> EInt8(uint64 id, i)
//     | DEnum(_, _, [], "EUInt8", [ DInt64 id; DUInt8 i ]) -> EUInt8(uint64 id, i)
//     | DEnum(_, _, [], "EInt16", [ DInt64 id; DInt16 i ]) -> EInt16(uint64 id, i)
//     | DEnum(_, _, [], "EUInt16", [ DInt64 id; DUInt16 i ]) -> EUInt16(uint64 id, i)
//     | DEnum(_, _, [], "EInt32", [ DInt64 id; DInt32 i ]) -> EInt32(uint64 id, i)
//     | DEnum(_, _, [], "EUInt32", [ DInt64 id; DUInt32 i ]) -> EUInt32(uint64 id, i)
//     | DEnum(_, _, [], "EInt128", [ DInt64 id; DInt128 i ]) -> EInt128(uint64 id, i)
//     | DEnum(_, _, [], "EUInt128", [ DInt64 id; DUInt128 i ]) ->
//       EUInt128(uint64 id, i)
//     | DEnum(_, _, [], "EFloat", [ DInt64 id; DFloat f ]) -> EFloat(uint64 id, f)
//     | DEnum(_, _, [], "EChar", [ DInt64 id; DString c ]) -> EChar(uint64 id, c)
//     | DEnum(_, _, [], "EString", [ DInt64 id; DList(_vtTODO, segments) ]) ->
//       EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


//     | DEnum(_, _, [], "EList", [ DInt64 id; DList(_vtTODO, inner) ]) ->
//       EList(uint64 id, List.map fromDT inner)

//     | DEnum(_, _, [], "EDict", [ DInt64 id; DList(_vtTODO, pairsList) ]) ->
//       let pairs =
//         pairsList
//         // TODO: this should be a List.map, and raise an exception
//         |> List.collect (fun pair ->
//           match pair with
//           | DTuple(DString k, v, _) -> [ (k, fromDT v) ]
//           | _ -> []) // TODO: raise exception
//       EDict(uint64 id, pairs)


//     | DEnum(_, _, [], "ETuple", [ DInt64 id; first; second; DList(_vtTODO, theRest) ]) ->
//       ETuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

//     | DEnum(_, _, [], "ERecord", [ DInt64 id; typeName; DList(_vtTODO1, fieldsList) ]) ->
//       let fields =
//         fieldsList
//         |> List.collect (fun field ->
//           match field with
//           | DTuple(DString name, expr, _) -> [ (name, fromDT expr) ]
//           | _ -> [])
//       ERecord(
//         uint64 id,
//         FQTypeName.fromDT typeName,
//         NEList.ofListUnsafe
//           "RT2DT.Expr.fromDT expected at least one field in ERecord"
//           []
//           fields
//       )

//     | DEnum(_,
//             _,
//             [],
//             "EEnum",
//             [ DInt64 id; typeName; DString caseName; DList(_vtTODO, fields) ]) ->
//       EEnum(uint64 id, FQTypeName.fromDT typeName, caseName, List.map fromDT fields)

//     | DEnum(_, _, [], "ELet", [ DInt64 id; lp; expr; body ]) ->
//       ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

//     | DEnum(_, _, [], "ERecordFieldAccess", [ DInt64 id; expr; DString fieldName ]) ->
//       ERecordFieldAccess(uint64 id, fromDT expr, fieldName)

//     | DEnum(_, _, [], "EVariable", [ DInt64 id; DString varName ]) ->
//       EVariable(uint64 id, varName)

//     | DEnum(_, _, [], "EIf", [ DInt64 id; cond; thenExpr; elseExpr ]) ->
//       let elseExpr =
//         match elseExpr with
//         | DEnum(_, _, _typeArgsDEnumTODO, "Some", [ dv ]) -> Some(fromDT dv)
//         | DEnum(_, _, _typeArgsDEnumTODO, "None", []) -> None
//         | _ ->
//           Exception.raiseInternal "Invalid else expression" [ "elseExpr", elseExpr ]
//       EIf(uint64 id, fromDT cond, fromDT thenExpr, elseExpr)

//     | DEnum(_, _, [], "EMatch", [ DInt64 id; arg; DList(_vtTODO, cases) ]) ->
//       let cases =
//         cases
//         |> List.collect (fun case ->
//           match case with
//           | DRecord(_, _, _, fields) ->
//             let whenCondition =
//               match Map.tryFind "whenCondition" fields with
//               | Some(DEnum(_, _, _, "Some", [ value ])) -> Some(fromDT value)
//               | Some(DEnum(_, _, _, "None", [])) -> None
//               | _ -> None
//             match Map.tryFind "pat" fields, Map.tryFind "rhs" fields with
//             | Some pat, Some rhs ->
//               [ { pat = MatchPattern.fromDT pat
//                   whenCondition = whenCondition
//                   rhs = fromDT rhs } ]
//             | _ -> []
//           | _ -> [])
//       EMatch(
//         uint64 id,
//         fromDT arg,
//         NEList.ofListUnsafe
//           "RT2DT.Expr.fromDT expected at least one case in EMatch"
//           []
//           cases
//       )

//     | DEnum(_, _, [], "ELambda", [ DInt64 id; DList(_vtTODO, pats); body ]) ->
//       let pats =
//         pats
//         |> List.map LetPattern.fromDT
//         |> NEList.ofListUnsafe
//           "RT2DT.Expr.fromDT expected at least one bound variable in ELambda"
//           []
//       ELambda(uint64 id, pats, fromDT body)


//     | DEnum(_,
//             _,
//             [],
//             "EApply",
//             [ DInt64 id; name; DList(_vtTODO1, typeArgs); DList(_vtTODO2, args) ]) ->
//       let args =
//         NEList.ofListUnsafe
//           "RT2DT.Expr.fromDT expected at least one argument in EApply"
//           []
//           args

//       EApply(
//         uint64 id,
//         fromDT name,
//         List.map TypeReference.fromDT typeArgs,
//         NEList.map fromDT args
//       )

//     | DEnum(_, _, [], "EFnName", [ DInt64 id; name ]) ->
//       EFnName(uint64 id, FQFnName.fromDT name)

//     | DEnum(_, _, [], "ERecordUpdate", [ DInt64 id; record; DList(_vtTODO, updates) ]) ->
//       let updates =
//         updates
//         |> List.collect (fun update ->
//           match update with
//           | DTuple(DString name, expr, _) -> [ (name, fromDT expr) ]
//           | _ -> [])
//       ERecordUpdate(
//         uint64 id,
//         fromDT record,
//         NEList.ofListUnsafe
//           "RT2DT.Expr.fromDT expected at least one field update in ERecordUpdate"
//           []
//           updates
//       )

//     // now for EAnd, EOr and EError
//     | DEnum(_, _, [], "EAnd", [ DInt64 id; left; right ]) ->
//       EAnd(uint64 id, fromDT left, fromDT right)

//     | DEnum(_, _, [], "EOr", [ DInt64 id; left; right ]) ->
//       EOr(uint64 id, fromDT left, fromDT right)

//     | DEnum(_, _, [], "EError", [ DInt64 id; rtError; DList(_vtTODO, exprs) ]) ->
//       EError(uint64 id, RuntimeError.fromDT rtError, List.map fromDT exprs)


//     | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ]


module RuntimeError =
  let toDT (e : RuntimeError) : Dval =
    e |> RuntimeTypes.RuntimeError.toDT |> Dval.toDT

  let fromDT (d : Dval) : RuntimeError =
    d |> Dval.fromDT |> RuntimeTypes.RuntimeError.fromDT


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

      | KTDB d -> "KTDB", [ ValueType.toDT d ]

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


module LambdaImpl =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.lambdaImpl

  let toDT (l : LambdaImpl) : Dval =
    let parameters =
      l.parameters
      |> NEList.toList
      |> List.map LetPattern.toDT
      |> fun p -> DList(VT.tuple VT.int64 VT.string [], p)
    let fields =
      [ ("typeSymbolTable",
         DDict(
           VT.known TypeReference.knownType,
           Map.map TypeReference.toDT l.typeSymbolTable
         ))
        ("symtable", DDict(VT.known Dval.knownType, Map.map Dval.toDT l.symtable))
        ("parameters", parameters)
        ("body", Expr.toDT l.body) ]

    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : LambdaImpl =
    match d with
    | DRecord(_, _, _, fields) ->
      { typeSymbolTable =
          fields |> D.mapField "typeSymbolTable" |> Map.map TypeReference.fromDT

        symtable = fields |> D.mapField "symtable" |> Map.map Dval.fromDT

        parameters =
          fields
          |> D.listField "parameters"
          |> List.map LetPattern.fromDT
          |> NEList.ofListUnsafe
            "RT2DT.Dval.fromDT expected at least one parameter in LambdaImpl"
            []

        body = fields |> D.field "body" |> Expr.fromDT }

    | _ -> Exception.raiseInternal "Invalid LambdaImpl" []

module FnValImpl =
  let typeName =
    FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeTypes.fnValImpl

  let toDT (fnValImpl : FnValImpl) : Dval =
    let (caseName, fields) =
      match fnValImpl with
      //| Lambda lambda -> "Lambda", [ LambdaImpl.toDT lambda ]
      | NamedFn fnName -> "NamedFn", [ FQFnName.toDT fnName ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : FnValImpl =
    match d with
    //| DEnum(_, _, [], "Lambda", [ lambda ]) -> Lambda(LambdaImpl.fromDT lambda)
    | DEnum(_, _, [], "NamedFn", [ fnName ]) -> NamedFn(FQFnName.fromDT fnName)
    | _ -> Exception.raiseInternal "Invalid FnValImpl" []



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

      | DFnVal fnImpl -> "DFnVal", [ FnValImpl.toDT fnImpl ]

      | DDB name -> "DDB", [ DString name ]

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

    | DEnum(_, _, [], "DFnVal", [ fnImpl ]) -> DFnVal(FnValImpl.fromDT fnImpl)

    | DEnum(_, _, [], "DDB", [ DString name ]) -> DDB name

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
