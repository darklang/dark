module LibExecution.ProgramTypesToDarkTypes

open Prelude

open RuntimeTypes

module PT = ProgramTypes
module VT = ValueType
module D = LibExecution.DvalDecoder
module C2DT = LibExecution.CommonToDarkTypes


// This isn't in PT but I'm not sure where else to put it...
// maybe rename this file to InternalTypesToDarkTypes?
module Sign =
  let typeName = FQTypeName.fqPackage PackageIDs.Type.LanguageTools.sign

  let toDT (s : Sign) : Dval =
    let (caseName, fields) =
      match s with
      | Positive -> "Positive", []
      | Negative -> "Negative", []
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : Sign =
    match d with
    //TODO: ensure that we are working with a right type
    | DEnum(_, _, [], "Positive", []) -> Positive
    | DEnum(_, _, [], "Negative", []) -> Negative
    | _ -> Exception.raiseInternal "Invalid sign" []


// TODO: should these be elsewhere?
let ownerField m = m |> D.field "owner" |> D.string
let modulesField m = m |> D.field "modules" |> D.list D.string
let nameField m = m |> D.field "name" |> D.string
let versionField m = m |> D.field "version" |> D.int32


module FQTypeName =
  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.FQTypeName.fqTypeName
  let knownType = KTCustomType(typeName, [])

  module Package =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.FQTypeName.package

    let toDT (u : PT.FQTypeName.Package) : Dval = DUuid u

    let fromDT (d : Dval) : PT.FQTypeName.Package =
      match d with
      | DUuid u -> u
      | _ -> Exception.raiseInternal "Invalid FQTypeName.Package" []


  let toDT (u : PT.FQTypeName.FQTypeName) : Dval =
    let (caseName, fields) =
      match u with
      | PT.FQTypeName.Package u -> "Package", [ Package.toDT u ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : PT.FQTypeName.FQTypeName =
    match d with
    | DEnum(_, _, [], "Package", [ u ]) -> PT.FQTypeName.Package(Package.fromDT u)
    | _ -> Exception.raiseInternal "Invalid FQTypeName" []


module FQFnName =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.FQFnName.fqFnName
  let knownType = KTCustomType(typeName, [])

  module Builtin =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.FQFnName.builtin

    let toDT (u : PT.FQFnName.Builtin) : Dval =
      let fields = [ "name", DString u.name; "version", DInt32 u.version ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.FQFnName.Builtin =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = nameField fields; version = versionField fields }
      | _ -> Exception.raiseInternal "Invalid FQFnName.Builtin" []

  module Package =
    let toDT (u : PT.FQFnName.Package) : Dval = DUuid u

    let fromDT (d : Dval) : PT.FQFnName.Package =
      match d with
      | DUuid u -> u
      | _ -> Exception.raiseInternal "Invalid FQFnName.Package" []


  let toDT (u : PT.FQFnName.FQFnName) : Dval =
    let (caseName, fields) =
      match u with
      | PT.FQFnName.Builtin u -> "Builtin", [ Builtin.toDT u ]
      | PT.FQFnName.Package u -> "Package", [ Package.toDT u ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : PT.FQFnName.FQFnName =
    match d with
    | DEnum(_, _, [], "Builtin", [ u ]) -> PT.FQFnName.Builtin(Builtin.fromDT u)
    | DEnum(_, _, [], "Package", [ u ]) -> PT.FQFnName.Package(Package.fromDT u)
    | _ -> Exception.raiseInternal "Invalid FQFnName" []


module FQValueName =
  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.FQValueName.fqValueName
  let knownType = KTCustomType(typeName, [])

  module Builtin =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.FQValueName.builtin
    let toDT (u : PT.FQValueName.Builtin) : Dval =
      let fields = [ "name", DString u.name; "version", DInt32 u.version ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.FQValueName.Builtin =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = nameField fields; version = versionField fields }
      | _ -> Exception.raiseInternal "Invalid FQValueName.Builtin" []

  module Package =
    let toDT (u : PT.FQValueName.Package) : Dval = DUuid u

    let fromDT (d : Dval) : PT.FQValueName.Package =
      match d with
      | DUuid u -> u
      | _ -> Exception.raiseInternal "Invalid FQValueName.Package" []

  let toDT (u : PT.FQValueName.FQValueName) : Dval =
    let (caseName, fields) =
      match u with
      | PT.FQValueName.Builtin u -> "Builtin", [ Builtin.toDT u ]
      | PT.FQValueName.Package u -> "Package", [ Package.toDT u ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : PT.FQValueName.FQValueName =
    match d with
    | DEnum(_, _, [], "Builtin", [ u ]) -> PT.FQValueName.Builtin(Builtin.fromDT u)

    | DEnum(_, _, [], "Package", [ u ]) -> PT.FQValueName.Package(Package.fromDT u)

    | _ -> Exception.raiseInternal "Invalid FQValueName" []


module NameResolutionError =
  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.nameResolutionError

  let knownType = KTCustomType(typeName, [])

  let toDT (e : PT.NameResolutionError) : Dval =
    let (caseName, fields) =
      match e with
      | PT.NotFound names ->
        "NotFound", [ DList(VT.string, List.map Dval.string names) ]
      | PT.InvalidName names ->
        "InvalidName", [ DList(VT.string, List.map Dval.string names) ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : PT.NameResolutionError =
    match d with
    | DEnum(_, _, [], "NotFound", [ names ]) ->
      PT.NameResolutionError.NotFound(names |> D.list D.string)

    | DEnum(_, _, [], "InvalidName", [ names ]) ->
      PT.NameResolutionError.InvalidName(names |> D.list D.string)

    | _ -> Exception.raiseInternal "Invalid NameResolutionError" []



module NameResolution =
  let toDT
    (nameValueType : KnownType)
    (f : 'p -> Dval)
    (result : PT.NameResolution<'p>)
    : Dval =
    C2DT.Result.toDT
      nameValueType
      NameResolutionError.knownType
      result
      f
      NameResolutionError.toDT

  let fromDT (f : Dval -> 'a) (d : Dval) : PT.NameResolution<'a> =
    C2DT.Result.fromDT f d NameResolutionError.fromDT


module TypeReference =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.typeReference
  let knownType = KTCustomType(typeName, [])

  let rec toDT (t : PT.TypeReference) : Dval =
    let (caseName, fields) =
      match t with
      | PT.TVariable name -> "TVariable", [ DString name ]

      | PT.TUnit -> "TUnit", []
      | PT.TBool -> "TBool", []
      | PT.TInt8 -> "TInt8", []
      | PT.TUInt8 -> "TUInt8", []
      | PT.TInt16 -> "TInt16", []
      | PT.TUInt16 -> "TUInt16", []
      | PT.TInt32 -> "TInt32", []
      | PT.TUInt32 -> "TUInt32", []
      | PT.TInt64 -> "TInt64", []
      | PT.TUInt64 -> "TUInt64", []
      | PT.TInt128 -> "TInt128", []
      | PT.TUInt128 -> "TUInt128", []
      | PT.TFloat -> "TFloat", []
      | PT.TChar -> "TChar", []
      | PT.TString -> "TString", []
      | PT.TDateTime -> "TDateTime", []
      | PT.TUuid -> "TUuid", []

      | PT.TList inner -> "TList", [ toDT inner ]

      | PT.TTuple(first, second, theRest) ->
        "TTuple",
        [ toDT first; toDT second; DList(VT.known knownType, List.map toDT theRest) ]

      | PT.TDict inner -> "TDict", [ toDT inner ]

      | PT.TCustomType(typeName, typeArgs) ->
        "TCustomType",
        [ NameResolution.toDT FQTypeName.knownType FQTypeName.toDT typeName
          DList(VT.known knownType, List.map toDT typeArgs) ]

      | PT.TDB inner -> "TDB", [ toDT inner ]

      | PT.TFn(args, ret) ->
        "TFn",
        [ DList(VT.known knownType, args |> NEList.toList |> List.map toDT)
          toDT ret ]

    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : PT.TypeReference =
    match d with
    | DEnum(_, _, [], "TVariable", [ DString name ]) -> PT.TVariable(name)

    | DEnum(_, _, [], "TUnit", []) -> PT.TUnit
    | DEnum(_, _, [], "TBool", []) -> PT.TBool
    | DEnum(_, _, [], "TInt64", []) -> PT.TInt64
    | DEnum(_, _, [], "TUInt64", []) -> PT.TUInt64
    | DEnum(_, _, [], "TInt8", []) -> PT.TInt8
    | DEnum(_, _, [], "TUInt8", []) -> PT.TUInt8
    | DEnum(_, _, [], "TInt16", []) -> PT.TInt16
    | DEnum(_, _, [], "TUInt16", []) -> PT.TUInt16
    | DEnum(_, _, [], "TInt32", []) -> PT.TInt32
    | DEnum(_, _, [], "TUInt32", []) -> PT.TUInt32
    | DEnum(_, _, [], "TInt128", []) -> PT.TInt128
    | DEnum(_, _, [], "TUInt128", []) -> PT.TUInt128
    | DEnum(_, _, [], "TFloat", []) -> PT.TFloat
    | DEnum(_, _, [], "TChar", []) -> PT.TChar
    | DEnum(_, _, [], "TString", []) -> PT.TString
    | DEnum(_, _, [], "TDateTime", []) -> PT.TDateTime
    | DEnum(_, _, [], "TUuid", []) -> PT.TUuid

    | DEnum(_, _, [], "TList", [ inner ]) -> PT.TList(fromDT inner)

    | DEnum(_, _, [], "TTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      PT.TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "TDict", [ inner ]) -> PT.TDict(fromDT inner)

    | DEnum(_, _, [], "TCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
      PT.TCustomType(
        NameResolution.fromDT FQTypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, [], "TDB", [ inner ]) -> PT.TDB(fromDT inner)
    | DEnum(_, _, [], "TFn", [ DList(_vtTODO, head :: tail); ret ]) ->
      PT.TFn(NEList.ofList head tail |> NEList.map fromDT, fromDT ret)
    | _ -> Exception.raiseInternal "Invalid TypeReference" []


module LetPattern =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.letPattern
  let knownType = KTCustomType(typeName, [])

  let rec toDT (p : PT.LetPattern) : Dval =
    let (caseName, fields) =
      match p with
      | PT.LPVariable(id, name) -> "LPVariable", [ DInt64(int64 id); DString name ]
      | PT.LPUnit id -> "LPUnit", [ DInt64(int64 id) ]
      | PT.LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt64(int64 id)
          toDT first
          toDT second
          DList(VT.known knownType, List.map toDT theRest) ]

    DEnum(typeName, typeName, [], caseName, fields)


  let rec fromDT (d : Dval) : PT.LetPattern =
    match d with
    | DEnum(_, _, [], "LPVariable", [ DInt64 id; DString name ]) ->
      PT.LPVariable(uint64 id, name)
    | DEnum(_, _, [], "LPUnit", [ DInt64 id ]) -> PT.LPUnit(uint64 id)
    | DEnum(_,
            _,
            [],
            "LPTuple",
            [ DInt64 id; first; second; DList(_vtTODO, theRest) ]) ->
      PT.LPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.matchPattern
  let knownType = KTCustomType(typeName, [])

  let rec toDT (p : PT.MatchPattern) : Dval =
    let (caseName, fields) =
      match p with
      | PT.MPVariable(id, name) -> "MPVariable", [ DInt64(int64 id); DString name ]

      | PT.MPUnit id -> "MPUnit", [ DInt64(int64 id) ]
      | PT.MPBool(id, b) -> "MPBool", [ DInt64(int64 id); DBool b ]
      | PT.MPInt64(id, i) -> "MPInt64", [ DInt64(int64 id); DInt64 i ]
      | PT.MPUInt64(id, i) -> "MPUInt64", [ DInt64(int64 id); DUInt64 i ]
      | PT.MPInt8(id, i) -> "MPInt8", [ DInt64(int64 id); DInt8 i ]
      | PT.MPUInt8(id, i) -> "MPUInt8", [ DInt64(int64 id); DUInt8 i ]
      | PT.MPInt16(id, i) -> "MPInt16", [ DInt64(int64 id); DInt16 i ]
      | PT.MPUInt16(id, i) -> "MPUInt16", [ DInt64(int64 id); DUInt16 i ]
      | PT.MPInt32(id, i) -> "MPInt32", [ DInt64(int64 id); DInt32 i ]
      | PT.MPUInt32(id, i) -> "MPUInt32", [ DInt64(int64 id); DUInt32 i ]
      | PT.MPInt128(id, i) -> "MPInt128", [ DInt64(int64 id); DInt128 i ]
      | PT.MPUInt128(id, i) -> "MPUInt128", [ DInt64(int64 id); DUInt128 i ]
      | PT.MPFloat(id, sign, whole, remainder) ->

        "MPFloat",
        [ DInt64(int64 id); Sign.toDT sign; DString whole; DString remainder ]
      | PT.MPChar(id, c) -> "MPChar", [ DInt64(int64 id); DString c ]
      | PT.MPString(id, s) -> "MPString", [ DInt64(int64 id); DString s ]

      | PT.MPList(id, inner) ->
        "MPList",
        [ DInt64(int64 id); DList(VT.known knownType, List.map toDT inner) ]
      | PT.MPListCons(id, head, tail) ->
        "MPListCons", [ DInt64(int64 id); toDT head; toDT tail ]
      | PT.MPTuple(id, first, second, theRest) ->
        "MPTuple",
        [ DInt64(int64 id)
          toDT first
          toDT second
          DList(VT.known knownType, List.map toDT theRest) ]
      | PT.MPEnum(id, caseName, fieldPats) ->
        "MPEnum",
        [ DInt64(int64 id)
          DString caseName
          DList(VT.known knownType, List.map toDT fieldPats) ]

      | PT.MPOr(id, patterns) ->
        let patterns = patterns |> NEList.toList |> List.map toDT
        "MPOr", [ DInt64(int64 id); DList(VT.known knownType, patterns) ]

    DEnum(typeName, typeName, [], caseName, fields)

  let rec fromDT (d : Dval) : PT.MatchPattern =
    match d with
    | DEnum(_, _, [], "MPVariable", [ DInt64 id; DString name ]) ->
      PT.MPVariable(uint64 id, name)

    | DEnum(_, _, [], "MPUnit", [ DInt64 id ]) -> PT.MPUnit(uint64 id)
    | DEnum(_, _, [], "MPBool", [ DInt64 id; DBool b ]) -> PT.MPBool(uint64 id, b)
    | DEnum(_, _, [], "MPInt64", [ DInt64 id; DInt64 i ]) -> PT.MPInt64(uint64 id, i)
    | DEnum(_, _, [], "MPUInt64", [ DInt64 id; DUInt64 i ]) ->
      PT.MPUInt64(uint64 id, i)
    | DEnum(_, _, [], "MPInt8", [ DInt64 id; DInt8 i ]) -> PT.MPInt8(uint64 id, i)
    | DEnum(_, _, [], "MPUInt8", [ DInt64 id; DUInt8 i ]) -> PT.MPUInt8(uint64 id, i)
    | DEnum(_, _, [], "MPInt16", [ DInt64 id; DInt16 i ]) -> PT.MPInt16(uint64 id, i)
    | DEnum(_, _, [], "MPUInt16", [ DInt64 id; DUInt16 i ]) ->
      PT.MPUInt16(uint64 id, i)
    | DEnum(_, _, [], "MPInt32", [ DInt64 id; DInt32 i ]) -> PT.MPInt32(uint64 id, i)
    | DEnum(_, _, [], "MPUInt32", [ DInt64 id; DUInt32 i ]) ->
      PT.MPUInt32(uint64 id, i)
    | DEnum(_, _, [], "MPInt128", [ DInt64 id; DInt128 i ]) ->
      PT.MPInt128(uint64 id, i)
    | DEnum(_, _, [], "MPUInt128", [ DInt64 id; DUInt128 i ]) ->
      PT.MPUInt128(uint64 id, i)
    | DEnum(_,
            _,
            [],
            "MPFloat",
            [ DInt64 id; sign; DString whole; DString remainder ]) ->
      PT.MPFloat(uint64 id, Sign.fromDT sign, whole, remainder)
    | DEnum(_, _, [], "MPChar", [ DInt64 id; DString c ]) -> PT.MPChar(uint64 id, c)
    | DEnum(_, _, [], "MPString", [ DInt64 id; DString s ]) ->
      PT.MPString(uint64 id, s)

    | DEnum(_, _, [], "MPList", [ DInt64 id; DList(_vtTODO, inner) ]) ->
      PT.MPList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, [], "MPListCons", [ DInt64 id; head; tail ]) ->
      PT.MPListCons(uint64 id, fromDT head, fromDT tail)
    | DEnum(_,
            _,
            [],
            "MPTuple",
            [ DInt64 id; first; second; DList(_vtTODO, theRest) ]) ->
      PT.MPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | DEnum(_,
            _,
            [],
            "MPEnum",
            [ DInt64 id; DString caseName; DList(_vtTODO, fieldPats) ]) ->
      PT.MPEnum(uint64 id, caseName, List.map fromDT fieldPats)

    | DEnum(_, _, [], "MPOr", [ DInt64 id; DList(_vtTODO, patterns) ]) ->
      let patterns =
        patterns
        |> List.map fromDT
        |> NEList.ofListUnsafe
          "PT2DT.MatchPattern.fromDT expected at least one pattern in MPOr"
          []
      PT.MPOr(uint64 id, patterns)

    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module BinaryOperation =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.binaryOperation

  let toDT (b : PT.BinaryOperation) : Dval =
    let (caseName, fields) =
      match b with
      | PT.BinOpAnd -> "BinOpAnd", []
      | PT.BinOpOr -> "BinOpOr", []
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : PT.BinaryOperation =
    match d with
    | DEnum(_, _, [], "BinOpAnd", []) -> PT.BinOpAnd
    | DEnum(_, _, [], "BinOpOr", []) -> PT.BinOpOr
    | _ -> Exception.raiseInternal "Invalid BinaryOperation" []


module InfixFnName =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.infixFnName

  let toDT (i : PT.InfixFnName) : Dval =
    let (caseName, fields) =
      match i with
      | PT.ArithmeticPlus -> "ArithmeticPlus", []
      | PT.ArithmeticMinus -> "ArithmeticMinus", []
      | PT.ArithmeticMultiply -> "ArithmeticMultiply", []
      | PT.ArithmeticDivide -> "ArithmeticDivide", []
      | PT.ArithmeticModulo -> "ArithmeticModulo", []
      | PT.ArithmeticPower -> "ArithmeticPower", []
      | PT.ComparisonGreaterThan -> "ComparisonGreaterThan", []
      | PT.ComparisonGreaterThanOrEqual -> "ComparisonGreaterThanOrEqual", []
      | PT.ComparisonLessThan -> "ComparisonLessThan", []
      | PT.ComparisonLessThanOrEqual -> "ComparisonLessThanOrEqual", []
      | PT.ComparisonEquals -> "ComparisonEquals", []
      | PT.ComparisonNotEquals -> "ComparisonNotEquals", []
      | PT.StringConcat -> "StringConcat", []

    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : PT.InfixFnName =
    match d with
    | DEnum(_, _, [], "ArithmeticPlus", []) -> PT.ArithmeticPlus
    | DEnum(_, _, [], "ArithmeticMinus", []) -> PT.ArithmeticMinus
    | DEnum(_, _, [], "ArithmeticMultiply", []) -> PT.ArithmeticMultiply
    | DEnum(_, _, [], "ArithmeticDivide", []) -> PT.ArithmeticDivide
    | DEnum(_, _, [], "ArithmeticModulo", []) -> PT.ArithmeticModulo
    | DEnum(_, _, [], "ArithmeticPower", []) -> PT.ArithmeticPower
    | DEnum(_, _, [], "ComparisonGreaterThan", []) -> PT.ComparisonGreaterThan
    | DEnum(_, _, [], "ComparisonGreaterThanOrEqual", []) ->
      PT.ComparisonGreaterThanOrEqual
    | DEnum(_, _, [], "ComparisonLessThan", []) -> PT.ComparisonLessThan
    | DEnum(_, _, [], "ComparisonLessThanOrEqual", []) ->
      PT.ComparisonLessThanOrEqual
    | DEnum(_, _, [], "ComparisonEquals", []) -> PT.ComparisonEquals
    | DEnum(_, _, [], "ComparisonNotEquals", []) -> PT.ComparisonNotEquals
    | DEnum(_, _, [], "StringConcat", []) -> PT.StringConcat
    | _ -> Exception.raiseInternal "Invalid InfixFnName" []


module Infix =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.infix

  let toDT (i : PT.Infix) : Dval =
    let (caseName, fields) =
      match i with
      | PT.InfixFnCall infixFnName -> "InfixFnCall", [ InfixFnName.toDT infixFnName ]
      | PT.BinOp binOp -> "BinOp", [ BinaryOperation.toDT binOp ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (d : Dval) : PT.Infix =
    match d with
    | DEnum(_, _, [], "InfixFnCall", [ infixFnName ]) ->
      PT.InfixFnCall(InfixFnName.fromDT infixFnName)
    | DEnum(_, _, [], "BinOp", [ binOp ]) -> PT.BinOp(BinaryOperation.fromDT binOp)
    | _ -> Exception.raiseInternal "Invalid Infix" []


module StringSegment =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.stringSegment
  let knownType = KTCustomType(typeName, [])

  let toDT (exprToDT : PT.Expr -> Dval) (s : PT.StringSegment) : Dval =
    let (caseName, fields) =
      match s with
      | PT.StringText text -> "StringText", [ DString text ]
      | PT.StringInterpolation expr -> "StringInterpolation", [ exprToDT expr ]
    DEnum(typeName, typeName, [], caseName, fields)

  let fromDT (exprFromDT : Dval -> PT.Expr) (d : Dval) : PT.StringSegment =
    match d with
    | DEnum(_, _, [], "StringText", [ DString text ]) -> PT.StringText text
    | DEnum(_, _, [], "StringInterpolation", [ expr ]) ->
      PT.StringInterpolation(exprFromDT expr)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


module PipeExpr =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.pipeExpr
  let knownType = KTCustomType(typeName, [])

  let toDT
    (exprKT : KnownType)
    (exprToDT : PT.Expr -> Dval)
    (s : PT.PipeExpr)
    : Dval =
    let (caseName, fields) =
      match s with
      | PT.EPipeVariable(id, varName, exprs) ->
        "EPipeVariable",
        [ DInt64(int64 id)
          DString varName
          DList(VT.known exprKT, List.map exprToDT exprs) ]

      | PT.EPipeLambda(id, args, body) ->
        let variables =
          args
          |> NEList.toList
          |> List.map LetPattern.toDT
          |> Dval.list (KTTuple(VT.int64, VT.string, []))
        "EPipeLambda", [ DInt64(int64 id); variables; exprToDT body ]

      | PT.EPipeInfix(id, infix, expr) ->
        "EPipeInfix", [ DInt64(int64 id); Infix.toDT infix; exprToDT expr ]

      | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
        "EPipeFnCall",
        [ DInt64(int64 id)
          NameResolution.toDT FQFnName.knownType FQFnName.toDT fnName
          DList(
            VT.known TypeReference.knownType,
            List.map TypeReference.toDT typeArgs
          )
          DList(VT.known exprKT, List.map exprToDT args) ]

      | PT.EPipeEnum(id, typeName, caseName, fields) ->
        "EPipeEnum",
        [ DInt64(int64 id)
          NameResolution.toDT FQTypeName.knownType FQTypeName.toDT typeName
          DString caseName
          DList(VT.known exprKT, List.map exprToDT fields) ]

    DEnum(typeName, typeName, [], caseName, fields)


  let fromDT (exprFromDT : Dval -> PT.Expr) (d : Dval) : PT.PipeExpr =
    match d with
    | DEnum(_,
            _,
            [],
            "EPipeVariable",
            [ DInt64 id; DString varName; DList(_vtTODO, args) ]) ->
      PT.EPipeVariable(uint64 id, varName, args |> List.map exprFromDT)

    | DEnum(_, _, [], "EPipeLambda", [ DInt64 id; variables; body ]) ->
      let variables =
        match variables with
        | DList(_vtTODO, pats) ->
          pats
          |> List.map LetPattern.fromDT
          |> NEList.ofListUnsafe
            "PT2DT.PipeExpr.fromDT expected at least one bound variable in EPipeLambda"
            []
        | _ -> Exception.raiseInternal "Invalid variables" []

      PT.EPipeLambda(uint64 id, variables, exprFromDT body)

    | DEnum(_, _, [], "EPipeInfix", [ DInt64 id; infix; expr ]) ->
      PT.EPipeInfix(uint64 id, Infix.fromDT infix, exprFromDT expr)

    | DEnum(_,
            _,
            [],
            "EPipeFnCall",
            [ DInt64 id; fnName; DList(_vtTODO1, typeArgs); DList(_vtTODO2, args) ]) ->
      PT.EPipeFnCall(
        uint64 id,
        NameResolution.fromDT FQFnName.fromDT fnName,
        List.map TypeReference.fromDT typeArgs,
        List.map exprFromDT args
      )

    | DEnum(_,
            _,
            [],
            "EPipeEnum",
            [ DInt64 id; typeName; DString caseName; DList(_vtTODO, fields) ]) ->
      PT.EPipeEnum(
        uint64 id,
        NameResolution.fromDT FQTypeName.fromDT typeName,
        caseName,
        List.map exprFromDT fields
      )

    | _ -> Exception.raiseInternal "Invalid PipeExpr" []


module Expr =
  let typeName = FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.expr
  let knownType = KTCustomType(typeName, [])

  let rec toDT (e : PT.Expr) : Dval =
    let (caseName, fields) =
      match e with
      | PT.EUnit id -> "EUnit", [ DInt64(int64 id) ]

      // simple data
      | PT.EBool(id, b) -> "EBool", [ DInt64(int64 id); DBool b ]
      | PT.EInt64(id, i) -> "EInt64", [ DInt64(int64 id); DInt64 i ]
      | PT.EUInt64(id, i) -> "EUInt64", [ DInt64(int64 id); DUInt64 i ]
      | PT.EInt8(id, i) -> "EInt8", [ DInt64(int64 id); DInt8 i ]
      | PT.EUInt8(id, i) -> "EUInt8", [ DInt64(int64 id); DUInt8 i ]
      | PT.EInt16(id, i) -> "EInt16", [ DInt64(int64 id); DInt16 i ]
      | PT.EUInt16(id, i) -> "EUInt16", [ DInt64(int64 id); DUInt16 i ]
      | PT.EInt32(id, i) -> "EInt32", [ DInt64(int64 id); DInt32 i ]
      | PT.EUInt32(id, i) -> "EUInt32", [ DInt64(int64 id); DUInt32 i ]
      | PT.EInt128(id, i) -> "EInt128", [ DInt64(int64 id); DInt128 i ]
      | PT.EUInt128(id, i) -> "EUInt128", [ DInt64(int64 id); DUInt128 i ]
      | PT.EFloat(id, sign, whole, remainder) ->
        "EFloat",
        [ DInt64(int64 id); Sign.toDT sign; DString whole; DString remainder ]

      | PT.EChar(id, c) -> "EChar", [ DInt64(int64 id); DString c ]
      | PT.EString(id, segments) ->
        "EString",
        [ DInt64(int64 id)
          DList(
            VT.known StringSegment.knownType,
            List.map (StringSegment.toDT toDT) segments
          ) ]

      // structures of data
      | PT.EList(id, items) ->
        "EList", [ DInt64(int64 id); DList(VT.known knownType, List.map toDT items) ]

      | PT.EDict(id, pairs) ->
        "EDict",
        [ DInt64(int64 id)
          DList(
            VT.tuple VT.string (VT.known knownType) [],
            pairs |> List.map (fun (k, v) -> DTuple(DString k, toDT v, []))
          ) ]

      | PT.ETuple(id, first, second, theRest) ->
        "ETuple",
        [ DInt64(int64 id)
          toDT first
          toDT second
          DList(VT.known knownType, List.map toDT theRest) ]

      | PT.ERecord(id, typeName, typeArgs, fields) ->
        let fields =
          DList(
            VT.tuple VT.string (VT.known knownType) [],
            fields
            |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))
          )

        "ERecord",
        [ DInt64(int64 id)
          NameResolution.toDT FQTypeName.knownType FQTypeName.toDT typeName
          DList(
            VT.known TypeReference.knownType,
            List.map TypeReference.toDT typeArgs
          )
          fields ]

      | PT.EEnum(id, typeName, typeArgs, caseName, fields) ->
        "EEnum",
        [ DInt64(int64 id)
          NameResolution.toDT FQTypeName.knownType FQTypeName.toDT typeName
          DList(
            VT.known TypeReference.knownType,
            List.map TypeReference.toDT typeArgs
          )
          DString caseName
          DList(VT.known knownType, List.map toDT fields) ]

      // declaring and accessing variables
      | PT.ELet(id, lp, expr, body) ->
        "ELet", [ DInt64(int64 id); LetPattern.toDT lp; toDT expr; toDT body ]

      | PT.ERecordFieldAccess(id, expr, fieldName) ->
        "ERecordFieldAccess", [ DInt64(int64 id); toDT expr; DString fieldName ]

      | PT.EVariable(id, varName) ->
        "EVariable", [ DInt64(int64 id); DString varName ]


      // control flow
      | PT.EIf(id, cond, thenExpr, elseExpr) ->
        "EIf",
        [ DInt64(int64 id)
          toDT cond
          toDT thenExpr
          elseExpr |> Option.map toDT |> Dval.option knownType ]

      | PT.EMatch(id, arg, cases) ->
        let matchCaseTypeName =
          FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.matchCase
        let cases =
          cases
          |> List.map (fun case ->

            let pattern = MatchPattern.toDT case.pat
            let whenCondition =
              case.whenCondition |> Option.map toDT |> Dval.option knownType
            let expr = toDT case.rhs
            DRecord(
              matchCaseTypeName,
              matchCaseTypeName,
              [],
              Map
                [ ("pat", pattern)
                  ("whenCondition", whenCondition)
                  ("rhs", expr) ]
            ))
          |> Dval.list (KTCustomType(matchCaseTypeName, []))

        "EMatch", [ DInt64(int64 id); toDT arg; cases ]

      | PT.EPipe(id, expr, pipeExprs) ->
        "EPipe",
        [ DInt64(int64 id)
          toDT expr
          DList(
            VT.known PipeExpr.knownType,
            List.map (PipeExpr.toDT knownType toDT) pipeExprs
          ) ]


      // function calls
      | PT.EInfix(id, infix, lhs, rhs) ->
        "EInfix", [ DInt64(int64 id); Infix.toDT infix; toDT lhs; toDT rhs ]

      | PT.ELambda(id, pats, body) ->
        let variables =
          DList(
            VT.tuple VT.int64 VT.string [],
            pats |> NEList.toList |> List.map LetPattern.toDT
          )
        "ELambda", [ DInt64(int64 id); variables; toDT body ]

      | PT.EValue(id, name) ->
        "EValue",
        [ DInt64(int64 id)
          NameResolution.toDT FQValueName.knownType FQValueName.toDT name ]

      | PT.EApply(id, name, typeArgs, args) ->
        "EApply",
        [ DInt64(int64 id)
          toDT name
          DList(
            VT.known TypeReference.knownType,
            List.map TypeReference.toDT typeArgs
          )
          DList(VT.known knownType, args |> NEList.toList |> List.map toDT) ]

      | PT.EFnName(id, name) ->
        "EFnName",
        [ DInt64(int64 id)
          NameResolution.toDT FQFnName.knownType FQFnName.toDT name ]

      | PT.ERecordUpdate(id, record, updates) ->
        let updates =
          DList(
            VT.tuple VT.string (VT.known knownType) [],
            updates
            |> NEList.toList
            |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))
          )

        "ERecordUpdate", [ DInt64(int64 id); toDT record; updates ]

      | PT.EStatement(id, expr, next) ->
        "EStatement", [ DInt64(int64 id); toDT expr; toDT next ]

    DEnum(typeName, typeName, [], caseName, fields)


  let rec fromDT (d : Dval) : PT.Expr =
    match d with
    | DEnum(_, _, [], "EUnit", [ DInt64 id ]) -> PT.EUnit(uint64 id)

    // simple data
    | DEnum(_, _, [], "EBool", [ DInt64 id; DBool b ]) -> PT.EBool(uint64 id, b)
    | DEnum(_, _, [], "EInt64", [ DInt64 id; DInt64 i ]) -> PT.EInt64(uint64 id, i)
    | DEnum(_, _, [], "EUInt64", [ DInt64 id; DUInt64 i ]) ->
      PT.EUInt64(uint64 id, i)
    | DEnum(_, _, [], "EInt8", [ DInt64 id; DInt8 i ]) -> PT.EInt8(uint64 id, i)
    | DEnum(_, _, [], "EUInt8", [ DInt64 id; DUInt8 i ]) -> PT.EUInt8(uint64 id, i)
    | DEnum(_, _, [], "EInt16", [ DInt64 id; DInt16 i ]) -> PT.EInt16(uint64 id, i)
    | DEnum(_, _, [], "EUInt16", [ DInt64 id; DUInt16 i ]) ->
      PT.EUInt16(uint64 id, i)
    | DEnum(_, _, [], "EInt32", [ DInt64 id; DInt32 i ]) -> PT.EInt32(uint64 id, i)
    | DEnum(_, _, [], "EUInt32", [ DInt64 id; DUInt32 i ]) ->
      PT.EUInt32(uint64 id, i)
    | DEnum(_, _, [], "EInt128", [ DInt64 id; DInt128 i ]) ->
      PT.EInt128(uint64 id, i)
    | DEnum(_, _, [], "EUInt128", [ DInt64 id; DUInt128 i ]) ->
      PT.EUInt128(uint64 id, i)
    | DEnum(_, _, [], "EFloat", [ DInt64 id; sign; DString whole; DString remainder ]) ->
      PT.EFloat(uint64 id, Sign.fromDT sign, whole, remainder)
    | DEnum(_, _, [], "EChar", [ DInt64 id; DString c ]) -> PT.EChar(uint64 id, c)
    | DEnum(_, _, [], "EString", [ DInt64 id; DList(_vtTODO, segments) ]) ->
      PT.EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


    // structures of data
    | DEnum(_, _, [], "EList", [ DInt64 id; DList(_vtTODO, inner) ]) ->
      PT.EList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, [], "EDict", [ DInt64 id; DList(_vtTODO, pairsList) ]) ->
      let pairs =
        pairsList
        |> List.collect (fun pair ->
          match pair with
          | DTuple(DString k, v, _) -> [ (k, fromDT v) ]
          | _ -> [])
      PT.EDict(uint64 id, pairs)


    | DEnum(_, _, [], "ETuple", [ DInt64 id; first; second; DList(_vtTODO, theRest) ]) ->
      PT.ETuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_,
            _,
            [],
            "ERecord",
            [ DInt64 id
              typeName
              DList(_vtTODOTypeArgs, typeArgs)
              DList(_vtTODO, fieldsList) ]) ->
      let typeArgs = List.map TypeReference.fromDT typeArgs
      let fields =
        fieldsList
        |> List.collect (fun field ->
          match field with
          | DTuple(DString name, expr, _) -> [ (name, fromDT expr) ]
          | _ -> [])
      PT.ERecord(
        uint64 id,
        NameResolution.fromDT FQTypeName.fromDT typeName,
        typeArgs,
        fields
      )


    | DEnum(_,
            _,
            [],
            "EEnum",
            [ DInt64 id
              typeName
              DList(_vtTODOTypeArgs, typeArgs)
              DString caseName
              DList(_vtTODO, fields) ]) ->
      let typeArgs = List.map TypeReference.fromDT typeArgs

      PT.EEnum(
        uint64 id,
        NameResolution.fromDT FQTypeName.fromDT typeName,
        typeArgs,
        caseName,
        List.map fromDT fields
      )

    // declaring and accessing variables
    | DEnum(_, _, [], "ELet", [ DInt64 id; lp; expr; body ]) ->
      PT.ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

    | DEnum(_, _, [], "ERecordFieldAccess", [ DInt64 id; expr; DString fieldName ]) ->
      PT.ERecordFieldAccess(uint64 id, fromDT expr, fieldName)

    | DEnum(_, _, [], "EVariable", [ DInt64 id; DString varName ]) ->
      PT.EVariable(uint64 id, varName)

    // control flow
    | DEnum(_, _, [], "EIf", [ DInt64 id; cond; thenExpr; elseExpr ]) ->
      let elseExpr =
        match elseExpr with
        | DEnum(_, _, _typeArgsDEnumTODO, "Some", [ dv ]) -> Some(fromDT dv)
        | DEnum(_, _, _typeArgsDEnumTODO, "None", []) -> None
        | _ ->
          Exception.raiseInternal "Invalid else expression" [ "elseExpr", elseExpr ]
      PT.EIf(uint64 id, fromDT cond, fromDT thenExpr, elseExpr)

    | DEnum(_, _, [], "EMatch", [ DInt64 id; arg; DList(_vtTODO, cases) ]) ->
      let (cases : List<PT.MatchCase>) =
        cases
        |> List.collect (fun case ->
          match case with
          | DRecord(_, _, _, fields) ->
            let whenCondition =
              match Map.tryFind "whenCondition" fields with
              | Some(DEnum(_, _, _, "Some", [ value ])) -> Some(fromDT value)
              | Some(DEnum(_, _, _, "None", [])) -> None
              | _ -> None
            match Map.tryFind "pat" fields, Map.tryFind "rhs" fields with
            | Some pat, Some rhs ->
              [ { pat = MatchPattern.fromDT pat
                  whenCondition = whenCondition
                  rhs = fromDT rhs } ]
            | _ -> []
          | _ -> [])
      PT.EMatch(uint64 id, fromDT arg, cases)

    | DEnum(_, _, [], "EPipe", [ DInt64 id; expr; DList(_vtTODO, pipeExprs) ]) ->
      PT.EPipe(uint64 id, fromDT expr, List.map (PipeExpr.fromDT fromDT) pipeExprs)

    // function calls
    | DEnum(_, _, [], "EInfix", [ DInt64 id; infix; lhs; rhs ]) ->
      PT.EInfix(uint64 id, Infix.fromDT infix, fromDT lhs, fromDT rhs)

    | DEnum(_, _, [], "ELambda", [ DInt64 id; DList(_vtTODO, pats); body ]) ->
      let pats =
        pats
        |> List.map LetPattern.fromDT
        |> NEList.ofListUnsafe
          "PT2DT.Expr.fromDT expected at least one bound variable in ELambda"
          []
      PT.ELambda(uint64 id, pats, fromDT body)


    | DEnum(_,
            _,
            [],
            "EApply",
            [ DInt64 id; name; DList(_vtTODO1, typeArgs); DList(_vtTODO2, args) ]) ->
      PT.EApply(
        uint64 id,
        fromDT name,
        List.map TypeReference.fromDT typeArgs,
        args |> NEList.ofListUnsafe "EApply" [] |> NEList.map fromDT
      )

    | DEnum(_, _, [], "EFnName", [ DInt64 id; name ]) ->
      PT.EFnName(uint64 id, NameResolution.fromDT FQFnName.fromDT name)

    | DEnum(_,
            _,
            [],
            "ERecordUpdate",
            [ DInt64 id; record; DList(_vtTODO, head :: tail) ]) ->
      let updates =
        NEList.ofList head tail
        |> NEList.map (fun update ->
          match update with
          | DTuple(DString name, expr, _) -> (name, fromDT expr)
          | _ ->
            Exception.raiseInternal "Invalid record update" [ "update", update ])
      PT.ERecordUpdate(uint64 id, fromDT record, updates)

    | DEnum(_, _, [], "EStatement", [ DInt64 id; expr; next ]) ->
      PT.EStatement(uint64 id, fromDT expr, fromDT next)

    | DEnum(_, _, [], "EValue", [ DInt64 id; name ]) ->
      PT.EValue(uint64 id, NameResolution.fromDT FQValueName.fromDT name)

    | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ]



module Deprecation =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.deprecation
  let knownType = KTCustomType(typeName, [])

  let toDT
    (innerType : KnownType)
    (inner : 'a -> Dval)
    (d : PT.Deprecation<'a>)
    : Dval =
    let (caseName, fields) =
      match d with
      | PT.Deprecation.NotDeprecated -> "NotDeprecated", []
      | PT.Deprecation.RenamedTo replacement -> "RenamedTo", [ inner replacement ]
      | PT.Deprecation.ReplacedBy replacement -> "ReplacedBy", [ inner replacement ]
      | PT.Deprecation.DeprecatedBecause reason ->
        "DeprecatedBecause", [ DString reason ]
    DEnum(typeName, typeName, [ VT.known innerType ], caseName, fields)

  let fromDT (inner : Dval -> 'a) (d : Dval) : PT.Deprecation<'a> =
    match d with
    | DEnum(_, _, _typeArgsDEnumTODO, "NotDeprecated", []) ->
      PT.Deprecation.NotDeprecated
    | DEnum(_, _, _typeArgsDEnumTODO, "RenamedTo", [ replacement ]) ->
      PT.Deprecation.RenamedTo(inner replacement)
    | DEnum(_, _, _typeArgsDEnumTODO, "ReplacedBy", [ replacement ]) ->
      PT.Deprecation.ReplacedBy(inner replacement)
    | DEnum(_, _, _typeArgsDEnumTODO, "DeprecatedBecause", [ DString reason ]) ->
      PT.Deprecation.DeprecatedBecause(reason)
    | _ -> Exception.raiseInternal "Invalid Deprecation" []


module TypeDeclaration =
  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.TypeDeclaration.typeDeclaration

  module RecordField =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.TypeDeclaration.recordField
    let knownType = KTCustomType(typeName, [])

    let toDT (rf : PT.TypeDeclaration.RecordField) : Dval =
      let fields =
        [ "name", DString rf.name
          "typ", TypeReference.toDT rf.typ
          "description", DString rf.description ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.TypeDeclaration.RecordField =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = fields |> D.field "name" |> D.string
          typ = fields |> D.field "typ" |> TypeReference.fromDT
          description = fields |> D.field "description" |> D.string }
      | _ -> Exception.raiseInternal "Invalid RecordField" []

  module EnumField =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.TypeDeclaration.enumField
    let knownType = KTCustomType(typeName, [])

    let toDT (ef : PT.TypeDeclaration.EnumField) : Dval =
      let fields =
        [ "typ", TypeReference.toDT ef.typ
          "label", ef.label |> Option.map DString |> Dval.option KTString
          "description", DString ef.description ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.TypeDeclaration.EnumField =
      match d with
      | DRecord(_, _, _, fields) ->
        { typ = fields |> D.field "typ" |> TypeReference.fromDT
          label =
            match Map.get "label" fields with
            | Some(DEnum(_, _, _typeArgsDEnumTODO, "Some", [ DString label ])) ->
              Some label
            | Some(DEnum(_, _, _typeArgsDEnumTODO, "None", [])) -> None
            | _ ->
              Exception.raiseInternal "Expected label to be an option of string" []
          description = fields |> D.field "description" |> D.string }
      | _ -> Exception.raiseInternal "Invalid EnumField" []


  module EnumCase =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.TypeDeclaration.enumCase
    let knownType = KTCustomType(typeName, [])

    let toDT (ec : PT.TypeDeclaration.EnumCase) : Dval =
      let fields =
        [ "name", DString ec.name
          "fields",
          DList(VT.known EnumField.knownType, List.map EnumField.toDT ec.fields)
          "description", DString ec.description ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.TypeDeclaration.EnumCase =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = fields |> D.field "name" |> D.string
          fields = fields |> D.field "fields" |> D.list EnumField.fromDT
          description = fields |> D.field "description" |> D.string }

      | _ -> Exception.raiseInternal "Invalid EnumCase" []


  module Definition =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.TypeDeclaration.definition

    let toDT (d : PT.TypeDeclaration.Definition) : Dval =
      let (caseName, fields) =
        match d with
        | PT.TypeDeclaration.Alias typeRef -> "Alias", [ TypeReference.toDT typeRef ]

        | PT.TypeDeclaration.Record fields ->
          "Record",
          [ DList(
              VT.known RecordField.knownType,
              fields |> NEList.toList |> List.map RecordField.toDT
            ) ]

        | PT.TypeDeclaration.Enum cases ->
          "Enum",
          [ DList(
              VT.known EnumCase.knownType,
              cases |> NEList.toList |> List.map EnumCase.toDT
            ) ]
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : PT.TypeDeclaration.Definition =
      match d with
      | DEnum(_, _, [], "Alias", [ typeRef ]) ->
        PT.TypeDeclaration.Alias(TypeReference.fromDT typeRef)

      | DEnum(_, _, [], "Record", [ DList(_vtTODO, firstField :: additionalFields) ]) ->
        PT.TypeDeclaration.Record(
          NEList.ofList firstField additionalFields |> NEList.map RecordField.fromDT
        )

      | DEnum(_, _, [], "Enum", [ DList(_vtTODO, firstCase :: additionalCases) ]) ->
        PT.TypeDeclaration.Enum(
          NEList.ofList firstCase additionalCases |> NEList.map EnumCase.fromDT
        )

      | _ -> Exception.raiseInternal "Invalid TypeDeclaration.Definition" []


  let toDT (td : PT.TypeDeclaration.T) : Dval =
    let fields =
      [ "typeParams", DList(VT.string, List.map DString td.typeParams)
        "definition", Definition.toDT td.definition ]
    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : PT.TypeDeclaration.T =
    match d with
    | DRecord(_, _, _, fields) ->
      { typeParams = fields |> D.field "typeParams" |> D.list D.string
        definition = fields |> D.field "definition" |> Definition.fromDT }
    | _ -> Exception.raiseInternal "Invalid TypeDeclaration" []


// -- Package stuff -- //

module PackageType =
  module Name =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.PackageType.name

    let toDT (n : PT.PackageType.Name) : Dval =
      let fields =
        [ "owner", DString n.owner
          "modules", DList(VT.string, List.map DString n.modules)
          "name", DString n.name ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.PackageType.Name =
      match d with
      | DRecord(_, _, _, fields) ->
        { owner = fields |> D.field "owner" |> D.string
          modules = fields |> D.field "modules" |> D.list D.string
          name = fields |> D.field "name" |> D.string }
      | _ -> Exception.raiseInternal "Invalid PackageType.Name" []


  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.PackageType.packageType

  let toDT (p : PT.PackageType.PackageType) : Dval =
    let fields =
      [ "id", DUuid p.id
        "name", Name.toDT p.name
        "declaration", TypeDeclaration.toDT p.declaration
        "description", DString p.description
        "deprecated",
        Deprecation.toDT FQTypeName.knownType FQTypeName.toDT p.deprecated ]
    DRecord(typeName, typeName, [], Map fields)


  let fromDT (d : Dval) : PT.PackageType.PackageType =
    match d with
    | DRecord(_, _, _, fields) ->
      { id = fields |> D.field "id" |> D.uuid
        name = fields |> D.field "name" |> Name.fromDT
        declaration = fields |> D.field "declaration" |> TypeDeclaration.fromDT
        description = fields |> D.field "description" |> D.string
        deprecated =
          fields |> D.field "deprecated" |> Deprecation.fromDT FQTypeName.fromDT }
    | _ -> Exception.raiseInternal "Invalid PackageType" []


module PackageValue =
  module Name =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.PackageValue.name

    let toDT (n : PT.PackageValue.Name) : Dval =
      let fields =
        [ "owner", DString n.owner
          "modules", DList(VT.string, List.map DString n.modules)
          "name", DString n.name ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.PackageValue.Name =
      match d with
      | DRecord(_, _, _, fields) ->
        { owner = fields |> D.field "owner" |> D.string
          modules = fields |> D.field "modules" |> D.list D.string
          name = fields |> D.field "name" |> D.string }
      | _ -> Exception.raiseInternal "Invalid PackageValue.Name" []


  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.PackageValue.packageValue

  let toDT (p : PT.PackageValue.PackageValue) : Dval =
    let fields =
      [ "id", DUuid p.id
        "name", Name.toDT p.name
        "body", Expr.toDT p.body
        "description", DString p.description
        "deprecated",
        Deprecation.toDT FQValueName.knownType FQValueName.toDT p.deprecated ]
    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : PT.PackageValue.PackageValue =
    match d with
    | DRecord(_, _, _, fields) ->
      { id = fields |> D.field "id" |> D.uuid
        name = fields |> D.field "name" |> Name.fromDT
        body = fields |> D.field "body" |> Expr.fromDT
        description = fields |> D.field "description" |> D.string
        deprecated =
          fields |> D.field "deprecated" |> Deprecation.fromDT FQValueName.fromDT }
    | _ -> Exception.raiseInternal "Invalid PackageValue" []


module PackageFn =
  module Name =
    let typeName =
      FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.PackageFn.name

    let toDT (n : PT.PackageFn.Name) : Dval =
      let fields =
        [ "owner", DString n.owner
          "modules", DList(VT.string, List.map DString n.modules)
          "name", DString n.name ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.PackageFn.Name =
      match d with
      | DRecord(_, _, _, fields) ->
        { owner = fields |> D.field "owner" |> D.string
          modules = fields |> D.field "modules" |> D.list D.string
          name = fields |> D.field "name" |> D.string }
      | _ -> Exception.raiseInternal "Invalid PackageFn.Name" []


  module Parameter =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.PackageFn.parameter

    let knownType = KTCustomType(typeName, [])

    let toDT (p : PT.PackageFn.Parameter) : Dval =
      let fields =
        [ "name", DString p.name
          "typ", TypeReference.toDT p.typ
          "description", DString p.description ]
      DRecord(typeName, typeName, [], Map fields)


    let fromDT (d : Dval) : PT.PackageFn.Parameter =
      match d with
      | DRecord(_, _, _, fields) ->
        { name = fields |> D.field "name" |> D.string
          typ = fields |> D.field "typ" |> TypeReference.fromDT
          description = fields |> D.field "description" |> D.string }
      | _ -> Exception.raiseInternal "Invalid PackageFn.Parameter" []


  let typeName =
    FQTypeName.fqPackage
      PackageIDs.Type.LanguageTools.ProgramTypes.PackageFn.packageFn

  let toDT (p : PT.PackageFn.PackageFn) : Dval =
    let fields =
      [ ("id", DUuid p.id)
        ("name", Name.toDT p.name)
        ("body", Expr.toDT p.body)
        ("typeParams", DList(VT.string, List.map DString p.typeParams))
        ("parameters",
         DList(
           VT.known Parameter.knownType,
           p.parameters |> NEList.toList |> List.map Parameter.toDT
         ))
        ("returnType", TypeReference.toDT p.returnType)
        ("description", DString p.description)
        ("deprecated", Deprecation.toDT FQFnName.knownType FQFnName.toDT p.deprecated) ]

    DRecord(typeName, typeName, [], Map fields)


  let fromDT (d : Dval) : PT.PackageFn.PackageFn =
    match d with
    | DRecord(_, _, _, fields) ->
      { id = fields |> D.field "id" |> D.uuid
        name = fields |> D.field "name" |> Name.fromDT
        body = fields |> D.field "body" |> Expr.fromDT
        typeParams = fields |> D.field "typeParams" |> D.list D.string
        parameters =
          fields
          |> D.field "parameters"
          |> D.list Parameter.fromDT
          |> NEList.ofListUnsafe "PackageFn.fromDT" []
        returnType = fields |> D.field "returnType" |> TypeReference.fromDT
        description = fields |> D.field "description" |> D.string
        deprecated =
          fields |> D.field "deprecated" |> Deprecation.fromDT FQFnName.fromDT }
    | _ -> Exception.raiseInternal "Invalid PackageFn" []



module Search =
  module EntityType =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.Search.entityType

    let toDT (et : PT.Search.EntityType) : Dval =
      let (caseName, fields) =
        match et with
        | PT.Search.EntityType.Type -> "Type", []
        | PT.Search.EntityType.Module -> "Module", []
        | PT.Search.EntityType.Fn -> "Fn", []
        | PT.Search.EntityType.Value -> "Value", []
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : PT.Search.EntityType =
      match d with
      | DEnum(_, _, [], "Type", []) -> PT.Search.EntityType.Type
      | DEnum(_, _, [], "Module", []) -> PT.Search.EntityType.Module
      | DEnum(_, _, [], "Fn", []) -> PT.Search.EntityType.Fn
      | DEnum(_, _, [], "Value", []) -> PT.Search.EntityType.Value
      | _ -> Exception.raiseInternal "Invalid EntityType" []


  module SearchDepth =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.Search.searchDepth

    let toDT (sd : PT.Search.SearchDepth) : Dval =
      let (caseName, fields) =
        match sd with
        | PT.Search.SearchDepth.OnlyDirectDescendants -> "OnlyDirectDescendants", []
      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : PT.Search.SearchDepth =
      match d with
      | DEnum(_, _, [], "OnlyDirectDescendants", []) ->
        PT.Search.SearchDepth.OnlyDirectDescendants
      | _ -> Exception.raiseInternal "Invalid SearchDepth" []

  module SearchQuery =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.Search.searchQuery

    let toDT (sq : PT.Search.SearchQuery) : Dval =
      let fields =
        [ "currentModule", DList(VT.string, List.map DString sq.currentModule)
          "text", DString sq.text
          "searchDepth", SearchDepth.toDT sq.searchDepth
          "entityTypes",
          DList(
            VT.known (KTCustomType(EntityType.typeName, [])),
            sq.entityTypes |> List.map EntityType.toDT
          )
          "exactMatch", DBool sq.exactMatch ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.Search.SearchQuery =
      match d with
      | DRecord(_, _, _, fields) ->
        { PT.Search.currentModule =
            fields |> D.field "currentModule" |> D.list D.string
          PT.Search.text = fields |> D.field "text" |> D.string
          PT.Search.searchDepth =
            fields |> D.field "searchDepth" |> SearchDepth.fromDT
          PT.Search.entityTypes =
            fields |> D.field "entityTypes" |> D.list EntityType.fromDT
          PT.Search.exactMatch = fields |> D.field "exactMatch" |> D.bool }
      | _ -> Exception.raiseInternal "Invalid SearchQuery" []

  module SearchResults =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.Search.searchResults

    let toDT (sr : PT.Search.SearchResults) : Dval =
      let fields =
        [ "submodules",
          sr.submodules
          |> List.map (fun modules ->
            modules |> List.map (fun m -> DString m) |> Dval.list KTString)
          |> Dval.list (KTList VT.string)
          "types",
          sr.types
          |> List.map PackageType.toDT
          |> Dval.list (KTCustomType(PackageType.typeName, []))
          "values",
          sr.values
          |> List.map PackageValue.toDT
          |> Dval.list (KTCustomType(PackageValue.typeName, []))
          "fns",
          sr.fns
          |> List.map PackageFn.toDT
          |> Dval.list (KTCustomType(PackageFn.typeName, [])) ]
      DRecord(typeName, typeName, [], Map fields)

    let fromDT (d : Dval) : PT.Search.SearchResults =
      match d with
      | DRecord(_, _, _, fields) ->
        { submodules = fields |> D.field "submodules" |> D.list (D.list D.string)
          types = fields |> D.field "types" |> D.list PackageType.fromDT
          values = fields |> D.field "values" |> D.list PackageValue.fromDT
          fns = fields |> D.field "fns" |> D.list PackageFn.fromDT }
      | _ -> Exception.raiseInternal "Invalid SearchResults" []



// -- User stuff -- //

module Handler =
  module CronInterval =
    let typeName =
      FQTypeName.fqPackage
        PackageIDs.Type.LanguageTools.ProgramTypes.Handler.cronInterval

    let toDT (ci : PT.Handler.CronInterval) : Dval =
      let (caseName, fields) =
        match ci with
        | PT.Handler.CronInterval.EveryMinute -> "EveryMinute", []
        | PT.Handler.CronInterval.EveryHour -> "EveryHour", []
        | PT.Handler.CronInterval.Every12Hours -> "Every12Hours", []
        | PT.Handler.CronInterval.EveryDay -> "EveryDay", []
        | PT.Handler.CronInterval.EveryWeek -> "EveryWeek", []
        | PT.Handler.CronInterval.EveryFortnight -> "EveryFortnight", []

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : PT.Handler.CronInterval =
      match d with
      | DEnum(_, _, [], "EveryMinute", []) -> PT.Handler.CronInterval.EveryMinute
      | DEnum(_, _, [], "EveryHour", []) -> PT.Handler.CronInterval.EveryHour
      | DEnum(_, _, [], "Every12Hours", []) -> PT.Handler.CronInterval.Every12Hours
      | DEnum(_, _, [], "EveryDay", []) -> PT.Handler.CronInterval.EveryDay
      | DEnum(_, _, [], "EveryWeek", []) -> PT.Handler.CronInterval.EveryWeek
      | DEnum(_, _, [], "EveryFortnight", []) ->
        PT.Handler.CronInterval.EveryFortnight
      | _ -> Exception.raiseInternal "Invalid CronInterval" []


  module Spec =
    let typeName =
      FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.Handler.spec

    let toDT (s : PT.Handler.Spec) : Dval =
      let (caseName, fields) =
        match s with
        | PT.Handler.Spec.HTTP(route, method) ->
          "HTTP", [ DString route; DString method ]
        | PT.Handler.Spec.Worker name -> "Worker", [ DString name ]
        | PT.Handler.Spec.Cron(name, interval) ->
          "Cron", [ DString name; CronInterval.toDT interval ]
        | PT.Handler.Spec.REPL name -> "REPL", [ DString name ]

      DEnum(typeName, typeName, [], caseName, fields)

    let fromDT (d : Dval) : PT.Handler.Spec =
      match d with
      | DEnum(_, _, [], "HTTP", [ DString route; DString method ]) ->
        PT.Handler.Spec.HTTP(route, method)
      | DEnum(_, _, [], "Worker", [ DString name ]) -> PT.Handler.Spec.Worker(name)
      | DEnum(_, _, [], "Cron", [ DString name; interval ]) ->
        PT.Handler.Spec.Cron(name, CronInterval.fromDT interval)
      | DEnum(_, _, [], "REPL", [ DString name ]) -> PT.Handler.Spec.REPL(name)
      | _ -> Exception.raiseInternal "Invalid Spec" []

  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.Handler.handler

  let toDT (h : PT.Handler.T) : Dval =
    let fields =
      [ "tlid", DUInt64(uint64 h.tlid)
        "ast", Expr.toDT h.ast
        "spec", Spec.toDT h.spec ]
    DRecord(typeName, typeName, [], Map fields)


  let fromDT (d : Dval) : PT.Handler.T =
    match d with
    | DRecord(_, _, _, fields) ->
      { tlid = fields |> D.field "tlid" |> D.uInt64
        ast = fields |> D.field "ast" |> Expr.fromDT
        spec = fields |> D.field "spec" |> Spec.fromDT }
    | _ -> Exception.raiseInternal "Invalid Handler" []


module DB =
  let typeName = FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.db

  let toDT (db : PT.DB.T) : Dval =
    let fields =
      [ "tlid", DUInt64(uint64 db.tlid)
        "name", DString db.name
        "version", DInt32 db.version
        "typ", TypeReference.toDT db.typ ]
    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : PT.DB.T =
    match d with
    | DRecord(_, _, _, fields) ->
      { tlid = fields |> D.field "tlid" |> D.uInt64
        name = fields |> D.field "name" |> D.string
        version = fields |> D.field "version" |> D.int32
        typ = fields |> D.field "typ" |> TypeReference.fromDT }
    | _ -> Exception.raiseInternal "Invalid DB" []


module Secret =
  let typeName =
    FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.secret

  let toDT (s : PT.Secret.T) : Dval =
    let fields =
      [ "name", DString s.name
        "value", DString s.value
        "version", DInt32 s.version ]
    DRecord(typeName, typeName, [], Map fields)

  let fromDT (d : Dval) : PT.Secret.T =
    match d with
    | DRecord(_, _, _, fields) ->
      { name = fields |> D.field "name" |> D.string
        value = fields |> D.field "value" |> D.string
        version = fields |> D.field "version" |> D.int32 }
    | _ -> Exception.raiseInternal "Invalid Secret" []
