module LibExecution.ProgramTypesToDarkTypes

open Prelude

open RuntimeTypes
module VT = ValueType
module PT = ProgramTypes
module NRE = LibExecution.NameResolutionError
module D = LibExecution.DvalDecoder

let languageToolsTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  TypeName.fqPackage "Darklang" ("LanguageTools" :: submodules) name version


let ptTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  languageToolsTyp ("ProgramTypes" :: submodules) name version

let errorsTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  languageToolsTyp ("Errors" :: submodules) name version


// This isn't in PT but I'm not sure where else to put it...
// maybe rename this file to InternalTypesToDarkTypes?
module Sign =
  let toDT (s : Sign) : Dval =
    let caseName, fields =
      match s with
      | Positive -> "Positive", []
      | Negative -> "Negative", []

    let typeName = languageToolsTyp [] "Sign" 0
    Dval.enum typeName typeName (Some []) caseName fields


  let fromDT (d : Dval) : Sign =
    match d with
    //TODO: ensure that we are working with a right type
    | DEnum(_, _, [], "Positive", []) -> Positive
    | DEnum(_, _, [], "Negative", []) -> Negative
    | _ -> Exception.raiseInternal "Invalid sign" []



module FQName =
  let ownerField = D.stringField "owner"
  let modulesField = D.stringListField "modules"

  let nameField = D.field "name"
  let versionField = D.intField "version"

  module BuiltIn =
    let toDT
      (nameValueType : ValueType)
      (nameMapper : 'name -> Dval)
      (u : PT.FQName.BuiltIn<'name>)
      : Ply<Dval> =
      Dval.record
        (ptTyp [ "FQName" ] "BuiltIn" 0)
        (Some [ nameValueType ])
        [ "modules", Dval.list VT.unknownTODO (List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : PT.FQName.BuiltIn<'name> =
      match d with
      | DRecord(_, _, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module UserProgram =
    let toDT
      (nameValueType : ValueType)
      (nameMapper : 'name -> Dval)
      (u : PT.FQName.UserProgram<'name>)
      : Ply<Dval> =
      Dval.record
        (ptTyp [ "FQName" ] "UserProgram" 0)
        (Some [ nameValueType ])
        [ "modules",
          Dval.list (ValueType.Known KTString) (List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT
      (nameMapper : Dval -> 'name)
      (v : Dval)
      : PT.FQName.UserProgram<'name> =
      let unwrap = Exception.unwrapOptionInternal
      match v with
      | DRecord(_, _, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module Package =
    let toDT
      (nameValueType : ValueType)
      (nameMapper : 'name -> Dval)
      (u : PT.FQName.Package<'name>)
      : Ply<Dval> =
      Dval.record
        (ptTyp [ "FQName" ] "Package" 0)
        (Some [ nameValueType ])
        [ "owner", DString u.owner
          "modules",
          Dval.list (ValueType.Known KTString) (List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : PT.FQName.Package<'name> =
      match d with
      | DRecord(_, _, _, m) ->
        let owner = ownerField m
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { owner = owner; modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []


  let toDT
    (nameValueType : ValueType)
    (nameMapper : 'name -> Dval)
    (u : PT.FQName.FQName<'name>)
    : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match u with
          | PT.FQName.UserProgram u ->
            let! name = UserProgram.toDT nameValueType nameMapper u
            return "UserProgram", [ name ]
          | PT.FQName.Package u ->
            let! name = Package.toDT nameValueType nameMapper u
            return "Package", [ name ]
          | PT.FQName.BuiltIn u ->
            let! name = BuiltIn.toDT nameValueType nameMapper u
            return "BuiltIn", [ name ]
        }

      let typeName = ptTyp [ "FQName" ] "FQName" 0
      return Dval.enum typeName typeName VT.typeArgsTODO' caseName fields
    }

  let fromDT (nameMapper : Dval -> 'name) (d : Dval) : PT.FQName.FQName<'name> =
    match d with
    | DEnum(_, _, _typeArgsDEnumTODO, "UserProgram", [ u ]) ->
      PT.FQName.UserProgram(UserProgram.fromDT nameMapper u)
    | DEnum(_, _, _typeArgsDEnumTODO, "Package", [ u ]) ->
      PT.FQName.Package(Package.fromDT nameMapper u)
    | DEnum(_, _, _typeArgsDEnumTODO, "BuiltIn", [ u ]) ->
      PT.FQName.BuiltIn(BuiltIn.fromDT nameMapper u)
    | _ -> Exception.raiseInternal "Invalid FQName" []


module TypeName =
  module Name =
    let valueType = VT.unknownTODO

    let toDT (u : PT.TypeName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.TypeName.TypeName name -> "TypeName", [ DString name ]

      let typeName = ptTyp [ "TypeName" ] "Name" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : PT.TypeName.Name =
      match d with
      | DEnum(_, _, [], "TypeName", [ DString name ]) -> PT.TypeName.TypeName(name)
      | _ -> Exception.raiseInternal "Invalid TypeName" []

  module BuiltIn =
    let toDT (u : PT.TypeName.BuiltIn) : Ply<Dval> =
      FQName.BuiltIn.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : PT.TypeName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : PT.TypeName.UserProgram) : Ply<Dval> =
      FQName.UserProgram.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : PT.TypeName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : PT.TypeName.Package) : Ply<Dval> =
      FQName.Package.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : PT.TypeName.Package = FQName.Package.fromDT Name.fromDT d


  let toDT (u : PT.TypeName.TypeName) : Ply<Dval> =
    FQName.toDT Name.valueType Name.toDT u

  let fromDT (d : Dval) : PT.TypeName.TypeName = FQName.fromDT Name.fromDT d


module FnName =
  module Name =
    let valueType = VT.unknownTODO

    let toDT (u : PT.FnName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.FnName.FnName name -> "FnName", [ DString name ]

      let typeName = ptTyp [ "FnName" ] "Name" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : PT.FnName.Name =
      match d with
      | DEnum(_, _, [], "FnName", [ DString name ]) -> PT.FnName.FnName(name)
      | _ -> Exception.raiseInternal "Invalid FnName" []

  module BuiltIn =
    let toDT (u : PT.FnName.BuiltIn) : Ply<Dval> =
      FQName.BuiltIn.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : PT.FnName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : PT.FnName.UserProgram) : Ply<Dval> =
      FQName.UserProgram.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : PT.FnName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : PT.FnName.Package) : Ply<Dval> =
      FQName.Package.toDT Name.valueType Name.toDT u
    let fromDT (d : Dval) : PT.FnName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : PT.FnName.FnName) : Ply<Dval> =
    FQName.toDT Name.valueType Name.toDT u
  let fromDT (d : Dval) : PT.FnName.FnName = FQName.fromDT Name.fromDT d

module ConstantName =
  module Name =
    let valueType = VT.unknownTODO

    let toDT (u : PT.ConstantName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.ConstantName.ConstantName name -> "ConstantName", [ DString name ]

      let typeName = ptTyp [ "ConstantName" ] "Name" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : PT.ConstantName.Name =
      match d with
      | DEnum(_, _, [], "ConstantName", [ DString name ]) ->
        PT.ConstantName.ConstantName(name)
      | _ -> Exception.raiseInternal "Invalid ConstantName" []

  module BuiltIn =
    let toDT (u : PT.ConstantName.BuiltIn) : Ply<Dval> =
      FQName.BuiltIn.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : PT.ConstantName.BuiltIn =
      FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : PT.ConstantName.UserProgram) : Ply<Dval> =
      FQName.UserProgram.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : PT.ConstantName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : PT.ConstantName.Package) : Ply<Dval> =
      FQName.Package.toDT Name.valueType Name.toDT u

    let fromDT (d : Dval) : PT.ConstantName.Package =
      FQName.Package.fromDT Name.fromDT d

  let toDT (u : PT.ConstantName.ConstantName) : Ply<Dval> =
    FQName.toDT Name.valueType Name.toDT u

  let fromDT (d : Dval) : PT.ConstantName.ConstantName = FQName.fromDT Name.fromDT d

module NameResolution =
  let toDT
    (nameValueType : ValueType)
    (f : 'p -> Ply<Dval>)
    (result : PT.NameResolution<'p>)
    : Ply<Dval> =
    uply {
      let errType = VT.unknownTODO // NameResolutionError

      match result with
      | Ok name ->
        let! name = f name
        return Dval.resultOk nameValueType errType name
      | Error err ->
        return!
          err
          |> NRE.RTE.Error.toDT
          |> Ply.map (Dval.resultError nameValueType errType)
    }

  let fromDT (f : Dval -> 'a) (d : Dval) : PT.NameResolution<'a> =
    match d with
    | DEnum(tn, _, _typeArgsDEnumTODO, "Ok", [ v ]) when tn = Dval.resultType ->
      Ok(f v)

    | DEnum(tn, _, _typeArgsDEnumTODO, "Error", [ v ]) when tn = Dval.resultType ->
      Error(NRE.RTE.Error.fromDT v)

    | _ -> Exception.raiseInternal "Invalid NameResolution" []

module TypeReference =
  let rec toDT (t : PT.TypeReference) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match t with
          | PT.TVariable name -> return "TVariable", [ DString name ]

          | PT.TUnit -> return "TUnit", []
          | PT.TBool -> return "TBool", []
          | PT.TInt -> return "TInt", []
          | PT.TFloat -> return "TFloat", []
          | PT.TChar -> return "TChar", []
          | PT.TString -> return "TString", []
          | PT.TDateTime -> return "TDateTime", []
          | PT.TUuid -> return "TUuid", []
          | PT.TBytes -> return "TBytes", []

          | PT.TList inner ->
            let! inner = toDT inner
            return "TList", [ inner ]

          | PT.TTuple(first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "TTuple", [ first; second; theRest ]

          | PT.TDict inner ->
            let! inner = toDT inner
            return "TDict", [ inner ]

          | PT.TCustomType(typeName, typeArgs) ->
            let! typeName = NameResolution.toDT VT.unknownTODO TypeName.toDT typeName
            let! typeArgs =
              typeArgs
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "TCustomType", [ typeName; typeArgs ]

          | PT.TDB inner ->
            let! inner = toDT inner
            return "TDB", [ inner ]

          | PT.TFn(args, ret) ->
            let! args =
              args
              |> NEList.toList
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            let! ret = toDT ret
            return "TFn", [ args; ret ]
        }

      let typeName = ptTyp [] "TypeReference" 0
      return Dval.enum typeName typeName (Some []) caseName fields
    }

  let rec fromDT (d : Dval) : PT.TypeReference =
    match d with
    | DEnum(_, _, [], "TVariable", [ DString name ]) -> PT.TVariable(name)

    | DEnum(_, _, [], "TUnit", []) -> PT.TUnit
    | DEnum(_, _, [], "TBool", []) -> PT.TBool
    | DEnum(_, _, [], "TInt", []) -> PT.TInt
    | DEnum(_, _, [], "TFloat", []) -> PT.TFloat
    | DEnum(_, _, [], "TChar", []) -> PT.TChar
    | DEnum(_, _, [], "TString", []) -> PT.TString
    | DEnum(_, _, [], "TDateTime", []) -> PT.TDateTime
    | DEnum(_, _, [], "TUuid", []) -> PT.TUuid
    | DEnum(_, _, [], "TBytes", []) -> PT.TBytes

    | DEnum(_, _, [], "TList", [ inner ]) -> PT.TList(fromDT inner)

    | DEnum(_, _, [], "TTuple", [ first; second; DList(_vtTODO, theRest) ]) ->
      PT.TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "TDict", [ inner ]) -> PT.TDict(fromDT inner)

    | DEnum(_, _, [], "TCustomType", [ typeName; DList(_vtTODO, typeArgs) ]) ->
      PT.TCustomType(
        NameResolution.fromDT TypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, [], "TDB", [ inner ]) -> PT.TDB(fromDT inner)
    | DEnum(_, _, [], "TFn", [ DList(_vtTODO, head :: tail); ret ]) ->
      PT.TFn(NEList.ofList head tail |> NEList.map fromDT, fromDT ret)
    | _ -> Exception.raiseInternal "Invalid TypeReference" []


module LetPattern =
  let rec toDT (p : PT.LetPattern) : Dval =
    let caseName, fields =
      match p with
      | PT.LPVariable(id, name) -> "LPVariable", [ DInt(int64 id); DString name ]
      | PT.LPUnit id -> "LPUnit", [ DInt(int64 id) ]
      | PT.LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          Dval.list VT.unknownTODO (List.map toDT theRest) ]

    let typeName = ptTyp [] "LetPattern" 0
    Dval.enum typeName typeName (Some []) caseName fields


  let rec fromDT (d : Dval) : PT.LetPattern =
    match d with
    | DEnum(_, _, [], "LPVariable", [ DInt id; DString name ]) ->
      PT.LPVariable(uint64 id, name)
    | DEnum(_, _, [], "LPUnit", [ DInt id ]) -> PT.LPUnit(uint64 id)
    | DEnum(_, _, [], "LPTuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      PT.LPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let rec toDT (p : PT.MatchPattern) : Dval =
    let caseName, fields =
      match p with
      | PT.MPVariable(id, name) -> "MPVariable", [ DInt(int64 id); DString name ]

      | PT.MPUnit id -> "MPUnit", [ DInt(int64 id) ]
      | PT.MPBool(id, b) -> "MPBool", [ DInt(int64 id); DBool b ]
      | PT.MPInt(id, i) -> "MPInt", [ DInt(int64 id); DInt i ]
      | PT.MPFloat(id, sign, whole, remainder) ->
        "MPFloat",
        [ DInt(int64 id); Sign.toDT sign; DString whole; DString remainder ]
      | PT.MPChar(id, c) -> "MPChar", [ DInt(int64 id); DString c ]
      | PT.MPString(id, s) -> "MPString", [ DInt(int64 id); DString s ]

      | PT.MPList(id, inner) ->
        "MPList", [ DInt(int64 id); Dval.list VT.unknownTODO (List.map toDT inner) ]
      | PT.MPListCons(id, head, tail) ->
        "MPListCons", [ DInt(int64 id); toDT head; toDT tail ]
      | PT.MPTuple(id, first, second, theRest) ->
        "MPTuple",
        [ DInt(int64 id)
          toDT first
          toDT second
          Dval.list VT.unknownTODO (List.map toDT theRest) ]
      | PT.MPEnum(id, caseName, fieldPats) ->
        "MPEnum",
        [ DInt(int64 id)
          DString caseName
          Dval.list VT.unknownTODO (List.map toDT fieldPats) ]

    let typeName = ptTyp [] "MatchPattern" 0
    Dval.enum typeName typeName (Some []) caseName fields

  let rec fromDT (d : Dval) : PT.MatchPattern =
    match d with
    | DEnum(_, _, [], "MPVariable", [ DInt id; DString name ]) ->
      PT.MPVariable(uint64 id, name)

    | DEnum(_, _, [], "MPUnit", [ DInt id ]) -> PT.MPUnit(uint64 id)
    | DEnum(_, _, [], "MPBool", [ DInt id; DBool b ]) -> PT.MPBool(uint64 id, b)
    | DEnum(_, _, [], "MPInt", [ DInt id; DInt i ]) -> PT.MPInt(uint64 id, i)
    | DEnum(_, _, [], "MPFloat", [ DInt id; sign; DString whole; DString remainder ]) ->
      PT.MPFloat(uint64 id, Sign.fromDT sign, whole, remainder)
    | DEnum(_, _, [], "MPChar", [ DInt id; DString c ]) -> PT.MPChar(uint64 id, c)
    | DEnum(_, _, [], "MPString", [ DInt id; DString s ]) ->
      PT.MPString(uint64 id, s)

    | DEnum(_, _, [], "MPList", [ DInt id; DList(_vtTODO, inner) ]) ->
      PT.MPList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, [], "MPListCons", [ DInt id; head; tail ]) ->
      PT.MPListCons(uint64 id, fromDT head, fromDT tail)
    | DEnum(_, _, [], "MPTuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      PT.MPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | DEnum(_,
            _,
            [],
            "MPEnum",
            [ DInt id; DString caseName; DList(_vtTODO, fieldPats) ]) ->
      PT.MPEnum(uint64 id, caseName, List.map fromDT fieldPats)
    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module BinaryOperation =
  let toDT (b : PT.BinaryOperation) : Dval =

    let caseName, fields =
      match b with
      | PT.BinOpAnd -> "BinOpAnd", []
      | PT.BinOpOr -> "BinOpOr", []

    let typeName = ptTyp [] "BinaryOperation" 0
    Dval.enum typeName typeName (Some []) caseName fields

  let fromDT (d : Dval) : PT.BinaryOperation =
    match d with
    | DEnum(_, _, [], "BinOpAnd", []) -> PT.BinOpAnd
    | DEnum(_, _, [], "BinOpOr", []) -> PT.BinOpOr
    | _ -> Exception.raiseInternal "Invalid BinaryOperation" []


module InfixFnName =
  let toDT (i : PT.InfixFnName) : Dval =
    let caseName, fields =
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

    let typeName = ptTyp [] "InfixFnName" 0
    Dval.enum typeName typeName (Some []) caseName fields

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
  let toDT (i : PT.Infix) : Dval =
    let caseName, fields =
      match i with
      | PT.InfixFnCall infixFnName -> "InfixFnCall", [ InfixFnName.toDT infixFnName ]
      | PT.BinOp binOp -> "BinOp", [ BinaryOperation.toDT binOp ]

    let typeName = ptTyp [] "Infix" 0
    Dval.enum typeName typeName (Some []) caseName fields

  let fromDT (d : Dval) : PT.Infix =
    match d with
    | DEnum(_, _, [], "InfixFnCall", [ infixFnName ]) ->
      PT.InfixFnCall(InfixFnName.fromDT infixFnName)
    | DEnum(_, _, [], "BinOp", [ binOp ]) -> PT.BinOp(BinaryOperation.fromDT binOp)
    | _ -> Exception.raiseInternal "Invalid Infix" []


module StringSegment =
  let toDT (exprToDT : PT.Expr -> Ply<Dval>) (s : PT.StringSegment) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match s with
          | PT.StringText text -> return "StringText", [ DString text ]
          | PT.StringInterpolation expr ->
            let! expr = exprToDT expr
            return "StringInterpolation", [ expr ]
        }

      let typeName = ptTyp [] "StringSegment" 0
      return Dval.enum typeName typeName (Some []) caseName fields
    }

  let fromDT (exprFromDT : Dval -> PT.Expr) (d : Dval) : PT.StringSegment =
    match d with
    | DEnum(_, _, [], "StringText", [ DString text ]) -> PT.StringText text
    | DEnum(_, _, [], "StringInterpolation", [ expr ]) ->
      PT.StringInterpolation(exprFromDT expr)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


module PipeExpr =
  let toDT (exprToDT : PT.Expr -> Ply<Dval>) (s : PT.PipeExpr) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match s with
          | PT.EPipeVariable(id, varName, exprs) ->
            let! exprs =
              exprs
              |> Ply.List.mapSequentially exprToDT
              |> Ply.map (Dval.list VT.unknownTODO)

            return "EPipeVariable", [ DInt(int64 id); DString varName; exprs ]

          | PT.EPipeLambda(id, args, body) ->
            let variables =
              args
              |> NEList.toList
              |> List.map (fun (id, varName) ->
                DTuple(DInt(int64 id), DString varName, []))
              |> Dval.list VT.unknownTODO

            let! body = exprToDT body

            return "EPipeLambda", [ DInt(int64 id); variables; body ]


          | PT.EPipeInfix(id, infix, expr) ->
            let! expr = exprToDT expr
            return "EPipeInfix", [ DInt(int64 id); Infix.toDT infix; expr ]

          | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
            let! name = NameResolution.toDT VT.unknownTODO FnName.toDT fnName
            let! typeArgs =
              typeArgs
              |> Ply.List.mapSequentially TypeReference.toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            let! args =
              args
              |> Ply.List.mapSequentially exprToDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EPipeFnCall", [ DInt(int64 id); name; typeArgs; args ]

          | PT.EPipeEnum(id, typeName, caseName, fields) ->
            let! typeName = NameResolution.toDT VT.unknownTODO TypeName.toDT typeName
            let! fields =
              fields
              |> Ply.List.mapSequentially exprToDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return
              "EPipeEnum", [ DInt(int64 id); typeName; DString caseName; fields ]
        }

      let typeName = ptTyp [] "PipeExpr" 0
      return Dval.enum typeName typeName (Some []) caseName fields
    }

  let fromDT (exprFromDT : Dval -> PT.Expr) (d : Dval) : PT.PipeExpr =
    match d with
    | DEnum(_,
            _,
            [],
            "EPipeVariable",
            [ DInt id; DString varName; DList(_vtTODO, args) ]) ->
      PT.EPipeVariable(uint64 id, varName, args |> List.map exprFromDT)

    | DEnum(_, _, [], "EPipeLambda", [ DInt id; variables; body ]) ->
      let variables =
        match variables with
        | DList(_vtTODO, head :: tail) ->
          NEList.ofList head tail
          |> NEList.map (function
            | DTuple(DInt id, DString varName, []) -> (uint64 id, varName)
            | _ -> Exception.raiseInternal "Invalid variable" [])
        | _ -> Exception.raiseInternal "Invalid variables" []

      PT.EPipeLambda(uint64 id, variables, exprFromDT body)

    | DEnum(_, _, [], "EPipeInfix", [ DInt id; infix; expr ]) ->
      PT.EPipeInfix(uint64 id, Infix.fromDT infix, exprFromDT expr)

    | DEnum(_,
            _,
            [],
            "EPipeFnCall",
            [ DInt id; fnName; DList(_vtTODO1, typeArgs); DList(_vtTODO2, args) ]) ->
      PT.EPipeFnCall(
        uint64 id,
        NameResolution.fromDT FnName.fromDT fnName,
        List.map TypeReference.fromDT typeArgs,
        List.map exprFromDT args
      )

    | DEnum(_,
            _,
            [],
            "EPipeEnum",
            [ DInt id; typeName; DString caseName; DList(_vtTODO, fields) ]) ->
      PT.EPipeEnum(
        uint64 id,
        NameResolution.fromDT TypeName.fromDT typeName,
        caseName,
        List.map exprFromDT fields
      )

    | _ -> Exception.raiseInternal "Invalid PipeExpr" []


module Expr =
  let rec toDT (e : PT.Expr) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match e with
          | PT.EUnit id -> return "EUnit", [ DInt(int64 id) ]

          // simple data
          | PT.EBool(id, b) -> return "EBool", [ DInt(int64 id); DBool b ]
          | PT.EInt(id, i) -> return "EInt", [ DInt(int64 id); DInt i ]
          | PT.EFloat(id, sign, whole, remainder) ->
            return
              "EFloat",
              [ DInt(int64 id); Sign.toDT sign; DString whole; DString remainder ]

          | PT.EChar(id, c) -> return "EChar", [ DInt(int64 id); DString c ]
          | PT.EString(id, segments) ->
            let! segments =
              segments
              |> Ply.List.mapSequentially (StringSegment.toDT toDT)
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EString", [ DInt(int64 id); segments ]

          // structures of data
          | PT.EList(id, items) ->
            let! items =
              items
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EList", [ DInt(int64 id); items ]

          | PT.EDict(id, pairs) ->
            let! pairs =
              pairs
              |> Ply.List.mapSequentially (fun (k, v) ->
                uply {
                  let! v = toDT v
                  return DTuple(DString k, v, [])
                })
              |> Ply.map (Dval.list VT.unknownTODO)

            return "EDict", [ DInt(int64 id); pairs ]

          | PT.ETuple(id, first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "ETuple", [ DInt(int64 id); first; second; theRest ]

          | PT.ERecord(id, typeName, fields) ->
            let! typeName = NameResolution.toDT VT.unknownTODO TypeName.toDT typeName

            let! fields =
              fields
              |> Ply.List.mapSequentially (fun (name, expr) ->
                uply {
                  let! expr = toDT expr
                  return DTuple(DString name, expr, [])
                })
              |> Ply.map (Dval.list VT.unknownTODO)

            return "ERecord", [ DInt(int64 id); typeName; fields ]

          | PT.EEnum(id, typeName, caseName, fields) ->
            let! typeName = NameResolution.toDT VT.unknownTODO TypeName.toDT typeName
            let! fields =
              fields
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EEnum", [ DInt(int64 id); typeName; DString caseName; fields ]

          // declaring and accessing variables
          | PT.ELet(id, lp, expr, body) ->
            let! expr = toDT expr
            let! body = toDT body
            return "ELet", [ DInt(int64 id); LetPattern.toDT lp; expr; body ]

          | PT.EFieldAccess(id, expr, fieldName) ->
            let! expr = toDT expr
            return "EFieldAccess", [ DInt(int64 id); expr; DString fieldName ]

          | PT.EVariable(id, varName) ->
            return "EVariable", [ DInt(int64 id); DString varName ]


          // control flow
          | PT.EIf(id, cond, thenExpr, elseExpr) ->
            let! cond = toDT cond
            let! thenExpr = toDT thenExpr
            let! elseExpr =
              elseExpr |> Ply.Option.map toDT |> Ply.map (Dval.option VT.unknownTODO)
            return "EIf", [ DInt(int64 id); cond; thenExpr; elseExpr ]

          | PT.EMatch(id, arg, cases) ->
            let! arg = toDT arg

            let! cases =
              cases
              |> Ply.List.mapSequentially (fun (pattern, expr) ->
                uply {
                  let! expr = toDT expr
                  return DTuple(MatchPattern.toDT pattern, expr, [])
                })
              |> Ply.map (Dval.list VT.unknownTODO)

            return "EMatch", [ DInt(int64 id); arg; cases ]

          | PT.EPipe(id, expr, pipeExprs) ->
            let! expr = toDT expr
            let! pipeExprs =
              pipeExprs
              |> Ply.List.mapSequentially (PipeExpr.toDT toDT)
              |> Ply.map (Dval.list VT.unknownTODO)

            return "EPipe", [ DInt(int64 id); expr; pipeExprs ]


          // function calls
          | PT.EInfix(id, infix, lhs, rhs) ->
            let! lhs = toDT lhs
            let! rhs = toDT rhs
            return "EInfix", [ DInt(int64 id); Infix.toDT infix; lhs; rhs ]

          | PT.ELambda(id, args, body) ->
            let variables =
              args
              |> NEList.toList
              |> List.map (fun (id, varName) ->
                DTuple(DInt(int64 id), DString varName, []))
              |> Dval.list VT.unknownTODO

            let! body = toDT body

            return "ELambda", [ DInt(int64 id); variables; body ]

          | PT.EConstant(id, name) ->
            let! name = NameResolution.toDT VT.unknownTODO ConstantName.toDT name
            return "EConstant", [ DInt(int64 id); name ]

          | PT.EApply(id, name, typeArgs, args) ->
            let! name = toDT name
            let! typeArgs =
              typeArgs
              |> Ply.List.mapSequentially TypeReference.toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            let! args =
              args
              |> NEList.toList
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "EApply", [ DInt(int64 id); name; typeArgs; args ]

          | PT.EFnName(id, name) ->
            let! name = NameResolution.toDT VT.unknownTODO FnName.toDT name
            return "EFnName", [ DInt(int64 id); name ]

          | PT.ERecordUpdate(id, record, updates) ->
            let! record = toDT record

            let! updates =
              updates
              |> NEList.toList
              |> Ply.List.mapSequentially (fun (name, expr) ->
                uply {
                  let! expr = toDT expr
                  return DTuple(DString name, expr, [])
                })

            return
              "ERecordUpdate",
              [ DInt(int64 id); record; Dval.list VT.unknownTODO (updates) ]
        }

      let typeName = ptTyp [] "Expr" 0
      return Dval.enum typeName typeName (Some []) caseName fields
    }

  let rec fromDT (d : Dval) : PT.Expr =
    match d with
    | DEnum(_, _, [], "EUnit", [ DInt id ]) -> PT.EUnit(uint64 id)

    // simple data
    | DEnum(_, _, [], "EBool", [ DInt id; DBool b ]) -> PT.EBool(uint64 id, b)
    | DEnum(_, _, [], "EInt", [ DInt id; DInt i ]) -> PT.EInt(uint64 id, i)
    | DEnum(_, _, [], "EFloat", [ DInt id; sign; DString whole; DString remainder ]) ->
      PT.EFloat(uint64 id, Sign.fromDT sign, whole, remainder)
    | DEnum(_, _, [], "EChar", [ DInt id; DString c ]) -> PT.EChar(uint64 id, c)
    | DEnum(_, _, [], "EString", [ DInt id; DList(_vtTODO, segments) ]) ->
      PT.EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


    // structures of data
    | DEnum(_, _, [], "EList", [ DInt id; DList(_vtTODO, inner) ]) ->
      PT.EList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, [], "EDict", [ DInt id; DList(_vtTODO, pairsList) ]) ->
      let pairs =
        pairsList
        |> List.collect (fun pair ->
          match pair with
          | DTuple(DString k, v, _) -> [ (k, fromDT v) ]
          | _ -> [])
      PT.EDict(uint64 id, pairs)


    | DEnum(_, _, [], "ETuple", [ DInt id; first; second; DList(_vtTODO, theRest) ]) ->
      PT.ETuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, [], "ERecord", [ DInt id; typeName; DList(_vtTODO, fieldsList) ]) ->
      let fields =
        fieldsList
        |> List.collect (fun field ->
          match field with
          | DTuple(DString name, expr, _) -> [ (name, fromDT expr) ]
          | _ -> [])
      PT.ERecord(uint64 id, NameResolution.fromDT TypeName.fromDT typeName, fields)


    | DEnum(_,
            _,
            [],
            "EEnum",
            [ DInt id; typeName; DString caseName; DList(_vtTODO, fields) ]) ->
      PT.EEnum(
        uint64 id,
        NameResolution.fromDT TypeName.fromDT typeName,
        caseName,
        List.map fromDT fields
      )

    // declaring and accessing variables
    | DEnum(_, _, [], "ELet", [ DInt id; lp; expr; body ]) ->
      PT.ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

    | DEnum(_, _, [], "EFieldAccess", [ DInt id; expr; DString fieldName ]) ->
      PT.EFieldAccess(uint64 id, fromDT expr, fieldName)

    | DEnum(_, _, [], "EVariable", [ DInt id; DString varName ]) ->
      PT.EVariable(uint64 id, varName)

    // control flow
    | DEnum(_, _, [], "EIf", [ DInt id; cond; thenExpr; elseExpr ]) ->
      let elseExpr =
        match elseExpr with
        | DEnum(_, _, _typeArgsDEnumTODO, "Some", [ dv ]) -> Some(fromDT dv)
        | DEnum(_, _, _typeArgsDEnumTODO, "None", []) -> None
        | _ ->
          Exception.raiseInternal "Invalid else expression" [ "elseExpr", elseExpr ]
      PT.EIf(uint64 id, fromDT cond, fromDT thenExpr, elseExpr)

    | DEnum(_, _, [], "EMatch", [ DInt id; arg; DList(_vtTODO, cases) ]) ->
      let cases =
        cases
        |> List.collect (fun case ->
          match case with
          | DTuple(pattern, expr, _) ->
            [ (MatchPattern.fromDT pattern, fromDT expr) ]
          | _ -> [])
      PT.EMatch(uint64 id, fromDT arg, cases)

    | DEnum(_, _, [], "EPipe", [ DInt id; expr; DList(_vtTODO, pipeExprs) ]) ->
      PT.EPipe(uint64 id, fromDT expr, List.map (PipeExpr.fromDT fromDT) pipeExprs)

    // function calls
    | DEnum(_, _, [], "EInfix", [ DInt id; infix; lhs; rhs ]) ->
      PT.EInfix(uint64 id, Infix.fromDT infix, fromDT lhs, fromDT rhs)

    | DEnum(_, _, [], "ELambda", [ DInt id; DList(_vtTODO, head :: tail); body ]) ->
      let args =
        NEList.ofList head tail
        |> NEList.map (fun arg ->
          match arg with
          | DTuple(DInt argId, DString varName, _) -> (uint64 argId, varName)
          | _ -> Exception.raiseInternal "Invalid lambda arg" [ "arg", arg ])
      PT.ELambda(uint64 id, args, fromDT body)


    | DEnum(_,
            _,
            [],
            "EApply",
            [ DInt id; name; DList(_vtTODO1, typeArgs); DList(_vtTODO2, args) ]) ->
      PT.EApply(
        uint64 id,
        fromDT name,
        List.map TypeReference.fromDT typeArgs,
        args |> NEList.ofListUnsafe "EApply" [] |> NEList.map fromDT
      )

    | DEnum(_, _, [], "EFnName", [ DInt id; name ]) ->
      PT.EFnName(uint64 id, NameResolution.fromDT FnName.fromDT name)

    | DEnum(_,
            _,
            [],
            "ERecordUpdate",
            [ DInt id; record; DList(_vtTODO, head :: tail) ]) ->
      let updates =
        NEList.ofList head tail
        |> NEList.map (fun update ->
          match update with
          | DTuple(DString name, expr, _) -> (name, fromDT expr)
          | _ ->
            Exception.raiseInternal "Invalid record update" [ "update", update ])
      PT.ERecordUpdate(uint64 id, fromDT record, updates)

    | e -> Exception.raiseInternal "Invalid Expr" [ "e", e ]


module Const =
  let rec toDT (c : PT.Const) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match c with
          | PT.Const.CUnit -> return "CUnit", []
          | PT.Const.CBool b -> return "CBool", [ DBool b ]
          | PT.Const.CInt i -> return "CInt", [ DInt i ]
          | PT.Const.CFloat(sign, w, f) ->
            return "CFloat", [ Sign.toDT sign; DString w; DString f ]
          | PT.Const.CChar c -> return "CChar", [ DChar c ]
          | PT.Const.CString s -> return "CString", [ DString s ]

          | PT.Const.CTuple(first, second, theRest) ->
            let! first = toDT first
            let! second = toDT second
            let! theRest =
              theRest
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "CTuple", [ first; second; theRest ]

          | PT.Const.CEnum(typeName, caseName, fields) ->
            let! typeName = NameResolution.toDT VT.unknownTODO TypeName.toDT typeName
            let! fields =
              fields
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return ("CEnum", [ typeName; DString caseName; fields ])

          | PT.Const.CList inner ->
            let! items =
              inner
              |> Ply.List.mapSequentially toDT
              |> Ply.map (Dval.list VT.unknownTODO)
            return "CList", [ items ]

          | PT.Const.CDict pairs ->
            let! pairs =
              pairs
              |> Ply.List.mapSequentially (fun (k, v) ->
                uply {
                  let! v = toDT v
                  return DTuple(DString k, v, [])
                })
              |> Ply.map (Dval.list VT.unknownTODO)
            return "CDict", [ pairs ]
        }

      let typeName = ptTyp [] "Const" 0
      return Dval.enum typeName typeName (Some []) caseName fields
    }

  let rec fromDT (d : Dval) : PT.Const =
    match d with
    | DEnum(_, _, [], "CInt", [ DInt i ]) -> PT.Const.CInt i
    | DEnum(_, _, [], "CBool", [ DBool b ]) -> PT.Const.CBool b
    | DEnum(_, _, [], "CString", [ DString s ]) -> PT.Const.CString s
    | DEnum(_, _, [], "CChar", [ DChar c ]) -> PT.Const.CChar c
    | DEnum(_, _, [], "CFloat", [ sign; DString w; DString f ]) ->
      PT.Const.CFloat(Sign.fromDT sign, w, f)
    | DEnum(_, _, [], "CUnit", []) -> PT.Const.CUnit
    | DEnum(_, _, [], "CTuple", [ first; second; DList(_vtTODO, rest) ]) ->
      PT.Const.CTuple(fromDT first, fromDT second, List.map fromDT rest)
    | DEnum(_, _, [], "CEnum", [ typeName; DString caseName; DList(_vtTODO, fields) ]) ->
      PT.Const.CEnum(
        NameResolution.fromDT TypeName.fromDT typeName,
        caseName,
        List.map fromDT fields
      )
    | DEnum(_, _, [], "CList", [ DList(_vtTODO, inner) ]) ->
      PT.Const.CList(List.map fromDT inner)
    | DEnum(_, _, [], "CDict", [ DList(_vtTODO, pairs) ]) ->
      let pairs =
        pairs
        |> List.map (fun pair ->
          match pair with
          | DTuple(k, v, _) -> (fromDT k, fromDT v)
          | _ -> Exception.raiseInternal "Invalid pair" [])
      PT.Const.CDict(
        List.map
          (fun (k, v) ->
            (match k with
             | PT.Const.CString s -> s
             | _ -> Exception.raiseInternal "Invalid key" []),
            v)
          pairs
      )


    | _ -> Exception.raiseInternal "Invalid Const" []

module Deprecation =
  let toDT (inner : 'a -> Ply<Dval>) (d : PT.Deprecation<'a>) : Ply<Dval> =
    uply {
      let! (caseName, fields) =
        uply {
          match d with
          | PT.Deprecation.NotDeprecated -> return "NotDeprecated", []

          | PT.Deprecation.RenamedTo replacement ->
            let! replacement = inner replacement
            return "RenamedTo", [ replacement ]

          | PT.Deprecation.ReplacedBy replacement ->
            let! replacement = inner replacement
            return "ReplacedBy", [ replacement ]

          | PT.Deprecation.DeprecatedBecause reason ->
            return "DeprecatedBecause", [ DString reason ]
        }

      let typeName = ptTyp [] "Deprecation" 0
      return Dval.enum typeName typeName VT.typeArgsTODO' caseName fields
    }

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
  module RecordField =
    let toDT (rf : PT.TypeDeclaration.RecordField) : Ply<Dval> =
      uply {
        let! typ = TypeReference.toDT rf.typ
        return!
          Dval.record
            (ptTyp [ "TypeDeclaration" ] "RecordField" 0)
            (Some [])
            [ "name", DString rf.name
              "typ", typ
              "description", DString rf.description ]
      }

    let fromDT (d : Dval) : PT.TypeDeclaration.RecordField =
      match d with
      | DRecord(_, _, _, fields) ->
        let name = fields |> D.stringField "name"
        let typ = fields |> D.field "typ" |> TypeReference.fromDT
        let description = D.stringField "description" fields

        { name = name; typ = typ; description = description }

      | _ -> Exception.raiseInternal "Invalid RecordField" []

  module EnumField =
    let toDT (ef : PT.TypeDeclaration.EnumField) : Ply<Dval> =
      uply {
        let! typ = TypeReference.toDT ef.typ
        return!
          Dval.record
            (ptTyp [ "TypeDeclaration" ] "EnumField" 0)
            (Some [])
            [ "typ", typ
              "label", ef.label |> Option.map DString |> Dval.option VT.string
              "description", DString ef.description ]
      }

    let fromDT (d : Dval) : PT.TypeDeclaration.EnumField =
      match d with
      | DRecord(_, _, _, fields) ->
        let typ = fields |> D.field "typ" |> TypeReference.fromDT

        let label =
          match Map.get "label" fields with
          | Some(DEnum(_, _, _typeArgsDEnumTODO, "Some", [ DString label ])) ->
            Some label
          | Some(DEnum(_, _, _typeArgsDEnumTODO, "None", [])) -> None
          | _ ->
            Exception.raiseInternal "Expected label to be an option of string" []

        let description = fields |> D.stringField "description"

        { typ = typ; label = label; description = description }

      | _ -> Exception.raiseInternal "Invalid EnumField" []


  module EnumCase =
    let toDT (ec : PT.TypeDeclaration.EnumCase) : Ply<Dval> =
      uply {
        let! fields =
          ec.fields
          |> Ply.List.mapSequentially EnumField.toDT
          |> Ply.map (Dval.list VT.unknownTODO)

        return!
          Dval.record
            (ptTyp [ "TypeDeclaration" ] "EnumCase" 0)
            (Some [])
            [ "name", DString ec.name
              "fields", fields
              "description", DString ec.description ]
      }

    let fromDT (d : Dval) : PT.TypeDeclaration.EnumCase =
      match d with
      | DRecord(_, _, _, attributes) ->
        let name = D.stringField "name" attributes
        let fields = attributes |> D.listField "fields" |> List.map EnumField.fromDT
        let description = attributes |> D.stringField "description"

        { name = name; fields = fields; description = description }

      | _ -> Exception.raiseInternal "Invalid EnumCase" []


  module Definition =
    let toDT (d : PT.TypeDeclaration.Definition) : Ply<Dval> =
      uply {
        let! (caseName, fields) =
          uply {
            match d with
            | PT.TypeDeclaration.Alias typeRef ->
              let! typeRef = TypeReference.toDT typeRef
              return "Alias", [ typeRef ]

            | PT.TypeDeclaration.Record fields ->
              let! fields =
                fields
                |> NEList.toList
                |> Ply.List.mapSequentially RecordField.toDT
                |> Ply.map (Dval.list VT.unknownTODO)
              return "Record", [ fields ]

            | PT.TypeDeclaration.Enum cases ->
              let! cases =
                cases
                |> NEList.toList
                |> Ply.List.mapSequentially EnumCase.toDT
                |> Ply.map (Dval.list VT.unknownTODO)
              return "Enum", [ cases ]
          }

        let typeName = ptTyp [ "TypeDeclaration" ] "Definition" 0
        return Dval.enum typeName typeName (Some []) caseName fields
      }

    let fromDT (d : Dval) : PT.TypeDeclaration.Definition =
      match d with
      | DEnum(_, _, [], "Alias", [ typeRef ]) ->
        PT.TypeDeclaration.Alias(TypeReference.fromDT typeRef)

      | DEnum(_, _, [], "Record", [ DList(_vtTODO, firstField :: additionalFields) ]) ->
        let fields = NEList.ofList firstField additionalFields
        PT.TypeDeclaration.Record(NEList.map RecordField.fromDT fields)

      | DEnum(_, _, [], "Enum", [ DList(_vtTODO, firstCase :: additionalCases) ]) ->
        let cases = NEList.ofList firstCase additionalCases
        PT.TypeDeclaration.Enum(NEList.map EnumCase.fromDT cases)

      | _ -> Exception.raiseInternal "Invalid TypeDeclaration.Definition" []


  let toDT (td : PT.TypeDeclaration.T) : Ply<Dval> =
    uply {
      let! definition = Definition.toDT td.definition

      return!
        Dval.record
          (ptTyp [ "TypeDeclaration" ] "TypeDeclaration" 0)
          (Some [])
          [ "typeParams", Dval.list VT.unknownTODO (List.map DString td.typeParams)
            "definition", definition ]
    }

  let fromDT (d : Dval) : PT.TypeDeclaration.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let typeParams = D.stringListField "typeParams" fields
      let definition = fields |> D.field "definition" |> Definition.fromDT

      { typeParams = typeParams; definition = definition }

    | _ -> Exception.raiseInternal "Invalid TypeDeclaration" []


module Handler =
  module CronInterval =
    let toDT (ci : PT.Handler.CronInterval) : Dval =
      let caseName, fields =
        match ci with
        | PT.Handler.CronInterval.EveryMinute -> "EveryMinute", []
        | PT.Handler.CronInterval.EveryHour -> "EveryHour", []
        | PT.Handler.CronInterval.Every12Hours -> "Every12Hours", []
        | PT.Handler.CronInterval.EveryDay -> "EveryDay", []
        | PT.Handler.CronInterval.EveryWeek -> "EveryWeek", []
        | PT.Handler.CronInterval.EveryFortnight -> "EveryFortnight", []

      let typeName = ptTyp [ "Handler" ] "CronInterval" 0
      Dval.enum typeName typeName (Some []) caseName fields

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
    let toDT (s : PT.Handler.Spec) : Dval =
      let caseName, fields =
        match s with
        | PT.Handler.Spec.HTTP(route, method) ->
          "HTTP", [ DString route; DString method ]
        | PT.Handler.Spec.Worker name -> "Worker", [ DString name ]
        | PT.Handler.Spec.Cron(name, interval) ->
          "Cron", [ DString name; CronInterval.toDT interval ]
        | PT.Handler.Spec.REPL name -> "REPL", [ DString name ]

      let typeName = ptTyp [ "Handler" ] "Spec" 0
      Dval.enum typeName typeName (Some []) caseName fields

    let fromDT (d : Dval) : PT.Handler.Spec =
      match d with
      | DEnum(_, _, [], "HTTP", [ DString route; DString method ]) ->
        PT.Handler.Spec.HTTP(route, method)
      | DEnum(_, _, [], "Worker", [ DString name ]) -> PT.Handler.Spec.Worker(name)
      | DEnum(_, _, [], "Cron", [ DString name; interval ]) ->
        PT.Handler.Spec.Cron(name, CronInterval.fromDT interval)
      | DEnum(_, _, [], "REPL", [ DString name ]) -> PT.Handler.Spec.REPL(name)
      | _ -> Exception.raiseInternal "Invalid Spec" []

  let toDT (h : PT.Handler.T) : Ply<Dval> =
    uply {
      let! ast = Expr.toDT h.ast

      return!
        Dval.record
          (ptTyp [ "Handler" ] "Handler" 0)
          (Some [])
          [ "tlid", DInt(int64 h.tlid); "ast", ast; "spec", Spec.toDT h.spec ]
    }

  let fromDT (d : Dval) : PT.Handler.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let ast = fields |> D.field "ast" |> Expr.fromDT
      let spec = fields |> D.field "spec" |> Spec.fromDT

      { tlid = tlid; ast = ast; spec = spec }

    | _ -> Exception.raiseInternal "Invalid Handler" []


module DB =
  let toDT (db : PT.DB.T) : Ply<Dval> =
    uply {
      let! typ = TypeReference.toDT db.typ

      return!
        Dval.record
          (ptTyp [] "DB" 0)
          (Some [])
          [ "tlid", DInt(int64 db.tlid)
            "name", DString db.name
            "version", DInt db.version
            "typ", typ ]
    }

  let fromDT (d : Dval) : PT.DB.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let name = fields |> D.stringField "name"
      let version = fields |> D.intField "version"
      let typ = fields |> D.field "typ" |> TypeReference.fromDT
      { tlid = tlid; name = name; version = version; typ = typ }

    | _ -> Exception.raiseInternal "Invalid DB" []


module UserType =
  let toDT (userType : PT.UserType.T) : Ply<Dval> =
    uply {
      let! name = TypeName.UserProgram.toDT userType.name
      let! declaration = TypeDeclaration.toDT userType.declaration
      let! deprecated = Deprecation.toDT TypeName.toDT userType.deprecated

      return!
        Dval.record
          (ptTyp [] "UserType" 0)
          (Some [])
          [ "tlid", DInt(int64 userType.tlid)
            "name", name
            "description", DString userType.description
            "declaration", declaration
            "deprecated", deprecated ]
    }

  let fromDT (d : Dval) : PT.UserType.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let name = fields |> D.field "name" |> TypeName.UserProgram.fromDT
      let declaration = fields |> D.field "declaration" |> TypeDeclaration.fromDT
      let description = fields |> D.stringField "description"
      let deprecated =
        fields |> D.field "deprecated" |> Deprecation.fromDT TypeName.fromDT

      { tlid = tlid
        name = name
        declaration = declaration
        description = description
        deprecated = deprecated }

    | _ -> Exception.raiseInternal "Invalid UserType" []


module UserFunction =
  module Parameter =
    let toDT (p : PT.UserFunction.Parameter) : Ply<Dval> =
      uply {
        let! typ = TypeReference.toDT p.typ
        return!
          Dval.record
            (ptTyp [ "UserFunction" ] "Parameter" 0)
            (Some [])
            [ "name", DString p.name
              "typ", typ
              "description", DString p.description ]
      }

    let fromDT (d : Dval) : PT.UserFunction.Parameter =
      match d with
      | DRecord(_, _, _, fields) ->
        let name = fields |> D.stringField "name"
        let typ = fields |> D.field "typ" |> TypeReference.fromDT
        let description = fields |> D.stringField "description"

        { name = name; typ = typ; description = description }

      | _ -> Exception.raiseInternal "Invalid UserFunction.Parameter" []


  let toDT (userFn : PT.UserFunction.T) : Ply<Dval> =
    uply {
      let! name = FnName.UserProgram.toDT userFn.name
      let! parameters =
        userFn.parameters
        |> NEList.toList
        |> Ply.List.mapSequentially Parameter.toDT
        |> Ply.map (Dval.list VT.unknownTODO)
      let! returnType = TypeReference.toDT userFn.returnType
      let! body = Expr.toDT userFn.body
      let! deprecated = Deprecation.toDT FnName.toDT userFn.deprecated

      return!
        Dval.record
          (ptTyp [ "UserFunction" ] "UserFunction" 0)
          (Some [])
          [ "tlid", DInt(int64 userFn.tlid)
            "name", name
            "typeParams",
            Dval.list VT.unknownTODO (List.map DString userFn.typeParams)
            "parameters", parameters
            "returnType", returnType
            "body", body
            "description", DString userFn.description
            "deprecated", deprecated ]
    }

  let fromDT (d : Dval) : PT.UserFunction.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let name = fields |> D.field "name" |> FnName.UserProgram.fromDT
      let typeParams = fields |> D.stringListField "typeParams"
      let parameters =
        fields
        |> D.listField "parameters"
        |> List.map Parameter.fromDT
        |> NEList.ofListUnsafe "userFunction needs more than one parameter" []
      let returnType = fields |> D.field "returnType" |> TypeReference.fromDT
      let description = fields |> D.stringField "description"
      let deprecated =
        fields |> D.field "deprecated" |> Deprecation.fromDT FnName.fromDT
      let body = fields |> D.field "body" |> Expr.fromDT

      { tlid = tlid
        name = name
        typeParams = typeParams
        parameters = parameters
        returnType = returnType
        body = body
        description = description
        deprecated = deprecated }

    | _ -> Exception.raiseInternal "Invalid UserFunction" []

module UserConstant =
  let toDT (userConstant : PT.UserConstant.T) : Ply<Dval> =
    uply {
      let! name = ConstantName.UserProgram.toDT userConstant.name
      let! body = Const.toDT userConstant.body
      let! deprecated = Deprecation.toDT ConstantName.toDT userConstant.deprecated

      return!
        Dval.record
          (ptTyp [] "UserConstant" 0)
          (Some [])
          [ "tlid", DInt(int64 userConstant.tlid)
            "name", name
            "body", body
            "description", DString userConstant.description
            "deprecated", deprecated ]
    }

  let fromDT (d : Dval) : PT.UserConstant.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"

      let name = fields |> D.field "name" |> ConstantName.UserProgram.fromDT
      let body = fields |> D.field "body" |> Const.fromDT
      let description = fields |> D.stringField "description"
      let deprecated =
        fields |> D.field "deprecated" |> Deprecation.fromDT ConstantName.fromDT

      { tlid = tlid
        name = name
        body = body
        description = description
        deprecated = deprecated }

    | _ -> Exception.raiseInternal "Invalid UserConstant" []

module Secret =
  let toDT (s : PT.Secret.T) : Ply<Dval> =
    Dval.record
      (ptTyp [] "Secret" 0)
      (Some [])
      [ "name", DString s.name; "value", DString s.value; "version", DInt s.version ]

  let fromDT (d : Dval) : PT.Secret.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let name = fields |> D.stringField "name"
      let value = fields |> D.stringField "value"
      let version = fields |> D.intField "version"

      { name = name; value = value; version = version }

    | _ -> Exception.raiseInternal "Invalid Secret" []


module PackageType =
  let toDT (p : PT.PackageType.T) : Ply<Dval> =
    uply {
      let! name = TypeName.Package.toDT p.name
      let! declaration = TypeDeclaration.toDT p.declaration
      let! deprecated = Deprecation.toDT TypeName.toDT p.deprecated
      return!
        Dval.record
          (ptTyp [] "PackageType" 0)
          (Some [])
          [ "tlid", DInt(int64 p.tlid)
            "id", DUuid p.id
            "name", name
            "declaration", declaration
            "description", DString p.description
            "deprecated", deprecated ]
    }

  let fromDT (d : Dval) : PT.PackageType.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let id = fields |> D.uuidField "id"
      let name = fields |> D.field "name" |> TypeName.Package.fromDT
      let declaration = fields |> D.field "declaration" |> TypeDeclaration.fromDT
      let description = fields |> D.stringField "description"
      let deprecated =
        fields |> D.field "deprecated" |> Deprecation.fromDT TypeName.fromDT

      { tlid = tlid
        id = id
        name = name
        declaration = declaration
        description = description
        deprecated = deprecated }

    | _ -> Exception.raiseInternal "Invalid PackageType" []


module PackageFn =
  module Parameter =
    let toDT (p : PT.PackageFn.Parameter) : Ply<Dval> =
      uply {
        let! typ = TypeReference.toDT p.typ
        return!
          Dval.record
            (ptTyp [ "PackageFn" ] "Parameter" 0)
            (Some [])
            [ "name", DString p.name
              "typ", typ
              "description", DString p.description ]
      }

    let fromDT (d : Dval) : PT.PackageFn.Parameter =
      match d with
      | DRecord(_, _, _, fields) ->
        let name = fields |> D.stringField "name"
        let typ = fields |> D.field "typ" |> TypeReference.fromDT
        let description = fields |> D.stringField "description"

        { name = name; typ = typ; description = description }

      | _ -> Exception.raiseInternal "Invalid PackageFn.Parameter" []

  let toDT (p : PT.PackageFn.T) : Ply<Dval> =
    uply {
      let! name = FnName.Package.toDT p.name
      let! body = Expr.toDT p.body
      let! parameters =
        p.parameters
        |> NEList.toList
        |> Ply.List.mapSequentially Parameter.toDT
        |> Ply.map (Dval.list VT.unknownTODO)
      let! returnType = TypeReference.toDT p.returnType
      let! deprecated = Deprecation.toDT FnName.toDT p.deprecated
      return!
        Dval.record
          (ptTyp [ "PackageFn" ] "PackageFn" 0)
          (Some [])
          [ "tlid", DInt(int64 p.tlid)
            "id", DUuid p.id
            "name", name
            "body", body
            "typeParams", List.map DString p.typeParams |> Dval.list VT.unknownTODO
            "parameters", parameters
            "returnType", returnType
            "description", DString p.description
            "deprecated", deprecated ]
    }

  let fromDT (d : Dval) : PT.PackageFn.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let id = fields |> D.uuidField "id"
      let name = fields |> D.field "name" |> FnName.Package.fromDT
      let body = fields |> D.field "body" |> Expr.fromDT
      let typeParams = fields |> D.stringListField "typeParams"

      let parameters =
        fields
        |> D.listField "parameters"
        |> List.map Parameter.fromDT
        |> NEList.ofListUnsafe "PackageFn.fromDT" []
      let returnType = fields |> D.field "returnType" |> TypeReference.fromDT
      let description = fields |> D.stringField "description"
      let deprecated =
        fields |> D.field "deprecated" |> Deprecation.fromDT FnName.fromDT

      { tlid = tlid
        id = id
        name = name
        body = body
        typeParams = typeParams
        parameters = parameters
        returnType = returnType
        description = description
        deprecated = deprecated }

    | _ -> Exception.raiseInternal "Invalid PackageFn" []

module PackageConstant =
  let toDT (p : PT.PackageConstant.T) : Ply<Dval> =
    uply {
      let! name = ConstantName.Package.toDT p.name
      let! body = Const.toDT p.body
      let! deprecated = Deprecation.toDT ConstantName.toDT p.deprecated
      return!
        Dval.record
          (ptTyp [] "PackageConstant" 0)
          (Some [])
          [ "tlid", DInt(int64 p.tlid)
            "id", DUuid p.id
            "name", name
            "body", body
            "description", DString p.description
            "deprecated", deprecated ]
    }

  let fromDT (d : Dval) : PT.PackageConstant.T =
    match d with
    | DRecord(_, _, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let id = fields |> D.uuidField "id"
      let name = fields |> D.field "name" |> ConstantName.Package.fromDT
      let body = fields |> D.field "body" |> Const.fromDT
      let description = fields |> D.stringField "description"
      let deprecated =
        fields |> D.field "deprecated" |> Deprecation.fromDT ConstantName.fromDT

      { tlid = tlid
        id = id
        name = name
        body = body
        description = description
        deprecated = deprecated }

    | _ -> Exception.raiseInternal "Invalid PackageConstant" []
