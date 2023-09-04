module LibExecution.ProgramTypesToDarkTypes

open Prelude

open RuntimeTypes
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
    match s with
    | Positive -> Dval.enum (languageToolsTyp [] "Sign" 0) "Positive" []
    | Negative -> Dval.enum (languageToolsTyp [] "Sign" 0) "Negative" []

  let fromDT (d : Dval) : Sign =
    match d with
    //TODO: ensure that we are working with a right type
    | DEnum(_, _, "Positive", []) -> Positive
    | DEnum(_, _, "Negative", []) -> Negative
    | _ -> Exception.raiseInternal "Invalid sign" []



module FQName =
  let ownerField = D.stringField "owner"
  let modulesField = D.stringListField "modules"

  let nameField = D.field "name"
  let versionField = D.intField "version"

  module BuiltIn =
    let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.BuiltIn<'name>) : Dval =
      Dval.record
        (ptTyp [ "FQName" ] "BuiltIn" 0)
        [ "modules", DList(List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : PT.FQName.BuiltIn<'name> =
      match d with
      | DRecord(_, _, m) ->
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module UserProgram =
    let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.UserProgram<'name>) : Dval =
      Dval.record
        (ptTyp [ "FQName" ] "UserProgram" 0)
        [ "modules", DList(List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT
      (nameMapper : Dval -> 'name)
      (v : Dval)
      : PT.FQName.UserProgram<'name> =
      let unwrap = Exception.unwrapOptionInternal
      match v with
      | DRecord(_, _, m) ->

        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []

  module Package =
    let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.Package<'name>) : Dval =
      Dval.record
        (ptTyp [ "FQName" ] "Package" 0)
        [ "owner", DString u.owner
          "modules", DList(List.map DString u.modules)
          "name", nameMapper u.name
          "version", DInt u.version ]

    let fromDT (nameMapper : Dval -> 'name) (d : Dval) : PT.FQName.Package<'name> =
      match d with
      | DRecord(_, _, m) ->
        let owner = ownerField m
        let modules = modulesField m
        let name = nameField m |> nameMapper
        let version = versionField m

        { owner = owner; modules = modules; name = name; version = version }

      | _ -> Exception.raiseInternal "Unexpected value" []


  let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.FQName<'name>) : Dval =
    let caseName, fields =
      match u with
      | PT.FQName.UserProgram u -> "UserProgram", [ UserProgram.toDT nameMapper u ]
      | PT.FQName.Package u -> "Package", [ Package.toDT nameMapper u ]
      | PT.FQName.BuiltIn u -> "BuiltIn", [ BuiltIn.toDT nameMapper u ]

    Dval.enum (ptTyp [ "FQName" ] "FQName" 0) caseName fields

  let fromDT (nameMapper : Dval -> 'name) (d : Dval) : PT.FQName.FQName<'name> =
    match d with
    | DEnum(_, _, "UserProgram", [ u ]) ->
      PT.FQName.UserProgram(UserProgram.fromDT nameMapper u)
    | DEnum(_, _, "Package", [ u ]) -> PT.FQName.Package(Package.fromDT nameMapper u)
    | DEnum(_, _, "BuiltIn", [ u ]) -> PT.FQName.BuiltIn(BuiltIn.fromDT nameMapper u)
    | _ -> Exception.raiseInternal "Invalid FQName" []


module TypeName =
  module Name =
    let toDT (u : PT.TypeName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.TypeName.TypeName name -> "TypeName", [ DString name ]

      Dval.enum (ptTyp [ "TypeName" ] "Name" 0) caseName fields

    let fromDT (d : Dval) : PT.TypeName.Name =
      match d with
      | DEnum(_, _, "TypeName", [ DString name ]) -> PT.TypeName.TypeName(name)
      | _ -> Exception.raiseInternal "Invalid TypeName" []

  module BuiltIn =
    let toDT (u : PT.TypeName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u
    let fromDT (d : Dval) : PT.TypeName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : PT.TypeName.UserProgram) : Dval =
      FQName.UserProgram.toDT Name.toDT u

    let fromDT (d : Dval) : PT.TypeName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : PT.TypeName.Package) : Dval = FQName.Package.toDT Name.toDT u
    let fromDT (d : Dval) : PT.TypeName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : PT.TypeName.TypeName) : Dval = FQName.toDT Name.toDT u
  let fromDT (d : Dval) : PT.TypeName.TypeName = FQName.fromDT Name.fromDT d


module FnName =
  module Name =
    let toDT (u : PT.FnName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.FnName.FnName name -> "FnName", [ DString name ]

      Dval.enum (ptTyp [ "FnName" ] "Name" 0) caseName fields

    let fromDT (d : Dval) : PT.FnName.Name =
      match d with
      | DEnum(_, _, "FnName", [ DString name ]) -> PT.FnName.FnName(name)
      | _ -> Exception.raiseInternal "Invalid FnName" []

  module BuiltIn =
    let toDT (u : PT.FnName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u
    let fromDT (d : Dval) : PT.FnName.BuiltIn = FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : PT.FnName.UserProgram) : Dval = FQName.UserProgram.toDT Name.toDT u
    let fromDT (d : Dval) : PT.FnName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : PT.FnName.Package) : Dval = FQName.Package.toDT Name.toDT u
    let fromDT (d : Dval) : PT.FnName.Package = FQName.Package.fromDT Name.fromDT d

  let toDT (u : PT.FnName.FnName) : Dval = FQName.toDT Name.toDT u
  let fromDT (d : Dval) : PT.FnName.FnName = FQName.fromDT Name.fromDT d

module ConstantName =
  module Name =
    let toDT (u : PT.ConstantName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.ConstantName.ConstantName name -> "ConstantName", [ DString name ]

      Dval.enum (ptTyp [ "ConstantName" ] "Name" 0) caseName fields

    let fromDT (d : Dval) : PT.ConstantName.Name =
      match d with
      | DEnum(_, _, "ConstantName", [ DString name ]) ->
        PT.ConstantName.ConstantName(name)
      | _ -> Exception.raiseInternal "Invalid ConstantName" []

  module BuiltIn =
    let toDT (u : PT.ConstantName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u
    let fromDT (d : Dval) : PT.ConstantName.BuiltIn =
      FQName.BuiltIn.fromDT Name.fromDT d

  module UserProgram =
    let toDT (u : PT.ConstantName.UserProgram) : Dval =
      FQName.UserProgram.toDT Name.toDT u

    let fromDT (d : Dval) : PT.ConstantName.UserProgram =
      FQName.UserProgram.fromDT Name.fromDT d

  module Package =
    let toDT (u : PT.ConstantName.Package) : Dval = FQName.Package.toDT Name.toDT u
    let fromDT (d : Dval) : PT.ConstantName.Package =
      FQName.Package.fromDT Name.fromDT d

  let toDT (u : PT.ConstantName.ConstantName) : Dval = FQName.toDT Name.toDT u
  let fromDT (d : Dval) : PT.ConstantName.ConstantName = FQName.fromDT Name.fromDT d

module NameResolution =
  let toDT (f : 'p -> Dval) (result : PT.NameResolution<'p>) : Dval =
    match result with
    | Ok name -> Dval.resultOk (f name)
    | Error err ->
      Dval.resultError (
        err |> NRE.RTE.toRuntimeError |> RuntimeTypes.RuntimeError.toDT
      )

  let fromDT (f : Dval -> 'a) (d : Dval) : PT.NameResolution<'a> =
    match d with
    | DEnum(tn, _, "Ok", [ v ]) when tn = Dval.resultType -> Ok(f v)

    | DEnum(tn, _, "Error", [ v ]) when tn = Dval.resultType ->
      Error(NRE.RTE.fromRuntimeError (RuntimeError.fromDT v))

    | _ -> Exception.raiseInternal "Invalid NameResolution" []

module TypeReference =
  let rec toDT (t : PT.TypeReference) : Dval =
    let name, fields =
      match t with
      | PT.TVariable name -> "TVariable", [ DString name ]

      | PT.TUnit -> "TUnit", []
      | PT.TBool -> "TBool", []
      | PT.TInt -> "TInt", []
      | PT.TFloat -> "TFloat", []
      | PT.TChar -> "TChar", []
      | PT.TString -> "TString", []
      | PT.TDateTime -> "TDateTime", []
      | PT.TUuid -> "TUuid", []
      | PT.TBytes -> "TBytes", []
      | PT.TPassword -> "TPassword", []

      | PT.TList inner -> "TList", [ toDT inner ]

      | PT.TTuple(first, second, theRest) ->
        "TTuple", [ toDT first; toDT second; DList(List.map toDT theRest) ]

      | PT.TDict inner -> "TDict", [ toDT inner ]

      | PT.TCustomType(typeName, typeArgs) ->
        "TCustomType",
        [ NameResolution.toDT TypeName.toDT typeName; DList(List.map toDT typeArgs) ]

      | PT.TDB inner -> "TDB", [ toDT inner ]
      | PT.TFn(args, ret) ->
        "TFn", [ DList(args |> NEList.toList |> List.map toDT); toDT ret ]

    Dval.enum (ptTyp [] "TypeReference" 0) name fields

  let rec fromDT (d : Dval) : PT.TypeReference =
    match d with
    | DEnum(_, _, "TVariable", [ DString name ]) -> PT.TVariable(name)

    | DEnum(_, _, "TUnit", []) -> PT.TUnit
    | DEnum(_, _, "TBool", []) -> PT.TBool
    | DEnum(_, _, "TInt", []) -> PT.TInt
    | DEnum(_, _, "TFloat", []) -> PT.TFloat
    | DEnum(_, _, "TChar", []) -> PT.TChar
    | DEnum(_, _, "TString", []) -> PT.TString
    | DEnum(_, _, "TDateTime", []) -> PT.TDateTime
    | DEnum(_, _, "TUuid", []) -> PT.TUuid
    | DEnum(_, _, "TBytes", []) -> PT.TBytes
    | DEnum(_, _, "TPassword", []) -> PT.TPassword

    | DEnum(_, _, "TList", [ inner ]) -> PT.TList(fromDT inner)

    | DEnum(_, _, "TTuple", [ first; second; DList theRest ]) ->
      PT.TTuple(fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, "TDict", [ inner ]) -> PT.TDict(fromDT inner)

    | DEnum(_, _, "TCustomType", [ typeName; DList typeArgs ]) ->
      PT.TCustomType(
        NameResolution.fromDT TypeName.fromDT typeName,
        List.map fromDT typeArgs
      )

    | DEnum(_, _, "TDB", [ inner ]) -> PT.TDB(fromDT inner)
    | DEnum(_, _, "TFn", [ DList(head :: tail); ret ]) ->
      PT.TFn(NEList.ofList head tail |> NEList.map fromDT, fromDT ret)
    | _ -> Exception.raiseInternal "Invalid TypeReference" []


module LetPattern =
  let rec toDT (p : PT.LetPattern) : Dval =
    let name, fields =
      match p with
      | PT.LPVariable(id, name) -> "LPVariable", [ DInt(int64 id); DString name ]
      | PT.LPUnit id -> "LPUnit", [ DInt(int64 id) ]
      | PT.LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]

    Dval.enum (ptTyp [] "LetPattern" 0) name fields


  let rec fromDT (d : Dval) : PT.LetPattern =
    match d with
    | DEnum(_, _, "LPVariable", [ DInt id; DString name ]) ->
      PT.LPVariable(uint64 id, name)
    | DEnum(_, _, "LPUnit", [ DInt id ]) -> PT.LPUnit(uint64 id)
    | DEnum(_, _, "LPTuple", [ DInt id; first; second; DList theRest ]) ->
      PT.LPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | _ -> Exception.raiseInternal "Invalid LetPattern" []


module MatchPattern =
  let rec toDT (p : PT.MatchPattern) : Dval =
    let name, fields =
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
        "MPList", [ DInt(int64 id); DList(List.map toDT inner) ]
      | PT.MPListCons(id, head, tail) ->
        "MPListCons", [ DInt(int64 id); toDT head; toDT tail ]
      | PT.MPTuple(id, first, second, theRest) ->
        "MPTuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]
      | PT.MPEnum(id, caseName, fieldPats) ->
        "MPEnum",
        [ DInt(int64 id); DString caseName; DList(List.map toDT fieldPats) ]

    Dval.enum (ptTyp [] "MatchPattern" 0) name fields

  let rec fromDT (d : Dval) : PT.MatchPattern =
    match d with
    | DEnum(_, _, "MPVariable", [ DInt id; DString name ]) ->
      PT.MPVariable(uint64 id, name)

    | DEnum(_, _, "MPUnit", [ DInt id ]) -> PT.MPUnit(uint64 id)
    | DEnum(_, _, "MPBool", [ DInt id; DBool b ]) -> PT.MPBool(uint64 id, b)
    | DEnum(_, _, "MPInt", [ DInt id; DInt i ]) -> PT.MPInt(uint64 id, i)
    | DEnum(_, _, "MPFloat", [ DInt id; sign; DString whole; DString remainder ]) ->
      PT.MPFloat(uint64 id, Sign.fromDT sign, whole, remainder)
    | DEnum(_, _, "MPChar", [ DInt id; DString c ]) -> PT.MPChar(uint64 id, c)
    | DEnum(_, _, "MPString", [ DInt id; DString s ]) -> PT.MPString(uint64 id, s)

    | DEnum(_, _, "MPList", [ DInt id; DList inner ]) ->
      PT.MPList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, "MPListCons", [ DInt id; head; tail ]) ->
      PT.MPListCons(uint64 id, fromDT head, fromDT tail)
    | DEnum(_, _, "MPTuple", [ DInt id; first; second; DList theRest ]) ->
      PT.MPTuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)
    | DEnum(_, _, "MPEnum", [ DInt id; DString caseName; DList fieldPats ]) ->
      PT.MPEnum(uint64 id, caseName, List.map fromDT fieldPats)
    | _ -> Exception.raiseInternal "Invalid MatchPattern" []


module BinaryOperation =
  let toDT (b : PT.BinaryOperation) : Dval =
    match b with
    | PT.BinOpAnd -> Dval.enum (ptTyp [] "BinaryOperation" 0) "BinOpAnd" []
    | PT.BinOpOr -> Dval.enum (ptTyp [] "BinaryOperation" 0) "BinOpOr" []

  let fromDT (d : Dval) : PT.BinaryOperation =
    match d with
    | DEnum(_, _, "BinOpAnd", []) -> PT.BinOpAnd
    | DEnum(_, _, "BinOpOr", []) -> PT.BinOpOr
    | _ -> Exception.raiseInternal "Invalid BinaryOperation" []


module InfixFnName =
  let toDT (i : PT.InfixFnName) : Dval =
    let name, fields =
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

    Dval.enum (ptTyp [] "InfixFnName" 0) name fields

  let fromDT (d : Dval) : PT.InfixFnName =
    match d with
    | DEnum(_, _, "ArithmeticPlus", []) -> PT.ArithmeticPlus
    | DEnum(_, _, "ArithmeticMinus", []) -> PT.ArithmeticMinus
    | DEnum(_, _, "ArithmeticMultiply", []) -> PT.ArithmeticMultiply
    | DEnum(_, _, "ArithmeticDivide", []) -> PT.ArithmeticDivide
    | DEnum(_, _, "ArithmeticModulo", []) -> PT.ArithmeticModulo
    | DEnum(_, _, "ArithmeticPower", []) -> PT.ArithmeticPower
    | DEnum(_, _, "ComparisonGreaterThan", []) -> PT.ComparisonGreaterThan
    | DEnum(_, _, "ComparisonGreaterThanOrEqual", []) ->
      PT.ComparisonGreaterThanOrEqual
    | DEnum(_, _, "ComparisonLessThan", []) -> PT.ComparisonLessThan
    | DEnum(_, _, "ComparisonLessThanOrEqual", []) -> PT.ComparisonLessThanOrEqual
    | DEnum(_, _, "ComparisonEquals", []) -> PT.ComparisonEquals
    | DEnum(_, _, "ComparisonNotEquals", []) -> PT.ComparisonNotEquals
    | DEnum(_, _, "StringConcat", []) -> PT.StringConcat
    | _ -> Exception.raiseInternal "Invalid InfixFnName" []


module Infix =
  let toDT (i : PT.Infix) : Dval =
    let name, fields =
      match i with
      | PT.InfixFnCall infixFnName -> "InfixFnCall", [ InfixFnName.toDT infixFnName ]
      | PT.BinOp binOp -> "BinOp", [ BinaryOperation.toDT binOp ]

    Dval.enum (ptTyp [] "Infix" 0) name fields

  let fromDT (d : Dval) : PT.Infix =
    match d with
    | DEnum(_, _, "InfixFnCall", [ infixFnName ]) ->
      PT.InfixFnCall(InfixFnName.fromDT infixFnName)
    | DEnum(_, _, "BinOp", [ binOp ]) -> PT.BinOp(BinaryOperation.fromDT binOp)
    | _ -> Exception.raiseInternal "Invalid Infix" []


module StringSegment =
  let toDT (exprToDT : PT.Expr -> Dval) (s : PT.StringSegment) : Dval =
    let name, fields =
      match s with
      | PT.StringText text -> "StringText", [ DString text ]
      | PT.StringInterpolation expr -> "StringInterpolation", [ exprToDT expr ]

    Dval.enum (ptTyp [] "StringSegment" 0) name fields

  let fromDT (exprFromDT : Dval -> PT.Expr) (d : Dval) : PT.StringSegment =
    match d with
    | DEnum(_, _, "StringText", [ DString text ]) -> PT.StringText text
    | DEnum(_, _, "StringInterpolation", [ expr ]) ->
      PT.StringInterpolation(exprFromDT expr)
    | _ -> Exception.raiseInternal "Invalid StringSegment" []


module PipeExpr =
  let toDT (exprToDT : PT.Expr -> Dval) (s : PT.PipeExpr) : Dval =
    let name, fields =
      match s with
      | PT.EPipeVariable(id, varName, exprs) ->
        "EPipeVariable",
        [ DInt(int64 id); DString varName; DList(List.map exprToDT exprs) ]
      | PT.EPipeLambda(id, args, body) ->
        let variables =
          args
          |> NEList.toList
          |> List.map (fun (id, varName) ->
            DTuple(DInt(int64 id), DString varName, []))
          |> DList

        "EPipeLambda", [ DInt(int64 id); variables; exprToDT body ]


      | PT.EPipeInfix(id, infix, expr) ->
        "EPipeInfix", [ DInt(int64 id); Infix.toDT infix; exprToDT expr ]

      | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
        "EPipeFnCall",
        [ DInt(int64 id)
          NameResolution.toDT FnName.toDT fnName
          DList(List.map TypeReference.toDT typeArgs)
          DList(List.map exprToDT args) ]

      | PT.EPipeEnum(id, typeName, caseName, fields) ->
        "EPipeEnum",
        [ DInt(int64 id)
          NameResolution.toDT TypeName.toDT typeName
          DString caseName
          DList(List.map exprToDT fields) ]

    Dval.enum (ptTyp [] "PipeExpr" 0) name fields

  let fromDT (exprFromDT : Dval -> PT.Expr) (d : Dval) : PT.PipeExpr =
    match d with
    | DEnum(_, _, "EPipeVariable", [ DInt id; DString varName; DList args ]) ->
      PT.EPipeVariable(uint64 id, varName, args |> List.map exprFromDT)

    | DEnum(_, _, "EPipeLambda", [ DInt id; variables; body ]) ->
      let variables =
        match variables with
        | DList(head :: tail) ->
          NEList.ofList head tail
          |> NEList.map (function
            | DTuple(DInt id, DString varName, []) -> (uint64 id, varName)
            | _ -> Exception.raiseInternal "Invalid variable" [])
        | _ -> Exception.raiseInternal "Invalid variables" []

      PT.EPipeLambda(uint64 id, variables, exprFromDT body)

    | DEnum(_, _, "EPipeInfix", [ DInt id; infix; expr ]) ->
      PT.EPipeInfix(uint64 id, Infix.fromDT infix, exprFromDT expr)

    | DEnum(_, _, "EPipeFnCall", [ DInt id; fnName; DList typeArgs; DList args ]) ->
      PT.EPipeFnCall(
        uint64 id,
        NameResolution.fromDT FnName.fromDT fnName,
        List.map TypeReference.fromDT typeArgs,
        List.map exprFromDT args
      )

    | DEnum(_, _, "EPipeEnum", [ DInt id; typeName; DString caseName; DList fields ]) ->
      PT.EPipeEnum(
        uint64 id,
        NameResolution.fromDT TypeName.fromDT typeName,
        caseName,
        List.map exprFromDT fields
      )

    | _ -> Exception.raiseInternal "Invalid PipeExpr" []


module Expr =
  let rec toDT (e : PT.Expr) : Dval =
    let name, fields =
      match e with
      | PT.EUnit id -> "EUnit", [ DInt(int64 id) ]

      // simple data
      | PT.EBool(id, b) -> "EBool", [ DInt(int64 id); DBool b ]
      | PT.EInt(id, i) -> "EInt", [ DInt(int64 id); DInt i ]
      | PT.EFloat(id, sign, whole, remainder) ->
        "EFloat",
        [ DInt(int64 id); Sign.toDT sign; DString whole; DString remainder ]
      | PT.EChar(id, c) -> "EChar", [ DInt(int64 id); DString c ]
      | PT.EString(id, segments) ->
        "EString",
        [ DInt(int64 id); DList(List.map (StringSegment.toDT toDT) segments) ]

      // structures of data
      | PT.EList(id, inner) ->
        "EList", [ DInt(int64 id); DList(List.map toDT inner) ]

      | PT.EDict(id, pairs) ->
        "EDict",
        [ DInt(int64 id)
          DList(List.map (fun (k, v) -> DTuple(DString k, toDT v, [])) pairs) ]

      | PT.ETuple(id, first, second, theRest) ->
        "ETuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]

      | PT.ERecord(id, name, fields) ->
        let fields =
          fields
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))

        "ERecord",
        [ DInt(int64 id); NameResolution.toDT TypeName.toDT name; DList(fields) ]

      | PT.EEnum(id, typeName, caseName, fields) ->
        "EEnum",
        [ DInt(int64 id)
          NameResolution.toDT TypeName.toDT typeName
          DString caseName
          DList(List.map toDT fields) ]

      // declaring and accessing variables
      | PT.ELet(id, lp, expr, body) ->
        "ELet", [ DInt(int64 id); LetPattern.toDT lp; toDT expr; toDT body ]

      | PT.EFieldAccess(id, expr, fieldName) ->
        "EFieldAccess", [ DInt(int64 id); toDT expr; DString fieldName ]

      | PT.EVariable(id, varName) -> "EVariable", [ DInt(int64 id); DString varName ]


      // control flow
      | PT.EIf(id, cond, thenExpr, elseExpr) ->
        let elseExpr = elseExpr |> Option.map toDT |> Dval.option
        "EIf", [ DInt(int64 id); toDT cond; toDT thenExpr; elseExpr ]

      | PT.EMatch(id, arg, cases) ->
        let cases =
          cases
          |> List.map (fun (pattern, expr) ->
            DTuple(MatchPattern.toDT pattern, toDT expr, []))

        "EMatch", [ DInt(int64 id); toDT arg; DList(cases) ]

      | PT.EPipe(id, expr, pipeExprs) ->
        "EPipe",
        [ DInt(int64 id); toDT expr; DList(List.map (PipeExpr.toDT toDT) pipeExprs) ]


      // function calls
      | PT.EInfix(id, infix, lhs, rhs) ->
        "EInfix", [ DInt(int64 id); Infix.toDT infix; toDT lhs; toDT rhs ]

      | PT.ELambda(id, args, body) ->
        let variables =
          args
          |> NEList.toList
          |> List.map (fun (id, varName) ->
            DTuple(DInt(int64 id), DString varName, []))
          |> DList

        "ELambda", [ DInt(int64 id); variables; toDT body ]

      | PT.EConstant(id, name) ->
        "EConstant", [ DInt(int64 id); NameResolution.toDT ConstantName.toDT name ]

      | PT.EApply(id, name, typeArgs, args) ->
        "EApply",
        [ DInt(int64 id)
          toDT name
          DList(List.map TypeReference.toDT typeArgs)
          DList(args |> NEList.toList |> List.map toDT) ]

      | PT.EFnName(id, name) ->
        "EFnName", [ DInt(int64 id); NameResolution.toDT FnName.toDT name ]

      | PT.ERecordUpdate(id, record, updates) ->
        let updates =
          updates
          |> NEList.toList
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))

        "ERecordUpdate", [ DInt(int64 id); toDT record; DList(updates) ]

    Dval.enum (ptTyp [] "Expr" 0) name fields

  let rec fromDT (d : Dval) : PT.Expr =
    match d with
    | DEnum(_, _, "EUnit", [ DInt id ]) -> PT.EUnit(uint64 id)

    // simple data
    | DEnum(_, _, "EBool", [ DInt id; DBool b ]) -> PT.EBool(uint64 id, b)
    | DEnum(_, _, "EInt", [ DInt id; DInt i ]) -> PT.EInt(uint64 id, i)
    | DEnum(_, _, "EFloat", [ DInt id; sign; DString whole; DString remainder ]) ->
      PT.EFloat(uint64 id, Sign.fromDT sign, whole, remainder)
    | DEnum(_, _, "EChar", [ DInt id; DString c ]) -> PT.EChar(uint64 id, c)
    | DEnum(_, _, "EString", [ DInt id; DList segments ]) ->
      PT.EString(uint64 id, List.map (StringSegment.fromDT fromDT) segments)


    // structures of data
    | DEnum(_, _, "EList", [ DInt id; DList inner ]) ->
      PT.EList(uint64 id, List.map fromDT inner)
    | DEnum(_, _, "EDict", [ DInt id; DList pairsList ]) ->
      let pairs =
        pairsList
        |> List.collect (fun pair ->
          match pair with
          | DTuple(DString k, v, _) -> [ (k, fromDT v) ]
          | _ -> [])
      PT.EDict(uint64 id, pairs)


    | DEnum(_, _, "ETuple", [ DInt id; first; second; DList theRest ]) ->
      PT.ETuple(uint64 id, fromDT first, fromDT second, List.map fromDT theRest)

    | DEnum(_, _, "ERecord", [ DInt id; typeName; DList fieldsList ]) ->
      let fields =
        fieldsList
        |> List.collect (fun field ->
          match field with
          | DTuple(DString name, expr, _) -> [ (name, fromDT expr) ]
          | _ -> [])
      PT.ERecord(uint64 id, NameResolution.fromDT TypeName.fromDT typeName, fields)


    | DEnum(_, _, "EEnum", [ DInt id; typeName; DString caseName; DList fields ]) ->
      PT.EEnum(
        uint64 id,
        NameResolution.fromDT TypeName.fromDT typeName,
        caseName,
        List.map fromDT fields
      )

    // declaring and accessing variables
    | DEnum(_, _, "ELet", [ DInt id; lp; expr; body ]) ->
      PT.ELet(uint64 id, LetPattern.fromDT lp, fromDT expr, fromDT body)

    | DEnum(_, _, "EFieldAccess", [ DInt id; expr; DString fieldName ]) ->
      PT.EFieldAccess(uint64 id, fromDT expr, fieldName)

    | DEnum(_, _, "EVariable", [ DInt id; DString varName ]) ->
      PT.EVariable(uint64 id, varName)

    // control flow
    | DEnum(_, _, "EIf", [ DInt id; cond; thenExpr; elseExpr ]) ->
      let elseExpr =
        match elseExpr with
        | DEnum(_, _, "Some", [ dv ]) -> Some(fromDT dv)
        | DEnum(_, _, "None", []) -> None
        | _ ->
          Exception.raiseInternal "Invalid else expression" [ "elseExpr", elseExpr ]
      PT.EIf(uint64 id, fromDT cond, fromDT thenExpr, elseExpr)

    | DEnum(_, _, "EMatch", [ DInt id; arg; DList cases ]) ->
      let cases =
        cases
        |> List.collect (fun case ->
          match case with
          | DTuple(pattern, expr, _) ->
            [ (MatchPattern.fromDT pattern, fromDT expr) ]
          | _ -> [])
      PT.EMatch(uint64 id, fromDT arg, cases)

    | DEnum(_, _, "EPipe", [ DInt id; expr; DList pipeExprs ]) ->
      PT.EPipe(uint64 id, fromDT expr, List.map (PipeExpr.fromDT fromDT) pipeExprs)

    // function calls
    | DEnum(_, _, "EInfix", [ DInt id; infix; lhs; rhs ]) ->
      PT.EInfix(uint64 id, Infix.fromDT infix, fromDT lhs, fromDT rhs)

    | DEnum(_, _, "ELambda", [ DInt id; DList(head :: tail); body ]) ->
      let args =
        NEList.ofList head tail
        |> NEList.map (fun arg ->
          match arg with
          | DTuple(DInt argId, DString varName, _) -> (uint64 argId, varName)
          | _ -> Exception.raiseInternal "Invalid lambda arg" [ "arg", arg ])
      PT.ELambda(uint64 id, args, fromDT body)


    | DEnum(_, _, "EApply", [ DInt id; name; DList typeArgs; DList args ]) ->
      PT.EApply(
        uint64 id,
        fromDT name,
        List.map TypeReference.fromDT typeArgs,
        args |> NEList.ofListUnsafe "EApply" [] |> NEList.map fromDT
      )

    | DEnum(_, _, "EFnName", [ DInt id; name ]) ->
      PT.EFnName(uint64 id, NameResolution.fromDT FnName.fromDT name)

    | DEnum(_, _, "ERecordUpdate", [ DInt id; record; DList(head :: tail) ]) ->
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
  let rec toDT (c : PT.Const) : Dval =
    let name, fields =
      match c with
      | PT.Const.CInt i -> "CInt", [ DInt i ]
      | PT.Const.CBool b -> "CBool", [ DBool b ]
      | PT.Const.CString s -> "CString", [ DString s ]
      | PT.Const.CChar c -> "CChar", [ DChar c ]
      | PT.Const.CFloat(sign, w, f) ->
        "CFloat", [ Sign.toDT sign; DString w; DString f ]
      | PT.Const.CUnit -> "CUnit", []
      | PT.Const.CTuple(first, second, rest) ->
        "CTuple", [ toDT first; toDT second; DList(List.map toDT rest) ]
      | PT.Const.CEnum(typeName, caseName, fields) ->
        "CEnum",
        [ NameResolution.toDT TypeName.toDT typeName
          DString caseName
          DList(List.map toDT fields) ]
    Dval.enum (ptTyp [] "Const" 0) name fields

  let rec fromDT (d : Dval) : PT.Const =
    match d with
    | DEnum(_, _, "CInt", [ DInt i ]) -> PT.Const.CInt i
    | DEnum(_, _, "CBool", [ DBool b ]) -> PT.Const.CBool b
    | DEnum(_, _, "CString", [ DString s ]) -> PT.Const.CString s
    | DEnum(_, _, "CChar", [ DChar c ]) -> PT.Const.CChar c
    | DEnum(_, _, "CFloat", [ sign; DString w; DString f ]) ->
      PT.Const.CFloat(Sign.fromDT sign, w, f)
    | DEnum(_, _, "CUnit", []) -> PT.Const.CUnit
    | DEnum(_, _, "CTuple", [ first; second; DList rest ]) ->
      PT.Const.CTuple(fromDT first, fromDT second, List.map fromDT rest)
    | DEnum(_, _, "CEnum", [ typeName; DString caseName; DList fields ]) ->
      PT.Const.CEnum(
        NameResolution.fromDT TypeName.fromDT typeName,
        caseName,
        List.map fromDT fields
      )
    | _ -> Exception.raiseInternal "Invalid Const" []

module Deprecation =
  let toDT (inner : 'a -> Dval) (d : PT.Deprecation<'a>) : Dval =
    let (caseName, fields) =
      match d with
      | PT.Deprecation.NotDeprecated -> "NotDeprecated", []
      | PT.Deprecation.RenamedTo replacement -> "RenamedTo", [ inner replacement ]
      | PT.Deprecation.ReplacedBy replacement -> "ReplacedBy", [ inner replacement ]
      | PT.Deprecation.DeprecatedBecause reason ->
        "DeprecatedBecause", [ DString reason ]

    Dval.enum (ptTyp [] "Deprecation" 0) caseName fields

  let fromDT (inner : Dval -> 'a) (d : Dval) : PT.Deprecation<'a> =
    match d with
    | DEnum(_, _, "NotDeprecated", []) -> PT.Deprecation.NotDeprecated
    | DEnum(_, _, "RenamedTo", [ replacement ]) ->
      PT.Deprecation.RenamedTo(inner replacement)
    | DEnum(_, _, "ReplacedBy", [ replacement ]) ->
      PT.Deprecation.ReplacedBy(inner replacement)
    | DEnum(_, _, "DeprecatedBecause", [ DString reason ]) ->
      PT.Deprecation.DeprecatedBecause(reason)
    | _ -> Exception.raiseInternal "Invalid Deprecation" []


module TypeDeclaration =
  module RecordField =
    let toDT (rf : PT.TypeDeclaration.RecordField) : Dval =
      Dval.record
        (ptTyp [ "TypeDeclaration" ] "RecordField" 0)
        [ "name", DString rf.name
          "typ", TypeReference.toDT rf.typ
          "description", DString rf.description ]

    let fromDT (d : Dval) : PT.TypeDeclaration.RecordField =
      match d with
      | DRecord(_, _, fields) ->
        let name = fields |> D.stringField "name"
        let typ = fields |> D.field "typ" |> TypeReference.fromDT
        let description = D.stringField "description" fields

        { name = name; typ = typ; description = description }

      | _ -> Exception.raiseInternal "Invalid RecordField" []

  module EnumField =
    let toDT (ef : PT.TypeDeclaration.EnumField) : Dval =
      Dval.record
        (ptTyp [ "TypeDeclaration" ] "EnumField" 0)
        [ "typ", TypeReference.toDT ef.typ
          "label", ef.label |> Option.map DString |> Dval.option
          "description", DString ef.description ]

    let fromDT (d : Dval) : PT.TypeDeclaration.EnumField =
      match d with
      | DRecord(_, _, fields) ->
        let typ = fields |> D.field "typ" |> TypeReference.fromDT

        let label =
          match Map.get "label" fields with
          | Some(DEnum(_, _, "Some", [ DString label ])) -> Some label
          | Some(DEnum(_, _, "None", [])) -> None
          | _ ->
            Exception.raiseInternal "Expected label to be an option of string" []

        let description = fields |> D.stringField "description"

        { typ = typ; label = label; description = description }

      | _ -> Exception.raiseInternal "Invalid EnumField" []


  module EnumCase =
    let toDT (ec : PT.TypeDeclaration.EnumCase) : Dval =
      Dval.record
        (ptTyp [ "TypeDeclaration" ] "EnumCase" 0)
        [ "name", DString ec.name
          "fields", DList(List.map EnumField.toDT ec.fields)
          "description", DString ec.description ]

    let fromDT (d : Dval) : PT.TypeDeclaration.EnumCase =
      match d with
      | DRecord(_, _, attributes) ->
        let name = D.stringField "name" attributes
        let fields = attributes |> D.listField "fields" |> List.map EnumField.fromDT
        let description = attributes |> D.stringField "description"

        { name = name; fields = fields; description = description }

      | _ -> Exception.raiseInternal "Invalid EnumCase" []


  module Definition =
    let toDT (d : PT.TypeDeclaration.Definition) : Dval =
      let caseName, fields =
        match d with
        | PT.TypeDeclaration.Alias typeRef -> "Alias", [ TypeReference.toDT typeRef ]

        | PT.TypeDeclaration.Record fields ->
          "Record", [ fields |> NEList.toList |> List.map RecordField.toDT |> DList ]

        | PT.TypeDeclaration.Enum cases ->
          "Enum", [ cases |> NEList.toList |> List.map EnumCase.toDT |> DList ]

      Dval.enum (ptTyp [ "TypeDeclaration" ] "Definition" 0) caseName fields

    let fromDT (d : Dval) : PT.TypeDeclaration.Definition =
      match d with
      | DEnum(_, _, "Alias", [ typeRef ]) ->
        PT.TypeDeclaration.Alias(TypeReference.fromDT typeRef)

      | DEnum(_, _, "Record", [ DList(firstField :: additionalFields) ]) ->
        let fields = NEList.ofList firstField additionalFields
        PT.TypeDeclaration.Record(NEList.map RecordField.fromDT fields)

      | DEnum(_, _, "Enum", [ DList(firstCase :: additionalCases) ]) ->
        let cases = NEList.ofList firstCase additionalCases
        PT.TypeDeclaration.Enum(NEList.map EnumCase.fromDT cases)

      | _ -> Exception.raiseInternal "Invalid TypeDeclaration.Definition" []


  let toDT (td : PT.TypeDeclaration.T) : Dval =
    Dval.record
      (ptTyp [ "TypeDeclaration" ] "TypeDeclaration" 0)
      [ "typeParams", DList(List.map DString td.typeParams)
        "definition", Definition.toDT td.definition ]

  let fromDT (d : Dval) : PT.TypeDeclaration.T =
    match d with
    | DRecord(_, _, fields) ->
      let typeParams = D.stringListField "typeParams" fields
      let definition = fields |> D.field "definition" |> Definition.fromDT

      { typeParams = typeParams; definition = definition }

    | _ -> Exception.raiseInternal "Invalid TypeDeclaration" []


module Handler =
  module CronInterval =
    let toDT (ci : PT.Handler.CronInterval) : Dval =
      let name, fields =
        match ci with
        | PT.Handler.CronInterval.EveryMinute -> "EveryMinute", []
        | PT.Handler.CronInterval.EveryHour -> "EveryHour", []
        | PT.Handler.CronInterval.Every12Hours -> "Every12Hours", []
        | PT.Handler.CronInterval.EveryDay -> "EveryDay", []
        | PT.Handler.CronInterval.EveryWeek -> "EveryWeek", []
        | PT.Handler.CronInterval.EveryFortnight -> "EveryFortnight", []

      Dval.enum (ptTyp [ "Handler" ] "CronInterval" 0) name fields

    let fromDT (d : Dval) : PT.Handler.CronInterval =
      match d with
      | DEnum(_, _, "EveryMinute", []) -> PT.Handler.CronInterval.EveryMinute
      | DEnum(_, _, "EveryHour", []) -> PT.Handler.CronInterval.EveryHour
      | DEnum(_, _, "Every12Hours", []) -> PT.Handler.CronInterval.Every12Hours
      | DEnum(_, _, "EveryDay", []) -> PT.Handler.CronInterval.EveryDay
      | DEnum(_, _, "EveryWeek", []) -> PT.Handler.CronInterval.EveryWeek
      | DEnum(_, _, "EveryFortnight", []) -> PT.Handler.CronInterval.EveryFortnight
      | _ -> Exception.raiseInternal "Invalid CronInterval" []


  module Spec =
    let toDT (s : PT.Handler.Spec) : Dval =
      let name, fields =
        match s with
        | PT.Handler.Spec.HTTP(route, method) ->
          "HTTP", [ DString route; DString method ]
        | PT.Handler.Spec.Worker name -> "Worker", [ DString name ]
        | PT.Handler.Spec.Cron(name, interval) ->
          "Cron", [ DString name; CronInterval.toDT interval ]
        | PT.Handler.Spec.REPL name -> "REPL", [ DString name ]

      Dval.enum (ptTyp [ "Handler" ] "Spec" 0) name fields

    let fromDT (d : Dval) : PT.Handler.Spec =
      match d with
      | DEnum(_, _, "HTTP", [ DString route; DString method ]) ->
        PT.Handler.Spec.HTTP(route, method)
      | DEnum(_, _, "Worker", [ DString name ]) -> PT.Handler.Spec.Worker(name)
      | DEnum(_, _, "Cron", [ DString name; interval ]) ->
        PT.Handler.Spec.Cron(name, CronInterval.fromDT interval)
      | DEnum(_, _, "REPL", [ DString name ]) -> PT.Handler.Spec.REPL(name)
      | _ -> Exception.raiseInternal "Invalid Spec" []

  let toDT (h : PT.Handler.T) : Dval =
    Dval.record
      (ptTyp [ "Handler" ] "Handler" 0)
      [ "tlid", DInt(int64 h.tlid)
        "ast", Expr.toDT h.ast
        "spec", Spec.toDT h.spec ]

  let fromDT (d : Dval) : PT.Handler.T =
    match d with
    | DRecord(_, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let ast = fields |> D.field "ast" |> Expr.fromDT
      let spec = fields |> D.field "spec" |> Spec.fromDT

      { tlid = tlid; ast = ast; spec = spec }

    | _ -> Exception.raiseInternal "Invalid Handler" []


module DB =
  let toDT (db : PT.DB.T) : Dval =
    Dval.record
      (ptTyp [] "DB" 0)
      [ "tlid", DInt(int64 db.tlid)
        "name", DString db.name
        "version", DInt db.version
        "typ", TypeReference.toDT db.typ ]

  let fromDT (d : Dval) : PT.DB.T =
    match d with
    | DRecord(_, _, fields) ->
      let tlid = fields |> D.uint64Field "tlid"
      let name = fields |> D.stringField "name"
      let version = fields |> D.intField "version"
      let typ = fields |> D.field "typ" |> TypeReference.fromDT
      { tlid = tlid; name = name; version = version; typ = typ }

    | _ -> Exception.raiseInternal "Invalid DB" []


module UserType =
  let toDT (userType : PT.UserType.T) : Dval =
    Dval.record
      (ptTyp [] "UserType" 0)
      [ "tlid", DInt(int64 userType.tlid)
        "name", TypeName.UserProgram.toDT userType.name
        "description", DString userType.description
        "declaration", TypeDeclaration.toDT userType.declaration
        "deprecated", Deprecation.toDT TypeName.toDT userType.deprecated ]

  let fromDT (d : Dval) : PT.UserType.T =
    match d with
    | DRecord(_, _, fields) ->
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
    let toDT (p : PT.UserFunction.Parameter) : Dval =
      Dval.record
        (ptTyp [ "UserFunction" ] "Parameter" 0)
        [ "name", DString p.name
          "typ", TypeReference.toDT p.typ
          "description", DString p.description ]

    let fromDT (d : Dval) : PT.UserFunction.Parameter =
      match d with
      | DRecord(_, _, fields) ->
        let name = fields |> D.stringField "name"
        let typ = fields |> D.field "typ" |> TypeReference.fromDT
        let description = fields |> D.stringField "description"

        { name = name; typ = typ; description = description }

      | _ -> Exception.raiseInternal "Invalid UserFunction.Parameter" []


  let toDT (userFn : PT.UserFunction.T) : Dval =
    Dval.record
      (ptTyp [ "UserFunction" ] "UserFunction" 0)
      [ "tlid", DInt(int64 userFn.tlid)
        "name", FnName.UserProgram.toDT userFn.name
        "typeParams", DList(List.map DString userFn.typeParams)
        "parameters",
        DList(userFn.parameters |> NEList.toList |> List.map Parameter.toDT)
        "returnType", TypeReference.toDT userFn.returnType
        "body", Expr.toDT userFn.body
        "description", DString userFn.description
        "deprecated", Deprecation.toDT FnName.toDT userFn.deprecated ]

  let fromDT (d : Dval) : PT.UserFunction.T =
    match d with
    | DRecord(_, _, fields) ->
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
  let toDT (userConstant : PT.UserConstant.T) : Dval =
    Dval.record
      (ptTyp [] "UserConstant" 0)
      [ "tlid", DInt(int64 userConstant.tlid)
        "name", ConstantName.UserProgram.toDT userConstant.name
        "body", Const.toDT userConstant.body
        "description", DString userConstant.description
        "deprecated", Deprecation.toDT ConstantName.toDT userConstant.deprecated ]

  let fromDT (d : Dval) : PT.UserConstant.T =
    match d with
    | DRecord(_, _, fields) ->
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
  let toDT (s : PT.Secret.T) : Dval =
    Dval.record
      (ptTyp [] "Secret" 0)
      [ "name", DString s.name; "value", DString s.value; "version", DInt s.version ]

  let fromDT (d : Dval) : PT.Secret.T =
    match d with
    | DRecord(_, _, fields) ->
      let name = fields |> D.stringField "name"
      let value = fields |> D.stringField "value"
      let version = fields |> D.intField "version"

      { name = name; value = value; version = version }

    | _ -> Exception.raiseInternal "Invalid Secret" []


module PackageType =
  let toDT (p : PT.PackageType.T) : Dval =
    Dval.record
      (ptTyp [] "PackageType" 0)
      [ "tlid", DInt(int64 p.tlid)
        "id", DUuid p.id
        "name", TypeName.Package.toDT p.name
        "declaration", TypeDeclaration.toDT p.declaration
        "description", DString p.description
        "deprecated", Deprecation.toDT TypeName.toDT p.deprecated ]

  let fromDT (d : Dval) : PT.PackageType.T =
    match d with
    | DRecord(_, _, fields) ->
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
    let toDT (p : PT.PackageFn.Parameter) : Dval =
      Dval.record
        (ptTyp [ "PackageFn" ] "Parameter" 0)
        [ "name", DString p.name
          "typ", TypeReference.toDT p.typ
          "description", DString p.description ]

    let fromDT (d : Dval) : PT.PackageFn.Parameter =
      match d with
      | DRecord(_, _, fields) ->
        let name = fields |> D.stringField "name"
        let typ = fields |> D.field "typ" |> TypeReference.fromDT
        let description = fields |> D.stringField "description"

        { name = name; typ = typ; description = description }

      | _ -> Exception.raiseInternal "Invalid PackageFn.Parameter" []

  let toDT (p : PT.PackageFn.T) : Dval =
    Dval.record
      (ptTyp [ "PackageFn" ] "PackageFn" 0)
      [ "tlid", DInt(int64 p.tlid)
        "id", DUuid p.id
        "name", FnName.Package.toDT p.name
        "body", Expr.toDT p.body
        "typeParams", DList(List.map DString p.typeParams)
        "parameters", DList(p.parameters |> NEList.toList |> List.map Parameter.toDT)
        "returnType", TypeReference.toDT p.returnType
        "description", DString p.description
        "deprecated", Deprecation.toDT FnName.toDT p.deprecated ]

  let fromDT (d : Dval) : PT.PackageFn.T =
    match d with
    | DRecord(_, _, fields) ->
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
  let toDT (p : PT.PackageConstant.T) : Dval =
    Dval.record
      (ptTyp [] "PackageConstant" 0)
      [ "tlid", DInt(int64 p.tlid)
        "id", DUuid p.id
        "name", ConstantName.Package.toDT p.name
        "body", Const.toDT p.body
        "description", DString p.description
        "deprecated", Deprecation.toDT ConstantName.toDT p.deprecated ]

  let fromDT (d : Dval) : PT.PackageConstant.T =
    match d with
    | DRecord(_, _, fields) ->
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
