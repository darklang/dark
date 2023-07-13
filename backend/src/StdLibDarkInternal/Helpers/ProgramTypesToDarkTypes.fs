module ProgramTypes2DarkTypes

open Prelude

open LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
open LibExecution.StdLib.Shortcuts

let stdlibTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.T =
  TypeName.fqPackage
    "Darklang"
    (NonEmptyList.ofList ([ "Stdlib" ] @ submodules))
    name
    version

let ptTyp (submodules : List<string>) (name : string) (version : int) : TypeName.T =
  stdlibTyp ("ProgramTypes" :: submodules) name version


// This isn't in PT but I'm not sure where else to put it...
// maybe rename this file to InternalTypesToDarkTypes?
module Sign =
  let toDT (s : Sign) : Dval =
    match s with
    | Positive -> DEnum(stdlibTyp [] "Sign" 0, "Positive", [])
    | Negative -> DEnum(stdlibTyp [] "Sign" 0, "Negative", [])

module FQName =
  module BuiltIn =
    let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.BuiltIn<'name>) : Dval =
      DRecord(
        ptTyp [ "FQName" ] "BuiltIn" 0,
        Map
          [ "modules", DList(List.map DString u.modules)
            "name", nameMapper u.name
            "version", DInt u.version ]
      )

  module UserProgram =
    let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.UserProgram<'name>) : Dval =
      DRecord(
        ptTyp [ "FQName" ] "UserProgram" 0,
        Map
          [ "modules", DList(List.map DString u.modules)
            "name", nameMapper u.name
            "version", DInt u.version ]
      )

  module Package =
    let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.Package<'name>) : Dval =
      DRecord(
        ptTyp [ "FQName" ] "Package" 0,
        Map
          [ "owner", DString u.owner
            "modules", DList(List.map DString (NonEmptyList.toList u.modules)) // CLEANUP source is a NonEmptyList
            "name", nameMapper u.name
            "version", DInt u.version ]
      )

  let toDT (nameMapper : 'name -> Dval) (u : PT.FQName.T<'name>) : Dval =
    let caseName, fields =
      match u with
      | PT.FQName.UserProgram u -> "UserProgram", [ UserProgram.toDT nameMapper u ]
      | PT.FQName.Package u -> "Package", [ Package.toDT nameMapper u ]
      | PT.FQName.BuiltIn u -> "BuiltIn", [ BuiltIn.toDT nameMapper u ]

    DEnum(ptTyp [ "FQName" ] "T" 0, caseName, fields)

module TypeName =
  module Name =
    let toDT (u : PT.TypeName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.TypeName.TypeName name -> "TypeName", [ DString name ]

      DEnum(ptTyp [ "TypeName" ] "Name" 0, caseName, fields)

  module BuiltIn =
    let toDT (u : PT.TypeName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u

  module UserProgram =
    let toDT (u : PT.TypeName.UserProgram) : Dval =
      FQName.UserProgram.toDT Name.toDT u

  module Package =
    let toDT (u : PT.TypeName.Package) : Dval = FQName.Package.toDT Name.toDT u

  let toDT (u : PT.TypeName.T) : Dval = FQName.toDT Name.toDT u


module FnName =
  module Name =
    let toDT (u : PT.FnName.Name) : Dval =
      let caseName, fields =
        match u with
        | PT.FnName.FnName name -> "FnName", [ DString name ]

      DEnum(ptTyp [ "FnName" ] "Name" 0, caseName, fields)

  module BuiltIn =
    let toDT (u : PT.FnName.BuiltIn) : Dval = FQName.BuiltIn.toDT Name.toDT u

  module UserProgram =
    let toDT (u : PT.FnName.UserProgram) : Dval = FQName.UserProgram.toDT Name.toDT u

  module Package =
    let toDT (u : PT.FnName.Package) : Dval = FQName.Package.toDT Name.toDT u

  let toDT (u : PT.FnName.T) : Dval = FQName.toDT Name.toDT u

module NameResolutionError =
  let toDT (u : LibExecution.Errors.NameResolutionError) : Dval =
    let caseName, fields =
      match u with
      | LibExecution.Errors.NotFound names ->
        "NotFound", [ DList(List.map DString names) ]
      | LibExecution.Errors.MissingModuleName names ->
        "MissingModuleName", [ DList(List.map DString names) ]
      | LibExecution.Errors.InvalidPackageName names ->
        "InvalidPackageName", [ DList(List.map DString names) ]

    DEnum(ptTyp [ "NameResolutionError" ] "NameResolutionError" 0, caseName, fields)

module NameResolution =
  let toDT (f : 'a -> Dval) (u : PT.NameResolution<'a>) : Dval =
    match u with
    | Ok n -> Dval.resultOk (f n)
    | Error e -> Dval.resultError (NameResolutionError.toDT e)

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
        "TCustomType", [ TypeName.toDT typeName; DList(List.map toDT typeArgs) ]

      | PT.TDB inner -> "TDB", [ toDT inner ]
      | PT.TFn(args, ret) -> "TFn", [ DList(List.map toDT args); toDT ret ]

    DEnum(ptTyp [] "TypeReference" 0, name, fields)

module LetPattern =
  let rec toDT (p : PT.LetPattern) : Dval =
    let name, fields =
      match p with
      | PT.LPVariable(id, name) -> "LPVariable", [ DInt(int64 id); DString name ]
      | PT.LPTuple(id, first, second, theRest) ->
        "LPTuple",
        [ DInt(int64 id); toDT first; toDT second; DList(List.map toDT theRest) ]

    DEnum(ptTyp [] "LetPattern" 0, name, fields)

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

    DEnum(ptTyp [] "MatchPattern" 0, name, fields)

module BinaryOperation =
  let toDT (b : PT.BinaryOperation) : Dval =
    match b with
    | PT.BinOpAnd -> DEnum(ptTyp [] "BinaryOperation" 0, "BinOpAnd", [])
    | PT.BinOpOr -> DEnum(ptTyp [] "BinaryOperation" 0, "BinOpOr", [])

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

    DEnum(ptTyp [] "InfixFnName" 0, name, fields)

module Infix =
  let toDT (i : PT.Infix) : Dval =
    let name, fields =
      match i with
      | PT.InfixFnCall infixFnName -> "InfixFnCall", [ InfixFnName.toDT infixFnName ]
      | PT.BinOp binOp -> "BinOp", [ BinaryOperation.toDT binOp ]

    DEnum(ptTyp [] "Infix" 0, name, fields)


module StringSegment =
  let toDT (exprToDT : PT.Expr -> Dval) (s : PT.StringSegment) : Dval =
    let name, fields =
      match s with
      | PT.StringText text -> "StringText", [ DString text ]
      | PT.StringInterpolation expr -> "StringInterpolation", [ exprToDT expr ]

    DEnum(ptTyp [] "StringSegment" 0, name, fields)


module PipeExpr =
  let toDT (exprToDT : PT.Expr -> Dval) (s : PT.PipeExpr) : Dval =
    let name, fields =
      match s with
      | PT.EPipeVariable(id, varName) ->
        "EPipeVariable", [ DInt(int64 id); DString varName ]

      | PT.EPipeLambda(id, args, body) ->
        let variables =
          args
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

    DEnum(ptTyp [] "PipeExpr" 0, name, fields)



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
      | PT.EIf(id, cond, ifTrue, ifFalse) ->
        "EIf", [ DInt(int64 id); toDT cond; toDT ifTrue; toDT ifFalse ]

      | PT.EMatch(id, arg, cases) ->
        let cases =
          cases
          |> List.map (fun (pattern, expr) ->
            DTuple(MatchPattern.toDT pattern, toDT expr, []))

        "EMatch", [ DInt(int64 id); toDT arg; DList(cases) ]

      | PT.EPipe(id, expr, pipeExpr, pipeExprs) ->
        "EPipe",
        [ DInt(int64 id)
          toDT expr
          PipeExpr.toDT toDT pipeExpr
          DList(List.map (PipeExpr.toDT toDT) pipeExprs) ]


      // function calls
      | PT.EInfix(id, infix, lhs, rhs) ->
        "EInfix", [ DInt(int64 id); Infix.toDT infix; toDT lhs; toDT rhs ]

      | PT.ELambda(id, args, body) ->
        let variables =
          args
          |> List.map (fun (id, varName) ->
            DTuple(DInt(int64 id), DString varName, []))
          |> DList

        "ELambda", [ DInt(int64 id); variables; toDT body ]

      | PT.EApply(id, PT.FnTargetName name, typeArgs, args) ->
        "EApply",
        [ DInt(int64 id)
          DEnum(
            ptTyp [] "FnTarget" 0,
            "FnTargetName",
            [ NameResolution.toDT FnName.toDT name ]
          )
          DList(List.map TypeReference.toDT typeArgs)
          DList(List.map toDT args) ]

      | PT.EApply(id, PT.FnTargetExpr expr, typeArgs, args) ->
        "EApply",
        [ DInt(int64 id)
          DEnum(ptTyp [] "FnTarget" 0, "FnTargetExpr", [ toDT expr ])
          DList(List.map TypeReference.toDT typeArgs)
          DList(List.map toDT args) ]

      | PT.ERecordUpdate(id, record, updates) ->
        let updates =
          updates
          |> List.map (fun (name, expr) -> DTuple(DString name, toDT expr, []))

        "ERecordUpdate", [ DInt(int64 id); toDT record; DList(updates) ]

    DEnum(ptTyp [] "Expr" 0, name, fields)


module Deprecation =
  let toDT (inner : 'a -> Dval) (d : PT.Deprecation<'a>) : Dval =
    let (caseName, fields) =
      match d with
      | PT.Deprecation.NotDeprecated -> "NotDeprecated", []
      | PT.Deprecation.RenamedTo replacement -> "RenamedTo", [ inner replacement ]
      | PT.Deprecation.ReplacedBy replacement -> "ReplacedBy", [ inner replacement ]
      | PT.Deprecation.DeprecatedBecause reason ->
        "DeprecatedBecause", [ DString reason ]

    DEnum(ptTyp [] "Deprecation" 0, caseName, fields)


module TypeDeclaration =
  module RecordField =
    let toDT (rf : PT.TypeDeclaration.RecordField) : Dval =
      DRecord(
        ptTyp [ "TypeDeclaration" ] "RecordField" 0,
        Map
          [ "name", DString rf.name
            "typ", TypeReference.toDT rf.typ
            "description", DString rf.description ]
      )

  module EnumField =
    let toDT (ef : PT.TypeDeclaration.EnumField) : Dval =
      DRecord(
        ptTyp [ "TypeDeclaration" ] "EnumField" 0,
        Map
          [ "typ", TypeReference.toDT ef.typ
            "label", ef.label |> Option.map DString |> Dval.option
            "description", DString ef.description ]
      )

  module EnumCase =
    let toDT (ec : PT.TypeDeclaration.EnumCase) : Dval =
      DRecord(
        ptTyp [ "TypeDeclaration" ] "EnumCase" 0,
        Map
          [ "name", DString ec.name
            "fields", DList(List.map EnumField.toDT ec.fields)
            "description", DString ec.description ]
      )

  module Definition =
    let toDT (d : PT.TypeDeclaration.Definition) : Dval =
      let caseName, fields =
        match d with
        | PT.TypeDeclaration.Alias typeRef -> "Alias", [ TypeReference.toDT typeRef ]

        | PT.TypeDeclaration.Record(firstField, additionalFields) ->
          "Record",
          [ RecordField.toDT firstField
            DList(List.map RecordField.toDT additionalFields) ]

        | PT.TypeDeclaration.Enum(firstCase, additionalCases) ->
          "Enum",
          [ EnumCase.toDT firstCase; DList(List.map EnumCase.toDT additionalCases) ]

      DEnum(ptTyp [ "TypeDeclaration" ] "Definition" 0, caseName, fields)

  let toDT (td : PT.TypeDeclaration.T) : Dval =
    DRecord(
      ptTyp [ "TypeDeclaration" ] "T" 0,
      Map
        [ "typeParams", DList(List.map DString td.typeParams)
          "definition", Definition.toDT td.definition ]
    )

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

      DEnum(ptTyp [ "Handler" ] "CronInterval" 0, name, fields)

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

      DEnum(ptTyp [ "Handler" ] "Spec" 0, name, fields)

  let toDT (h : PT.Handler.T) : Dval =
    DRecord(
      ptTyp [ "Handler" ] "T" 0,
      Map
        [ "tlid", DInt(int64 h.tlid)
          "ast", Expr.toDT h.ast
          "typ", Spec.toDT h.spec ]
    )

module DB =
  let toDT (db : PT.DB.T) : Dval =
    DRecord(
      ptTyp [ "DB" ] "T" 0,
      Map
        [ "tlid", DInt(int64 db.tlid)
          "name", DString db.name
          "version", DInt db.version
          "typ", TypeReference.toDT db.typ ]
    )


module UserType =
  let toDT (userType : PT.UserType.T) : Dval =
    DRecord(
      ptTyp [] "UserType" 0,
      Map
        [ "tlid", DInt(int64 userType.tlid)
          "name", TypeName.UserProgram.toDT userType.name
          "deprecated", Deprecation.toDT TypeName.toDT userType.deprecated
          "declaration", TypeDeclaration.toDT userType.declaration ]
    )


module UserFunction =
  module Parameter =
    let toDT (p : PT.UserFunction.Parameter) : Dval =
      DRecord(
        ptTyp [ "UserFunction" ] "Parameter" 0,
        Map
          [ "name", DString p.name
            "typ", TypeReference.toDT p.typ
            "description", DString p.description ]
      )

  let toDT (userFn : PT.UserFunction.T) : Dval =
    DRecord(
      ptTyp [ "UserFunction" ] "T" 0,
      Map
        [ "tlid", DInt(int64 userFn.tlid)
          "name", FnName.UserProgram.toDT userFn.name
          "typeParams", DList(List.map DString userFn.typeParams)
          "parameters", DList(List.map Parameter.toDT userFn.parameters)
          "returnType", TypeReference.toDT userFn.returnType
          "body", Expr.toDT userFn.body
          "description", DString userFn.description
          "deprecated", Deprecation.toDT FnName.toDT userFn.deprecated ]
    )

module Secret =
  let toDT (s : PT.Secret.T) : Dval =
    DRecord(
      ptTyp [ "Secret" ] "T" 0,
      Map
        [ "name", DString s.name
          "value", DString s.value
          "version", DInt s.version ]
    )


module PackageType =
  let toDT (p : PT.PackageType.T) : Dval =
    DRecord(
      ptTyp [ "PackageType" ] "T" 0,
      Map
        [ "tlid", DInt(int64 p.tlid)
          "id", DUuid p.id
          "name", TypeName.Package.toDT p.name
          "declaration", TypeDeclaration.toDT p.declaration
          "description", DString p.description
          "deprecated", Deprecation.toDT TypeName.toDT p.deprecated ]
    )


module PackageFn =
  module Parameter =
    let toDT (p : PT.PackageFn.Parameter) : Dval =
      DRecord(
        ptTyp [ "PackageFn" ] "Parameter" 0,
        Map
          [ "name", DString p.name
            "typ", TypeReference.toDT p.typ
            "description", DString p.description ]
      )

  let toDT (p : PT.PackageFn.T) : Dval =
    DRecord(
      ptTyp [ "PackageFn" ] "T" 0,
      Map
        [ "tlid", DInt(int64 p.tlid)
          "id", DUuid p.id
          "name", FnName.Package.toDT p.name
          "body", Expr.toDT p.body
          "typeParams", DList(List.map DString p.typeParams)
          "parameters", DList(List.map Parameter.toDT p.parameters)
          "returnType", TypeReference.toDT p.returnType
          "description", DString p.description
          "deprecated", Deprecation.toDT FnName.toDT p.deprecated ]
    )
