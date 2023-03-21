/// Converts F# types to Dark ProgramTypes
module FSharpToProgramTypes

open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes


// A placeholder is used to indicate what still needs to be filled
let private placeholder =
  PT.EString(12345678UL, [ PT.StringText "PLACEHOLDER VALUE" ])

// This is a "Partial active pattern" that you can use as a Pattern to match a Placeholder value
let private (|Placeholder|_|) (input : PT.Expr) =
  if input = placeholder then Some() else None

// TODO: support custom types that aren't the 0th version of the type
let private matchingCustomTypes
  // TODO: we don't used the defs here - just the names
  (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
  (nameOfType : string)
  : List<PT.FQTypeName.T> =
  availableTypes
  |> List.choose (fun (typeName, _def) ->
    match typeName with
    | PT.FQTypeName.User u -> if u.typ = nameOfType then Some typeName else None
    | PT.FQTypeName.Stdlib t -> if t.typ = nameOfType then Some typeName else None)


module DType =
  let rec fromSynType
    (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
    (typ : SynType)
    : PT.DType =


    let c = fromSynType availableTypes

    let rec fromNameAndTypeArgs
      availableTypes
      (name : string)
      (typeArgs : List<SynType>)
      : PT.DType =
      match name, typeArgs with
      // no type args
      | "bool", [] -> PT.TBool
      | "int", [] -> PT.TInt
      | "string", [] -> PT.TStr
      | "char", [] -> PT.TChar
      | "float", [] -> PT.TFloat
      | "DateTime", [] -> PT.TDateTime
      | "UUID", [] -> PT.TUuid
      | "unit", [] -> PT.TUnit
      | "Password", [] -> PT.TPassword

      // with type args
      | "List", [ arg ] -> PT.TList(c arg)
      | "Option", [ arg ] -> PT.TOption(c arg)

      // Some user- or stdlib- type
      | _ ->
        match matchingCustomTypes availableTypes name with
        | [ matchedType ] -> PT.TCustomType(matchedType, List.map c typeArgs)
        | _ ->
          Exception.raiseInternal $"Unsupported type" [ "name", name; "type", typ ]


    match typ with
    | SynType.Paren (t, _) -> c t

    // Variable types (i.e. "generic types")
    // e.g. `'a` in `'a -> bool`
    | SynType.Var (SynTypar (id, _, _), _) -> PT.TVariable(id.idText)


    // Function types
    // e.g. `'a -> bool` in `let friends (lambda: ('a -> bool)) = ...`
    | SynType.Fun (arg, ret, _, _) -> PT.TFn([ c arg ], c ret)


    // Named types. covers:
    // - built-in F# types like `bool`
    // - Stdlib-defined types
    // - User-defined types
    | SynType.App (SynType.LongIdent (SynLongIdent ([ ident ], _, _)),
                   _,
                   typeArgs,
                   _,
                   _,
                   _,
                   _) -> fromNameAndTypeArgs availableTypes ident.idText typeArgs

    | SynType.LongIdent (SynLongIdent ([ ident ], _, _)) ->
      let typeArgs = []
      fromNameAndTypeArgs availableTypes ident.idText typeArgs

    | _ -> Exception.raiseInternal $"Unsupported type" [ "type", typ ]


module LetPattern =
  let rec fromSynPat ast (pat : SynPat) : PT.LetPattern =
    let r = fromSynPat ast
    match pat with
    | SynPat.Paren (subPat, _) -> r subPat
    | SynPat.Wild (_) -> PT.LPVariable(gid (), "_")
    | SynPat.Named (SynIdent (name, _), _, _, _) ->
      PT.LPVariable(gid (), name.idText)
    | _ ->
      Exception.raiseInternal
        "Unsupported let or use expr pat type"
        [ "ast", ast; "pat", pat ]


module MatchPattern =
  let rec fromSynPat (pat : SynPat) : PT.MatchPattern =
    let id = gid ()
    let r = fromSynPat

    match pat with
    | SynPat.Named (SynIdent (name, _), _, _, _) -> PT.MPVariable(id, name.idText)
    | SynPat.Wild _ -> PT.MPVariable(gid (), "_") // wildcard, not blank
    | SynPat.Const (SynConst.Int32 n, _) -> PT.MPInteger(id, n)
    | SynPat.Const (SynConst.Int64 n, _) -> PT.MPInteger(id, int64 n)
    | SynPat.Const (SynConst.UInt64 n, _) -> PT.MPInteger(id, int64 n)
    | SynPat.Const (SynConst.UserNum (n, "I"), _) -> PT.MPInteger(id, parseInt64 n)
    | SynPat.Const (SynConst.Char c, _) -> PT.MPCharacter(id, string c)
    | SynPat.Const (SynConst.Bool b, _) -> PT.MPBool(id, b)
    | SynPat.Const (SynConst.Unit, _) -> PT.MPUnit(id)
    | SynPat.Null _ ->
      Exception.raiseInternal "null pattern not supported, use `()`" [ "pat", pat ]
    | SynPat.Paren (pat, _) -> r pat
    | SynPat.Const (SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      PT.MPFloat(id, sign, whole, fraction)
    | SynPat.Const (SynConst.String (s, _, _), _) -> PT.MPString(id, s)
    | SynPat.LongIdent (SynLongIdent ([ constructorName ], _, _),
                        _,
                        _,
                        SynArgPats.Pats args,
                        _,
                        _) ->
      let args = List.map r args
      PT.MPConstructor(id, constructorName.idText, args)
    | SynPat.Tuple (_isStruct, (first :: second :: theRest), _range) ->
      PT.MPTuple(id, r first, r second, List.map r theRest)
    | _ -> Exception.raiseInternal "unhandled pattern" [ "pattern", pat ]


module Expr =
  // CLEANUP - blanks here aren't allowed
  let private nameOrBlank (v : string) : string = if v = "___" then "" else v

  let private parseFn (fnName : string) : string * int =
    match fnName with
    | Regex "^([a-z][a-z0-9A-Z]*)_v(\d+)$" [ name; version ] -> name, (int version)
    | Regex "^([a-z][a-z0-9A-Z]*)$" [ name ] -> name, 0
    | _ ->
      Exception.raiseInternal
        "Bad format in one word function name"
        [ "fnName", fnName ]

  let private ops =
    Map.ofList [ ("op_Addition", PT.ArithmeticPlus)
                 ("op_Subtraction", PT.ArithmeticMinus)
                 ("op_Multiply", PT.ArithmeticMultiply)
                 ("op_Division", PT.ArithmeticDivide)
                 ("op_Modulus", PT.ArithmeticModulo)
                 ("op_Concatenate", PT.ArithmeticPower)
                 ("op_GreaterThan", PT.ComparisonGreaterThan)
                 ("op_GreaterThanOrEqual", PT.ComparisonGreaterThanOrEqual)
                 ("op_LessThan", PT.ComparisonLessThan)
                 ("op_LessThanOrEqual", PT.ComparisonLessThanOrEqual)
                 ("op_EqualsEquals", PT.ComparisonEquals)
                 ("op_BangEquals", PT.ComparisonNotEquals)
                 ("op_PlusPlus", PT.StringConcat) ]

  let rec fromSynExpr
    (availableTypes : List<PT.FQTypeName.T * PT.CustomType.T>)
    (ast : SynExpr)
    : PT.Expr =
    let c = fromSynExpr availableTypes

    let convertLambdaVar (var : SynSimplePat) : string =
      match var with
      | SynSimplePat.Id (name, _, _, _, _, _) -> nameOrBlank name.idText
      | _ -> Exception.raiseInternal "unsupported lambdaVar" [ "var", var ]

    // Add a pipetarget after creating it
    let cPlusPipeTarget (e : SynExpr) : PT.Expr =
      match c e with
      | PT.EFnCall (id, name, typeArgs, args) ->
        PT.EFnCall(id, name, typeArgs, PT.EPipeTarget(gid ()) :: args)
      | PT.EInfix (id, op, Placeholder, arg2) ->
        PT.EInfix(id, op, PT.EPipeTarget(gid ()), arg2)
      | PT.EInfix (id, op, arg1, Placeholder) ->
        PT.EInfix(id, op, PT.EPipeTarget(gid ()), arg1)
      | other -> other

    let id = gid ()

    match ast with
    // constant values
    | SynExpr.Null _ ->
      Exception.raiseInternal "null not supported, use `()`" [ "ast", ast ]
    | SynExpr.Const (SynConst.Unit _, _) -> PT.EUnit id
    | SynExpr.Const (SynConst.Int32 n, _) -> PT.EInteger(id, n)
    | SynExpr.Const (SynConst.Int64 n, _) -> PT.EInteger(id, int64 n)
    | SynExpr.Const (SynConst.UInt64 n, _) -> PT.EInteger(id, int64 n)
    | SynExpr.Const (SynConst.UserNum (n, "I"), _) -> PT.EInteger(id, parseInt64 n)
    | SynExpr.Const (SynConst.Char c, _) -> PT.ECharacter(id, string c)
    | SynExpr.Const (SynConst.Bool b, _) -> PT.EBool(id, b)
    | SynExpr.Const (SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      PT.EFloat(id, sign, whole, fraction)

    // Strings
    | SynExpr.Const (SynConst.String (s, _, _), _) ->
      PT.EString(id, [ PT.StringText s ])
    | SynExpr.InterpolatedString (parts, _, _) ->
      let parts =
        parts
        |> List.map (function
          | SynInterpolatedStringPart.String (s, _) -> PT.StringText s
          | SynInterpolatedStringPart.FillExpr (e, _) -> PT.StringInterpolation(c e))
      PT.EString(id, parts)


    // Simple identifiers/operators like `==`
    | SynExpr.LongIdent (_, SynLongIdent ([ ident ], _, _), _, _) when
      Map.containsKey ident.idText ops
      ->
      let op =
        Map.get ident.idText ops
        |> Exception.unwrapOptionInternal
             "can't find operation"
             [ "name", ident.idText ]
      PT.EInfix(id, PT.InfixFnCall op, placeholder, placeholder)

    | SynExpr.LongIdent (_, SynLongIdent ([ ident ], _, _), _, _) when
      List.contains ident.idText [ "op_BooleanAnd"; "op_BooleanOr" ]
      ->
      let op =
        match ident.idText with
        | "op_BooleanAnd" -> PT.BinOpAnd
        | "op_BooleanOr" -> PT.BinOpOr
        | _ -> Exception.raiseInternal "unhandled operation" [ "name", ident.idText ]

      PT.EInfix(id, PT.BinOp op, placeholder, placeholder)

    | SynExpr.LongIdent (_, SynLongIdent ([ ident ], _, _), _, _) when
      ident.idText = "op_UnaryNegation"
      ->
      let name = PT.FQFnName.stdlibFqName "Int" "negate" 0
      PT.EFnCall(id, name, [], [])

    | SynExpr.Ident ident when Set.contains ident.idText PT.FQFnName.oneWordFunctions ->
      let name, version = parseFn ident.idText
      PT.EFnCall(id, PT.FQFnName.stdlibFqName "" name version, [], [])

    | SynExpr.Ident ident when ident.idText = "Nothing" ->
      PT.EConstructor(id, None, "Nothing", [])


    // Enum constructors
    | SynExpr.Ident name ->
      let matchingEnumCases =
        availableTypes
        |> List.choose (fun (typeName, typeDef) ->
          match typeDef with
          | PT.CustomType.Enum (firstCase, additionalCases) ->
            firstCase :: additionalCases
            |> List.tryFind (fun enumCase -> enumCase.name = name.idText)
            |> Option.map (fun _ -> typeName)

          | PT.CustomType.Record _ -> None)

      match matchingEnumCases with
      | [] -> PT.EVariable(id, name.idText)
      | [ typeName ] ->
        let fields =
          // When parsing an enum ctor, we don't know the fields _here_.
          // They are filled in shortly elsewhere - review this file for other cases
          // of "EConstructor" to locate such.
          []

        PT.EConstructor(id, Some typeName, name.idText, fields)
      | _ ->
        Exception.raiseInternal
          "There are more than 1 values that match this name, so the parser isn't sure which one to choose"
          [ "name", name.idText ]

    // Lists and arrays
    | SynExpr.ArrayOrList (_, exprs, _) -> PT.EList(id, exprs |> List.map c)

    // a literal list is sometimes made up of nested Sequentials
    | SynExpr.ArrayOrListComputed (_, (SynExpr.Sequential _ as seq), _) ->
      let rec seqAsList expr : List<SynExpr> =
        match expr with
        | SynExpr.Sequential (_, _, expr1, expr2, _) -> expr1 :: seqAsList expr2
        | _ -> [ expr ]
      PT.EList(id, seq |> seqAsList |> List.map c)

    | SynExpr.ArrayOrListComputed (_, SynExpr.Tuple (_, exprs, _, _), _) ->
      PT.EList(id, exprs |> List.map c)

    | SynExpr.ArrayOrListComputed (_, expr, _) -> PT.EList(id, [ c expr ])


    // Tuples
    | SynExpr.Tuple (_, first :: second :: rest, _, _) ->
      PT.ETuple(id, c first, c second, List.map c rest)


    // Function calls
    // Long Identifiers like DateTime.now, represented in the form of ["DateTime"; "now"]
    // (LongIdent = Ident list)
    | SynExpr.LongIdent (_, SynLongIdent ([ modName; fnName ], _, _), _, _) when
      System.Char.IsUpper(modName.idText[0])
      ->
      let module_ = modName.idText
      let name, version = parseFn fnName.idText
      PT.EFnCall(gid (), PT.FQFnName.stdlibFqName module_ name version, [], [])

    // e.g. `Json.serialize<T>`
    | SynExpr.TypeApp (SynExpr.LongIdent (_,
                                          SynLongIdent ([ modName; fnName ], _, _),
                                          _,
                                          _),
                       _,
                       typeArgs,
                       _,
                       _,
                       _,
                       _) ->
      let module_ = modName.idText
      let name, version = parseFn fnName.idText
      let typeArgs =
        typeArgs
        |> List.map (fun synType -> DType.fromSynType availableTypes synType)
      PT.EFnCall(gid (), PT.FQFnName.stdlibFqName module_ name version, typeArgs, [])

    // package manager function calls
    // (preliminary support)
    | SynExpr.LongIdent (_,
                         SynLongIdent ([ owner; package; modName; fnName ], _, _),
                         _,
                         _) when
      owner.idText = "Test" && package.idText = "Test" && modName.idText = "Test"
      ->
      PT.EFnCall(
        gid (),
        PT.FQFnName.packageFqName "test" "test" "Test" fnName.idText 0,
        [],
        []
      )


    // Field access
    | SynExpr.LongIdent (_, SynLongIdent ([ var; f1; f2; f3 ], _, _), _, _) ->
      let obj1 =
        PT.EFieldAccess(id, PT.EVariable(gid (), var.idText), nameOrBlank f1.idText)
      let obj2 = PT.EFieldAccess(id, obj1, nameOrBlank f2.idText)
      PT.EFieldAccess(id, obj2, nameOrBlank f3.idText)

    | SynExpr.LongIdent (_, SynLongIdent ([ var; field1; field2 ], _, _), _, _) ->
      let obj1 =
        PT.EFieldAccess(
          id,
          PT.EVariable(gid (), var.idText),
          nameOrBlank field1.idText
        )
      PT.EFieldAccess(id, obj1, nameOrBlank field2.idText)

    | SynExpr.LongIdent (_, SynLongIdent ([ var; field ], _, _), _, _) ->
      PT.EFieldAccess(id, PT.EVariable(gid (), var.idText), nameOrBlank field.idText)

    | SynExpr.DotGet (expr, _, SynLongIdent ([ field ], _, _), _) ->
      PT.EFieldAccess(id, c expr, nameOrBlank field.idText)


    // Lambdas
    | SynExpr.Lambda (_,
                      false,
                      SynSimplePats.SimplePats (outerVars, _),
                      body,
                      _,
                      _,
                      _) ->
      let rec extractVarsAndBody expr =
        match expr with
        // The 2nd param indicates this was part of a lambda
        | SynExpr.Lambda (_, true, SynSimplePats.SimplePats (vars, _), body, _, _, _) ->
          let nestedVars, body = extractVarsAndBody body
          vars @ nestedVars, body
        // The 2nd param indicates this was not nested
        | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (vars, _), body, _, _, _) ->
          vars, body
        | SynExpr.Lambda _ ->
          Exception.raiseInternal "TODO: other types of lambda" [ "expr", expr ]
        | _ -> [], expr

      let nestedVars, body = extractVarsAndBody body
      let vars =
        (outerVars @ nestedVars)
        |> List.map convertLambdaVar
        |> (List.map (fun name -> (gid (), name)))
      PT.ELambda(id, vars, c body)


    // if/then expressions
    | SynExpr.IfThenElse (cond, thenExpr, Some elseExpr, _, _, _, _) ->
      PT.EIf(id, c cond, c thenExpr, c elseExpr)


    // `let` bindings
    | SynExpr.LetOrUse (_,
                        _,
                        [ SynBinding (_, _, _, _, _, _, _, pat, _, rhs, _, _, _) ],
                        body,
                        _,
                        _) ->

      PT.ELet(id, LetPattern.fromSynPat ast pat, c rhs, c body)


    // `match` exprs:
    //
    // ```fsharp
    // // 'cond'
    // match Some 1 with // 'cond'
    // // 'cases'
    // | None -> ...
    // | Some 1 -> ...
    // | ...
    | SynExpr.Match (_, cond, cases, _, _) ->
      let convertCase
        (SynMatchClause (pat, _, expr, _, _, _) : SynMatchClause)
        : PT.MatchPattern * PT.Expr =
        (MatchPattern.fromSynPat pat, c expr)
      PT.EMatch(id, c cond, List.map convertCase cases)


    // records: `{ A = 2; B = "yellow" }`
    | SynExpr.Record (_, _, fields, _) ->
      let fields =
        fields
        |> List.map (fun field ->
          match field with
          | SynExprRecordField ((SynLongIdent ([ name ], _, _), _), _, Some expr, _) ->
            (nameOrBlank name.idText, c expr)
          | f -> Exception.raiseInternal "Not an expected field" [ "field", f ])

      let typeName =
        // TODO: determine the appropriate typeName
        // based on the fields and types available
        None

      PT.ERecord(id, typeName, fields)

    | SynExpr.Paren (expr, _, _, _) -> c expr // just unwrap

    | SynExpr.Do (expr, _) -> c expr // just unwrap


    // Pipes (|>)
    // nested pipes - F# uses 2 Apps to represent a pipe. The outer app has an
    // op_PipeRight, and the inner app has two arguments. Those arguments might
    // also be pipes
    | SynExpr.App (_,
                   _,
                   SynExpr.Ident pipe,
                   SynExpr.App (_, _, nestedPipes, arg, _),
                   _)
    | SynExpr.App (_,
                   _,
                   SynExpr.LongIdent (_, SynLongIdent ([ pipe ], _, _), _, _),
                   SynExpr.App (_, _, nestedPipes, arg, _),
                   _) when pipe.idText = "op_PipeRight" ->
      match c nestedPipes with
      | PT.EPipe (id, arg1, Placeholder, []) ->
        // when we just built the lowest, the second one goes here
        PT.EPipe(id, arg1, cPlusPipeTarget arg, [])
      | PT.EPipe (id, arg1, arg2, rest) ->
        PT.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
      // Exception.raiseInternal $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
      | other ->
        // Exception.raiseInternal $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
        // the very bottom on the pipe chain, this is the first and second expressions
        PT.EPipe(id, other, cPlusPipeTarget arg, [])

    | SynExpr.App (_, _, SynExpr.Ident pipe, expr, _)
    | SynExpr.App (_,
                   _,
                   SynExpr.LongIdent (_, SynLongIdent ([ pipe ], _, _), _, _),
                   expr,
                   _) when pipe.idText = "op_PipeRight" ->
      // the very bottom on the pipe chain, this is just the first expression
      PT.EPipe(id, c expr, placeholder, [])


    // Enum values (EConstructors)
    // TODO: remove this explicit handling
    // when the Option and Result types are defined in StdLib
    | SynExpr.App (_, _, SynExpr.Ident name, arg, _) when
      List.contains name.idText [ "Ok"; "Nothing"; "Just"; "Error" ]
      ->
      PT.EConstructor(id, None, name.idText, [ c arg ])


    // Feature flags
    // We need to handle them now, or th  below `App` case will
    // make itet recognized as a variable referencess
    | SynExpr.App (_,
                   _,
                   SynExpr.Ident name,
                   SynExpr.Const (SynConst.String (label, _, _), _),
                   _) when name.idText = "flag" ->
      PT.EFeatureFlag(gid (), label, placeholder, placeholder, placeholder)

    // Callers with multiple args are encoded as apps wrapping other apps.
    | SynExpr.App (_, _, funcExpr, arg, _) -> // function application (binops and fncalls)
      match c funcExpr with
      | PT.EFnCall (id, name, typeArgs, args) ->
        PT.EFnCall(id, name, typeArgs, args @ [ c arg ])
      | PT.EInfix (id, op, Placeholder, arg2) -> PT.EInfix(id, op, c arg, arg2)
      | PT.EInfix (id, op, arg1, Placeholder) -> PT.EInfix(id, op, arg1, c arg)
      // Fill in the feature flag fields (back to front)
      | PT.EFeatureFlag (id, label, Placeholder, oldexpr, newexpr) ->
        PT.EFeatureFlag(id, label, c arg, oldexpr, newexpr)
      | PT.EFeatureFlag (id, label, condexpr, Placeholder, newexpr) ->
        PT.EFeatureFlag(id, label, condexpr, c arg, newexpr)
      | PT.EFeatureFlag (id, label, condexpr, oldexpr, Placeholder) ->
        PT.EFeatureFlag(id, label, condexpr, oldexpr, c arg)
      // A pipe with one entry
      | PT.EPipe (id, arg1, Placeholder, []) ->
        PT.EPipe(id, arg1, cPlusPipeTarget arg, [])
      // A pipe with more than one entry
      | PT.EPipe (id, arg1, arg2, rest) ->
        PT.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
      | PT.EVariable (id, name) ->
        if Set.contains name PT.FQFnName.oneWordFunctions then
          let (name, version) = parseFn name
          PT.EFnCall(id, PT.FQFnName.stdlibFqName "" name version, [], [ c arg ])
        else
          PT.EFnCall(id, PT.FQFnName.User name, [], [ c arg ])

      // Enums
      | PT.EConstructor (id, typeName, caseName, _fields) ->
        let fields =
          match c arg with
          | PT.ETuple (_, first, second, theRest) -> first :: second :: theRest
          | other -> [ other ]

        PT.EConstructor(id, typeName, caseName, fields)

      | e ->
        Exception.raiseInternal
          "Unsupported expression in app"
          [ "fnCall expr", funcExpr
            "converted specific fncall exp", e
            "argument", arg ]

    | SynExpr.FromParseError _ as expr ->
      Exception.raiseInternal "There was a parser error parsing" [ "expr", expr ]
    | expr ->
      Exception.raiseInternal "Unsupported expression" [ "ast", ast; "expr", expr ]


module UserFunction =
  let rec private parseArgPat
    availableTypes
    (pat : SynPat)
    : PT.UserFunction.Parameter =
    let r = parseArgPat availableTypes

    match pat with
    | SynPat.Paren (pat, _) -> r pat
    | SynPat.Const (SynConst.Unit, _) ->
      { id = gid (); name = "unit"; typ = PT.TUnit; description = "" }
    | SynPat.Typed (SynPat.Named (SynIdent (id, _), _, _, _), typ, _) ->
      { id = gid ()
        name = id.idText
        typ = DType.fromSynType availableTypes typ
        description = "" }
    | _ -> Exception.raiseInternal "Unsupported argPat" [ "pat", pat ]

  let private parseSignature
    availableTypes
    (pat : SynPat)
    : string * List<PT.UserFunction.Parameter> =
    match pat with
    | SynPat.LongIdent (SynLongIdent ([ name ], _, _), _, _, argPats, _, _) ->
      let parameters =
        match argPats with
        | SynArgPats.Pats pats -> List.map (parseArgPat availableTypes) pats
        | SynArgPats.NamePatPairs _ ->
          Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]
      (name.idText, parameters)
    | _ -> Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]

  let fromSynBinding availableTypes (binding : SynBinding) : PT.UserFunction.T =
    match binding with
    // CLEANUP use returnInfo
    | SynBinding (_, _, _, _, _, _, _, pat, _returnInfo, expr, _, _, _) ->
      let (name, parameters) = parseSignature availableTypes pat
      { tlid = gid ()
        name = name
        typeArgs = [] // TODO: review
        parameters = parameters
        returnType = PT.TVariable "a"
        description = ""
        infix = false
        body = Expr.fromSynExpr availableTypes expr }


module CustomType =
  module Enum =
    let private parseField
      availableTypes
      (typ : SynField)
      : PT.CustomType.EnumField =
      match typ with
      | SynField (_, _, fieldName, typ, _, _, _, _, _) ->
        { id = gid ()
          typ = DType.fromSynType availableTypes typ
          label = fieldName |> Option.map (fun id -> id.idText) }

    let private parseCase
      availableTypes
      (case : SynUnionCase)
      : PT.CustomType.EnumCase =
      match case with
      | SynUnionCase (_, SynIdent (id, _), typ, _, _, _, _) ->
        match typ with
        | SynUnionCaseKind.Fields fields ->
          { id = gid ()
            name = id.idText
            fields = List.map (parseField availableTypes) fields }
        | _ -> Exception.raiseInternal $"Unsupported enum case" [ "case", case ]

    let fromCases availableTypes typeDef (cases : List<SynUnionCase>) =
      let parseCase = parseCase availableTypes

      let firstCase, additionalCases =
        match cases with
        | [] ->
          Exception.raiseInternal
            $"Can't parse enum without any cases"
            [ "typeDef", typeDef ]
        | firstCase :: additionalCases -> firstCase, additionalCases

      PT.CustomType.Enum(parseCase firstCase, List.map parseCase additionalCases)

  module Record =
    let private parseField
      availableTypes
      (field : SynField)
      : PT.CustomType.RecordField =
      match field with
      | SynField (_, _, Some id, typ, _, _, _, _, _) ->
        { id = gid (); name = id.idText; typ = DType.fromSynType availableTypes typ }
      | _ -> Exception.raiseInternal $"Unsupported field" [ "field", field ]

    let fromFields availableTypes typeDef (fields : List<SynField>) =
      match fields with
      | [] ->
        Exception.raiseInternal
          $"Unsupported record type with no fields"
          [ "typeDef", typeDef ]
      | firstField :: additionalFields ->
        let parseField = parseField availableTypes

        PT.CustomType.Record(
          parseField firstField,
          List.map parseField additionalFields
        )


module UserType =
  let fromSynTypeDefn availableTypes (typeDef : SynTypeDefn) : PT.UserType.T =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _),
                                           _),
                   _,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = { typ = id.idText; version = 0 }
        definition = CustomType.Record.fromFields availableTypes typeDef fields }

    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (_, cases, _),
                                           _),
                   _,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = { typ = id.idText; version = 0 }
        definition = CustomType.Enum.fromCases availableTypes typeDef cases }
    | _ ->
      Exception.raiseInternal $"Unsupported type definition" [ "typeDef", typeDef ]


module UserDB =
  let private parseDBSchemaField availableTypes (field : SynField) : PT.DB.Col =
    match field with
    | SynField (_, _, Some id, typ, _, _, _, _, _) ->
      { name = Some(id.idText) // CLEANUP
        nameID = gid ()
        typ = Some(DType.fromSynType availableTypes typ)
        typeID = gid () }
    | _ -> Exception.raiseInternal $"Unsupported DB schema field" [ "field", field ]

  let fromSynTypeDefn availableTypes (typeDef : SynTypeDefn) : PT.DB.T =
    match typeDef with
    | SynTypeDefn (SynComponentInfo (_, _params, _, [ id ], _, _, _, _),
                   SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Record (_, fields, _),
                                           _),
                   _members,
                   _,
                   _,
                   _) ->
      { tlid = gid ()
        name = id.idText
        nameID = gid ()
        version = 0
        cols = List.map (parseDBSchemaField availableTypes) fields }
    | _ ->
      Exception.raiseInternal $"Unsupported db definition" [ "typeDef", typeDef ]


module PackageFn =
  let fromSynBinding
    ((p1, p2, p3) : string * string * string)
    (binding : SynBinding)
    : PT.Package.Fn =
    let availableTypes = [] // eventually, we'll likely need package types supported here
    let userFn = UserFunction.fromSynBinding availableTypes binding
    { name =
        { owner = p1
          package = p2
          module_ = p3
          function_ = userFn.name
          version = 0 }
      typeArgs = [] // TODO: review
      parameters =
        userFn.parameters
        |> List.map (fun (p : PT.UserFunction.Parameter) ->
          { name = p.name; typ = p.typ; description = "" })
      returnType = userFn.returnType
      description = userFn.description
      deprecated = false
      author = ""
      tlid = userFn.tlid
      body = userFn.body }
