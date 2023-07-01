module Parser.ProgramTypes

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

open Utils

// A placeholder is used to indicate what still needs to be filled
let placeholder = PT.EString(12345678UL, [ PT.StringText "PLACEHOLDER VALUE" ])
let pipePlaceholder = PT.EPipeVariable(12345678UL, "PIPE PLACEHOLDER VALUE")

// This is a "Partial active pattern" that you can use as a Pattern to match a Placeholder value
let (|Placeholder|_|) (input : PT.Expr) =
  if input = placeholder then Some() else None

let (|PipePlaceholder|_|) (input : PT.PipeExpr) =
  if input = pipePlaceholder then Some() else None

module TypeName =
  let resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (name : PT.TypeName.T)
    : PT.TypeName.T =
    match name with
    | PT.FQName.Package _ -> name
    | PT.FQName.UserProgram n ->
      match n.modules with
      | "PACKAGE" :: owner :: package :: rest ->
        PT.FQName.Package
          { owner = owner
            modules = NonEmptyList.ofList (package :: rest)
            name = n.name
            version = n.version }
      | _ ->
        if Set.contains n userTypes then
          PT.FQName.UserProgram n
        else
          PT.FQName.BuiltIn
            { name = n.name; version = n.version; modules = n.modules }
    | PT.FQName.BuiltIn n ->
      let userName : PT.TypeName.UserProgram =
        { modules = n.modules; name = n.name; version = n.version }
      if Set.contains userName userTypes then
        PT.FQName.UserProgram(userName)
      else
        PT.FQName.BuiltIn(n)

module FnName =
  let resolveNames
    (userFns : Set<PT.FnName.UserProgram>)
    (name : PT.FnName.T)
    : PT.FnName.T =
    match name with
    | PT.FQName.Package _ -> name
    | PT.FQName.UserProgram n ->
      match n.modules with
      | "PACKAGE" :: owner :: package :: rest ->
        PT.FQName.Package
          { owner = owner
            modules = NonEmptyList.ofList (package :: rest)
            name = n.name
            version = n.version }
      | _ ->
        if Set.contains n userFns then
          PT.FQName.UserProgram n
        else
          PT.FQName.BuiltIn
            { name = n.name; version = n.version; modules = n.modules }
    | PT.FQName.BuiltIn n ->
      let userName : PT.FnName.UserProgram =
        { modules = n.modules; name = n.name; version = n.version }
      if Set.contains userName userFns then
        PT.FQName.UserProgram(userName)
      else
        PT.FQName.BuiltIn(n)


module TypeReference =

  let private parseTypeRef (name : string) : string * int =
    match name with
    | Regex "^([A-Z][a-z0-9A-Z]*)_v(\d+)$" [ name; version ] -> name, (int version)
    | Regex "^([A-Z][a-z0-9A-Z]*)$" [ name ] -> name, 0
    | _ -> Exception.raiseInternal "Bad format in typeRef" [ "name", name ]


  let rec fromNamesAndTypeArgs
    (names : List<Ident>)
    (typeArgs : List<SynType>)
    : PT.TypeReference =
    let modules =
      List.initial names
      |> Option.defaultValue []
      |> List.map (fun name -> name.idText)
    let name = List.last names |> Exception.unwrapOptionInternal "typeName" []
    match modules, parseTypeRef name.idText, typeArgs with
    // no type args
    | [], ("Bool", 0), [] -> PT.TBool
    | [], ("Bytes", 0), [] -> PT.TBytes
    | [], ("Int", 0), [] -> PT.TInt
    | [], ("String", 0), [] -> PT.TString
    | [], ("Char", 0), [] -> PT.TChar
    | [], ("Float", 0), [] -> PT.TFloat
    | [], ("DateTime", 0), [] -> PT.TDateTime
    | [], ("Uuid", 0), [] -> PT.TUuid
    | [], ("Unit", 0), [] -> PT.TUnit
    | [], ("Password", 0), [] -> PT.TPassword

    // with type args
    | [], ("List", 0), [ arg ] -> PT.TList(fromSynType arg)
    | [], ("Option", 0), [ arg ] -> PT.TOption(fromSynType arg)
    | [], ("Dict", 0), [ valArg ] -> PT.TDict(fromSynType valArg)
    // TYPESCLEANUP - don't use word Tuple here
    | [], ("Tuple", 0), first :: second :: theRest ->
      PT.TTuple(fromSynType first, fromSynType second, List.map fromSynType theRest)
    | modules, (name, version), args ->
      let tn =
        PT.FQName.UserProgram
          { modules = modules; name = PT.TypeName.TypeName name; version = version }
      PT.TCustomType(tn, List.map fromSynType typeArgs)

  and fromSynType (typ : SynType) : PT.TypeReference =
    let c = fromSynType

    match typ with
    | SynType.Paren(t, _) -> c t

    // Variable types (i.e. "generic types")
    // e.g. `'a` in `'a -> bool`
    | SynType.Var(SynTypar(id, _, _), _) -> PT.TVariable(id.idText)

    | SynType.Tuple(_, args, _) ->
      let args =
        args
        |> List.filterMap (fun arg ->
          match arg with
          | SynTupleTypeSegment.Type t -> Some t
          | SynTupleTypeSegment.Star _ -> None
          | SynTupleTypeSegment.Slash _ -> None)
      match args with
      | [] -> Exception.raiseInternal "Tuple type with no args" [ "type", typ ]
      | [ _ ] ->
        Exception.raiseInternal "Tuple type with only one arg" [ "type", typ ]
      | first :: second :: theRest ->
        PT.TTuple(c first, c second, List.map c theRest)

    // Function types
    // e.g. `'a -> bool` in `let friends (lambda: ('a -> bool)) = ...`
    | SynType.Fun(arg, ret, _, _) -> PT.TFn([ c arg ], c ret)


    // Named types. covers:
    // - built-in F# types like `bool`
    // - Stdlib-defined types
    // - User-defined types
    | SynType.App(SynType.LongIdent(SynLongIdent(names, _, _)),
                  _,
                  typeArgs,
                  _,
                  _,
                  _,
                  range) -> fromNamesAndTypeArgs names typeArgs

    | SynType.LongIdent(SynLongIdent(names, _, _)) -> fromNamesAndTypeArgs names []

    | _ -> Exception.raiseInternal $"Unsupported type" [ "type", typ ]

  let rec resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (typ : PT.TypeReference)
    : PT.TypeReference =
    let c = resolveNames userTypes
    match typ with
    | PT.TCustomType(tn, args) ->
      PT.TCustomType(TypeName.resolveNames userTypes tn, List.map c args)
    | PT.TFn(args, ret) -> PT.TFn(List.map c args, c ret)
    | PT.TTuple(first, second, theRest) ->
      PT.TTuple(c first, c second, List.map c theRest)
    | PT.TList arg -> PT.TList(c arg)
    | PT.TOption arg -> PT.TOption(c arg)
    | PT.TDict valArg -> PT.TDict(c valArg)
    | PT.TVariable _ -> typ
    | PT.TDB arg -> PT.TDB(c arg)
    | PT.TBool
    | PT.TBytes
    | PT.TInt
    | PT.TString
    | PT.TChar
    | PT.TFloat
    | PT.TDateTime
    | PT.TUuid
    | PT.TUnit
    | PT.TPassword -> typ


module SimpleTypeArgs =
  let fromSynTyparDecls (typeParams : Option<SynTyparDecls>) : List<string> =
    match typeParams with
    | None -> []
    | Some typeParams ->
      match typeParams with
      | SynTyparDecls.PostfixList(decls, constraints, _) ->
        match constraints with
        | [] ->
          decls
          |> List.map (fun decl ->
            let SynTyparDecl (_, decl) = decl

            match decl with
            | SynTyparDecl(_, SynTypar(name, TyparStaticReq.None, _)) -> name.idText
            | _ ->
              Exception.raiseInternal "Unsupported type parameter" [ "decl", decl ])
        | _ ->
          Exception.raiseInternal
            "Unsupported constraints in function type arg declaration"
            [ "constraints", constraints ]

      | SynTyparDecls.PrefixList _
      | SynTyparDecls.SinglePrefix _ ->
        Exception.raiseInternal
          "Unsupported type params of function declaration"
          [ "typeParams", typeParams ]

module LetPattern =
  let rec fromSynPat (pat : SynPat) : PT.LetPattern =
    let mapPat = fromSynPat

    match pat with
    | SynPat.Paren(subPat, _) -> mapPat subPat
    | SynPat.Wild(_) -> PT.LPVariable(gid (), "_")
    | SynPat.Named(SynIdent(name, _), _, _, _) -> PT.LPVariable(gid (), name.idText)

    | SynPat.Tuple(_, (first :: second :: theRest), _) ->
      PT.LetPattern.LPTuple(
        gid (),
        mapPat first,
        mapPat second,
        List.map mapPat theRest
      )

    | _ ->
      Exception.raiseInternal "Unsupported let or use expr pat type" [ "pat", pat ]


module MatchPattern =
  let rec fromSynPat (pat : SynPat) : PT.MatchPattern =
    let id = gid ()
    let r = fromSynPat

    let convertEnumArg (ast : SynPat) : List<PT.MatchPattern> =
      // if the arg is a tuple with one paren around it, it's just arguments to the
      // enum. But if it has two parens around it, it's a single tuple.
      // eg: (Foo(1, 2)) vs (Foo((1, 2)))
      match ast with
      | SynPat.Paren(SynPat.Paren(SynPat.Tuple(_, t1 :: t2 :: trest, _), _), _) ->
        [ PT.MPTuple(gid (), r t1, r t2, List.map r trest) ]
      | SynPat.Paren(SynPat.Tuple(_, args, _), _) -> List.map r args
      | SynPat.Tuple(_, args, _) -> List.map r args
      | e -> [ r e ]

    match pat with
    | SynPat.Named(SynIdent(name, _), _, _, _) -> PT.MPVariable(id, name.idText)
    | SynPat.Wild _ -> PT.MPVariable(gid (), "_") // wildcard, not blank
    | SynPat.Const(SynConst.Int32 n, _) -> PT.MPInt(id, n)
    | SynPat.Const(SynConst.Int64 n, _) -> PT.MPInt(id, int64 n)
    | SynPat.Const(SynConst.UInt64 n, _) -> PT.MPInt(id, int64 n)
    | SynPat.Const(SynConst.UserNum(n, "I"), _) -> PT.MPInt(id, parseInt64 n)
    | SynPat.Const(SynConst.Char c, _) -> PT.MPChar(id, string c)
    | SynPat.Const(SynConst.Bool b, _) -> PT.MPBool(id, b)
    | SynPat.Const(SynConst.Unit, _) -> PT.MPUnit(id)
    | SynPat.Null _ ->
      Exception.raiseInternal "null pattern not supported, use `()`" [ "pat", pat ]
    | SynPat.Paren(pat, _) -> r pat
    | SynPat.Const(SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      PT.MPFloat(id, sign, whole, fraction)
    | SynPat.Const(SynConst.String(s, _, _), _) -> PT.MPString(id, s)
    | SynPat.LongIdent(SynLongIdent(names, _, _), _, _, SynArgPats.Pats args, _, _) ->
      let enumName =
        List.last names |> Exception.unwrapOptionInternal "missing enum name" []
      let modules =
        List.initial names |> Option.unwrap [] |> List.map (fun i -> i.idText)
      if modules <> [] then
        Exception.raiseInternal
          "Module in enum pattern casename. Only use the casename in Enum patterns"
          [ "pat", pat ]
      let args = List.map convertEnumArg args |> List.concat
      PT.MPEnum(id, enumName.idText, args)
    | SynPat.Tuple(_isStruct, (first :: second :: theRest), _range) ->
      PT.MPTuple(id, r first, r second, List.map r theRest)
    | SynPat.ListCons(headPat, tailPat, _, _) ->
      PT.MPListCons(id, r headPat, r tailPat)
    | SynPat.ArrayOrList(_, pats, _) -> PT.MPList(id, List.map r pats)
    | _ -> Exception.raiseInternal "unhandled pattern" [ "pattern", pat ]


module Expr =
  // CLEANUP - blanks here aren't allowed
  let private nameOrBlank (v : string) : string = if v = "___" then "" else v

  let parseFn (fnName : string) : Option<string * int> =
    match fnName with
    | Regex "^([a-z][a-z0-9A-Z]*[']?)_v(\d+)$" [ name; version ] ->
      Some(name, (int version))
    | Regex "^([a-z][a-z0-9A-Z]*[']?)$" [ name ] -> Some(name, 0)
    | _ -> None

  let parseEnum (enumName : string) : Option<string> =
    // No version on the Enum case, that's on the type
    match enumName with
    | Regex "^([A-Z][a-z0-9A-Z]*)$" [ name ] -> Some name
    | _ -> None

  let parseTypeName (typeName : string) : string * int =
    match typeName with
    | Regex "^([A-Z][a-z0-9A-Z]*[']?)_v(\d+)$" [ name; version ] ->
      name, (int version)
    | Regex "^([A-Z][a-z0-9A-Z]*[']?)$" [ name ] -> name, 0
    | _ -> Exception.raiseInternal "Bad format in type name" [ "typeName", typeName ]

  let parseNames (e : SynExpr) : List<string> =
    match e with
    | SynExpr.LongIdent(_, SynLongIdent(names, _, _), _, _) ->
      names |> List.map (fun i -> i.idText)
    | SynExpr.Ident name -> [ name.idText ]
    | _ -> Exception.raiseInternal "Bad format in names" [ "e", e ]


  let private ops =
    Map.ofList
      [ ("op_Addition", PT.ArithmeticPlus)
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

  let rec fromSynExpr' (ast : SynExpr) : PT.Expr =
    let c = fromSynExpr'

    let convertEnumArg (ast : SynExpr) : List<PT.Expr> =
      // if the arg is a tuple with one paren around it, it's just arguments to the
      // enum. But if it has two parens around it, it's a single tuple.
      // eg: (Foo(1, 2)) vs (Foo((1, 2)))
      match ast with
      | SynExpr.Paren(SynExpr.Paren(SynExpr.Tuple(_, t1 :: t2 :: trest, _, _),
                                    _,
                                    _,
                                    _),
                      _,
                      _,
                      _) -> [ PT.ETuple(gid (), c t1, c t2, List.map c trest) ]
      | SynExpr.Paren(SynExpr.Tuple(_, args, _, _), _, _, _) -> List.map c args
      | SynExpr.Tuple(_, args, _, _) -> List.map c args
      | e -> [ c e ]


    let convertLambdaVar (var : SynSimplePat) : string =
      match var with
      | SynSimplePat.Id(name, _, _, _, _, _) -> nameOrBlank name.idText
      | _ -> Exception.raiseInternal "unsupported lambdaVar" [ "var", var ]

    let synToPipeExpr (e : SynExpr) : PT.PipeExpr =
      match c e with
      | PT.EApply(id, PT.FnTargetName name, typeArgs, args) ->
        PT.EPipeFnCall(id, name, typeArgs, args)
      | PT.EInfix(id, op, Placeholder, arg2) -> PT.EPipeInfix(id, op, arg2)
      | PT.EInfix(id, op, arg1, Placeholder) -> PT.EPipeInfix(id, op, arg1)
      | PT.EEnum(id, typeName, caseName, fields) ->
        PT.EPipeEnum(id, typeName, caseName, fields)
      | PT.EVariable(id, name) -> PT.EPipeVariable(id, name)
      | PT.EEnum(id, typeName, caseName, fields) ->
        PT.EPipeEnum(id, typeName, caseName, fields)
      | PT.ELambda(id, vars, body) -> PT.EPipeLambda(id, vars, body)
      | other ->
        Exception.raiseInternal
          "Expected a function, got something else."
          [ "expr", other ]


    let id = gid ()

    match ast with

    // Literals (ints, chars, bools, etc)
    | SynExpr.Null _ ->
      Exception.raiseInternal "null not supported, use `()`" [ "ast", ast ]
    | SynExpr.Const(SynConst.Unit _, _) -> PT.EUnit id
    | SynExpr.Const(SynConst.Int32 n, _) -> PT.EInt(id, n)
    | SynExpr.Const(SynConst.Int64 n, _) -> PT.EInt(id, int64 n)
    | SynExpr.Const(SynConst.UInt64 n, _) -> PT.EInt(id, int64 n)
    | SynExpr.Const(SynConst.UserNum(n, "I"), _) -> PT.EInt(id, parseInt64 n)
    | SynExpr.Const(SynConst.Char c, _) -> PT.EChar(id, string c)
    | SynExpr.Const(SynConst.Bool b, _) -> PT.EBool(id, b)
    | SynExpr.Const(SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      PT.EFloat(id, sign, whole, fraction)


    // Strings
    | SynExpr.Const(SynConst.String(s, _, _), _) ->
      PT.EString(id, [ PT.StringText s ])
    | SynExpr.InterpolatedString(parts, _, _) ->
      let parts =
        parts
        |> List.filterMap (function
          | SynInterpolatedStringPart.String("", _) -> None
          | SynInterpolatedStringPart.String(s, _) -> Some(PT.StringText s)
          | SynInterpolatedStringPart.FillExpr(e, _) ->
            Some(PT.StringInterpolation(c e)))
      PT.EString(id, parts)


    // Simple identifiers/operators like `==`
    | SynExpr.LongIdent(_, SynLongIdent([ ident ], _, _), _, _) when
      Map.containsKey ident.idText ops
      ->
      let op =
        Map.get ident.idText ops
        |> Exception.unwrapOptionInternal
          "can't find operation"
          [ "name", ident.idText ]
      PT.EInfix(id, PT.InfixFnCall op, placeholder, placeholder)


    // Binary Ops: && / ||
    | SynExpr.LongIdent(_, SynLongIdent([ ident ], _, _), _, _) when
      List.contains ident.idText [ "op_BooleanAnd"; "op_BooleanOr" ]
      ->
      let op =
        match ident.idText with
        | "op_BooleanAnd" -> PT.BinOpAnd
        | "op_BooleanOr" -> PT.BinOpOr
        | _ -> Exception.raiseInternal "unhandled operation" [ "name", ident.idText ]

      PT.EInfix(id, PT.BinOp op, placeholder, placeholder)


    // Negation
    | SynExpr.LongIdent(_, SynLongIdent([ ident ], _, _), _, _) when
      ident.idText = "op_UnaryNegation"
      ->
      let name = PT.FnName.fqBuiltIn [ "Int" ] "negate" 0
      PT.EApply(id, PT.FnTargetName name, [], [])


    // One word functions like `equals`
    | SynExpr.Ident ident when Set.contains ident.idText PT.FnName.oneWordFunctions ->
      match parseFn ident.idText with
      | Some(name, version) ->
        PT.EApply(id, PT.FnTargetName(PT.FnName.fqBuiltIn [] name version), [], [])
      | None -> PT.EVariable(id, ident.idText)

    // List literals
    | SynExpr.ArrayOrList(_, exprs, _) -> PT.EList(id, exprs |> List.map c)

    // a literal list is sometimes made up of nested Sequentials
    | SynExpr.ArrayOrListComputed(_, (SynExpr.Sequential _ as seq), _) ->
      let rec seqAsList expr : List<SynExpr> =
        match expr with
        | SynExpr.Sequential(_, _, expr1, expr2, _) -> expr1 :: seqAsList expr2
        | _ -> [ expr ]
      PT.EList(id, seq |> seqAsList |> List.map c)

    | SynExpr.ArrayOrListComputed(_,
                                  SynExpr.Tuple(_, first :: second :: theRest, _, _),
                                  _) ->
      PT.ETuple(id, c first, c second, List.map c theRest)

    | SynExpr.ArrayOrListComputed(_, expr, _) -> PT.EList(id, [ c expr ])


    // Tuples
    | SynExpr.Tuple(_, first :: second :: rest, _, _) ->
      PT.ETuple(id, c first, c second, List.map c rest)

    // Enum values (EEnums)
    // TODO: remove this explicit handling
    // when the Option type are defined in StdLib
    | SynExpr.App(_, _, SynExpr.Ident name, arg, _) when
      List.contains name.idText [ "Nothing"; "Just" ]
      ->
      let typeName = PT.TypeName.fqBuiltIn [] "Option" 0
      PT.EEnum(id, typeName, name.idText, convertEnumArg arg)

    // Enum values (EEnums)
    | SynExpr.Ident name when List.contains name.idText [ "Nothing"; "Just" ] ->
      let typeName = PT.TypeName.fqBuiltIn [] "Option" 0
      PT.EEnum(id, typeName, name.idText, [])


    // Enum/FnCalls - e.g. `Result.Ok` or `Result.mapSecond`
    | SynExpr.LongIdent(_, SynLongIdent(names, _, _), _, _) when
      (names
       |> List.initial
       |> Option.unwrap []
       |> List.all (fun n -> n.idText <> "" && (System.Char.IsUpper(n.idText[0]))))
      ->
      let modules =
        List.initial names |> Option.unwrap [] |> List.map (fun i -> i.idText)
      let name =
        List.last names
        |> Exception.unwrapOptionInternal "empty list" []
        |> fun i -> i.idText

      match parseFn name with
      | Some(name, version) ->
        PT.EApply(
          gid (),
          PT.FnTargetName(PT.FnName.fqUserProgram modules name version),
          [],
          []
        )
      | None ->
        match parseEnum name with
        | Some enumName ->
          let typename =
            List.last modules |> Exception.unwrapOptionInternal "empty list" []
          let (typ, version) = parseTypeName typename
          let modules = List.initial modules |> Option.unwrap []
          PT.EEnum(
            gid (),
            PT.TypeName.fqUserProgram modules typ version,
            enumName,
            []
          )
        | None -> Exception.raiseInternal "invalid enum name" [ "name", name ]


    // Variable enums - Ok, Error, Nothing, and Just are handled elsewhere,
    // and Enums are expected to be fully qualified
    | SynExpr.Ident name -> PT.EVariable(id, name.idText)


    // e.g. `Json.serialize<T>`
    | SynExpr.TypeApp(SynExpr.Ident name, _, typeArgs, _, _, _, _) ->
      let typeArgs =
        typeArgs |> List.map (fun synType -> TypeReference.fromSynType synType)


      let name, version =
        parseFn name.idText
        |> Exception.unwrapOptionInternal
          "invalid fn name"
          [ "name", name.idText; "ast", ast ]
      PT.EApply(
        gid (),
        PT.FnTargetName(PT.FnName.fqUserProgram [] name version),
        typeArgs,
        []
      )

    // e.g. `Module1.Module2.fnName<String>`
    | SynExpr.TypeApp(SynExpr.LongIdent(_,
                                        SynLongIdent(first :: second :: theRest, _, _),
                                        _,
                                        _),
                      _,
                      typeArgs,
                      _,
                      _,
                      _,
                      _) ->

      match List.rev (first :: second :: theRest) with
      // the last item is the function name
      // the preceding items are the module names
      | fnName :: modNameParts ->
        let modules = modNameParts |> List.rev |> List.map (fun i -> i.idText)

        let name, version =
          parseFn fnName.idText |> Exception.unwrapOptionInternal "invalid fn" []

        let typeArgs =
          typeArgs |> List.map (fun synType -> TypeReference.fromSynType synType)

        PT.EApply(
          gid (),
          PT.FnTargetName(PT.FnName.fqUserProgram modules name version),
          typeArgs,
          []
        )

      | _ ->
        // should never happen
        Exception.raiseInternal "invalid fn" []



    // Field access: a.b.c.d
    | SynExpr.LongIdent(_, SynLongIdent(names, _, _), _, _) ->
      match names with
      | [] -> Exception.raiseInternal "empty list in LongIdent" []
      | var :: fields ->
        List.fold
          (PT.EVariable(gid (), var.idText))
          (fun acc (field : Ident) ->
            PT.EFieldAccess(id, acc, nameOrBlank field.idText))
          fields

    // (...).a.b
    | SynExpr.DotGet(expr, _, SynLongIdent(fields, _, _), _) ->
      List.fold
        (c expr)
        (fun acc (field : Ident) ->
          PT.EFieldAccess(id, acc, nameOrBlank field.idText))
        fields

    // Lambdas
    | SynExpr.Lambda(_, false, SynSimplePats.SimplePats(outerVars, _), body, _, _, _) ->
      let rec extractVarsAndBody expr =
        match expr with
        // The 2nd param indicates this was part of a lambda
        | SynExpr.Lambda(_, true, SynSimplePats.SimplePats(vars, _), body, _, _, _) ->
          let nestedVars, body = extractVarsAndBody body
          vars @ nestedVars, body
        // The 2nd param indicates this was not nested
        | SynExpr.Lambda(_, false, SynSimplePats.SimplePats(vars, _), body, _, _, _) ->
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


    // if/else expressions
    | SynExpr.IfThenElse(cond, thenExpr, Some elseExpr, _, _, _, _) ->
      PT.EIf(id, c cond, c thenExpr, c elseExpr)

    // if (no else) expression
    | SynExpr.IfThenElse(cond, thenExpr, None, _, _, _, _) ->
      PT.EIf(id, c cond, c thenExpr, PT.EUnit(gid ()))


    // `let` bindings
    | SynExpr.LetOrUse(_,
                       _,
                       [ SynBinding(_, _, _, _, _, _, _, pat, _, rhs, _, _, _) ],
                       body,
                       _,
                       _) ->

      PT.ELet(id, LetPattern.fromSynPat pat, c rhs, c body)


    // `match` exprs:
    //
    // ```fsharp
    // match Some 1 with // 'cond'
    // | None -> ... // cases
    // | Some 1 -> ...
    // | ...
    | SynExpr.Match(_, cond, cases, _, _) ->
      let convertCase
        (SynMatchClause(pat, _, expr, _, _, _) : SynMatchClause)
        : PT.MatchPattern * PT.Expr =
        (MatchPattern.fromSynPat pat, c expr)
      PT.EMatch(id, c cond, List.map convertCase cases)


    // Parens (eg `(5)`)
    | SynExpr.Paren(expr, _, _, _) -> c expr // just unwrap

    // "Typed" (we don't use this)
    | SynExpr.Typed(expr, _, _) -> c expr // just unwrap

    // Do (eg do ())
    | SynExpr.Do(expr, _) -> c expr // just unwrap


    // Sequential code: (a; b) -> let _ = a in b
    | SynExpr.Sequential(_, _, a, b, _) ->
      PT.ELet(id, PT.LPVariable(gid (), "_"), c a, c b)


    // Pipes (|>)
    // nested pipes - F# uses 2 Apps to represent a pipe. The outer app has an
    // op_PipeRight, and the inner app has two arguments. Those arguments might
    // also be pipes
    | SynExpr.App(_, _, SynExpr.Ident pipe, SynExpr.App(_, _, nestedPipes, arg, _), _)
    | SynExpr.App(_,
                  _,
                  SynExpr.LongIdent(_, SynLongIdent([ pipe ], _, _), _, _),
                  SynExpr.App(_, _, nestedPipes, arg, _),
                  _) when pipe.idText = "op_PipeRight" ->
      match c nestedPipes with
      | PT.EPipe(id, arg1, PipePlaceholder, []) ->
        // when we just built the lowest, the second one goes here
        PT.EPipe(id, arg1, synToPipeExpr arg, [])
      | PT.EPipe(id, arg1, arg2, rest) ->
        PT.EPipe(id, arg1, arg2, rest @ [ synToPipeExpr arg ])
      // Exception.raiseInternal $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
      | other ->
        Exception.raiseInternal
          $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
          [ "arg", arg ]

    // the very bottom on the pipe chain, this is the first and second expressions
    | SynExpr.App(_, _, SynExpr.Ident pipe, expr, _)
    | SynExpr.App(_,
                  _,
                  SynExpr.LongIdent(_, SynLongIdent([ pipe ], _, _), _, _),
                  expr,
                  _) when pipe.idText = "op_PipeRight" ->
      // the very bottom on the pipe chain, this is just the first expression
      PT.EPipe(id, c expr, pipePlaceholder, [])

    // e.g. MyMod.MyRecord
    | SynExpr.App(_,
                  _,
                  SynExpr.TypeApp(name, _, typeArgs, _, _, _, _),
                  (SynExpr.Record _ as expr),
                  _) ->
      if List.length typeArgs <> 0 then
        Exception.raiseInternal "Record should not have type args" [ "expr", expr ]

      match c expr with
      | PT.ERecord(id, typeName, fields) -> PT.ERecord(id, typeName, fields)
      | PT.EDict(id, fields) -> PT.EDict(id, fields)
      | _ -> Exception.raiseInternal "Not an expected record" [ "expr", expr ]


    // Records: MyRecord { x = 5 } or Dict { x = 5 }
    | SynExpr.App(_, _, name, SynExpr.Record(_, _, fields, _), _) when
      List.all (fun n -> String.isCapitalized n) (parseNames name)
      ->
      let names = parseNames name
      let typename =
        List.last names |> Exception.unwrapOptionInternal "empty list" []
      let (typ, version) = parseTypeName typename
      let modules = List.initial names |> Option.unwrap []

      let fields =
        fields
        |> List.map (fun field ->
          match field with
          | SynExprRecordField((SynLongIdent([ name ], _, _), _), _, Some expr, _) ->
            (nameOrBlank name.idText, c expr)
          | f -> Exception.raiseInternal "Not an expected field" [ "field", f ])

      if names = [ "Dict" ] then
        PT.EDict(id, fields)
      else
        // We use a user name here, and we'll resolve it in the post pass when we
        // have the types available
        let typeName = PT.TypeName.fqUserProgram modules typ version
        PT.ERecord(id, typeName, fields)

    // Record update: {myRecord with x = 5 }
    | SynExpr.Record(_, Some(baseRecord, _), updates, _) ->
      let updates =
        updates
        |> List.map (fun field ->
          match field with
          | SynExprRecordField((SynLongIdent([ name ], _, _), _), _, Some expr, _) ->
            (nameOrBlank name.idText, c expr)
          | f ->
            Exception.raiseInternal "Not an expected updates field" [ "field", f ])
      PT.ERecordUpdate(id, c baseRecord, updates)

    // Callers with multiple args are encoded as apps wrapping other apps.
    | SynExpr.App(_, _, funcExpr, arg, _) -> // function application (binops and fncalls)
      match c funcExpr with
      | PT.EApply(id, name, typeArgs, args) ->
        PT.EApply(id, name, typeArgs, args @ [ c arg ])
      | PT.EInfix(id, op, Placeholder, arg2) -> PT.EInfix(id, op, c arg, arg2)
      | PT.EInfix(id, op, arg1, Placeholder) -> PT.EInfix(id, op, arg1, c arg)
      // A pipe with one entry
      | PT.EPipe(id, arg1, PipePlaceholder, []) ->
        PT.EPipe(id, arg1, synToPipeExpr arg, [])
      // A pipe with more than one entry
      | PT.EPipe(id, arg1, arg2, rest) ->
        PT.EPipe(id, arg1, arg2, rest @ [ synToPipeExpr arg ])
      | PT.EVariable(id, name) ->
        parseFn name
        |> Option.map (fun (name, version) ->
          PT.EApply(
            id,
            PT.FnTargetName(PT.FnName.fqUserProgram [] name version),
            [],
            [ c arg ]
          ))
        |> Option.orElseWith (fun () ->
          parseEnum name
          |> Option.map (fun name ->
            PT.EEnum(
              id,
              PT.TypeName.fqUserProgram [] name 0,
              name,
              convertEnumArg arg
            )))
        |> Exception.unwrapOptionInternal
          "Unsupported function call"
          [ "fnCall expr", funcExpr
            "converted specific fncall exp", c funcExpr
            "argument", arg ]
      // Enums
      | PT.EEnum(id, typeName, caseName, fields) ->
        PT.EEnum(id, typeName, caseName, fields @ convertEnumArg arg)

      | e ->
        Exception.raiseInternal
          "Unsupported expression in app"
          [ "fnCall expr", funcExpr
            "converted specific fncall exp", e
            "argument", arg ]


    // Error handling
    | SynExpr.FromParseError _ as expr ->
      Exception.raiseInternal "There was a parser error parsing" [ "expr", expr ]
    | expr ->
      Exception.raiseInternal
        "Unsupported expression in parser"
        [ "ast", ast; "expr", expr ]

  let fromSynExpr (ast : SynExpr) : PT.Expr =
    try
      fromSynExpr' ast
    with e ->
      print e.Message
      print (string ast)
      reraise ()

  // Second pass of parsing, fixing the thing it's impossible to get right on the
  // first pass, such as whether function names are user or stdlib names. Parse the
  // whole program once, and then run this on any expressions, passing in User types
  // and functions. It converts user types that are not in the list to Stdlib types.
  // TODO: we need some sort of unambiguous way to refer to user types
  let resolveNames
    (userFunctions : Set<PT.FnName.UserProgram>)
    (userTypes : Set<PT.TypeName.UserProgram>)
    (e : PT.Expr)
    : PT.Expr =
    let resolvePipeExprNames =
      (fun e ->
        match e with
        | PT.EPipeFnCall(id, name, typeArgs, args) ->
          PT.EPipeFnCall(id, name, typeArgs, args)
        | PT.EPipeEnum(id, typeName, caseName, fields) ->
          PT.EPipeEnum(
            id,
            TypeName.resolveNames userTypes typeName,
            caseName,
            fields
          )
        // pipes with variables might be fn calls
        | PT.EPipeVariable(id, name) ->
          match parseFn name with
          | Some(name, version) ->
            if
              Set.contains (PT.FnName.userProgram [] name version) userFunctions
            then
              PT.EPipeFnCall(id, PT.FnName.fqUserProgram [] name version, [], [])
            else
              e
          | None -> e
        | _ -> e)

    LibExecution.ProgramTypesAst.preTraversal
      identity
      resolvePipeExprNames
      identity
      (TypeName.resolveNames userTypes)
      (FnName.resolveNames userFunctions)
      identity
      identity
      e

module Function =
  type Parameter = { name : string; typ : PT.TypeReference }

  type T =
    { name : string
      version : int
      parameters : List<Parameter>
      typeParams : List<string>
      returnType : PT.TypeReference
      body : PT.Expr }


  let rec parseParamPattern (pat : SynPat) : Parameter =
    let r = parseParamPattern

    match pat with
    | SynPat.Paren(pat, _) -> r pat

    | SynPat.Const(SynConst.Unit, _) -> { name = "unit"; typ = PT.TUnit }

    | SynPat.Typed(SynPat.Named(SynIdent(id, _), _, _, _), typ, _) ->
      { name = id.idText; typ = TypeReference.fromSynType typ }

    | SynPat.Typed(SynPat.Typed _ as nested,
                   SynType.App(SynType.LongIdent(SynLongIdent(names, _, _)),
                               _,
                               args,
                               _,
                               _,
                               _,
                               _),
                   _) ->
      let nested = r nested
      { name = nested.name; typ = TypeReference.fromNamesAndTypeArgs names args }

    | _ -> Exception.raiseInternal "Unsupported paramPattern" [ "pat", pat ]

  let parseReturnInfo
    (returnInfo : Option<SynBindingReturnInfo>)
    : PT.TypeReference =
    match returnInfo with
    | Some(SynBindingReturnInfo(typeName, _, _, _)) ->
      TypeReference.fromSynType typeName
    | None ->
      Exception.raiseInternal
        "Functions must have return types specified"
        [ "returnInfo", returnInfo ]


  let private parseSignature
    (pat : SynPat)
    : string * List<string> * List<Parameter> =
    match pat with
    | SynPat.LongIdent(SynLongIdent([ name ], _, _), _, typeArgPats, argPats, _, _) ->
      let typeParams =
        match typeArgPats with
        | None -> []
        | Some(SynValTyparDecls(pats, _)) -> SimpleTypeArgs.fromSynTyparDecls pats

      let parameters =
        match argPats with
        | SynArgPats.Pats pats -> List.map parseParamPattern pats

        | SynArgPats.NamePatPairs _ ->
          Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]

      (name.idText, typeParams, parameters)

    | _ -> Exception.raiseInternal "Unsupported pattern" [ "pat", pat ]


  let fromSynBinding (binding : SynBinding) : T =
    match binding with
    | SynBinding(_, _, _, _, _, _, _, pat, returnInfo, expr, _, _, _) ->
      let (name, typeParams, parameters) = parseSignature pat
      let returnType = parseReturnInfo returnInfo
      let (name, version) =
        Expr.parseFn name
        |> Exception.unwrapOptionInternal
          "invalid fn name"
          [ "name", name; "binding", binding ]
      { name = name
        version = version
        typeParams = typeParams
        parameters = parameters
        returnType = returnType
        body = Expr.fromSynExpr expr }

module UserFunction =
  let fromSynBinding (b : SynBinding) : PT.UserFunction.T =
    let f = Function.fromSynBinding b
    { tlid = gid ()

      name = PT.FnName.userProgram [] f.name f.version
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> List.map (fun p -> { name = p.name; description = ""; typ = p.typ })
      returnType = f.returnType
      description = ""
      deprecated = PT.NotDeprecated
      body = f.body }

  let resolveNames
    (userFunctions : Set<PT.FnName.UserProgram>)
    (userTypes : Set<PT.TypeName.UserProgram>)
    (f : PT.UserFunction.T)
    : PT.UserFunction.T =
    { tlid = f.tlid
      name = f.name
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> List.map (fun p ->
          { p with typ = TypeReference.resolveNames userTypes p.typ })
      returnType = TypeReference.resolveNames userTypes f.returnType
      description = f.description
      deprecated = f.deprecated
      body = Expr.resolveNames userFunctions userTypes f.body }


module PackageFn =
  let fromSynBinding
    (owner : string)
    (modules : NonEmptyList<string>)
    (b : SynBinding)
    : PT.PackageFn.T =
    let f = Function.fromSynBinding b
    { tlid = gid ()
      id = System.Guid.NewGuid()
      name = PT.FnName.package owner modules f.name f.version
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> List.map (fun p -> { name = p.name; description = ""; typ = p.typ })
      returnType = f.returnType
      description = ""
      deprecated = PT.NotDeprecated
      body = f.body }

  let resolveNames (f : PT.PackageFn.T) : PT.PackageFn.T =
    { tlid = f.tlid
      id = f.id
      name = f.name
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> List.map (fun p ->
          { p with typ = TypeReference.resolveNames Set.empty p.typ })
      returnType = TypeReference.resolveNames Set.empty f.returnType
      description = f.description
      deprecated = f.deprecated
      body = Expr.resolveNames Set.empty Set.empty f.body }


module CustomType =
  module EnumCase =
    let private parseField (typ : SynField) : PT.CustomType.EnumField =
      match typ with
      | SynField(_, _, fieldName, typ, _, _, _, _, _) ->
        { typ = TypeReference.fromSynType typ
          label = fieldName |> Option.map (fun id -> id.idText)
          description = "" }

    let parseCase (case : SynUnionCase) : PT.CustomType.EnumCase =
      match case with
      | SynUnionCase(_, SynIdent(id, _), typ, _, _, _, _) ->
        match typ with
        | SynUnionCaseKind.Fields fields ->
          { name = id.idText; fields = List.map parseField fields; description = "" }
        | _ -> Exception.raiseInternal $"Unsupported enum case" [ "case", case ]

    let resolveNames
      (userTypes : Set<PT.TypeName.UserProgram>)
      (t : PT.CustomType.EnumCase)
      : PT.CustomType.EnumCase =
      { name = t.name
        fields =
          t.fields
          |> List.map (fun f ->
            { f with typ = TypeReference.resolveNames userTypes f.typ })
        description = t.description }


  module RecordField =
    let parseField (field : SynField) : PT.CustomType.RecordField =
      match field with
      | SynField(_, _, Some id, typ, _, _, _, _, _) ->
        { name = id.idText; typ = TypeReference.fromSynType typ; description = "" }
      | _ -> Exception.raiseInternal $"Unsupported field" [ "field", field ]

    let resolveNames
      (userTypes : Set<PT.TypeName.UserProgram>)
      (t : PT.CustomType.RecordField)
      : PT.CustomType.RecordField =
      { name = t.name
        typ = TypeReference.resolveNames userTypes t.typ
        description = t.description }

  let fromFields typeDef (fields : List<SynField>) : PT.CustomType.T =
    match fields with
    | [] ->
      Exception.raiseInternal
        $"Unsupported record type with no fields"
        [ "typeDef", typeDef ]
    | firstField :: additionalFields ->

      PT.CustomType.Record(
        RecordField.parseField firstField,
        List.map RecordField.parseField additionalFields
      )

  let fromCases typeDef (cases : List<SynUnionCase>) : PT.CustomType.T =
    let firstCase, additionalCases =
      match cases with
      | [] ->
        Exception.raiseInternal
          $"Can't parse enum without any cases"
          [ "typeDef", typeDef ]
      | firstCase :: additionalCases -> firstCase, additionalCases

    PT.CustomType.Enum(
      EnumCase.parseCase firstCase,
      List.map EnumCase.parseCase additionalCases
    )


  let fromSynTypeDefn
    (typeDef : SynTypeDefn)
    : (List<string> * List<string> * PT.CustomType.T) =
    match typeDef with
    | SynTypeDefn(SynComponentInfo(_, typeParams, _, ids, _, _, _, _),
                  SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.TypeAbbrev(_, typ, _),
                                         _),
                  _,
                  _,
                  _,
                  _) ->
      SimpleTypeArgs.fromSynTyparDecls typeParams,
      ids |> List.map string,
      PT.CustomType.Alias(TypeReference.fromSynType typ)

    | SynTypeDefn(SynComponentInfo(_, typeParams, _, ids, _, _, _, _),
                  SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Record(_, fields, _),
                                         _),
                  _,
                  _,
                  _,
                  _) ->
      SimpleTypeArgs.fromSynTyparDecls typeParams,
      ids |> List.map string,
      fromFields typeDef fields

    | SynTypeDefn(SynComponentInfo(_, typeParams, _, ids, _, _, _, _),
                  SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_, cases, _), _),
                  _,
                  _,
                  _,
                  _) ->
      SimpleTypeArgs.fromSynTyparDecls typeParams,
      ids |> List.map string,
      fromCases typeDef cases
    | _ ->
      Exception.raiseInternal $"Unsupported type definition" [ "typeDef", typeDef ]

  let resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (t : PT.CustomType.T)
    : PT.CustomType.T =
    match t with
    | PT.CustomType.Enum(firstCase, additionalCases) ->
      PT.CustomType.Enum(
        EnumCase.resolveNames userTypes firstCase,
        additionalCases |> List.map (EnumCase.resolveNames userTypes)
      )
    | PT.CustomType.Record(firstField, additionalFields) ->
      PT.CustomType.Record(
        RecordField.resolveNames userTypes firstField,
        additionalFields |> List.map (RecordField.resolveNames userTypes)
      )
    | PT.CustomType.Alias typ ->
      PT.CustomType.Alias(TypeReference.resolveNames userTypes typ)


module UserType =
  let fromSynTypeDefn (typeDef : SynTypeDefn) : PT.UserType.T =
    let (typeParamNames, names, definition) = CustomType.fromSynTypeDefn typeDef
    let (name, version) =
      List.last names
      |> Exception.unwrapOptionInternal
        "user type should have name"
        [ "typeDef", typeDef ]
      |> Expr.parseTypeName
    let modules = names |> List.initial |> Option.unwrap []

    { tlid = gid ()
      name = PT.TypeName.userProgram modules name version
      typeParams = typeParamNames
      definition = definition }

  let resolveNames
    (userTypes : Set<PT.TypeName.UserProgram>)
    (t : PT.UserType.T)
    : PT.UserType.T =
    { tlid = t.tlid
      name = t.name
      typeParams = t.typeParams
      definition = CustomType.resolveNames userTypes t.definition }

module PackageType =
  let fromSynTypeDefn
    (owner : string)
    (modules : NonEmptyList<string>)
    (typeDef : SynTypeDefn)
    : PT.PackageType.T =
    let (typeParmNames, names, definition) = CustomType.fromSynTypeDefn typeDef
    let (name, version) =
      List.last names
      |> Exception.unwrapOptionInternal
        "user type should have name"
        [ "typeDef", typeDef ]
      |> Expr.parseTypeName
    { tlid = gid ()
      id = System.Guid.NewGuid()
      name = PT.TypeName.package owner modules name version
      description = ""
      deprecated = PT.NotDeprecated
      typeParams = typeParmNames
      definition = definition }

  let resolveNames (f : PT.PackageType.T) : PT.PackageType.T =
    { tlid = f.tlid
      id = f.id
      name = f.name
      description = f.description
      deprecated = f.deprecated
      typeParams = f.typeParams
      definition = CustomType.resolveNames Set.empty f.definition }


/// Returns an incomplete parse of a PT expression. Requires calling
/// Expr.resolveNames before using
// TODO it's hard to use the type system here since there's a lot of places we stash
// PT.Expr, but that's even more reason to try and prevent partial parses.
let initialParse (filename : string) (code : string) : PT.Expr =
  code
  |> Utils.parseAsFSharpSourceFile filename
  |> Utils.singleExprFromImplFile
  |> Expr.fromSynExpr

// Shortcut function for tests that ignore user functions and types
let parseIgnoringUser (filename : string) (code : string) : PT.Expr =
  code |> initialParse filename |> Expr.resolveNames Set.empty Set.empty

let parseRTExpr
  (fns : Set<PT.FnName.UserProgram>)
  (types : Set<PT.TypeName.UserProgram>)
  (filename : string)
  (code : string)
  : LibExecution.RuntimeTypes.Expr =
  code
  |> initialParse filename
  |> Expr.resolveNames fns types
  |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT
