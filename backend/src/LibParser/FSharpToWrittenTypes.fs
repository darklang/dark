module internal LibParser.FSharpToWrittenTypes

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

open Prelude

module WT = WrittenTypes
module PT = LibExecution.ProgramTypes

open Utils

let (|LongIdentPat|_|) (names : List<string>) (input : LongIdent) =
  if longIdentToList input = names then Some() else None

let (|SynExprLongIdentPat|_|) (names : List<string>) (input : SynExpr) =
  match input with
  | SynExpr.LongIdent(_, SynLongIdent(longIdent, _, _), _, _) when
    longIdentToList longIdent = names
    ->
    Some()
  | _ -> None

let (|IdentExprPat|_|) (input : SynExpr) =
  match input with
  | SynExpr.LongIdent(_, SynLongIdent(longIdent, _, _), _, _) ->
    Some(longIdentToList longIdent)
  | SynExpr.Ident name -> Some [ name.idText ]
  | _ -> None

let parseExprName (e : SynExpr) : NEList<string> =
  match e with
  | SynExpr.LongIdent(_, SynLongIdent(head :: tail, _, _), _, _) ->
    NEList.ofList head tail |> NEList.map (fun i -> i.idText)
  | SynExpr.Ident name -> NEList.singleton name.idText
  | _ -> Exception.raiseInternal "Bad format in expr name" [ "e", e ]

let parseTypeName (t : SynType) : NEList<string> =
  match t with
  | SynType.LongIdent(SynLongIdent(head :: tail, _, _)) ->
    NEList.ofList head tail |> NEList.map (fun i -> i.idText)
  | _ -> Exception.raiseInternal "Bad format in type name" [ "t", t ]



module TypeReference =

  let rec fromNamesAndTypeArgs
    (names : NEList<string>)
    (typeArgs : List<SynType>)
    : WT.TypeReference =
    let modules = NEList.initial names
    let name = NEList.last names
    match modules, name, typeArgs with
    // no type args
    | [], "Bool", [] -> WT.TBool
    | [], "Bytes", [] -> WT.TBytes
    | [], "Int", [] -> WT.TInt
    | [], "String", [] -> WT.TString
    | [], "Char", [] -> WT.TChar
    | [], "Float", [] -> WT.TFloat
    | [], "DateTime", [] -> WT.TDateTime
    | [], "Uuid", [] -> WT.TUuid
    | [], "Unit", [] -> WT.TUnit

    // with type args
    | [], "List", [ arg ] -> WT.TList(fromSynType arg)
    | [], "Dict", [ valArg ] -> WT.TDict(fromSynType valArg)
    // TYPESCLEANUP - don't use word Tuple here
    | [], "Tuple", first :: second :: theRest ->
      WT.TTuple(fromSynType first, fromSynType second, List.map fromSynType theRest)
    | _ -> WT.TCustomType(WT.Unresolved(names), List.map fromSynType typeArgs)

  and fromSynType (typ : SynType) : WT.TypeReference =
    let c = fromSynType

    match typ with
    | SynType.Paren(t, _) -> c t

    // Variable types (i.e. "generic types")
    // e.g. `'a` in `'a -> bool`
    | SynType.Var(SynTypar(id, _, _), _) -> WT.TVariable(id.idText)

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
        WT.TTuple(c first, c second, List.map c theRest)

    // Function types
    // e.g. `'a -> bool` in `let friends (lambda: ('a -> bool)) = ...`
    | SynType.Fun(arg, ret, _, _) -> WT.TFn(NEList.singleton (c arg), c ret)


    // Named types. covers:
    // - built-in F# types like `bool`
    // - Stdlib-defined types
    // - User-defined types
    | SynType.App(name, _, typeArgs, _, _, _, range) ->
      let name = parseTypeName name
      fromNamesAndTypeArgs name typeArgs

    | SynType.LongIdent _ as name ->
      let name = parseTypeName name
      fromNamesAndTypeArgs name []

    | _ -> Exception.raiseInternal $"Unsupported type" [ "type", typ ]


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
  let rec fromSynPat (pat : SynPat) : WT.LetPattern =
    let mapPat = fromSynPat

    match pat with
    | SynPat.Paren(subPat, _) -> mapPat subPat
    | SynPat.Wild(_) -> WT.LPVariable(gid (), "_")
    | SynPat.Named(SynIdent(name, _), _, _, _) -> WT.LPVariable(gid (), name.idText)
    | SynPat.Const(SynConst.Unit, _) -> WT.LPUnit(gid ())

    | SynPat.Tuple(_, (first :: second :: theRest), _) ->
      WT.LPTuple(gid (), mapPat first, mapPat second, List.map mapPat theRest)

    | _ ->
      Exception.raiseInternal "Unsupported let or use expr pat type" [ "pat", pat ]


module MatchPattern =
  let rec fromSynPat (pat : SynPat) : WT.MatchPattern =
    let id = gid ()
    let r = fromSynPat

    let convertEnumArg (ast : SynPat) : List<WT.MatchPattern> =
      // if the arg is a tuple with one paren around it, it's just arguments to the
      // enum. But if it has two parens around it, it's a single tuple.
      // eg: (Foo(1, 2)) vs (Foo((1, 2)))
      match ast with
      | SynPat.Paren(SynPat.Paren(SynPat.Tuple(_, t1 :: t2 :: trest, _), _), _) ->
        [ WT.MPTuple(gid (), r t1, r t2, List.map r trest) ]
      | SynPat.Paren(SynPat.Tuple(_, args, _), _) -> List.map r args
      | SynPat.Tuple(_, args, _) -> List.map r args
      | e -> [ r e ]

    match pat with
    | SynPat.Named(SynIdent(name, _), _, _, _) -> WT.MPVariable(id, name.idText)
    | SynPat.Wild _ -> WT.MPVariable(gid (), "_") // wildcard, not blank
    | SynPat.Const(SynConst.Int32 n, _) -> WT.MPInt(id, n)
    | SynPat.Const(SynConst.Int64 n, _) -> WT.MPInt(id, int64 n)
    | SynPat.Const(SynConst.UInt64 n, _) -> WT.MPInt(id, int64 n)
    | SynPat.Const(SynConst.Char c, _) -> WT.MPChar(id, string c)
    | SynPat.Const(SynConst.Bool b, _) -> WT.MPBool(id, b)
    | SynPat.Const(SynConst.Unit, _) -> WT.MPUnit(id)
    | SynPat.Null _ ->
      Exception.raiseInternal "null pattern not supported, use `()`" [ "pat", pat ]
    | SynPat.Paren(pat, _) -> r pat
    | SynPat.Const(SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      WT.MPFloat(id, sign, whole, fraction)
    | SynPat.Const(SynConst.String(s, _, _), _) ->
      WT.MPString(id, String.normalize s)
    | SynPat.LongIdent(SynLongIdent(names, _, _), _, _, SynArgPats.Pats args, _, _) ->
      let enumName =
        List.last names |> Exception.unwrapOptionInternal "missing enum name" []
      let modules = List.initial names |> List.map (fun i -> i.idText)
      if modules <> [] then
        Exception.raiseInternal
          "Module in enum pattern casename. Only use the casename in Enum patterns"
          [ "pat", pat ]
      let args = List.map convertEnumArg args |> List.concat
      WT.MPEnum(id, enumName.idText, args)
    | SynPat.Tuple(_isStruct, (first :: second :: theRest), _range) ->
      WT.MPTuple(id, r first, r second, List.map r theRest)
    | SynPat.ListCons(headPat, tailPat, _, _) ->
      WT.MPListCons(id, r headPat, r tailPat)
    | SynPat.ArrayOrList(_, pats, _) -> WT.MPList(id, List.map r pats)
    | _ -> Exception.raiseInternal "unhandled pattern" [ "pattern", pat ]


module Expr =
  // CLEANUP - blanks here aren't allowed
  let private nameOrBlank (v : string) : string = if v = "___" then "" else v

  let parseFn (fnName : string) : Result<string * int, string> =
    match fnName with
    | Regex.Regex "^([a-z][a-z0-9A-Z]*[']?)_v(\d+)$" [ name; version ] ->
      Ok(name, (int version))
    | Regex.Regex "^([a-z][a-z0-9A-Z]*[']?)$" [ name ] -> Ok(name, 0)
    | _ -> Error "Bad format in fn name"

  let parseEnum (enumName : string) : Option<string> =
    // No version on the Enum case, that's on the type
    match enumName with
    | Regex.Regex "^([A-Z][a-z0-9A-Z]*)$" [ name ] -> Some name
    | _ -> None

  let parseTypeName (typeName : string) : Result<string * int, string> =
    match typeName with
    | Regex.Regex "^([A-Z][a-z0-9A-Z]*[']?)_v(\d+)$" [ name; version ] ->
      Ok(name, (int version))
    | Regex.Regex "^([A-Z][a-z0-9A-Z]*[']?)$" [ name ] -> Ok(name, 0)
    | _ -> Error "Bad format in type name"


  let private ops =
    Map.ofList
      [ ("op_Addition", WT.ArithmeticPlus)
        ("op_Subtraction", WT.ArithmeticMinus)
        ("op_Multiply", WT.ArithmeticMultiply)
        ("op_Division", WT.ArithmeticDivide)
        ("op_Modulus", WT.ArithmeticModulo)
        ("op_Concatenate", WT.ArithmeticPower)
        ("op_GreaterThan", WT.ComparisonGreaterThan)
        ("op_GreaterThanOrEqual", WT.ComparisonGreaterThanOrEqual)
        ("op_LessThan", WT.ComparisonLessThan)
        ("op_LessThanOrEqual", WT.ComparisonLessThanOrEqual)
        ("op_EqualsEquals", WT.ComparisonEquals)
        ("op_BangEquals", WT.ComparisonNotEquals)
        ("op_PlusPlus", WT.StringConcat) ]

  let rec fromSynExpr' (ast : SynExpr) : WT.Expr =
    let c = fromSynExpr'

    let convertEnumArg (ast : SynExpr) : List<WT.Expr> =
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
                      _) -> [ WT.ETuple(gid (), c t1, c t2, List.map c trest) ]
      | SynExpr.Paren(SynExpr.Tuple(_, args, _, _), _, _, _) -> List.map c args
      | SynExpr.Tuple(_, args, _, _) -> List.map c args
      | e -> [ c e ]


    let convertLambdaVar (var : SynSimplePat) : string =
      match var with
      | SynSimplePat.Id(name, _, _, _, _, _) -> nameOrBlank name.idText
      | _ -> Exception.raiseInternal "unsupported lambdaVar" [ "var", var ]

    let synToPipeExpr (e : SynExpr) : WT.PipeExpr =
      match c e with
      | WT.EApply(id,
                  WT.EFnName(_id, name),
                  typeArgs,
                  { head = WT.EPlaceHolder; tail = [] }) ->
        WT.EPipeFnCall(id, name, typeArgs, [])
      | WT.EApply(id, WT.EFnName(_id, name), typeArgs, args) ->
        WT.EPipeFnCall(id, name, typeArgs, NEList.toList args)
      | WT.EInfix(id, op, WT.EPlaceHolder, arg2) -> WT.EPipeInfix(id, op, arg2)
      | WT.EInfix(id, op, arg1, WT.EPlaceHolder) -> WT.EPipeInfix(id, op, arg1)
      | WT.EEnum(id, typeName, caseName, fields) ->
        WT.EPipeEnum(id, typeName, caseName, fields)
      | WT.EVariable(id, name) -> WT.EPipeVariableOrUserFunction(id, name)
      | WT.ELambda(id, vars, body) -> WT.EPipeLambda(id, vars, body)
      | other ->
        Exception.raiseInternal
          "Expected a function, got something else."
          [ "expr", other ]


    let id = gid ()

    match ast with

    // Literals (ints, chars, bools, etc)
    | SynExpr.Null _ ->
      Exception.raiseInternal "null not supported, use `()`" [ "ast", ast ]
    | SynExpr.Const(SynConst.Unit _, _) -> WT.EUnit id
    | SynExpr.Const(SynConst.Int32 n, _) -> WT.EInt(id, n)
    | SynExpr.Const(SynConst.Int64 n, _) -> WT.EInt(id, int64 n)
    | SynExpr.Const(SynConst.UInt64 n, _) -> WT.EInt(id, int64 n)
    | SynExpr.Const(SynConst.Char c, _) -> WT.EChar(id, string c)
    | SynExpr.Const(SynConst.Bool b, _) -> WT.EBool(id, b)
    | SynExpr.Const(SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      WT.EFloat(id, sign, whole, fraction)


    // Strings
    | SynExpr.Const(SynConst.String(s, _, _), _) ->
      WT.EString(id, [ WT.StringText(String.normalize s) ])
    | SynExpr.InterpolatedString(parts, _, _) ->
      let parts =
        parts
        |> List.filterMap (function
          | SynInterpolatedStringPart.String("", _) -> None
          | SynInterpolatedStringPart.String(s, _) ->
            Some(WT.StringText(String.normalize s))
          | SynInterpolatedStringPart.FillExpr(e, _) ->
            Some(WT.StringInterpolation(c e)))
      WT.EString(id, parts)


    // Simple identifiers/operators like `==`
    | SynExpr.LongIdent(_, SynLongIdent([ ident ], _, _), _, _) when
      Map.containsKey ident.idText ops
      ->
      let op =
        Map.get ident.idText ops
        |> Exception.unwrapOptionInternal
          "can't find operation"
          [ "name", ident.idText ]
      WT.EInfix(id, WT.InfixFnCall op, WT.EPlaceHolder, WT.EPlaceHolder)


    // Binary Ops: && / ||
    | SynExprLongIdentPat [ "op_BooleanAnd" ] ->
      WT.EInfix(id, WT.BinOp WT.BinOpAnd, WT.EPlaceHolder, WT.EPlaceHolder)

    | SynExprLongIdentPat [ "op_BooleanOr" ] ->
      WT.EInfix(id, WT.BinOp WT.BinOpOr, WT.EPlaceHolder, WT.EPlaceHolder)

    // Negation
    | SynExprLongIdentPat [ "op_UnaryNegation" ] ->
      WT.EApply(
        id,
        WT.EFnName(gid (), WT.KnownBuiltin([ "Int" ], "negate", 0)),
        [],
        NEList.singleton WT.EPlaceHolder
      )

    // Variables
    | SynExpr.Ident ident -> WT.EVariable(id, ident.idText)

    // List literals
    | SynExpr.ArrayOrList(_, exprs, _) -> WT.EList(id, exprs |> List.map c)

    // a literal list is sometimes made up of nested Sequentials
    | SynExpr.ArrayOrListComputed(_, (SynExpr.Sequential _ as seq), _) ->
      let rec seqAsList expr : List<SynExpr> =
        match expr with
        | SynExpr.Sequential(_, _, expr1, expr2, _) -> expr1 :: seqAsList expr2
        | _ -> [ expr ]
      WT.EList(id, seq |> seqAsList |> List.map c)

    | SynExpr.ArrayOrListComputed(_,
                                  SynExpr.Tuple(_, first :: second :: theRest, _, _),
                                  _) ->
      WT.ETuple(id, c first, c second, List.map c theRest)

    | SynExpr.ArrayOrListComputed(_, expr, _) -> WT.EList(id, [ c expr ])


    // Tuples
    | SynExpr.Tuple(_, first :: second :: rest, _, _) ->
      WT.ETuple(id, c first, c second, List.map c rest)

    // Enum/FnCalls - e.g. `Result.Ok` or `Result.mapSecond`
    | IdentExprPat(head :: tail) when
      (head :: tail |> List.initial |> List.all String.isCapitalized)
      ->
      let names = NEList.ofList head tail
      let (modules, name) = NEList.splitLast names

      if String.isCapitalized name then
        WT.EEnum(gid (), modules, name, [])
      else
        WT.EApply(
          gid (),
          WT.EFnName(gid (), WT.Unresolved(names)),
          [],
          NEList.singleton WT.EPlaceHolder
        )


    // Enums are expected to be fully qualified
    | SynExpr.Ident name -> WT.EVariable(id, name.idText)


    // e.g. `Json.serialize<T>`
    // e.g. `Module1.Module2.fnName<String>`
    | SynExpr.TypeApp(IdentExprPat names, _, typeArgs, _, _, _, _) ->
      let names = NEList.ofListUnsafe "Empty function name" [] names

      let typeArgs =
        typeArgs |> List.map (fun synType -> TypeReference.fromSynType synType)

      WT.EApply(
        gid (),
        WT.EFnName(gid (), WT.Unresolved names),
        typeArgs,
        NEList.singleton WT.EPlaceHolder
      )


    // Field access: a.b.c.d
    | SynExpr.LongIdent(_, SynLongIdent(names, _, _), _, _) ->
      match names with
      | [] -> Exception.raiseInternal "empty list in LongIdent" []
      | var :: fields ->
        List.fold
          (fun acc (field : Ident) ->
            WT.EFieldAccess(id, acc, nameOrBlank field.idText))
          (WT.EVariable(gid (), var.idText))
          fields

    // (...).a.b
    | SynExpr.DotGet(expr, _, SynLongIdent(fields, _, _), _) ->
      List.fold
        (fun acc (field : Ident) ->
          WT.EFieldAccess(id, acc, nameOrBlank field.idText))
        (c expr)
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
      let vars = NEList.ofListUnsafe "Empty lambda vars" [] vars
      WT.ELambda(id, vars, c body)


    // if/else expressions
    | SynExpr.IfThenElse(cond, thenExpr, elseExpr, _, _, _, _) ->
      WT.EIf(id, c cond, c thenExpr, Option.map c elseExpr)


    // `let` bindings
    | SynExpr.LetOrUse(_,
                       _,
                       [ SynBinding(_, _, _, _, _, _, _, pat, _, rhs, _, _, _) ],
                       body,
                       _,
                       _) ->

      WT.ELet(id, LetPattern.fromSynPat pat, c rhs, c body)


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
        : WT.MatchPattern * WT.Expr =
        (MatchPattern.fromSynPat pat, c expr)
      WT.EMatch(id, c cond, List.map convertCase cases)


    // Parens (eg `(5)`)
    | SynExpr.Paren(expr, _, _, _) -> c expr // just unwrap

    // "Typed" (we don't use this)
    | SynExpr.Typed(expr, _, _) -> c expr // just unwrap

    // Do (eg do ())
    | SynExpr.Do(expr, _) -> c expr // just unwrap


    // Sequential code: (a; b) -> let _ = a in b
    | SynExpr.Sequential(_, _, a, b, _) ->
      WT.ELet(id, WT.LPVariable(gid (), "_"), c a, c b)


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
      | WT.EPipe(id, arg1, []) ->
        // when we just built the lowest, the second one goes here
        WT.EPipe(id, arg1, [ synToPipeExpr arg ])
      | WT.EPipe(id, arg1, rest) -> WT.EPipe(id, arg1, rest @ [ synToPipeExpr arg ])
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
      WT.EPipe(id, c expr, [])

    // e.g. MyMod.MyRecord
    | SynExpr.App(_,
                  _,
                  SynExpr.TypeApp(name, _, typeArgs, _, _, _, _),
                  (SynExpr.Record _ as expr),
                  _) ->
      if List.length typeArgs <> 0 then
        Exception.raiseInternal "Record should not have type args" [ "expr", expr ]

      match c expr with
      | WT.ERecord(id, typeName, fields) -> WT.ERecord(id, typeName, fields)
      | WT.EDict(id, fields) -> WT.EDict(id, fields)
      | _ -> Exception.raiseInternal "Not an expected record" [ "expr", expr ]


    // Records: MyRecord { x = 5 } or Dict { x = 5 }
    | SynExpr.App(_, _, name, SynExpr.Record(_, _, fields, _), _) when
      NEList.forall (fun n -> String.isCapitalized n) (parseExprName name)
      ->
      let names = parseExprName name
      let typename = NEList.last names
      let modules = NEList.initial names

      let fields =
        fields
        |> List.map (fun field ->
          match field with
          | SynExprRecordField((SynLongIdent([ name ], _, _), _), _, Some expr, _) ->
            (nameOrBlank name.idText, c expr)
          | f -> Exception.raiseInternal "Not an expected field" [ "field", f ])

      if names = NEList.singleton "Dict" then
        WT.EDict(id, fields)
      else
        WT.ERecord(id, WT.Unresolved names, fields)

    // Record update: {myRecord with x = 5 }
    | SynExpr.Record(_, Some(baseRecord, _), updates, _) ->
      let updates =
        updates
        |> NEList.ofListUnsafe
          "Record updates should not be empty"
          [ "baseRecord", baseRecord ]
        |> NEList.map (fun field ->
          match field with
          | SynExprRecordField((SynLongIdent([ name ], _, _), _), _, Some expr, _) ->
            (nameOrBlank name.idText, c expr)
          | f ->
            Exception.raiseInternal "Not an expected updates field" [ "field", f ])
      WT.ERecordUpdate(id, c baseRecord, updates)

    // Callers with multiple args are encoded as apps wrapping other apps.
    | SynExpr.App(_, _, funcExpr, arg, _) -> // function application (binops and fncalls)
      match c funcExpr with
      | WT.EApply(id, name, typeArgs, { head = WT.EPlaceHolder; tail = [] }) ->
        WT.EApply(id, name, typeArgs, NEList.singleton (c arg))
      | WT.EApply(id, name, typeArgs, args) ->
        WT.EApply(id, name, typeArgs, NEList.pushBack (c arg) args)
      | WT.EInfix(id, op, WT.EPlaceHolder, arg2) -> WT.EInfix(id, op, c arg, arg2)
      | WT.EInfix(id, op, arg1, WT.EPlaceHolder) -> WT.EInfix(id, op, arg1, c arg)
      | WT.EPipe(id, arg1, rest) -> WT.EPipe(id, arg1, rest @ [ synToPipeExpr arg ])
      | WT.EVariable(id, name) ->
        if String.isCapitalized name then
          WT.EEnum(id, [], name, convertEnumArg arg)
        else
          WT.EApply(
            id,
            WT.EFnName(gid (), WT.Unresolved(NEList.singleton name)),
            [],
            NEList.singleton (c arg)
          )
      // Enums
      | WT.EEnum(id, names, caseName, fields) ->
        WT.EEnum(id, names, caseName, fields @ convertEnumArg arg)

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

  let fromSynExpr (ast : SynExpr) : WT.Expr =
    try
      fromSynExpr' ast
    with e ->
      print e.Message
      print (string ast)
      reraise ()

module Function =
  type Parameter = { name : string; typ : WT.TypeReference }

  type T =
    { name : string
      version : int
      parameters : NEList<Parameter>
      typeParams : List<string>
      returnType : WT.TypeReference
      body : WT.Expr }


  let rec parseParamPattern (pat : SynPat) : Parameter =
    let r = parseParamPattern

    match pat with
    | SynPat.Paren(pat, _) -> r pat

    | SynPat.Const(SynConst.Unit, _) -> { name = "unit"; typ = WT.TUnit }

    | SynPat.Typed(SynPat.Named(SynIdent(id, _), _, _, _), typ, _) ->
      { name = id.idText; typ = TypeReference.fromSynType typ }

    | SynPat.Typed(SynPat.Typed _ as nested,
                   SynType.App(name, _, args, _, _, _, _),
                   _) ->
      let name = parseTypeName name
      let nested = r nested

      { name = nested.name; typ = TypeReference.fromNamesAndTypeArgs name args }

    | _ -> Exception.raiseInternal "Unsupported paramPattern" [ "pat", pat ]

  let parseReturnInfo
    (returnInfo : Option<SynBindingReturnInfo>)
    : WT.TypeReference =
    match returnInfo with
    | Some(SynBindingReturnInfo(typeName, _, _, _)) ->
      TypeReference.fromSynType typeName
    | None ->
      Exception.raiseInternal
        "Functions must have return types specified"
        [ "returnInfo", returnInfo ]


  let private parseSignature
    (pat : SynPat)
    : string * List<string> * NEList<Parameter> =
    match pat with
    | SynPat.LongIdent(SynLongIdent([ name ], _, _), _, typeArgPats, argPats, _, _) ->
      let typeParams =
        match typeArgPats with
        | None -> []
        | Some(SynValTyparDecls(pats, _)) -> SimpleTypeArgs.fromSynTyparDecls pats

      let parameters =
        match argPats with
        | SynArgPats.Pats(head :: tail) ->
          NEList.ofList head tail |> NEList.map parseParamPattern

        | SynArgPats.Pats [] ->
          Exception.raiseInternal "No parameters found in function" []

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
        |> Exception.unwrapResultInternal [ "name", name; "binding", binding ]
      { name = name
        version = version
        typeParams = typeParams
        parameters = parameters
        returnType = returnType
        body = Expr.fromSynExpr expr }

module UserFunction =
  let fromSynBinding
    (moduleName : List<string>)
    (b : SynBinding)
    : WT.UserFunction.T =
    let f = Function.fromSynBinding b
    { name = PT.FnName.userProgram moduleName f.name f.version
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> NEList.map (fun p -> { name = p.name; description = ""; typ = p.typ })
      returnType = f.returnType
      description = ""
      body = f.body }

module Constant =

  type T = { name : string; version : int; body : WT.Const }

  let fromSynExpr (expr : SynExpr) : WT.Const =
    let rec c (e : WT.Expr) : WT.Const =
      match e with
      | WT.EUnit _ -> WT.CUnit
      | WT.EInt(_, n) -> WT.CInt n
      | WT.EChar(_, c) -> WT.CChar c
      | WT.EBool(_, b) -> WT.CBool b
      | WT.EFloat(_, sign, whole, fraction) -> WT.CFloat(sign, whole, fraction)
      | WT.EString(_, [ WT.StringText str ]) -> WT.CString str
      | WT.ETuple(_, first, second, rest) ->
        WT.CTuple(c first, c second, List.map c rest)
      | WT.EEnum(_, typeName, caseName, fields) ->
        WT.CEnum(typeName, caseName, List.map c fields)
      | WT.EList(_, items) -> WT.CList(List.map c items)
      | WT.EDict(_, fields) -> WT.CDict(List.map (fun (k, v) -> (k, c v)) fields)
      | _ -> Exception.raiseInternal "Unsupported constant" [ "expr", expr ]
    expr |> Expr.fromSynExpr |> c

  let fromSynBinding (binding : SynBinding) : T =
    match binding with
    | SynBinding(_,
                 _,
                 _,
                 _,
                 _,
                 _,
                 _,
                 SynPat.Named(SynIdent(name, _), _, _, _),
                 _,
                 expr,
                 _,
                 _,
                 _) ->
      match Expr.parseFn name.idText with
      | Ok(name, version) ->
        { name = name; version = version; body = fromSynExpr expr }
      | Error _ ->
        Exception.raiseInternal "Unsupported constant" [ "binding", binding ]
    | _ -> Exception.raiseInternal "Unsupported constant" [ "binding", binding ]


module UserConstant =
  let fromSynBinding
    (moduleName : List<string>)
    (b : SynBinding)
    : WT.UserConstant.T =
    let c = Constant.fromSynBinding b
    { name = PT.ConstantName.userProgram moduleName c.name c.version
      description = ""
      body = c.body }


module PackageFn =
  let fromSynBinding
    (owner : string)
    (modules : List<string>)
    (b : SynBinding)
    : WT.PackageFn.T =
    let f = Function.fromSynBinding b
    { name = PT.FnName.package owner modules f.name f.version
      typeParams = f.typeParams
      parameters =
        f.parameters
        |> NEList.map (fun p -> { name = p.name; description = ""; typ = p.typ })
      returnType = f.returnType
      description = ""
      body = f.body }

module TypeDeclaration =
  module EnumCase =
    let private parseField (typ : SynField) : WT.TypeDeclaration.EnumField =
      match typ with
      | SynField(_, _, fieldName, typ, _, _, _, _, _) ->
        { typ = TypeReference.fromSynType typ
          label = fieldName |> Option.map (fun id -> id.idText)
          description = "" }

    let parseCase (case : SynUnionCase) : WT.TypeDeclaration.EnumCase =
      match case with
      | SynUnionCase(_, SynIdent(id, _), typ, _, _, _, _) ->
        match typ with
        | SynUnionCaseKind.Fields fields ->
          { name = id.idText; fields = List.map parseField fields; description = "" }
        | _ -> Exception.raiseInternal $"Unsupported enum case" [ "case", case ]


  module RecordField =
    let parseField (field : SynField) : WT.TypeDeclaration.RecordField =
      match field with
      | SynField(_, _, Some id, typ, _, _, _, _, _) ->
        { name = id.idText; typ = TypeReference.fromSynType typ; description = "" }
      | _ -> Exception.raiseInternal $"Unsupported field" [ "field", field ]

  let fromFields typeDef (fields : List<SynField>) : WT.TypeDeclaration.Definition =
    match fields with
    | [] ->
      Exception.raiseInternal
        $"Unsupported record type with no fields"
        [ "typeDef", typeDef ]
    | firstField :: additionalFields ->

      WT.TypeDeclaration.Record(
        NEList.ofList firstField additionalFields
        |> NEList.map RecordField.parseField
      )

  module Definition =
    let fromCases
      typeDef
      (cases : List<SynUnionCase>)
      : WT.TypeDeclaration.Definition =
      match cases with
      | [] ->
        Exception.raiseInternal
          $"Can't parse enum without any cases"
          [ "typeDef", typeDef ]
      | firstCase :: additionalCases ->
        NEList.ofList firstCase additionalCases
        |> NEList.map EnumCase.parseCase
        |> WT.TypeDeclaration.Enum




    let fromSynTypeDefn
      (typeDef : SynTypeDefn)
      : (List<string> * List<string> * WT.TypeDeclaration.Definition) =
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
        WT.TypeDeclaration.Alias(TypeReference.fromSynType typ)

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
                    SynTypeDefnRepr.Simple(SynTypeDefnSimpleRepr.Union(_, cases, _),
                                           _),
                    _,
                    _,
                    _,
                    _) ->
        SimpleTypeArgs.fromSynTyparDecls typeParams,
        ids |> List.map string,
        fromCases typeDef cases
      | _ ->
        Exception.raiseInternal $"Unsupported type definition" [ "typeDef", typeDef ]


module UserType =
  let fromSynTypeDefn
    (moduleName : List<string>)
    (typeDef : SynTypeDefn)
    : WT.UserType.T =
    let (typeParams, names, definition) =
      TypeDeclaration.Definition.fromSynTypeDefn typeDef
    let (name, version) =
      List.last names
      |> Exception.unwrapOptionInternal
        "user type should have name"
        [ "typeDef", typeDef ]
      |> Expr.parseTypeName
      |> Exception.unwrapResultInternal []
    let modules = moduleName @ names |> List.initial

    { name = PT.TypeName.userProgram modules name version
      description = ""
      declaration = { definition = definition; typeParams = typeParams } }

module PackageType =
  let fromSynTypeDefn
    (owner : string)
    (modules : List<string>)
    (typeDef : SynTypeDefn)
    : WT.PackageType.T =
    let (typeParams, names, definition) =
      TypeDeclaration.Definition.fromSynTypeDefn typeDef
    let (name, version) =
      List.last names
      |> Exception.unwrapOptionInternal
        "user type should have name"
        [ "typeDef", typeDef ]
      |> Expr.parseTypeName
      |> Exception.unwrapResultInternal []
    { name = PT.TypeName.package owner modules name version
      description = ""
      declaration = { typeParams = typeParams; definition = definition } }

module PackageConstant =
  let fromSynBinding
    (owner : string)
    (modules : List<string>)
    (b : SynBinding)
    : WT.PackageConstant.T =
    let c = Constant.fromSynBinding b
    { name = PT.ConstantName.package owner modules c.name c.version
      description = ""
      body = c.body }


let initialParse (filename : string) (code : string) : WT.Expr =
  code
  |> Utils.parseAsFSharpSourceFile filename
  |> Utils.singleExprFromImplFile
  |> Expr.fromSynExpr
