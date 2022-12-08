/// Converts strings of F# into Dark. Used for testing.
module FSharpToExpr

// refer to https://fsharp.github.io/fsharp-compiler-docs

open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes

let parse (input) : SynExpr =
  let file = "test.fs"
  let checker = FSharpChecker.Create()

  // Throws an exception here if we don't do this:
  // https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/service/FSharpCheckerResults.fs#L1555
  let parsingOptions = { FSharpParsingOptions.Default with SourceFiles = [| file |] }

  let results =
    checker.ParseFile(file, Text.SourceText.ofString input, parsingOptions)
    |> Async.RunSynchronously

  match results.ParseTree with
  | (ParsedInput.ImplFile (ParsedImplFileInput (_,
                                                _,
                                                _,
                                                _,
                                                _,
                                                [ SynModuleOrNamespace (_,
                                                                        _,
                                                                        _,
                                                                        [ SynModuleDecl.DoExpr (_,
                                                                                                expr,
                                                                                                _) ],
                                                                        _,
                                                                        _,
                                                                        _,
                                                                        _) ],
                                                _))) ->
    // Extract declarations and walk over them
    expr
  | _ ->
    Exception.raiseInternal
      $"wrong shape tree"
      [ "parseTree", results.ParseTree; "input", input ]

// A placeholder is used to indicate what still needs to be filled
let placeholder = PT.EString(12345678UL, "PLACEHOLDER VALUE")

// This is a "Partial active pattern" that you can use as a Pattern to match a Placeholder value
let (|Placeholder|_|) (input : PT.Expr) =
  if input = placeholder then Some() else None

let nameOrBlank (v : string) : string = if v = "___" then "" else v

let rec convertToExpr' (ast : SynExpr) : PT.Expr =
  let c = convertToExpr'

  let rec convertPattern (pat : SynPat) : PT.MatchPattern =
    let id = gid ()
    match pat with
    | SynPat.Named (name, _, _, _) when name.idText = "blank" -> PT.MPBlank id
    | SynPat.Named (name, _, _, _) -> PT.MPVariable(id, name.idText)
    | SynPat.Wild _ -> PT.MPVariable(gid (), "_") // wildcard, not blank
    | SynPat.Const (SynConst.Int32 n, _) -> PT.MPInteger(id, n)
    | SynPat.Const (SynConst.Int64 n, _) -> PT.MPInteger(id, int64 n)
    | SynPat.Const (SynConst.UserNum (n, "I"), _) -> PT.MPInteger(id, parseInt64 n)
    | SynPat.Const (SynConst.Char c, _) -> PT.MPCharacter(id, string c)
    | SynPat.Const (SynConst.Bool b, _) -> PT.MPBool(id, b)
    | SynPat.Null _ -> PT.MPNull id
    | SynPat.Paren (pat, _) -> convertPattern pat
    | SynPat.Const (SynConst.Double d, _) ->
      let sign, whole, fraction = readFloat d
      PT.MPFloat(id, sign, whole, fraction)
    | SynPat.Const (SynConst.String (s, _, _), _) -> PT.MPString(id, s)
    | SynPat.LongIdent (LongIdentWithDots ([ constructorName ], _),
                        _,
                        _,
                        _,
                        SynArgPats.Pats args,
                        _,
                        _) ->
      let args = List.map convertPattern args
      PT.MPConstructor(id, constructorName.idText, args)
    | SynPat.Tuple (_isStruct, (first :: second :: theRest), _range) ->
      PT.MPTuple(
        id,
        convertPattern first,
        convertPattern second,
        List.map convertPattern theRest
      )
    | _ -> Exception.raiseInternal "unhandled pattern" [ "pattern", pat ]

  let convertLambdaVar (var : SynSimplePat) : string =
    match var with
    | SynSimplePat.Id (name, _, _, _, _, _) -> nameOrBlank name.idText
    | _ -> Exception.raiseInternal "unsupported lambdaVar" [ "var", var ]

  // Add a pipetarget after creating it
  let cPlusPipeTarget (e : SynExpr) : PT.Expr =
    match c e with
    | PT.EFnCall (id, name, args, ster) ->
      PT.EFnCall(id, name, PT.EPipeTarget(gid ()) :: args, ster)
    | PT.EBinOp (id, name, Placeholder, arg2, ster) ->
      PT.EBinOp(id, name, PT.EPipeTarget(gid ()), arg2, ster)
    | PT.EBinOp (id, name, arg1, Placeholder, ster) ->
      PT.EBinOp(id, name, PT.EPipeTarget(gid ()), arg1, ster)
    | other -> other

  let ops =
    Map.ofList [ ("op_Addition", "+")
                 ("op_Subtraction", "-")
                 ("op_Multiply", "*")
                 ("op_Division", "/")
                 ("op_PlusPlus", "++")
                 ("op_GreaterThan", ">")
                 ("op_GreaterThanOrEqual", ">=")
                 ("op_LessThan", "<")
                 ("op_LessThanOrEqual", "<=")
                 ("op_Modulus", "%")
                 ("op_Concatenate", "^")
                 ("op_EqualsEquals", "==")
                 ("op_Equality", "==")
                 ("op_BangEquals", "!=")
                 ("op_Inequality", "!=") ]

  let id = gid ()

  match ast with
  // constant values
  | SynExpr.Null _ -> PT.ENull id
  | SynExpr.Const (SynConst.Int32 n, _) -> PT.EInteger(id, n)
  | SynExpr.Const (SynConst.Int64 n, _) -> PT.EInteger(id, int64 n)
  | SynExpr.Const (SynConst.UserNum (n, "I"), _) -> PT.EInteger(id, parseInt64 n)
  | SynExpr.Const (SynConst.Char c, _) -> PT.ECharacter(id, string c)
  | SynExpr.Const (SynConst.Bool b, _) -> PT.EBool(id, b)
  | SynExpr.Const (SynConst.Double d, _) ->
    let sign, whole, fraction = readFloat d
    PT.EFloat(id, sign, whole, fraction)
  | SynExpr.Const (SynConst.String (s, _, _), _) -> PT.EString(id, s)

  // simple identifiers like `==`
  | SynExpr.Ident ident when Map.containsKey ident.idText ops ->
    let op =
      Map.get ident.idText ops
      |> Exception.unwrapOptionInternal
           "can't find operation"
           [ "name", ident.idText ]
    let fn : PT.FQFnName.InfixStdlibFnName = { module_ = None; function_ = op }
    PT.EBinOp(id, fn, placeholder, placeholder, PT.NoRail)

  | SynExpr.Ident ident when ident.idText = "op_UnaryNegation" ->
    let name = PTParser.FQFnName.stdlibFqName "Int" "negate" 0
    PT.EFnCall(id, name, [], PT.NoRail)

  | SynExpr.Ident ident when
    Set.contains ident.idText PTParser.FQFnName.oneWordFunctions
    ->
    PT.EFnCall(id, PTParser.FQFnName.parse ident.idText, [], PT.NoRail)

  | SynExpr.Ident ident when ident.idText = "Nothing" ->
    PT.EConstructor(id, "Nothing", [])

  | SynExpr.Ident ident when ident.idText = "blank" -> PT.EBlank id

  | SynExpr.Ident name -> PT.EVariable(id, name.idText)

  // lists and arrays
  | SynExpr.ArrayOrList (_, exprs, _) -> PT.EList(id, exprs |> List.map c)

  // A literal list is sometimes made up of nested Sequentials
  | SynExpr.ArrayOrListComputed (_, (SynExpr.Sequential _ as seq), _) ->
    let rec seqAsList expr : List<SynExpr> =
      match expr with
      | SynExpr.Sequential (_, _, expr1, expr2, _) -> expr1 :: seqAsList expr2
      | _ -> [ expr ]
    PT.EList(id, seq |> seqAsList |> List.map c)

  | SynExpr.ArrayOrListComputed (_, SynExpr.Tuple (_, exprs, _, _), _) ->
    PT.EList(id, exprs |> List.map c)

  | SynExpr.ArrayOrListComputed (_, expr, _) -> PT.EList(id, [ c expr ])

  // tuples
  | SynExpr.Tuple (_, first :: second :: rest, _, _) ->
    PT.ETuple(id, c first, c second, List.map c rest)

  // Long Identifiers like Date.now, represented in the form of ["Date"; "now"]
  // (LongIdent = Ident list)
  | SynExpr.LongIdent (_, LongIdentWithDots ([ modName; fnName ], _), _, _) when
    System.Char.IsUpper(modName.idText[0])
    ->
    let module_ = modName.idText

    let name, ster =
      match fnName.idText with
      // ster = send to error
      | Regex "(.+)_v(\d+)_ster" [ name; version ] ->
        ($"{module_}::{name}_v{int version}", PT.Rail)
      | Regex "(.+)_v(\d+)" [ name; version ] ->
        ($"{module_}::{name}_v{int version}", PT.NoRail)
      | Regex "(.*)" [ name ] when Map.containsKey name ops ->
        // Things like `Date::<`, written `Date.(<)`
        let name =
          Map.get name ops
          |> Exception.unwrapOptionInternal
               "can't find function name"
               [ "name", name ]
        ($"{module_}::{name}", PT.NoRail)
      | Regex "(.+)_ster" [ name ] -> ($"{module_}::{name}", PT.Rail)
      | Regex "(.+)" [ name ] -> ($"{module_}::{name}", PT.NoRail)
      | _ ->
        Exception.raiseInternal
          $"Bad format in function name"
          [ "name", fnName.idText ]

    PT.EFnCall(gid (), PTParser.FQFnName.parse name, [], ster)

  // Preliminary support for package manager functions
  | SynExpr.LongIdent (_,
                       LongIdentWithDots ([ owner; package; modName; fnName ], _),
                       _,
                       _) when
    owner.idText = "Test" && package.idText = "Test" && modName.idText = "Test"
    ->
    let fnName = fnName.idText
    let name, ster =
      if String.endsWith "_ster" fnName then
        String.dropRight 5 fnName, PT.Rail
      else
        fnName, PT.NoRail
    let name = $"test/test/Test::{name}_v0"
    PT.EFnCall(gid (), PTParser.FQFnName.parse name, [], ster)

  | SynExpr.LongIdent (_, LongIdentWithDots ([ var; f1; f2; f3 ], _), _, _) ->
    let obj1 =
      PT.EFieldAccess(id, PT.EVariable(gid (), var.idText), nameOrBlank f1.idText)
    let obj2 = PT.EFieldAccess(id, obj1, nameOrBlank f2.idText)
    PT.EFieldAccess(id, obj2, nameOrBlank f3.idText)

  | SynExpr.LongIdent (_, LongIdentWithDots ([ var; field1; field2 ], _), _, _) ->
    let obj1 =
      PT.EFieldAccess(
        id,
        PT.EVariable(gid (), var.idText),
        nameOrBlank field1.idText
      )
    PT.EFieldAccess(id, obj1, nameOrBlank field2.idText)

  | SynExpr.LongIdent (_, LongIdentWithDots ([ var; field ], _), _, _) ->
    PT.EFieldAccess(id, PT.EVariable(gid (), var.idText), nameOrBlank field.idText)

  | SynExpr.DotGet (expr, _, LongIdentWithDots ([ field ], _), _) ->
    PT.EFieldAccess(id, c expr, nameOrBlank field.idText)

  | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (outerVars, _), body, _, _, _) ->
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

  // When we add patterns on the left hand side of lets, the pattern below
  // could be expanded to use convertPat
  | SynExpr.LetOrUse (_,
                      _,
                      [ SynBinding (_,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _,
                                    SynPat.Named (name, _, _, _),
                                    _,
                                    rhs,
                                    _,
                                    _,
                                    _) ],
                      body,
                      _,
                      _) -> PT.ELet(id, name.idText, c rhs, c body)

  | SynExpr.LetOrUse (_,
                      _,
                      [ SynBinding (_,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _,
                                    _,
                                    SynPat.Wild (_),
                                    _,
                                    rhs,
                                    _,
                                    _,
                                    _) ],
                      body,
                      _,
                      _) -> PT.ELet(id, "_", c rhs, c body)

  | SynExpr.Match (_, _, cond, _, clauses, _) ->
    let convertClause
      (SynMatchClause (pat, _, expr, _, _, _) : SynMatchClause)
      : PT.MatchPattern * PT.Expr =
      (convertPattern pat, c expr)
    PT.EMatch(id, c cond, List.map convertClause clauses)

  | SynExpr.Record (_, _, fields, _) ->
    fields
    |> List.map (fun field ->
      match field with
      | SynExprRecordField ((LongIdentWithDots ([ name ], _), _), _, Some expr, _) ->
        (nameOrBlank name.idText, c expr)
      | f -> Exception.raiseInternal "Not an expected field" [ "field", f ])
    |> fun rows -> PT.ERecord(id, rows)

  | SynExpr.Paren (expr, _, _, _) -> c expr // just unwrap

  | SynExpr.Do (expr, _) -> c expr // just unwrap

  // nested pipes - F# uses 2 Apps to represent a pipe. The outer app has an
  // op_PipeRight, and the inner app has two arguments. Those arguments might
  // also be pipes
  | SynExpr.App (_, _, SynExpr.Ident pipe, SynExpr.App (_, _, nestedPipes, arg, _), _) when
    pipe.idText = "op_PipeRight"
    ->
    match c nestedPipes with
    | PT.EPipe (id, arg1, Placeholder, []) as pipe ->
      // when we just built the lowest, the second one goes here
      PT.EPipe(id, arg1, cPlusPipeTarget arg, [])
    | PT.EPipe (id, arg1, arg2, rest) as pipe ->
      PT.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
    // Exception.raiseInternal $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
    | other ->
      // Exception.raiseInternal $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
      // the very bottom on the pipe chain, this is the first and second expressions
      PT.EPipe(id, other, cPlusPipeTarget arg, [])

  | SynExpr.App (_, _, SynExpr.Ident pipe, expr, _) when pipe.idText = "op_PipeRight" ->
    // the very bottom on the pipe chain, this is just the first expression
    PT.EPipe(id, c expr, placeholder, [])

  | SynExpr.App (_, _, SynExpr.Ident name, arg, _) when
    List.contains name.idText [ "Ok"; "Nothing"; "Just"; "Error" ]
    ->
    PT.EConstructor(id, name.idText, [ c arg ])

  | SynExpr.App (_, _, SynExpr.App (_, _, SynExpr.Ident ident, left, _), right, _) when
    ident.idText = "op_BooleanAnd"
    ->
    PT.EAnd(id, c left, c right)

  | SynExpr.App (_, _, SynExpr.App (_, _, SynExpr.Ident ident, left, _), right, _) when
    ident.idText = "op_BooleanOr"
    ->
    PT.EOr(id, c left, c right)

  // Feature flag now or else it'll get recognized as a var
  | SynExpr.App (_,
                 _,
                 SynExpr.Ident name,
                 SynExpr.Const (SynConst.String (label, _, _), _),
                 _) when name.idText = "flag" ->
    PT.EFeatureFlag(gid (), label, placeholder, placeholder, placeholder)

  // Callers with multiple args are encoded as apps wrapping other apps.
  | SynExpr.App (_, _, funcExpr, arg, _) -> // function application (binops and fncalls)
    match c funcExpr with
    | PT.EFnCall (id, name, args, ster) ->
      PT.EFnCall(id, name, args @ [ c arg ], ster)
    | PT.EBinOp (id, name, Placeholder, arg2, ster) ->
      PT.EBinOp(id, name, c arg, arg2, ster)
    | PT.EBinOp (id, name, arg1, Placeholder, ster) ->
      PT.EBinOp(id, name, arg1, c arg, ster)
    // Fill in the feature flag fields (back to front)
    | PT.EFeatureFlag (id, label, Placeholder, oldexpr, newexpr) ->
      PT.EFeatureFlag(id, label, c arg, oldexpr, newexpr)
    | PT.EFeatureFlag (id, label, condexpr, Placeholder, newexpr) ->
      PT.EFeatureFlag(id, label, condexpr, c arg, newexpr)
    | PT.EFeatureFlag (id, label, condexpr, oldexpr, Placeholder) ->
      PT.EFeatureFlag(id, label, condexpr, oldexpr, c arg)
    // A pipe with one entry
    | PT.EPipe (id, arg1, Placeholder, []) as pipe ->
      PT.EPipe(id, arg1, cPlusPipeTarget arg, [])
    // A pipe with more than one entry
    | PT.EPipe (id, arg1, arg2, rest) as pipe ->
      PT.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
    // Function calls sending to error rail
    | PT.EVariable (id, name) when String.endsWith "_ster" name ->
      let name = String.dropRight 5 name
      PT.EFnCall(id, PTParser.FQFnName.parse name, [ c arg ], PT.Rail)
    | PT.EVariable (id, name) ->
      PT.EFnCall(id, PTParser.FQFnName.parse name, [ c arg ], PT.NoRail)
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

let convertToExpr e =
  e
  |> convertToExpr'
  |> LibExecution.ProgramTypesAst.postTraversal (fun e ->
    match e with
    | PT.EFnCall (id, PT.FQFnName.User "partial", [ PT.EString (_, msg); arg ], _) ->
      PT.EPartial(gid (), msg, arg)
    | PT.EFnCall (id,
                  PT.FQFnName.User "partial",
                  [ PT.EPipeTarget _; PT.EString (_, msg); arg ],
                  _) -> PT.EPartial(gid (), msg, arg)
    | e -> e)

let convertToTest (ast : SynExpr) : bool * PT.Expr * PT.Expr =
  // Split equality into actual vs expected in tests.
  let convert (x : SynExpr) : PT.Expr = convertToExpr x

  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_, _, SynExpr.Ident ident, actual, _),
                 expected,
                 _) when ident.idText = "op_Equality" ->
    // Exception.raiseInternal $"whole thing: {actual}"
    (true, convert actual, convert expected)
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_, _, SynExpr.Ident ident, actual, _),
                 expected,
                 _) when ident.idText = "op_Inequality" ->
    // Exception.raiseInternal $"whole thing: {actual}"
    (false, convert actual, convert expected)
  | _ -> true, convert ast, PT.EBool(gid (), true)

let parsePTExpr (code : string) : PT.Expr = code |> parse |> convertToExpr

let parseRTExpr (code : string) : RT.Expr =
  parsePTExpr code |> LibExecution.ProgramTypesToRuntimeTypes.Expr.toRT
