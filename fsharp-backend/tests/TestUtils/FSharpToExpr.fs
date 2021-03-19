module FSharpToExpr

// Converts strings of F# into Dark. Used for testing.

open FSharp.Compiler
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.SourceCodeServices

open Prelude
open Tablecloth

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes

open LibBackend.ProgramTypes.Shortcuts

let parse (input) : SynExpr =
  let file = "test.fs"
  let input = $"{input}"
  let checker = SourceCodeServices.FSharpChecker.Create()

  // Throws an exception here if we don't do this:
  // https://github.com/fsharp/FSharp.Compiler.Service/blob/122520fa62edec7be5d00854989b282bf3ce7315/src/fsharp/service/FSharpCheckerResults.fs#L1555
  let parsingOptions = { FSharpParsingOptions.Default with SourceFiles = [| file |] }

  let results =
    checker.ParseFile(file, Text.SourceText.ofString input, parsingOptions)
    |> Async.RunSynchronously

  match results.ParseTree with
  | Some (ParsedInput.ImplFile (ParsedImplFileInput (_,
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
  | _ -> failwith $" - wrong shape tree: {results.ParseTree}"

// A placeholder is used to indicate what still needs to be filled
let placeholder = PT.EString(12345678UL, "PLACEHOLDER VALUE")

// This is a "Partial active pattern" that you can use as a Pattern to match a Placeholder value
let (|Placeholder|_|) (input : PT.Expr) =
  if input = placeholder then Some() else None


let rec convertToExpr (ast : SynExpr) : PT.Expr =
  let c = convertToExpr

  let splitFloat (d : float) : Sign * bigint * bigint =
    match System.Decimal(d).ToString() with
    | Regex "-([0-9]+)\.(\d+)" [ whole; fraction ] ->
        (Negative, whole |> parseBigint, parseBigint fraction)
    | Regex "([0-9]+)\.(\d+)" [ whole; fraction ] ->
        (Positive, whole |> parseBigint, parseBigint fraction)
    | Regex "-([0-9]+)" [ whole ] -> (Negative, whole |> parseBigint, 0I)
    | Regex "([0-9]+)" [ whole ] -> (Positive, whole |> parseBigint, 0I)
    | str -> failwith $"Could not splitFloat {d}"

  let rec convertPattern (pat : SynPat) : PT.Pattern =
    match pat with
    | SynPat.Named (SynPat.Wild (_), name, _, _, _) when name.idText = "blank" ->
        pBlank ()
    | SynPat.Named (SynPat.Wild (_), name, _, _, _) -> pVar name.idText
    | SynPat.Const (SynConst.Int32 n, _) -> pInt n
    | SynPat.Const (SynConst.UserNum (n, "I"), _) ->
        PT.PInteger(gid (), parseBigint n)
    | SynPat.Const (SynConst.Char c, _) -> pChar c
    | SynPat.Const (SynConst.Bool b, _) -> pBool b
    | SynPat.Null _ -> pNull ()
    | SynPat.Const (SynConst.Double d, _) ->
        let sign, whole, fraction = splitFloat d
        pFloat sign whole fraction
    | SynPat.Const (SynConst.String (s, _), _) -> pString s
    | SynPat.LongIdent (LongIdentWithDots ([ constructorName ], _),
                        _,
                        _,
                        Pats args,
                        _,
                        _) ->
        let args = List.map convertPattern args
        pConstructor constructorName.idText args

    | _ -> failwith $" - unhandled pattern: {pat} "

  let convertLambdaVar (var : SynSimplePat) : string =
    match var with
    | SynSimplePat.Id (name, _, _, _, _, _) -> name.idText
    | _ -> failwith $"unsupported lambdaVar {var}"

  // Add a pipetarget after creating it
  let cPlusPipeTarget (e : SynExpr) : PT.Expr =
    match c e with
    | PT.EFnCall (id, name, args, ster) ->
        PT.EFnCall(id, name, ePipeTarget () :: args, ster)
    | PT.EBinOp (id, name, Placeholder, arg2, ster) ->
        PT.EBinOp(id, name, ePipeTarget (), arg2, ster)
    | PT.EBinOp (id, name, arg1, Placeholder, ster) ->
        PT.EBinOp(id, name, ePipeTarget (), arg1, ster)
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
                 ("op_BooleanAnd", "&&")
                 ("op_BooleanOr", "||") ]

  match ast with
  | SynExpr.Const (SynConst.Int32 n, _) -> eInt n
  | SynExpr.Const (SynConst.UserNum (n, "I"), _) ->
      PT.EInteger(gid (), parseBigint n)
  | SynExpr.Null _ -> eNull ()
  | SynExpr.Const (SynConst.Char c, _) -> eChar c
  | SynExpr.Const (SynConst.Bool b, _) -> eBool b
  | SynExpr.Const (SynConst.Double d, _) ->
      let sign, whole, fraction = splitFloat d
      eFloat sign whole fraction
  | SynExpr.Const (SynConst.String (s, _), _) -> eStr s
  | SynExpr.Ident ident when Map.containsKey ident.idText ops ->
      let op = Map.get ident.idText ops |> Option.unwrapUnsafe
      eBinOp "" op 0 placeholder placeholder
  | SynExpr.Ident ident when ident.idText = "op_UnaryNegation" ->
      eFn "Int" "negate" 0 []
  | SynExpr.Ident ident when ident.idText = "toString_v0" -> eFn "" "toString" 0 []
  | SynExpr.Ident ident when ident.idText = "Nothing" -> eNothing ()
  | SynExpr.Ident ident when ident.idText = "blank" -> eBlank ()
  | SynExpr.Ident name -> eVar name.idText
  | SynExpr.ArrayOrList (_, exprs, _) -> exprs |> List.map c |> eList
  // A literal list is sometimes made up of nested Sequentials
  | SynExpr.ArrayOrListOfSeqExpr (_,
                                  SynExpr.CompExpr (_,
                                                    _,
                                                    (SynExpr.Sequential _ as seq),
                                                    _),
                                  _) ->
      let rec seqAsList expr : List<SynExpr> =
        match expr with
        | SynExpr.Sequential (_, _, expr1, expr2, _) -> expr1 :: seqAsList expr2
        | _ -> [ expr ]

      seq |> seqAsList |> List.map c |> eList
  | SynExpr.ArrayOrListOfSeqExpr (_,
                                  SynExpr.CompExpr (_,
                                                    _,
                                                    SynExpr.Tuple (_, exprs, _, _),
                                                    _),
                                  _) -> exprs |> List.map c |> eList
  | SynExpr.ArrayOrListOfSeqExpr (_, SynExpr.CompExpr (_, _, expr, _), _) ->
      eList [ c expr ]

  // Note to self: LongIdent = Ident list
  | SynExpr.LongIdent (_, LongIdentWithDots ([ modName; fnName ], _), _, _) when
    System.Char.IsUpper(modName.idText.[0]) ->
      let name, version, ster =
        match fnName.idText with
        | Regex "(.+)_v(\d+)_ster" [ name; version ] -> (name, int version, PT.Rail)
        | Regex "(.+)_v(\d+)" [ name; version ] -> (name, int version, PT.NoRail)
        | Regex "(.*)" [ name ] when Map.containsKey name ops ->
            // Things like `Date::<`, written `Date.(<)`
            (Map.get name ops |> Option.unwrapUnsafe, 0, PT.NoRail)
        | _ -> failwith $"Bad format in function name: \"{fnName.idText}\""

      let desc = PT.FQFnName.stdlibFqName modName.idText name version
      PT.EFnCall(gid (), desc, [], ster)
  | SynExpr.LongIdent (_, LongIdentWithDots ([ var; f1; f2; f3 ], _), _, _) ->
      let obj1 = eFieldAccess (eVar var.idText) f1.idText
      let obj2 = eFieldAccess obj1 f2.idText
      eFieldAccess obj2 f3.idText
  | SynExpr.LongIdent (_, LongIdentWithDots ([ var; field1; field2 ], _), _, _) ->
      let obj1 = eFieldAccess (eVar var.idText) field1.idText
      eFieldAccess obj1 field2.idText
  | SynExpr.LongIdent (_, LongIdentWithDots ([ var; field ], _), _, _) ->
      eFieldAccess (eVar var.idText) field.idText
  | SynExpr.DotGet (expr, _, LongIdentWithDots ([ field ], _), _) ->
      PT.EFieldAccess(gid (), c expr, field.idText)
  | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (outerVars, _), body, _, _) ->
      let rec extractVarsAndBody expr =
        match expr with
        // The 2nd param indicates this was part of a lambda
        | SynExpr.Lambda (_, true, SynSimplePats.SimplePats (vars, _), body, _, _) ->
            let nestedVars, body = extractVarsAndBody body
            vars @ nestedVars, body
        // The 2nd param indicates this was not nested
        | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (vars, _), body, _, _) ->
            vars, body
        | SynExpr.Lambda _ -> failwith $"TODO: other types of lambda: {expr}"
        | _ -> [], expr

      let nestedVars, body = extractVarsAndBody body
      let vars = List.map convertLambdaVar (outerVars @ nestedVars)
      eLambda vars (c body)
  | SynExpr.IfThenElse (cond, thenExpr, Some elseExpr, _, _, _, _) ->
      eIf (c cond) (c thenExpr) (c elseExpr)
  // When we add patterns on the lhs of lets, the pattern below could be
  // expanded to use convertPat
  | SynExpr.LetOrUse (_,
                      _,
                      [ Binding (_,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _,
                                 _,
                                 SynPat.Named (SynPat.Wild (_), name, _, _, _),
                                 _,
                                 rhs,
                                 _,
                                 _) ],
                      body,
                      _) -> eLet name.idText (c rhs) (c body)
  | SynExpr.LetOrUse (_,
                      _,
                      [ Binding (_, _, _, _, _, _, _, SynPat.Wild (_), _, rhs, _, _) ],
                      body,
                      _) -> eLet "_" (c rhs) (c body)
  | SynExpr.Match (_, cond, clauses, _) ->
      let convertClause
        (Clause (pat, _, expr, _, _) : SynMatchClause)
        : PT.Pattern * PT.Expr =
        (convertPattern pat, c expr)

      eMatch (c cond) (List.map convertClause clauses)
  | SynExpr.Record (_, _, fields, _) ->
      fields
      |> List.map
           (function
           | ((LongIdentWithDots ([ name ], _), _), Some expr, _) ->
               (name.idText, c expr)
           | f -> failwith $"Not an expected field {f}")
      |> eRecord
  | SynExpr.Paren (expr, _, _, _) -> c expr // just unwrap
  | SynExpr.Do (expr, _) -> c expr // just unwrap
  // nested pipes - F# uses 2 Apps to represent a pipe. The outer app has an
  // op_PipeRight, and the inner app has two arguments. Those arguments might
  // also be pipes
  | SynExpr.App (_, _, SynExpr.Ident pipe, SynExpr.App (_, _, nestedPipes, arg, _), _) when
    pipe.idText = "op_PipeRight" ->
      match c nestedPipes with
      | PT.EPipe (id, arg1, Placeholder, []) as pipe ->
          // when we just built the lowest, the second one goes here
          PT.EPipe(id, arg1, cPlusPipeTarget arg, [])
      | PT.EPipe (id, arg1, arg2, rest) as pipe ->
          PT.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
      // failwith $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
      | other ->
          // failwith $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
          // the very bottom on the pipe chain, this is the first and second expressions
          ePipe (other) (cPlusPipeTarget arg) []
  | SynExpr.App (_, _, SynExpr.Ident pipe, expr, _) when pipe.idText = "op_PipeRight" ->
      // the very bottom on the pipe chain, this is just the first expression
      ePipe (c expr) placeholder []
  | SynExpr.App (_, _, SynExpr.Ident name, arg, _) when
    List.contains name.idText [ "Ok"; "Nothing"; "Just"; "Error" ] ->
      eConstructor name.idText [ c arg ]
  // Feature flag now or else it'll get recognized as a var
  | SynExpr.App (_,
                 _,
                 SynExpr.Ident name,
                 SynExpr.Const (SynConst.String (label, _), _),
                 _) when name.idText = "flag" ->
      eflag label placeholder placeholder placeholder
  // Most functions are LongIdents, toString isn't
  | SynExpr.App (_, _, SynExpr.Ident name, arg, _) when name.idText = "toString_v0" ->
      let desc = PT.FQFnName.stdlibFqName "" "toString" 0
      PT.EFnCall(gid (), desc, [ c arg ], PT.NoRail)
  // Callers with multiple args are encoded as apps wrapping other apps.
  | SynExpr.App (_, _, funcExpr, arg, _) -> // function application (binops and fncalls)
      match c funcExpr with
      | PT.EFnCall (id, name, args, ster) ->
          PT.EFnCall(id, name, args @ [ c arg ], ster)
      // FSTODO are these in the right order? might fail for non-commutative binops
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
      | PT.EVariable (id, name) ->
          PT.EFnCall(id, PT.FQFnName.userFqName name, [ c arg ], PT.NoRail)
      | e ->
          failwith (
            $"Unsupported expression in app: full ast:\n{ast}\n\n"
            + $"specific fncall expr:\n({funcExpr}),"
            + $"\nconverted specific fncall expr:\n{e},\nargument: {arg})"
          )
  | SynExpr.FromParseError _ as expr ->
      failwith $"There was a parser error parsing: {expr}"
  | expr -> failwith $"Unsupported expression: {ast}"

let convertToTest (ast : SynExpr) : PT.Expr * PT.Expr =
  // Split equality into actual vs expected in tests.
  let convert (x : SynExpr) : PT.Expr = convertToExpr x

  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_, _, SynExpr.Ident ident, actual, _),
                 expected,
                 _) when ident.idText = "op_Equality" ->
      // failwith $"whole thing: {actual}"
      (convert actual, convert expected)
  | _ -> convert ast, PT.Shortcuts.eBool true

let parsePTExpr (code : string) : PT.Expr = code |> parse |> convertToExpr
let parseRTExpr (code : string) : RT.Expr = (parsePTExpr code).toRuntimeType()
