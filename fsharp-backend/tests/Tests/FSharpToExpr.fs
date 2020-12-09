module FSharpToExpr

// Converts strings of F# into Dark. Used for testing.

open FSharp.Compiler
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.SourceCodeServices

open Prelude

module DarkTypes = LibBackend.ProgramSerialization.ProgramTypes
module D = DarkTypes
open LibExecution.SharedTypes
open LibBackend.ProgramSerialization.ProgramTypes.Shortcuts

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


let rec convertToExpr (ast : SynExpr) : D.Expr =
  let c = convertToExpr

  let parseFloat d : string * string =
    match sprintf "%f" d with
    | Regex "(.*)\.(.*)" [ whole; fraction ] -> (whole, fraction |> int |> string)
    | str -> failwith $"Could not parse {str}"

  let rec convertPattern (pat : SynPat) : D.Pattern =
    match pat with
    | SynPat.Named (SynPat.Wild (_), name, _, _, _) when name.idText = "blank" ->
        pBlank ()
    | SynPat.Named (SynPat.Wild (_), name, _, _, _) -> pVar name.idText
    | SynPat.Const (SynConst.Int32 n, _) -> pInt n
    | SynPat.Const (SynConst.UserNum (n, "I"), _) -> D.PInteger(gid (), n)
    | SynPat.Const (SynConst.Char c, _) -> pChar c
    | SynPat.Const (SynConst.Bool b, _) -> pBool b
    | SynPat.Null _ -> pNull ()
    | SynPat.Const (SynConst.Double d, _) ->
        let whole, fraction = parseFloat d
        pFloatStr whole fraction
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
  let cPlusPipeTarget (e : SynExpr) : D.Expr =
    match c e with
    | D.EFnCall (id, name, args, ster) ->
        D.EFnCall(id, name, ePipeTarget () :: args, ster)
    | D.EBinOp (id, name, D.EBlank _, arg2, ster) ->
        D.EBinOp(id, name, ePipeTarget (), arg2, ster)
    | D.EBinOp (id, name, arg1, D.EBlank _, ster) ->
        D.EBinOp(id, name, ePipeTarget (), arg1, ster)
    | other -> other

  match ast with
  | SynExpr.Const (SynConst.Int32 n, _) -> eInt n
  | SynExpr.Const (SynConst.UserNum (n, "I"), _) -> D.EInteger(gid (), n)
  | SynExpr.Null _ -> eNull ()
  | SynExpr.Const (SynConst.Char c, _) -> eChar c
  | SynExpr.Const (SynConst.Bool b, _) -> eBool b
  | SynExpr.Const (SynConst.Double d, _) ->
      let whole, fraction = parseFloat d
      eFloatStr whole fraction
  | SynExpr.Const (SynConst.String (s, _), _) -> eStr s
  | SynExpr.Ident ident when ident.idText = "op_Addition" ->
      eBinOp "" "+" 0 (eBlank ()) (eBlank ())
  | SynExpr.Ident ident when ident.idText = "op_Subtraction" ->
      eBinOp "" "-" 0 (eBlank ()) (eBlank ())
  | SynExpr.Ident ident when ident.idText = "op_PlusPlus" ->
      eBinOp "" "++" 0 (eBlank ()) (eBlank ())
  | SynExpr.Ident ident when ident.idText = "op_EqualsEquals" ->
      eBinOp "" "==" 0 (eBlank ()) (eBlank ())
  | SynExpr.Ident ident when ident.idText = "op_GreaterThan" ->
      eBinOp "" ">" 0 (eBlank ()) (eBlank ())
  | SynExpr.Ident ident when ident.idText = "op_LessThan" ->
      eBinOp "" "<" 0 (eBlank ()) (eBlank ())
  | SynExpr.Ident ident when ident.idText = "op_Concatenate" ->
      eBinOp "" "^" 0 (eBlank ()) (eBlank ())
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
  | SynExpr.LongIdent (_,
                       // Note to self: LongIdent = Ident list
                       LongIdentWithDots ([ modName; fnName ], _),
                       _,
                       _) ->
      let name, version, ster =
        match fnName.idText with
        | Regex "(.+)_v(\d+)_ster" [ name; version ] -> (name, int version, D.Rail)
        | Regex "(.+)_v(\d+)" [ name; version ] -> (name, int version, D.NoRail)
        | _ -> failwith $"Bad format in function name: \"{fnName.idText}\""

      let desc = D.FQFnName.stdlibName modName.idText name version
      D.EFnCall(gid (), desc, [], ster)
  | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (outerVars, _), body, _) ->
      let rec extractVarsAndBody expr =
        match expr with
        // The 2nd param indicates this was part of a lambda
        | SynExpr.Lambda (_, true, SynSimplePats.SimplePats (vars, _), body, _) ->
            let nestedVars, body = extractVarsAndBody body
            vars @ nestedVars, body
        // The 2nd param indicates this was not nested
        | SynExpr.Lambda (_, false, SynSimplePats.SimplePats (vars, _), body, _) ->
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
      let convertClause (Clause (pat, _, expr, _, _) : SynMatchClause)
                        : D.Pattern * D.Expr =
        (convertPattern pat, c expr)

      eMatch (c cond) (List.map convertClause clauses)
  | SynExpr.Record (_, _, fields, _) ->
      fields
      |> List.map (function
           | ((LongIdentWithDots ([ name ], _), _), Some expr, _) ->
               (name.idText, c expr)
           | f -> failwith $"Not an expected field {f}")
      |> eRecord
  | SynExpr.Paren (expr, _, _, _) -> c expr // just unwrap
  | SynExpr.Do (expr, _) -> c expr // just unwrap
  // nested pipes - F# uses 2 Apps to represent a pipe. The outer app has an
  // op_PipeRight, and the inner app has two arguments. Those arguments might
  // also be pipes
  | SynExpr.App (_, _, SynExpr.Ident pipe, SynExpr.App (_, _, nestedPipes, arg, _), _) when pipe.idText =
                                                                                              "op_PipeRight" ->
      match c nestedPipes with
      | D.EPipe (id, arg1, D.EString (_, "SENTINEL EXPR FOR PIPES"), []) as pipe ->
          // when we just built the lowest, the second one goes here
          D.EPipe(id, arg1, cPlusPipeTarget arg, [])
      | D.EPipe (id, arg1, arg2, rest) as pipe ->
          D.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
      // failwith $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
      | other ->
          // failwith $"Pipe: {nestedPipes},\n\n{arg},\n\n{pipe}\n\n, {c arg})"
          // the very bottom on the pipe chain, this is the first and second expressions
          ePipe (other) (cPlusPipeTarget arg) []
  | SynExpr.App (_, _, SynExpr.Ident pipe, expr, _) when pipe.idText = "op_PipeRight" ->
      // the very bottom on the pipe chain, this is just the first expression
      ePipe (c expr) (eStr "SENTINEL EXPR FOR PIPES") []
  | SynExpr.App (_, _, SynExpr.Ident name, arg, _) when List.contains
                                                          name.idText
                                                          [ "Ok"
                                                            "Nothing"
                                                            "Just"
                                                            "Error" ] ->
      eConstructor name.idText [ c arg ]
  // Most functions are LongIdents, toString isn't
  | SynExpr.App (_, _, SynExpr.Ident name, arg, _) when name.idText = "toString" ->
      let desc = D.FQFnName.stdlibName "" "toString" 0
      D.EFnCall(gid (), desc, [ c arg ], D.NoRail)

  // Callers with multiple args are encoded as apps wrapping other apps.
  | SynExpr.App (_, _, funcExpr, arg, _) -> // function application (binops and fncalls)
      match c funcExpr with
      | D.EFnCall (id, name, args, ster) ->
          D.EFnCall(id, name, args @ [ c arg ], ster)
      | D.EBinOp (id, name, D.EBlank _, arg2, ster) ->
          D.EBinOp(id, name, c arg, arg2, ster)
      | D.EBinOp (id, name, arg1, D.EBlank _, ster) ->
          D.EBinOp(id, name, arg1, c arg, ster)
      // A pipe with one entry
      | D.EPipe (id, arg1, D.EString (_, "SENTINEL EXPR FOR PIPES"), []) as pipe ->
          D.EPipe(id, arg1, cPlusPipeTarget arg, [])
      // A pipe with more than one entry
      | D.EPipe (id, arg1, arg2, rest) as pipe ->
          D.EPipe(id, arg1, arg2, rest @ [ cPlusPipeTarget arg ])
      | e -> failwith $"Unsupported expression in app: {ast},\n\n{e},\n\n{arg})"
  | SynExpr.FromParseError _ as expr ->
      failwith $"There was a parser error parsing: {expr}"
  | expr -> failwith $"Unsupported expression: {ast}"

let convertToTest (ast : SynExpr)
                  : LibExecution.RuntimeTypes.Expr * LibExecution.RuntimeTypes.Expr =
  // Split equality into actual vs expected in tests.
  let convert (x : SynExpr) : LibExecution.RuntimeTypes.Expr =
    (convertToExpr x).toRuntimeType()

  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_, _, SynExpr.Ident ident, actual, _),
                 expected,
                 _) when ident.idText = "op_Equality" ->
      // failwith $"whole thing: {actual}"
      (convert actual, convert expected)
  | _ -> convert ast, LibExecution.RuntimeTypes.Shortcuts.eBool true

let parseDarkExpr (code : string) : D.Expr = code |> parse |> convertToExpr
