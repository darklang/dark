module FSharpToExpr

open Expecto

module R = LibExecution.Runtime
open FSharp.Compiler
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

let parse (input) : SynExpr =
  let file = "test.fs"
  let input = $"do {input}"
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
                                                                                                     SynExpr.Do (expr,
                                                                                                                 _),
                                                                                                     _) ],
                                                                             _,
                                                                             _,
                                                                             _,
                                                                             _) ],
                                                     _))) ->
      // Extract declarations and walk over them
      expr
  | _ -> failwith $" - wrong shape tree: {results}"


open R.Shortcuts

exception MyException of string

let convert (ast : SynExpr) : R.Expr * R.Expr =
  let rec convertPattern (pat : SynPat) : R.Pattern =
    match pat with
    | SynPat.Named (SynPat.Wild (_), name, _, _, _) -> pVar name.idText
    | _ -> failwith $" - unhandled pattern: {pat} "

  let convertLambdaVar (var : SynSimplePat) : string =
    match var with
    | SynSimplePat.Id (name, _, _, _, _, _) -> name.idText
    | _ -> failwith $"unsupported lambdaVar {var}"

  let rec convert' (expr : SynExpr) : R.Expr =
    let c = convert'
    match expr with
    | SynExpr.Const (SynConst.Int32 n, _) -> eInt n
    | SynExpr.Const (SynConst.Char c, _) -> eChar c
    | SynExpr.Const (SynConst.Bool b, _) -> eBool b
    | SynExpr.Const (SynConst.Double d, _) ->
        let parts = (sprintf "%f" d).Split "."
        if parts.Length <> 2 then (raise (MyException $"Parts are {parts}, d is {d}"))
        eFloatStr (parts.[0]) (string (int parts.[1]))
    | SynExpr.Const (SynConst.String (s, _), _) -> eStr s
    | SynExpr.Ident ident when ident.idText = "op_Addition" ->
        eBinOp "" "+" 0 (eBlank ()) (eBlank ())
    | SynExpr.Ident ident when ident.idText = "op_Equality" ->
        eBinOp "" "==" 0 (eBlank ()) (eBlank ())
    | SynExpr.Ident ident when ident.idText = "op_PipeRight" ->
        ePipe (eBlank ()) (eBlank ()) []
    | SynExpr.Ident ident when ident.idText = "Nothing" -> eNothing ()
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

    | SynExpr.LongIdent (_,
                         // Note to self: LongIdent = Ident list
                         LongIdentWithDots ([ modName; fnName ], _),
                         _,
                         _) ->
        let name, version =
          match fnName.idText.Split "_v" with
          | [| name; version |] -> (name, int version)
          | _ -> failwith $"Version name isn't expected format {fnName.idText}"

        eFn modName.idText name version []
    | SynExpr.Lambda (_, _, SynSimplePats.SimplePats (vars, _), body, _) ->
        let vars = List.map convertLambdaVar vars
        eLambda vars (c body)
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
    | SynExpr.Paren (expr, _, _, _) -> c expr // just unwrap
    // Callers with multiple args are encoded as apps wrapping other apps.
    | SynExpr.App (_, _, nested, arg, _) -> // binops
        match c nested with
        | R.EFnCall (id, name, args, ster) ->
            R.EFnCall(id, name, args @ [ c arg ], ster)
        | R.EBinOp (id, name, R.EBlank _, arg2, ster) ->
            R.EBinOp(id, name, c arg, arg2, ster)
        | R.EBinOp (id, name, arg1, R.EBlank _, ster) ->
            R.EBinOp(id, name, arg1, c arg, ster)
        | R.EPipe (id, arg1, arg2, args) ->
            let arg =
              match c arg with
              | R.EFnCall (id, name, args, ster) ->
                  R.EFnCall(id, name, ePipeTarget () :: args, ster)
              | R.EBinOp (id, name, arg1, arg2, ster) ->
                  R.EBinOp(id, name, arg1, arg2, ster)
              | a -> a

            if arg1.isBlank then R.EPipe(id, arg, arg2, args)
            else if arg2.isBlank then R.EPipe(id, arg1, arg, args)
            else R.EPipe(id, arg1, arg2, args @ [ arg ])
        | e -> failwith $"Unsupported expression: {(expr, e, arg)}"
    | expr -> failwith $"Unsupported expression: {expr}"

  // Split equality into actual vs expected
  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_, _, SynExpr.Ident ident, actual, _),
                 expected,
                 _) when ident.idText = "op_Equality" ->
      (convert' actual, convert' expected)
  | _ -> convert' ast, eBool true


let t (comment : string) (code : string) : Test =
  let name = $"{comment} ({code})"
  if code.StartsWith "//" then
    ptestTask name { return (Expect.equal "skipped" "skipped" "") }
  else
    testTask name {
      try
        let source = parse code
        let actualProg, expectedResult = convert source
        let! actual = LibExecution.Execution.run actualProg
        let! expected = LibExecution.Execution.run expectedResult

        return (Expect.equal
                  actual
                  expected
                  // $"{source} => {actualProg} = {expectedResult}")
                  $"{actualProg} = {expectedResult}")
      with
      | MyException msg -> return (Expect.equal "" msg "")
      | e -> return (Expect.equal "" (e.ToString()) "")
    }
