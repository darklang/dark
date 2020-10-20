module ExecUtils

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

  let rec convert' (expr : SynExpr) : R.Expr =
    let c = convert'
    match expr with
    | SynExpr.Const (SynConst.Int32 n, _) -> eint n
    | SynExpr.Const (SynConst.Char c, _) -> echar c
    | SynExpr.Const (SynConst.Bool b, _) -> ebool b
    | SynExpr.Const (SynConst.Double d, _) ->
        let parts = (sprintf "%f" d).Split "."
        if parts.Length <> 2 then (raise (MyException $"Parts are {parts}, d is {d}"))
        efloatStr (parts.[0]) (string (int parts.[1]))
    | SynExpr.Const (SynConst.String (s, _), _) -> estr s
    | SynExpr.Ident ident when ident.idText = "Nothing" -> enothing ()
    | SynExpr.Ident op_Equality -> efn "" "==" 0 []
    | SynExpr.ArrayOrList (_, exprs, _) -> exprs |> List.map c |> elist
    | SynExpr.ArrayOrListOfSeqExpr (_,
                                    SynExpr.CompExpr (_,
                                                      _,
                                                      SynExpr.Tuple (_, exprs, _, _),
                                                      _),
                                    _) -> exprs |> List.map c |> elist

    | SynExpr.LongIdent (_,
                         // Note to self: LongIdent = Ident list
                         LongIdentWithDots ([ modName; fnName ], _),
                         _,
                         _) ->
        let name, version =
          match fnName.idText.Split "_v" with
          | [| name; version |] -> (name, int version)
          | _ -> failwith $"Version name isn't expected format {fnName.idText}"

        efn modName.idText name version []
    | SynExpr.Paren (expr, _, _, _) -> c expr // just unwrap
    // Callers with multiple args are encoded as apps wrapping other apps.
    | SynExpr.App (_, _, (expr), arg, _) -> // binops
        match c expr with
        | R.EFnCall (id, name, args, ster) ->
            R.EFnCall(id, name, args @ [ c arg ], ster)
        | expr -> failwith $"Unsupported expression: {expr}"
    | expr -> failwith $"Unsupported expression: {expr}"

  // Split equality into actual vs expected
  match ast with
  | SynExpr.App (_,
                 _,
                 SynExpr.App (_, _, SynExpr.Ident op_Equality, arg2, _),
                 arg1,
                 _) -> (convert' arg1, convert' arg2)
  | _ -> convert' ast, ebool true


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
        return (Expect.equal actual expected "")
      with
      | MyException msg -> return (Expect.equal "" msg "")
      | e -> return (Expect.equal "" (e.ToString()) "")
    }
