module LibParser.SourceFile

module WT = LibParser.WrittenTypes

// Keep the parser's nested module tree source-accurate, but give lowering paths a
// shared flat view where each item carries the module path it appeared under.
type Item =
  | Fn of List<string> * WT.FnDecl
  | Type of List<string> * WT.TypeDecl
  | Value of List<string> * WT.ValueDecl
  | Expr of List<string> * WT.Expr
  | TypeDB of List<string> * WT.TypeDecl
  | Test of List<string> * WT.Test

let rec private collectItems
  (path : List<string>)
  (decls : List<WT.Declaration>)
  : List<Item> =
  decls
  |> List.collect (fun d ->
    match d with
    | WT.DModule m -> collectItems (path @ WT.moduleNameParts m) m.declarations
    | WT.DTrait t -> [ Type(path, WT.desugarTrait t) ]
    | WT.DImpl impl ->
      let d = WT.desugarImpl path impl
      (d.methods |> List.map (fun fn -> Fn(d.memberPath, fn)))
      @ [ match d.instance with
          | Choice1Of2 v -> Value(d.memberPath, v)
          | Choice2Of2 fn -> Fn(d.memberPath, fn) ]
    | WT.DFunction fn -> [ Fn(path, fn) ]
    | WT.DType t -> [ Type(path, t) ]
    | WT.DValue v -> [ Value(path, v) ]
    | WT.DExpr e -> [ Expr(path, e) ]
    | WT.DTypeDB t -> [ TypeDB(path, t) ]
    | WT.DTest t -> [ Test(path, t) ])

let items (sf : WT.SourceFile) : List<Item> =
  collectItems [] sf.declarations
  @ (sf.exprsToEval |> List.map (fun e -> Expr([], e)))
