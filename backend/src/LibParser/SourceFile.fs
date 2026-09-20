module LibParser.SourceFile

module WT = LibParser.WrittenTypes

// Keep the parser's nested module tree source-accurate, but give lowering paths a
// shared flat view where each item carries the module path it appeared under.
type Item =
  | Fn of List<string> * WT.FnDecl
  | Type of List<string> * WT.TypeDecl
  | Value of List<string> * WT.ValueDecl
  | Trait of List<string> * WT.TraitDecl
  /// The impl at its member path (`<module>[.<Type>].<Trait>`); its method fns come
  /// out as `Fn` items at that same path, with the impl's type params and bounds.
  | Impl of List<string> * WT.ImplDecl
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
    | WT.DTrait t -> [ Trait(path, t) ]
    | WT.DImpl impl ->
      let memberPath = WT.implMemberPath path impl
      (impl.methods
       |> List.map (fun m ->
         Fn(
           memberPath,
           { m with
               typeParams = impl.typeParams @ m.typeParams
               bounds = impl.bounds @ m.bounds }
         )))
      @ [ Impl(memberPath, impl) ]
    | WT.DFunction fn -> [ Fn(path, fn) ]
    | WT.DType t -> [ Type(path, t) ]
    | WT.DValue v -> [ Value(path, v) ]
    | WT.DExpr e -> [ Expr(path, e) ]
    | WT.DTypeDB t -> [ TypeDB(path, t) ]
    | WT.DTest t -> [ Test(path, t) ])

let items (sf : WT.SourceFile) : List<Item> =
  collectItems [] sf.declarations
  @ (sf.exprsToEval |> List.map (fun e -> Expr([], e)))
