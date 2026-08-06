/// The names in a package item that still do not resolve.
///
/// Authoring is allowed to produce these, and that is not a bug: `DeferredResolver` exists precisely
/// because an item can reference something that has not been added yet, and `WipRefresh` re-resolves once
/// it arrives. That covers "not resolved YET".
///
/// This answers the other question -- what is STILL unresolved -- which is what `commit` needs, because
/// commit is where work stops being a draft and becomes history other machines rely on. An item that never
/// resolves is one that fails at the point somebody calls it.
///
/// A traversal, not a decision. It reports names and refuses nothing; whether an unresolved name should
/// stop a commit is decided in Dark, in `Cli.Commit`.
///
/// Deliberately the same shape as `DeferredResolver`. That module walks this tree to REWRITE errors into
/// resolutions; this one walks it to COLLECT the ones still there. Keeping the two shaped alike is what
/// stops one growing a case the other forgets -- if you add an expression form, both files should change.
module LibDB.UnresolvedCheck

open Prelude

module PT = LibExecution.ProgramTypes


/// One unresolved reference, as the user typed it (`Stdlib.List.hed`).
let private fromNR (n : PT.NameResolution<'a>) : List<string> =
  match n.resolved with
  | Ok _ -> []
  | Error _ -> [ String.concat "." n.originalName ]


let rec private inTypeRef (t : PT.TypeReference) : List<string> =
  match t with
  | PT.TUnit
  | PT.TBool
  | PT.TInt8
  | PT.TUInt8
  | PT.TInt16
  | PT.TUInt16
  | PT.TInt32
  | PT.TUInt32
  | PT.TInt64
  | PT.TUInt64
  | PT.TInt128
  | PT.TUInt128
  | PT.TInt
  | PT.TFloat
  | PT.TChar
  | PT.TString
  | PT.TUuid
  | PT.TDateTime
  | PT.TBlob
  | PT.TVariable _ -> []

  | PT.TStream inner
  | PT.TList inner
  | PT.TDict inner
  | PT.TDB inner -> inTypeRef inner

  | PT.TTuple(first, second, rest) ->
    inTypeRef first @ inTypeRef second @ List.collect inTypeRef rest

  | PT.TCustomType(name, typeArgs) -> fromNR name @ List.collect inTypeRef typeArgs

  | PT.TFn(args, ret) ->
    (args |> NEList.toList |> List.collect inTypeRef) @ inTypeRef ret


let rec private inExpr (expr : PT.Expr) : List<string> =
  match expr with
  | PT.EUnit _
  | PT.EBool _
  | PT.EInt8 _
  | PT.EUInt8 _
  | PT.EInt16 _
  | PT.EUInt16 _
  | PT.EInt32 _
  | PT.EUInt32 _
  | PT.EInt64 _
  | PT.EUInt64 _
  | PT.EInt128 _
  | PT.EUInt128 _
  | PT.EInt _
  | PT.EFloat _
  | PT.EChar _
  | PT.EVariable _
  | PT.EArg _
  | PT.ESelf _ -> []

  | PT.EString(_, segments) -> List.collect inStringSegment segments

  | PT.EIf(_, cond, thenExpr, elseExpr) ->
    inExpr cond
    @ inExpr thenExpr
    @ (match elseExpr with
       | Some e -> inExpr e
       | None -> [])

  | PT.EPipe(_, lhs, parts) -> inExpr lhs @ List.collect inPipeExpr parts

  | PT.EMatch(_, arg, cases) -> inExpr arg @ List.collect inMatchCase cases

  | PT.ELet(_, _, value, body) -> inExpr value @ inExpr body

  | PT.EList(_, items) -> List.collect inExpr items

  | PT.EDict(_, pairs) -> pairs |> List.collect (fun (_, e) -> inExpr e)

  | PT.ETuple(_, first, second, rest) ->
    inExpr first @ inExpr second @ List.collect inExpr rest

  | PT.EApply(_, fnExpr, typeArgs, args) ->
    inExpr fnExpr
    @ List.collect inTypeRef typeArgs
    @ (args |> NEList.toList |> List.collect inExpr)

  | PT.EFnName(_, name) -> fromNR name

  | PT.ELambda(_, _, body) -> inExpr body

  | PT.EInfix(_, _, lhs, rhs) -> inExpr lhs @ inExpr rhs

  | PT.ERecord(_, typeName, typeArgs, fields) ->
    fromNR typeName
    @ List.collect inTypeRef typeArgs
    @ (fields |> List.collect (fun (_, e) -> inExpr e))

  | PT.ERecordFieldAccess(_, record, _) -> inExpr record

  | PT.ERecordUpdate(_, record, updates) ->
    inExpr record
    @ (updates |> NEList.toList |> List.collect (fun (_, e) -> inExpr e))

  | PT.EEnum(_, typeName, typeArgs, _, fields) ->
    fromNR typeName @ List.collect inTypeRef typeArgs @ List.collect inExpr fields

  | PT.EValue(_, name) -> fromNR name

  | PT.EStatement(_, first, next) -> inExpr first @ inExpr next

and private inStringSegment (seg : PT.StringSegment) : List<string> =
  match seg with
  | PT.StringText _ -> []
  | PT.StringInterpolation e -> inExpr e

and private inMatchCase (case_ : PT.MatchCase) : List<string> =
  // The pattern carries no NameResolutions -- `MPEnum` holds a bare case name -- so only the guard and the
  // right-hand side can contribute.
  (match case_.whenCondition with
   | Some e -> inExpr e
   | None -> [])
  @ inExpr case_.rhs

and private inPipeExpr (pe : PT.PipeExpr) : List<string> =
  match pe with
  | PT.EPipeLambda(_, _, body) -> inExpr body
  | PT.EPipeInfix(_, _, rhs) -> inExpr rhs
  | PT.EPipeFnCall(_, name, typeArgs, args) ->
    fromNR name @ List.collect inTypeRef typeArgs @ List.collect inExpr args
  | PT.EPipeEnum(_, typeName, _, fields) ->
    fromNR typeName @ List.collect inExpr fields
  | PT.EPipeVariable(_, _, args) -> List.collect inExpr args


let private inTypeDeclaration (d : PT.TypeDeclaration.T) : List<string> =
  match d.definition with
  | PT.TypeDeclaration.Alias t -> inTypeRef t
  | PT.TypeDeclaration.Record fields ->
    fields |> NEList.toList |> List.collect (fun f -> inTypeRef f.typ)
  | PT.TypeDeclaration.Enum cases ->
    cases
    |> NEList.toList
    |> List.collect (fun c -> c.fields |> List.collect (fun f -> inTypeRef f.typ))


/// What one op leaves unresolved, with a label for the thing it is talking about.
///
/// Only the content-carrying ops can hold a reference. `SetName` binds an existing hash, and the decision
/// ops (`Deprecate`, `Resolve`, `Decide`) carry no expressions.
let inOp (op : PT.PackageOp) : Option<string * List<string>> =
  let found =
    match op with
    | PT.PackageOp.AddFn fn ->
      let (PT.Hash hash) = fn.hash
      Some(
        hash,
        inExpr fn.body
        @ (fn.parameters |> NEList.toList |> List.collect (fun p -> inTypeRef p.typ))
        @ inTypeRef fn.returnType
      )
    | PT.PackageOp.AddValue v ->
      let (PT.Hash hash) = v.hash
      Some(hash, inExpr v.body)
    | PT.PackageOp.AddType t ->
      let (PT.Hash hash) = t.hash
      Some(hash, inTypeDeclaration t.declaration)
    | PT.PackageOp.SetName _
    | PT.PackageOp.Deprecate _
    | PT.PackageOp.Undeprecate _
    | PT.PackageOp.Resolve _
    | PT.PackageOp.Decide _
    // A branch event names a branch, not a package item, so there is no reference in it to be unresolved.
    | PT.PackageOp.BranchEvent _ -> None

  match found with
  | Some(_, []) -> None
  | Some(hash, names) -> Some(hash, List.distinct names)
  | None -> None
