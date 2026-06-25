/// Static per-fn capability analysis, on ProgramTypes (the source AST), not the RT instruction stream.
/// Walk a fn's PT body for the fns it calls — a builtin contributes its declared caps
/// (`fn.capabilities`, looked up by the caller), a package fn is a node to recurse into. The transitive,
/// cycle-safe fold is `Capabilities.effectiveCaps`. PURE: the caller pre-loads the reachable PT fns
/// (the async load of that closure stays out of the fold).
module LibExecution.CapabilityAnalysis

open Prelude

module PT = LibExecution.ProgramTypes
module Caps = LibExecution.Capabilities

/// A resolved fn name, or nothing if it didn't resolve.
let private nameRef
  (nr : PT.NameResolution<PT.FQFnName.FQFnName>)
  : List<PT.FQFnName.FQFnName> =
  match nr.resolved with
  | Ok resolved -> [ resolved.name ]
  | Error _ -> []

/// Every fn-name an expression references — `EFnName` and the piped `EPipeFnCall` — recursing all
/// subexpressions. Mirrors `DependencyExtractor.extractFromExpr`, but collects fn names not deps.
let rec fnRefs (expr : PT.Expr) : List<PT.FQFnName.FQFnName> =
  let r = fnRefs
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
  | PT.ESelf _
  | PT.EValue _ -> []

  | PT.EFnName(_, nr) -> nameRef nr

  | PT.EString(_, segments) -> segments |> List.collect segRefs
  | PT.EIf(_, c, t, e) ->
    List.concat [ r c; r t; e |> Option.map r |> Option.defaultValue [] ]
  | PT.EPipe(_, lhs, parts) -> List.concat [ r lhs; parts |> List.collect pipeRefs ]
  | PT.EMatch(_, arg, cases) -> List.concat [ r arg; cases |> List.collect caseRefs ]
  | PT.ELet(_, _, value, body) -> List.concat [ r value; r body ]
  | PT.EList(_, items) -> items |> List.collect r
  | PT.EDict(_, pairs) -> pairs |> List.collect (snd >> r)
  | PT.ETuple(_, first, second, rest) ->
    List.concat [ r first; r second; rest |> List.collect r ]
  | PT.EApply(_, fnExpr, _, args) ->
    List.concat [ r fnExpr; args |> NEList.toList |> List.collect r ]
  | PT.ELambda(_, _, body) -> r body
  | PT.EInfix(_, _, lhs, rhs) -> List.concat [ r lhs; r rhs ]
  | PT.ERecord(_, _, _, fields) -> fields |> List.collect (snd >> r)
  | PT.ERecordFieldAccess(_, record, _) -> r record
  | PT.ERecordUpdate(_, record, updates) ->
    List.concat [ r record; updates |> NEList.toList |> List.collect (snd >> r) ]
  | PT.EEnum(_, _, _, _, fields) -> fields |> List.collect r
  | PT.EStatement(_, first, next) -> List.concat [ r first; r next ]

and private segRefs (segment : PT.StringSegment) : List<PT.FQFnName.FQFnName> =
  match segment with
  | PT.StringText _ -> []
  | PT.StringInterpolation expr -> fnRefs expr

and private caseRefs (case : PT.MatchCase) : List<PT.FQFnName.FQFnName> =
  List.concat
    [ case.whenCondition |> Option.map fnRefs |> Option.defaultValue []
      fnRefs case.rhs ]

and private pipeRefs (pe : PT.PipeExpr) : List<PT.FQFnName.FQFnName> =
  match pe with
  | PT.EPipeLambda(_, _, body) -> fnRefs body
  | PT.EPipeInfix(_, _, rhs) -> fnRefs rhs
  | PT.EPipeFnCall(_, nr, _, args) ->
    List.concat [ nameRef nr; args |> List.collect fnRefs ]
  | PT.EPipeEnum(_, _, _, fields) -> fields |> List.collect fnRefs
  | PT.EPipeVariable(_, _, args) -> args |> List.collect fnRefs


/// Effective needs of a package fn = the field-wise JOIN of the needs of every builtin reachable
/// through its PT call graph. `capsFor` looks up a builtin's declared `fn.capabilities`; `getFn`
/// resolves a package-fn name to its pre-loaded PT body; cycles terminate via a visited set.
let effectiveCapsOfFn
  (capsFor : string -> Caps.Capabilities)
  (getFn : PT.FQFnName.Package -> Option<PT.PackageFn.PackageFn>)
  (root : PT.FQFnName.Package)
  : Caps.Capabilities =
  let mutable visited = Set.empty
  let mutable acc = Caps.noCaps
  let rec go (p : PT.FQFnName.Package) : unit =
    if not (Set.contains p visited) then
      visited <- Set.add p visited
      match getFn p with
      | Some fn ->
        for fqfn in fnRefs fn.body do
          match fqfn with
          | PT.FQFnName.Builtin b -> acc <- Caps.join acc (capsFor b.name)
          | PT.FQFnName.Package q -> go q
      | None -> ()
  go root
  acc
