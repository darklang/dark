/// Write onto each trait-method call the implementation it resolves to, BEFORE the batch is
/// hashed, so a stored call goes on running what it was written against and a newer
/// implementation arrives as an ordinary repoint rather than changing the call underneath it.
///
/// The at-rest checker says which implementations apply at each call node; the store says
/// which of them is newer. This module is only the join of those two answers onto the ops.
///
/// It lives in Builtins rather than in LibDB because the checker is written in Dark, so
/// resolving a call means EXECUTING Dark, which needs an execution state.
module Builtins.Matter.Libs.PM.TraitCalls

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PTAst = LibExecution.ProgramTypesAst
module PackageRefs = LibExecution.PackageRefs
module Exe = LibExecution.Execution
module Dval = LibExecution.Dval
module Dependencies = LibDB.DependencyExtractor


// --------------------
// What the checker worked out, decoded from its report.
// --------------------

/// One `LanguageTools.AtRestTypeChecker.Model.Resolution`, as F# sees it.
///
/// `at` is widened to `id` here rather than in Dark: node ids are `Int64` throughout the
/// solver (`Model.Constraint.at` and the rest), and `id` is `uint64`, so the one conversion
/// belongs at the language boundary where everything else is being decoded anyway.
type private Resolution =
  | Call of at : id * method_ : string * impls : List<PT.Hash>
  | Deferred of at : id * param : string
  | CallerBound of
    at : id *
    param : string *
    trait_ : PT.FQTraitName.Package *
    impls : List<PT.Hash>
  | CallerBoundDeferred of
    at : id *
    param : string *
    trait_ : PT.FQTraitName.Package *
    fromParam : string


/// Decoders that ANSWER NOTHING rather than raising, unlike `PT2DT`'s.
///
/// Type arguments are matched with a wildcard rather than against `[]`. A DEnum built by Dark
/// does not necessarily carry the same type arguments as the same case built by `PT2DT`, and a
/// pattern that insists on `[]` then matches nothing while looking completely reasonable: it
/// cost an hour of hunting for a producer that was working the whole time.
///
/// A resolution this build cannot read must mean "no pin", exactly as a call the checker
/// could not resolve means "no pin": the op goes un-rewritten and the runtime dispatches.
/// Raising here would turn a report we merely failed to understand into a failed save, and
/// the whole point of the three-state `ImplChoice` is that not knowing is representable.
module private Decode =
  let hash (d : Dval) : Option<PT.Hash> =
    match d with
    | DEnum(_, _, _, "Hash", [ DString h ]) -> Some(PT.Hash h)
    | _ -> None

  let hashes (d : Dval) : List<PT.Hash> =
    match d with
    | DList(_, items) -> items |> List.choose hash
    | _ -> []

  let private nodeId (d : Dval) : Option<id> =
    match d with
    | DInt64 i when i >= 0L -> Some(uint64 i)
    | _ -> None

  let resolution (d : Dval) : Option<Resolution> =
    match d with
    | DEnum(_, _, _, "Call", [ at; DString method_; impls ]) ->
      nodeId at |> Option.map (fun at -> Call(at, method_, hashes impls))
    | DEnum(_, _, _, "Deferred", [ at; DString param ]) ->
      nodeId at |> Option.map (fun at -> Deferred(at, param))
    | DEnum(_, _, _, "CallerBound", [ at; DString param; trait_; impls ]) ->
      match nodeId at, hash trait_ with
      | Some at, Some trait_ -> Some(CallerBound(at, param, trait_, hashes impls))
      | _ -> None
    | DEnum(_,
            _,
            _,
            "CallerBoundDeferred",
            [ at; DString param; trait_; DString from ]) ->
      match nodeId at, hash trait_ with
      | Some at, Some trait_ -> Some(CallerBoundDeferred(at, param, trait_, from))
      | _ -> None
    | _ -> None

  let private field (name : string) (fields : DvalMap) : Option<Dval> =
    Map.tryFind name fields

  /// The item each `ItemReport` is about, and what it resolved, for the two kinds that have
  /// a body to resolve anything in. A type, trait or impl reports no resolutions.
  let private itemReport (d : Dval) : Option<PT.Hash * List<Resolution>> =
    match d with
    | DRecord(_, _, _, fields) ->
      let resolutions =
        match field "resolutions" fields with
        | Some(DList(_, items)) -> items |> List.choose resolution
        | _ -> []
      match field "item" fields with
      | Some(DEnum(_, _, _, ("PackageFn" | "PackageValue"), [ h ])) ->
        hash h |> Option.map (fun h -> (h, resolutions))
      | _ -> None
    | _ -> None

  /// `LanguageTools.AtRestTypeChecker.Report` -> what each item resolved, by item hash.
  let report (d : Dval) : Map<PT.Hash, List<Resolution>> =
    match d with
    | DRecord(_, _, _, fields) ->
      match field "items" fields with
      | Some(DList(_, items)) ->
        items
        |> List.choose itemReport
        |> List.filter (fun (_, resolutions) -> not (List.isEmpty resolutions))
        |> Map.ofList
      | _ -> Map.empty
    | _ -> Map.empty

// --------------------
// Why there is no cache here
// --------------------
//
// A whole-tree check is about 84 seconds of interpreted Dark, and the dev reload pays it on
// every `.dark` change because that is how pinning learns what to pin. The obvious fix is to
// reuse what the last reload worked out, keyed by each item's content-addressed hash, and it
// does not work. Written down because the design looks right and fails in a way that reports
// success.
//
// A resolution is keyed BY NODE ID, and the parser generates node ids fresh on every parse:
// the same source parsed twice gives one `EInfix` id of 574556921872619684 and then
// 55962466547940726. Content hashing ignores ids, which is what makes the content hash a
// stable cache KEY, and is exactly why the cached VALUE is worthless: the entry hits, hands
// back resolutions addressed to nodes this parse never created, and `AstTransformer` applies
// none of them. Measured: 3,653 resolutions reused across 5,870 items, 0 ops pinned, against
// 4,586 pinned by the same tree with no cache. Fast, agreed with itself, and wrong.
//
// What would work, and is a follow-up rather than a line: cache the finished PINNED OP rather
// than the resolutions. An op carries its own ids, ids do not participate in hashing, so
// substituting last run's pinned op for this run's unpinned one yields the same content hash
// as pinning it afresh. That needs the binary serializer and its own correctness argument.
//
// `notes/traits/warm-vs-cold.sh` is what caught this, by comparing the PIN COUNT rather than
// the time. A cache that never hits and a cache that hits uselessly both look like a cache
// that works if you only measure agreement or only measure speed.

// --------------------
// Resolving.
// --------------------

let private checkPackageOpsName () =
  RT.FQFnName.fqPackage (
    PackageRefs.Fn.LanguageTools.AtRestTypeChecker.checkPackageOpsOn ()
  )

let private packageOpKT () =
  KTCustomType(
    FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.ProgramTypes.packageOp ()),
    []
  )


/// Ask the Dark checker what every call in this batch resolves to.
///
/// A checker that fails to answer leaves the batch alone, the same way a call it could not
/// resolve does. This is dev tooling and an authoring path, not a place to lose a save over
/// the checker having a bad day.
let private askChecker
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Ply<Map<PT.Hash, List<Resolution>>> =
  uply {
    // The branch explicitly, not whatever `exeState` happens to carry: a call is pinned to the
    // implementation live on the branch being SAVED to.
    let args =
      NEList.ofList
        (DUuid branchId.Guid)
        [ Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT) ]
    match! Exe.executeFunction exeState (checkPackageOpsName ()) [] args with
    | Ok report ->
      let decoded = Decode.report report
      // An empty report and an empty answer are the same zero here; `guarded` puts the
      // reason in `warnings`, which this decoder otherwise drops.
      if Map.isEmpty decoded then
        let warnings =
          match report with
          | DRecord(_, _, _, fields) ->
            match Map.tryFind "warnings" fields with
            | Some(DList(_, ws)) -> List.length ws
            | _ -> 0
          | _ -> 0
        if warnings > 0 then
          print
            $"  [traitcalls] the checker reported no resolutions and {warnings} warning(s); the batch was not checked"
      return decoded
    | Error(rte, _) ->
      // A swallowed checker failure and a checker with nothing to say are the same zero, and
      // the zero is what nobody re-checks, so say which one happened. Kept deliberately: the
      // Dark side catches per item and turns each failure into an ordinary verdict, so this is
      // the only place a SYSTEMIC failure of the check would otherwise be invisible.
      print $"  [traitcalls] the at-rest checker did not answer: {rte}"
      return Map.empty
  }


/// Whether this batch could have anything to resolve at all.
///
/// An ordinary save of a fn with no trait call and no operator pays only these AST walks: the
/// checker is not cheap, and on a whole-tree reload it is most of the work. An operator IS a
/// trait method, so the cheap test is "does this mention a trait, or any operator at all".
///
/// A unary operator counts: `-x` and `~x` are stored as their builtins and run as `Negate` and
/// `BitwiseNot`, and a body whose only operator was one of them never reached the checker.
let private isUnaryOperator (nr : PT.NameResolution<PT.FQFnName.FQFnName>) : bool =
  match nr.resolved with
  | Ok { name = PT.FQFnName.Builtin { name = name; version = 0 } } ->
    name = PT.InfixFnName.negateBuiltinName
    || name = PT.InfixFnName.bitwiseNotBuiltinName
  | _ -> false

let rec private hasInfix (expr : PT.Expr) : bool =
  match expr with
  | PT.EInfix _ -> true
  | PT.EFnName(_, nr, _) when isUnaryOperator nr -> true
  | PT.EPipe(_, first, parts) ->
    hasInfix first
    || parts
       |> List.exists (fun p ->
         match p with
         | PT.EPipeInfix _ -> true
         | _ -> false)
    || (PTAst.subExprs expr |> List.exists hasInfix)
  | _ -> PTAst.subExprs expr |> List.exists hasInfix


/// A trait method named outright (`Show.show x`), as opposed to an operator.
let rec private namesATraitMethod (expr : PT.Expr) : bool =
  match expr with
  | PT.EFnName(_, { resolved = Ok { name = PT.FQFnName.TraitMethod _ } }, _) -> true
  | _ -> PTAst.subExprs expr |> List.exists namesATraitMethod


/// Whether this batch could have anything to resolve at all.
///
/// An ordinary save of a fn with no trait call and no operator pays only these AST walks.
/// The package functions an expression calls by name, piped calls included: `n |> Stdlib.toString`
/// names its fn in the pipe part rather than in an `EFnName`, and a save whose only bounded call is
/// piped decides on this whether it reaches the checker at all.
let rec private calledPackageFns (expr : PT.Expr) : List<PT.Hash> =
  let here =
    match expr with
    | PT.EFnName(_, { resolved = Ok { name = PT.FQFnName.Package h } }, _) -> [ h ]
    | PT.EPipe(_, _, parts) ->
      parts
      |> List.choose (function
        | PT.EPipeFnCall(_,
                         { resolved = Ok { name = PT.FQFnName.Package h } },
                         _,
                         _,
                         _) -> Some h
        | _ -> None)
    | _ -> []
  here @ (PTAst.subExprs expr |> List.collect calledPackageFns)


/// Could anything in this batch have a choice to record?
///
/// Three clauses, and the third is the one an authoring batch needs. A save of
/// `let f () = Stdlib.max 1L 2L` contains no operator and names no trait: its only dependency
/// is on `Stdlib.max`, an ordinary package fn. But `max` is BOUNDED, so this call is the only
/// place that can record which implementation its type argument implied, and skipping the
/// batch means a bounded call authored through the CLI records nothing.
///
/// The cheap clauses run first and `List.exists` short-circuits, so a whole-tree reload
/// answers on the first operator it meets and never reaches the store lookups below.
let private worthChecking
  (pm : PT.PackageManager)
  (ops : List<PT.PackageOp>)
  : Ply<bool> =
  uply {
    let cheaply =
      ops
      |> List.exists (fun op ->
        match op with
        | PT.PackageOp.AddFn fn ->
          hasInfix fn.body
          || namesATraitMethod fn.body
          || (Dependencies.extractFromFn fn
              |> List.exists (fun d -> d.itemKind = PT.ItemKind.Trait))
        // A value's body is an expression too, and `let scale = 3L * 4L` is an operator call
        // the checker types the same way. It is evaluated once at load rather than per call,
        // so this is not about speed; it is about the value meaning the same thing after
        // someone else's implementation arrives.
        | PT.PackageOp.AddValue value -> hasInfix value.body
        | _ -> false)

    if cheaply then
      return true
    else
      let called =
        ops
        |> List.collect (fun op ->
          match op with
          | PT.PackageOp.AddFn fn -> calledPackageFns fn.body
          | PT.PackageOp.AddValue value -> calledPackageFns value.body
          | _ -> [])
        |> List.distinct

      // Whether any of them is bounded. Sequential and short-circuiting by hand, because a
      // batch that reaches here is an authoring batch of one or two items with a handful of
      // calls, and the lookups are what the store caches anyway.
      let mutable bounded = false
      for h in called do
        if not bounded then
          match! pm.getFn h with
          | Some fn -> bounded <- not (List.isEmpty fn.bounds)
          | None -> ()

      return bounded
  }


/// Write onto each trait-method call in <param ops> the implementation it resolves to.
///
/// <param branchId> separately from <param pm>, because `pm` carries the branch's names and
/// not its deprecations. <param storeHoldsOps> lets a large batch be checked in parallel.
let private resolveIn
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (pm : PT.PackageManager)
  (storeHoldsOps : bool)
  (ops : List<PT.PackageOp>)
  : Ply<List<PT.PackageOp>> =
  uply {
    let! worth = worthChecking pm ops
    if not worth then
      return ops
    else
      // A deprecated implementation is not a candidate at run time, so it must not be one
      // here either: `dark constraints` tells you to deprecate one of two rivals, and pinning
      // the one you just retired would make that advice a trap.
      let! deprecated = LibDB.Queries.getDeprecatedTraitImplHashesFor branchId
      let! storedStamps = LibDB.Queries.getTraitImplStamps ()

      // The batch's own traits and implementations, which the store does not hold until after
      // this. A call saved together with the implementation it resolved to is pinned to it, and
      // that implementation is the newest there is, so it carries a stamp from now.
      let batchTraits =
        ops
        |> List.choose (function
          | PT.PackageOp.AddTrait t -> Some(t.hash, t)
          | _ -> None)
        |> Map.ofList
      let batchImpls =
        ops
        |> List.choose (function
          | PT.PackageOp.AddTraitImpl i -> Some(i.hash, i)
          | _ -> None)
        |> Map.ofList
      let getTrait (h : PT.FQTraitName.Package) : Ply<Option<PT.Trait.Trait>> =
        match Map.tryFind h batchTraits with
        | Some t -> Ply(Some t)
        | None -> pm.getTrait h
      let getTraitImpl (h : PT.Hash) : Ply<Option<PT.TraitImpl.TraitImpl>> =
        match Map.tryFind h batchImpls with
        | Some i -> Ply(Some i)
        | None -> pm.getTraitImpl h
      let stamps =
        batchImpls
        |> Map.fold
          (fun found (PT.Hash h) _ ->
            if Map.containsKey h found then
              found
            else
              Map.add h (LibDB.OriginTs.next ()) found)
          storedStamps

      // Chunks load what they do not carry from the store, so only a caller whose batch is
      // already in the store can split it; an authoring batch is not, and stays whole. The
      // count is fixed rather than per-core so every machine pins the same tree: two items
      // with one hash get only one copy pinned, and which one depends on the split.
      let size =
        if storeHoldsOps then
          max 256 (List.length ops / 32 + 1)
        else
          max 1 (List.length ops)
      let! chunks =
        ops
        |> List.chunkBySize size
        |> List.map (fun chunk ->
          System.Threading.Tasks.Task.Run<Map<PT.Hash, List<Resolution>>>(fun () ->
            Ply.toTask (askChecker exeState branchId chunk)))
        |> System.Threading.Tasks.Task.WhenAll
      let resolved = chunks |> Array.fold (Map.foldBack Map.add) Map.empty

      if Map.isEmpty resolved then
        return ops
      else

        /// The winner among the implementations that apply, and the fn it names for the method.
        let implFnFor
          (method_ : string)
          (implHashes : List<PT.Hash>)
          : Ply<Option<PT.ResolvedName<PT.FQFnName.Package>>> =
          uply {
            let implHashes =
              implHashes
              |> List.filter (fun (PT.Hash h) -> not (Set.contains h deprecated))
            // One implementation is not an ordering question: it is the answer. Several are,
            // and an unstamped pair has no answer, so the call is left to resolve at run time
            // and `dark constraints` reports the pair.
            let winner =
              match implHashes with
              | [] -> None
              | [ only ] -> Some only
              | several ->
                several
                |> List.map (fun (PT.Hash h as hash) ->
                  (hash, stamps |> Map.tryFind h |> Option.defaultValue "", h))
                |> LibExecution.Lww.winnerOf
            match winner with
            | None -> return None
            | Some winner ->
              let! impl = getTraitImpl winner
              // The implementation's own reference to the fn, location and all, which is why
              // the edge this produces reads like any other and a rename reaches it.
              return
                impl
                |> Option.bind (fun i ->
                  i.methods
                  |> List.tryPick (fun (name, nr) ->
                    if name <> method_ then
                      None
                    else
                      match nr.resolved with
                      | Ok { name = PT.FQFnName.Package h; location = loc } ->
                        Some { name = h; location = loc }
                      | _ -> None))
          }

        let pinsByItem = Dictionary<PT.Hash, Map<id, PT.FQFnName.ImplChoice>>()
        let boundsByItem =
          Dictionary<PT.Hash, Map<id, List<PT.FQFnName.BoundImpl>>>()

        for KeyValue(itemHash, resolutions) in resolved do
          // Accumulated in loops rather than folds: the trait and the impl fn are both awaited,
          // so the fold version is three nested `uply` continuations deep.
          let mutable pins = Map.empty
          let mutable bounds = Map.empty

          for resolution in resolutions do
            match resolution with
            | Call(at, method_, implHashes) ->
              match! implFnFor method_ implHashes with
              | Some implFn -> pins <- Map.add at (PT.FQFnName.Chosen implFn) pins
              | None -> ()
            | _ -> ()

          // A call whose self type is one of the item's own type params is not a call nobody
          // could work out: it is waiting for its caller, and it says so. Second, so that a
          // call the checker actually resolved wins over a deferral at the same node.
          for resolution in resolutions do
            match resolution with
            | Deferred(at, param) ->
              if not (Map.containsKey at pins) then
                pins <- Map.add at (PT.FQFnName.FromTypeParam param) pins
            | _ -> ()

          // The other half: what this item's CALLS worked out for the bounds of the fns they
          // name. That is what makes a call into a bounded fn static: the callee's body defers
          // to its type param, and the call says which implementation that param implied.
          for resolution in resolutions do
            match resolution with
            | CallerBound(at, param, traitHash, implHashes) ->
              match! getTrait traitHash with
              | Some trait_ ->
                // One entry per method of the trait, so the callee's body finds a fn for
                // whichever method it calls without reading the implementation item at run time.
                for m in NEList.toList trait_.methods do
                  match! implFnFor m.name implHashes with
                  | Some implFn ->
                    let entry : PT.FQFnName.BoundImpl =
                      { param = param
                        trait_ = traitHash
                        method_ = m.name
                        choice = PT.FQFnName.Chosen implFn }
                    let existing = Map.tryFind at bounds |> Option.defaultValue []
                    bounds <- Map.add at (entry :: existing) bounds
                  | None -> ()
              | None -> ()
            | CallerBoundDeferred(at, param, traitHash, fromParam) ->
              match! getTrait traitHash with
              | Some trait_ ->
                for m in NEList.toList trait_.methods do
                  let entry : PT.FQFnName.BoundImpl =
                    { param = param
                      trait_ = traitHash
                      method_ = m.name
                      choice = PT.FQFnName.FromTypeParam fromParam }
                  let existing = Map.tryFind at bounds |> Option.defaultValue []
                  bounds <- Map.add at (entry :: existing) bounds
              | None -> ()
            | _ -> ()

          if not (Map.isEmpty pins) then pinsByItem[itemHash] <- pins
          if not (Map.isEmpty bounds) then boundsByItem[itemHash] <- bounds

        let mappingFor (hash : PT.Hash) : Option<LibDB.AstTransformer.HashMapping> =
          let pins =
            match pinsByItem.TryGetValue hash with
            | true, pins -> pins
            | _ -> Map.empty
          let bounds =
            match boundsByItem.TryGetValue hash with
            | true, bounds -> bounds
            | _ -> Map.empty
          if Map.isEmpty pins && Map.isEmpty bounds then
            None
          else
            Some
              { LibDB.AstTransformer.emptyMapping with
                  pins = pins
                  boundImpls = bounds }

        return
          ops
          |> List.map (fun op ->
            match op with
            | PT.PackageOp.AddFn fn ->
              match mappingFor fn.hash with
              | Some mapping ->
                PT.PackageOp.AddFn(LibDB.AstTransformer.transformFn mapping fn)
              | None -> op
            | PT.PackageOp.AddValue value ->
              match mappingFor value.hash with
              | Some mapping ->
                PT.PackageOp.AddValue(
                  LibDB.AstTransformer.transformValue mapping value
                )
              | None -> op
            | _ -> op)
  }


// --------------------
// Saying what was left unpinned.
// --------------------

/// The calls in <param expr> that will be decided at run time rather than by this save: a trait
/// method or a trait operator still `Unknown`, and a call into a BOUNDED fn that recorded nothing
/// for its bounds (so the callee's deferral has nothing to read). <param isBounded> answers for a
/// callee by hash.
let rec private unpinned (isBounded : PT.Hash -> bool) (expr : PT.Expr) : int =
  let traitOperator (infix : PT.Infix) =
    match infix with
    | PT.InfixFnCall name -> Option.isSome (LibExecution.NumericTraits.ofInfix name)
    | PT.BinOp _ -> false
  let named (nr : PT.NameResolution<PT.FQFnName.FQFnName>) (recorded : bool) =
    match nr.resolved with
    | Ok { name = PT.FQFnName.TraitMethod { implFn = PT.FQFnName.Unknown } } -> 1
    | Ok { name = PT.FQFnName.Package h } when not recorded && isBounded h -> 1
    // A pinned `-x` names the trait method, so one still naming the builtin was not pinned.
    | _ when
      isUnaryOperator nr && Option.isSome (LibExecution.NumericTraits.ofNegate ())
      ->
      1
    | _ -> 0
  let here =
    match expr with
    | PT.EFnName(_, nr, boundImpls) -> named nr (not (List.isEmpty boundImpls))
    | PT.EInfix(_, infix, _, _, PT.FQFnName.Unknown) when traitOperator infix -> 1
    | PT.EPipe(_, _, parts) ->
      parts
      |> List.sumBy (fun part ->
        match part with
        | PT.EPipeFnCall(_, nr, _, _, boundImpls) ->
          named nr (not (List.isEmpty boundImpls))
        | PT.EPipeInfix(_, infix, _, PT.FQFnName.Unknown) when traitOperator infix ->
          1
        | _ -> 0)
    | _ -> 0
  here + (PTAst.subExprs expr |> List.sumBy (unpinned isBounded))


/// Write onto each trait-method call in <param ops> the implementation it resolves to, and on an
/// authoring save, say how many calls it could not.
///
/// The count is what makes a partly pinned save visible. A save the checker could not settle at
/// all printed a line; one it settled partly printed nothing, and its unsettled calls ran whichever
/// implementation was newest, which a later rival could change without anyone being told.
let resolveTraitCalls
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (pm : PT.PackageManager)
  (storeHoldsOps : bool)
  (ops : List<PT.PackageOp>)
  : Ply<List<PT.PackageOp>> =
  uply {
    let! resolved = resolveIn exeState branchId pm storeHoldsOps ops

    // A reload has its own report (`Resolved trait calls: ...`); this is for a person saving.
    if not storeHoldsOps then
      let batchFns =
        resolved
        |> List.choose (function
          | PT.PackageOp.AddFn fn -> Some(fn.hash, fn)
          | _ -> None)
        |> Map.ofList
      let names =
        resolved
        |> List.choose (function
          | PT.PackageOp.SetName(loc, PT.Reference.PackageFn h, _)
          | PT.PackageOp.SetName(loc, PT.Reference.PackageValue h, _) ->
            Some(h, String.concat "." (loc.owner :: loc.modules @ [ loc.name ]))
          | _ -> None)
        |> Map.ofList

      // Which callees are bounded, looked up once each.
      let callees =
        resolved
        |> List.collect (function
          | PT.PackageOp.AddFn fn -> calledPackageFns fn.body
          | PT.PackageOp.AddValue v -> calledPackageFns v.body
          | _ -> [])
        |> List.distinct
      let mutable bounded = Set.empty
      for h in callees do
        match Map.tryFind h batchFns with
        | Some fn ->
          if not (List.isEmpty fn.bounds) then bounded <- Set.add h bounded
        | None ->
          match! pm.getFn h with
          | Some fn ->
            if not (List.isEmpty fn.bounds) then bounded <- Set.add h bounded
          | None -> ()
      let isBounded h = Set.contains h bounded

      let left =
        resolved
        |> List.choose (fun op ->
          let hash, count =
            match op with
            | PT.PackageOp.AddFn fn -> fn.hash, unpinned isBounded fn.body
            | PT.PackageOp.AddValue v -> v.hash, unpinned isBounded v.body
            | _ -> PT.Hash "", 0
          if count = 0 then
            None
          else
            // A name when the save carries one; `dark fn` saves the body before its name.
            let (PT.Hash h) = hash
            let name =
              Map.tryFind hash names
              |> Option.defaultValue (
                if h.Length > 8 then h.Substring(0, 8) else "an item"
              )
            Some $"{name} ({count})")

      if not (List.isEmpty left) then
        let total = List.length left
        let which = String.concat ", " left
        print
          $"  [traitcalls] trait calls this save could not pin, in {total} item(s), will run whichever implementation is newest when they run: {which}"

    return resolved
  }
