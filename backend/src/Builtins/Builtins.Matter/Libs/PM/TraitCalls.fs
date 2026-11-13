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
/// A resolution this build cannot read must mean "nothing recorded", exactly as a call the
/// checker could not resolve does: the op goes un-rewritten and the runtime dispatches.
/// Raising here would turn a report we merely failed to understand into a failed save, and
/// the whole point of the three-state `ImplChoice` is that not knowing is representable.
module private Decode =
  let hash (d : Dval) : Option<PT.Hash> =
    match d with
    | DEnum(_, _, _, "Hash", [ DString h ]) -> Some(PT.Hash h)
    | _ -> None

  let hashes (d : Dval) : List<PT.Hash> =
    match d with
    | DList(_, Items(items)) -> items |> List.choose hash
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

  let private field (name : string) (fields : RecordFields) : Option<Dval> =
    RecordFields.tryFind name fields

  /// The item each `ItemReport` is about, and what it resolved, for the three kinds that have
  /// a body to resolve anything in. A type, trait or impl reports no resolutions.
  let private itemReport (d : Dval) : Option<PT.Hash * List<Resolution>> =
    match d with
    | DRecord(_, _, _, fields) ->
      let resolutions =
        match field "resolutions" fields with
        | Some(DList(_, Items(items))) -> items |> List.choose resolution
        | _ -> []
      match field "item" fields with
      | Some(DEnum(_, _, _, ("PackageFn" | "PackageValue" | "PackageTest"), [ h ])) ->
        hash h |> Option.map (fun h -> (h, resolutions))
      | _ -> None
    | _ -> None

  /// `LanguageTools.AtRestTypeChecker.Report` -> what each item resolved, by item hash. An item
  /// that resolved nothing is kept, with no resolutions: the reload's cache records that answer too.
  let report (d : Dval) : Map<PT.Hash, List<Resolution>> =
    match d with
    | DRecord(_, _, _, fields) ->
      match field "items" fields with
      | Some(DList(_, Items(items))) -> items |> List.choose itemReport |> Map.ofList
      | _ -> Map.empty
    | _ -> Map.empty

  /// The items the checker found a definite type error in.
  let failed (d : Dval) : Set<PT.Hash> =
    match d with
    | DRecord(_, _, _, fields) ->
      match field "items" fields with
      | Some(DList(_, Items(items))) ->
        items
        |> List.choose (fun item ->
          match item with
          | DRecord(_, _, _, fields) ->
            match field "verdict" fields, field "item" fields with
            | Some(DEnum(_, _, _, "Failed", [])), Some(DEnum(_, _, _, _, [ h ])) ->
              hash h
            | _ -> None
          | _ -> None)
        |> Set.ofList
      | _ -> Set.empty
    | _ -> Set.empty

// --------------------
// The reload's cache
// --------------------
//
// A whole-tree check is the biggest part of a package reload, and most bodies are the same as last
// time. So a reload keeps what the checker answered for each body (`LibDB.TraitCallCache`) and
// asks it only about the others.
//
// An answer is addressed BY NODE ID, and the parser makes new ids on every parse: the same source
// parsed twice gives one `EInfix` id of 574556921872619684 and then 55962466547940726. Caching the
// ids themselves was tried, and it failed while looking fine: it hit for 3,653 items and rewrote 0
// ops, because no id matched (`notes/traits/warm-vs-cold.sh` caught it by comparing the RESOLVED
// COUNT, not the time). So the cache keeps each node as its POSITION in the body (`Positions`),
// and a hit turns positions back into this parse's ids. A content hash ignores ids but not shape,
// so two bodies with one hash have their nodes in the same places. The node count is stored too,
// and a body that disagrees is a miss.
//
// Only the checker's answer is cached, applied to this parse's op. The op itself is never reused:
// docs are in the op but not in its hash, so last run's op could carry last run's docs. Which
// implementation wins among several (`implFnFor`) is still worked out every time, from the store.
//
// The key is the body's own hash and `environmentOf`. The body's hash covers everything it names,
// by hash, all the way down. `environmentOf` covers what the checker reads any other way: the
// implementations of a trait, names of types and traits (`Option`, `Result`), deprecations, the
// checker's own code, the builtins' signatures and the build, less the assemblies in
// `outsideTheChecker` but with the files of theirs the checker runs. Change any of those and
// every body is checked again.
//
// Only a reload uses it. A save is one or two items, and its batch is not in the store yet.

/// Where each node of a body is, so an answer can outlive the parse that produced it.
module private Positions =
  let private pipePartId (part : PT.PipeExpr) : id =
    match part with
    | PT.EPipeLambda(id, _, _)
    | PT.EPipeInfix(id, _, _, _)
    | PT.EPipeFnCall(id, _, _, _, _)
    | PT.EPipeEnum(id, _, _, _)
    | PT.EPipeVariable(id, _, _) -> id

  /// The ids a resolution can be at, in pre-order: each expression, then the parts of its pipe if
  /// it is one. An `at` that is not here makes the answer uncacheable rather than wrong.
  let ofBody (body : PT.Expr) : List<id> =
    let rec go (found : List<id>) (expr : PT.Expr) : List<id> =
      let found = PT.Expr.toID expr :: found
      let found =
        match expr with
        | PT.EPipe(_, _, parts) ->
          parts |> List.fold (fun found part -> pipePartId part :: found) found
        | _ -> found
      PTAst.subExprs expr |> List.fold go found
    go [] body |> List.rev

  /// The body of a fn, value or test op, with the key its answer is cached under.
  let bodyOf (op : PT.PackageOp) : Option<string * PT.Hash * PT.Expr> =
    match op with
    | PT.PackageOp.AddFn fn ->
      let (PT.Hash h) = fn.hash
      Some($"fn {h}", fn.hash, fn.body)
    | PT.PackageOp.AddValue value ->
      let (PT.Hash h) = value.hash
      Some($"value {h}", value.hash, value.body)
    | PT.PackageOp.AddTest test ->
      let (PT.Hash h) = test.hash
      Some($"test {h}", test.hash, test.body)
    | _ -> None


/// An answer as text: one line per resolution, tab-separated, each node as its position.
module private Encoding =
  let private writable (s : string) = not (s.Contains '\t' || s.Contains '\n')

  let private hashes (hs : List<PT.Hash>) : string =
    hs |> List.map (fun (PT.Hash h) -> h) |> String.concat " "

  let private readHashes (s : string) : List<PT.Hash> =
    s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.map PT.Hash

  /// <param resolutions> with each node as its index in <param body>'s positions. None when a node
  /// is not among them, or a name cannot be written.
  let encode (body : List<id>) (resolutions : List<Resolution>) : Option<string> =
    let positionOf =
      body
      |> List.indexed
      |> List.fold
        (fun found (i, id) ->
          if Map.containsKey id found then found else Map.add id i found)
        Map.empty
    let line (resolution : Resolution) : Option<string> =
      let at (id : id) (rest : List<string>) =
        Map.tryFind id positionOf |> Option.map (fun p -> string p :: rest)
      let fields =
        match resolution with
        | Call(id, method_, impls) ->
          at id [ method_; hashes impls ] |> Option.map (fun f -> "call" :: f)
        | Deferred(id, param) ->
          at id [ param ] |> Option.map (fun f -> "deferred" :: f)
        | CallerBound(id, param, PT.Hash trait_, impls) ->
          at id [ param; trait_; hashes impls ] |> Option.map (fun f -> "bound" :: f)
        | CallerBoundDeferred(id, param, PT.Hash trait_, fromParam) ->
          at id [ param; trait_; fromParam ]
          |> Option.map (fun f -> "boundDeferred" :: f)
      fields
      |> Option.filter (List.forall writable)
      |> Option.map (String.concat "\t")
    let lines = resolutions |> List.map line
    if List.forall Option.isSome lines then
      Some(lines |> List.choose identity |> String.concat "\n")
    else
      None

  /// What <param text> says, with each position as its id in <param body>. None when anything in
  /// it does not read, which makes it a miss.
  let decode (body : List<id>) (text : string) : Option<List<Resolution>> =
    let ids = Array.ofList body
    let at (p : string) : Option<id> =
      match System.Int32.TryParse p with
      | true, i when i >= 0 && i < ids.Length -> Some ids[i]
      | _ -> None
    let line (l : string) : Option<Resolution> =
      match l.Split('\t') |> List.ofArray with
      | [ "call"; p; method_; impls ] ->
        at p |> Option.map (fun id -> Call(id, method_, readHashes impls))
      | [ "deferred"; p; param ] ->
        at p |> Option.map (fun id -> Deferred(id, param))
      | [ "bound"; p; param; trait_; impls ] ->
        at p
        |> Option.map (fun id ->
          CallerBound(id, param, PT.Hash trait_, readHashes impls))
      | [ "boundDeferred"; p; param; trait_; fromParam ] ->
        at p
        |> Option.map (fun id ->
          CallerBoundDeferred(id, param, PT.Hash trait_, fromParam))
      | _ -> None
    if text = "" then
      Some []
    else
      let lines = text.Split('\n') |> List.ofArray |> List.map line
      if List.forall Option.isSome lines then
        Some(List.choose identity lines)
      else
        None


/// The assemblies a reload's build is hashed without (`HostProcess.runtimeHashWithout`). The
/// files of theirs the checker runs are hashed instead (`CheckerSources.checkerFiles`), so an
/// edit to one of those misses and an edit to any other file of theirs keeps the cache.
///
/// Kept in whole, edits to them miss: the interpreter and the types it runs on (LibExecution,
/// Prelude), the store the checker reads through (LibDB, LibSerialization) and the config it
/// reads (LibConfig), this module and the package reads it makes (Builtins.Matter), the conversion
/// of each signature to the value the checker reads (Builtins.Language), and LocalExec, which
/// builds the checker's execution state. So does any assembly not named here, a new one included.
let outsideTheChecker : Set<string> =
  Set.ofList
    [ "Builtins.Pure"
      "Builtins.Cli"
      "Builtins.CliHost"
      "Builtins.Http.Client"
      "Builtins.Http.Server"
      "Builtins.Random"
      "Builtins.Time"
      // Run by the checker only through a parser builtin, which `CheckerSources` follows.
      "LibParser"
      "TestUtils" ]


/// Every builtin fn's signature, everything but the description and the body, and every builtin
/// value but its description.
let private builtinSignatures (exeState : RT.ExecutionState) : List<string> =
  let fns =
    exeState.fns.builtIn
    |> Seq.map (fun (KeyValue(name, fn)) ->
      // Joined by hand: a list's ToString stops after three elements.
      let joined (items : List<'a>) = items |> List.map string |> String.concat "; "
      let parameters = fn.parameters |> List.map (fun p -> $"{p.name}: {p.typ}")
      sprintf
        "builtin fn %O"
        (name,
         joined fn.typeParams,
         joined parameters,
         fn.returnType,
         fn.previewable,
         fn.deprecated,
         joined (Set.toList fn.callEffects)))
  let values =
    exeState.values.builtIn
    |> Seq.map (fun (KeyValue(name, value)) ->
      sprintf "builtin value %O" (name, value.typ, value.deprecated, value.body))
  Seq.append fns values |> Seq.sort |> List.ofSeq


/// Everything a body's answer depends on that the body's own hash does not cover, as one hash.
///
/// The body's hash covers what it names, by hash and transitively, so fns, values and tests stay out:
/// editing one changes the hash of every body that reaches it, and those miss on their own. What
/// is in here is what the checker reads another way. The types, traits and implementations, and
/// the names bound to them, because it finds implementations and the standard types by name. The
/// deprecations and the stamps, since they decide which implementations count. The checker's own
/// entry point, whose hash covers the Dark it runs. The builtins' signatures, which it type-checks
/// builtin calls against; and <param build>, the reload's hash of the F# it runs on, this module
/// included and `outsideTheChecker` left out but for the files of theirs the checker runs.
///
/// Docs and reference names are left out: they say what something is called, not what it does.
let private environmentOf
  (exeState : RT.ExecutionState)
  (build : string)
  (branchId : PT.BranchId)
  (deprecated : Set<string>)
  (stamps : Map<string, string>)
  (ops : List<PT.PackageOp>)
  : string =
  let hash (PT.Hash h) = h
  let location (l : PT.PackageLocation) =
    String.concat "." (l.owner :: l.modules @ [ l.name ])
  let reference (r : PT.Reference) =
    match r with
    | PT.Reference.PackageType h -> $"type {hash h}"
    | PT.Reference.PackageValue h -> $"value {hash h}"
    | PT.Reference.PackageFn h -> $"fn {hash h}"
    | PT.Reference.PackageTrait h -> $"trait {hash h}"
    | PT.Reference.PackageTraitImpl h -> $"impl {hash h}"
    | PT.Reference.PackageTest h -> $"test {hash h}"
  let namesABody (r : PT.Reference) =
    match r with
    | PT.Reference.PackageFn _
    | PT.Reference.PackageValue _
    | PT.Reference.PackageTest _ -> true
    | _ -> false
  let fromOps =
    ops
    |> List.choose (fun op ->
      match op with
      | PT.PackageOp.AddFn _
      | PT.PackageOp.AddValue _
      | PT.PackageOp.AddTest _
      | PT.PackageOp.UpdateDoc _
      | PT.PackageOp.SetReferenceNames _ -> None
      | PT.PackageOp.SetName(_, target, _) when namesABody target -> None
      | PT.PackageOp.AddType t -> Some $"add type {hash t.hash}"
      | PT.PackageOp.AddTrait t -> Some $"add trait {hash t.hash}"
      | PT.PackageOp.AddTraitImpl i -> Some $"add impl {hash i.hash}"
      | PT.PackageOp.SetName(loc, target, _) ->
        Some $"name {location loc} {reference target}"
      // Unbind, Deprecate, Undeprecate, Rename, Decision: rare, small, and all of them can change
      // what a name or an implementation means.
      | other -> Some(sprintf "%O" other))
    |> List.sort
  // By stamp, then hash, as `Lww.winnerOf` would order them.
  let byStamp =
    stamps
    |> Map.toList
    |> List.sortBy (fun (h, stamp) -> (stamp, h))
    |> List.map fst
    |> String.concat " "
  let checker = PackageRefs.Fn.LanguageTools.AtRestTypeChecker.checkPackageOpsOn ()
  let lines =
    [ "trait-call-cache 3"
      $"runtime {build}"
      $"branch {branchId.Guid}"
      $"checker {checker}"
      $"stamps {byStamp}" ]
    @ builtinSignatures exeState
    @ (deprecated |> Set.toList |> List.map (fun h -> $"deprecated {h}"))
    @ fromOps
  String.concat "\n" lines
  |> System.Text.Encoding.UTF8.GetBytes
  |> System.Security.Cryptography.SHA256.HashData
  |> System.Convert.ToHexStringLower

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
/// the checker having a bad day. None is that failure, so the reload's cache does not record
/// it as an answer.
let private askChecker
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Ply<Option<Map<PT.Hash, List<Resolution>> * Set<PT.Hash>>> =
  uply {
    // The branch explicitly, not whatever `exeState` happens to carry: a call resolves to the
    // implementation live on the branch being SAVED to.
    let args =
      NEList.ofList
        (DUuid branchId.Guid)
        [ Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT) ]
    match! Exe.executeFunction exeState (checkPackageOpsName ()) [] args with
    | Ok report ->
      let decoded = Decode.report report
      let failed = Decode.failed report
      // An empty report and an empty answer are the same zero here; `guarded` puts the
      // reason in `warnings`, which this decoder otherwise drops.
      if
        Set.isEmpty failed
        && decoded |> Map.forall (fun _ resolutions -> List.isEmpty resolutions)
      then
        let warnings =
          match report with
          | DRecord(_, _, _, fields) ->
            match RecordFields.tryFind "warnings" fields with
            | Some(DList(_, Items(ws))) -> List.length ws
            | _ -> 0
          | _ -> 0
        if warnings > 0 then
          printErr
            $"  [traitcalls] the checker reported no resolutions and {warnings} warning(s); the batch was not checked"
          return None
        else
          return Some(decoded, failed)
      else
        return Some(decoded, failed)
    | Error(rte, _) ->
      // A swallowed checker failure and a checker with nothing to say are the same zero, and
      // the zero is what nobody re-checks, so say which one happened. Kept deliberately: the
      // Dark side catches per item and turns each failure into an ordinary verdict, so this is
      // the only place a SYSTEMIC failure of the check would otherwise be invisible.
      let shown = Exe.runtimeErrorDebugString rte
      printErr $"  [traitcalls] the at-rest checker did not answer: {shown}"
      return None
  }


/// Whether this batch could have anything to resolve at all.
///
/// An ordinary save of a fn with no trait call and no operator pays only these AST walks: the
/// checker is not cheap, and on a whole-tree reload it is most of the work. An operator IS a
/// trait method, so the cheap test is "does this mention a trait, or any operator at all".
let rec private hasInfix (expr : PT.Expr) : bool =
  match expr with
  | PT.EInfix _ -> true
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
  | PT.EFnName(_, { resolved = Ok { name = PT.FQFnName.TraitMethod _ } }, _, _) ->
    true
  | _ -> PTAst.subExprs expr |> List.exists namesATraitMethod


/// Whether this batch could have anything to resolve at all.
///
/// An ordinary save of a fn with no trait call and no operator pays only these AST walks.
/// The package functions an expression calls by name, piped calls included: `n |> Stdlib.display`
/// names its fn in the pipe part rather than in an `EFnName`, and a save whose only bounded call is
/// piped decides on this whether it reaches the checker at all.
let rec private calledPackageFns (expr : PT.Expr) : List<PT.Hash> =
  let here =
    match expr with
    | PT.EFnName(_, { resolved = Ok { name = PT.FQFnName.Package h } }, _, _) ->
      [ h ]
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
/// `let f() = Stdlib.max(1L, 2L)` contains no operator and names no trait: its only dependency
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
        | PT.PackageOp.AddValue value ->
          hasInfix value.body || namesATraitMethod value.body
        // A test body calls bounded generics and operators like any fn, and resolving them
        // is what lets the test runner prove it effect-free and run it in-process.
        | PT.PackageOp.AddTest test ->
          hasInfix test.body || namesATraitMethod test.body
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
          | PT.PackageOp.AddTest test -> calledPackageFns test.body
          | _ -> [])
        |> List.distinct

      // Whether any of them is bounded. Sequential and short-circuiting, because a batch that
      // reaches here is an authoring batch of one or two items with a handful of calls, and the
      // lookups are what the store caches anyway.
      return!
        called
        |> Ply.List.foldSequentially
          (fun bounded h ->
            if bounded then
              uply { return true }
            else
              uply {
                match! pm.getFn h with
                | Some fn -> return not (List.isEmpty fn.bounds)
                | None -> return false
              })
          false
  }


/// Write onto each trait-method call in <param ops> the implementation it resolves to.
///
/// <param branchId> separately from <param pm>, because `pm` carries the branch's names and
/// not its deprecations. <param reload> is the build a package reload runs on, None for a save:
/// a reload's batch is already in the store, which lets it be checked in parallel, and the build
/// keys its cache.
let private resolveIn
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (pm : PT.PackageManager)
  (reload : Option<string>)
  (ops : List<PT.PackageOp>)
  : Ply<List<PT.PackageOp> * Set<PT.Hash>> =
  uply {
    let storeHoldsOps = Option.isSome reload
    let! worth = worthChecking pm ops
    if not worth then
      return (ops, Set.empty)
    else
      // A deprecated implementation is not a candidate at run time, so it must not be one
      // here either: `dark constraints` tells you to deprecate one of two rivals, and resolving
      // to the one you just retired would make that advice a trap.
      let! deprecated = LibDB.Queries.getDeprecatedTraitImplHashesFor branchId
      let! storedStamps = LibDB.Queries.getTraitImplStamps ()

      // The batch's own traits and implementations, which the store does not hold until after
      // this. A call saved together with the implementation it resolved to keeps it, and
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

      // The reload's cache (see "The reload's cache" above). Each body's answer is looked up
      // for its LAST copy: two items with one hash are resolved through whichever copy the
      // checker reported, and a later report replaces an earlier one, chunk after chunk and
      // inside the checker's own fold, so that is the last.
      let environment =
        reload
        |> Option.map (fun build ->
          environmentOf exeState build branchId deprecated storedStamps ops)
      let! cached =
        match environment with
        | Some environment -> LibDB.TraitCallCache.load environment
        | None -> System.Threading.Tasks.Task.FromResult Map.empty
      let lastCopies =
        ops
        |> List.choose Positions.bodyOf
        |> List.fold
          (fun found (key, hash, body) -> Map.add key (hash, body) found)
          Map.empty
      let reused =
        lastCopies
        |> Map.toList
        |> List.choose (fun (key, (hash, body)) ->
          Map.tryFind key cached
          |> Option.bind (fun (entry : LibDB.TraitCallCache.Entry) ->
            let positions = Positions.ofBody body
            if List.length positions <> entry.nodes then
              None
            else
              Encoding.decode positions entry.resolutions
              |> Option.map (fun resolutions -> (key, (hash, resolutions)))))
        |> Map.ofList
      let isReused (op : PT.PackageOp) =
        match Positions.bodyOf op with
        | Some(key, _, _) -> Map.containsKey key reused
        | None -> false
      let toCheck = ops |> List.filter (isReused >> not)
      let nothingToCheck =
        storeHoldsOps
        && not (List.exists (Positions.bodyOf >> Option.isSome) toCheck)

      // Chunks load what they do not carry from the store, so only a caller whose batch is
      // already in the store can split it; an authoring batch is not, and stays whole. The
      // count is fixed rather than per-core so every machine resolves the same tree: two
      // items with one hash get only one copy resolved, and which one depends on the split.
      let size =
        if storeHoldsOps then
          max 256 (List.length toCheck / 32 + 1)
        else
          max 1 (List.length toCheck)
      let! chunks =
        if nothingToCheck then
          System.Threading.Tasks.Task.FromResult [||]
        else
          toCheck
          |> List.chunkBySize size
          |> List.map (fun chunk ->
            System.Threading.Tasks.Task.Run<Option<Map<PT.Hash, List<Resolution>> *
            Set<PT.Hash>>>
              (fun () -> Ply.toTask (askChecker exeState branchId chunk)))
          |> System.Threading.Tasks.Task.WhenAll
      let answered =
        chunks
        |> Array.choose identity
        |> Array.map fst
        |> Array.fold (Map.foldBack Map.add) Map.empty
      let failed = chunks |> Array.choose identity |> Array.map snd |> Set.unionMany
      let resolved =
        reused
        |> Map.fold
          (fun found _ (hash, resolutions) -> Map.add hash resolutions found)
          answered

      // What the checker just answered, for the bodies it was asked about, beside what was
      // reused. A body it did not report, or whose answer names a node its last copy does not
      // have, is left out, and is checked again next time.
      let saveCache (environment : string) : unit =
        let fresh =
          lastCopies
          |> Map.toList
          |> List.choose (fun (key, (hash, body)) ->
            if Map.containsKey key reused then
              None
            else
              Map.tryFind hash answered
              |> Option.bind (fun resolutions ->
                let positions = Positions.ofBody body
                Encoding.encode positions resolutions
                |> Option.map (fun text ->
                  (key,
                   ({ nodes = List.length positions; resolutions = text }
                   : LibDB.TraitCallCache.Entry)))))
        let kept =
          cached
          |> Map.toList
          |> List.filter (fun (key, _) -> Map.containsKey key reused)
        try
          LibDB.TraitCallCache.replace environment (kept @ fresh)
        with e ->
          printErr $"  [traitcalls] the cache was not saved: {e.Message}"
        let left = Map.count lastCopies - Map.count reused - List.length fresh
        print
          $"  [traitcalls] cache: {Map.count reused} of {Map.count lastCopies} bodies reused, {List.length fresh} checked and saved, {left} not saved"
      environment |> Option.iter saveCache

      if Map.isEmpty resolved then
        return (ops, failed)
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

        let addBound
          (at : id)
          (entry : PT.FQFnName.BoundImpl)
          (bounds : Map<id, List<PT.FQFnName.BoundImpl>>)
          : Map<id, List<PT.FQFnName.BoundImpl>> =
          let existing = Map.tryFind at bounds |> Option.defaultValue []
          Map.add at (entry :: existing) bounds

        let mappingOf
          (resolutions : List<Resolution>)
          : Ply<Option<LibDB.AstTransformer.HashMapping>> =
          uply {
            let! chosen =
              resolutions
              |> Ply.List.foldSequentially
                (fun impls resolution ->
                  match resolution with
                  | Call(at, method_, implHashes) ->
                    uply {
                      match! implFnFor method_ implHashes with
                      | Some implFn ->
                        return Map.add at (PT.FQFnName.Chosen implFn) impls
                      | None -> return impls
                    }
                  | _ -> uply { return impls })
                Map.empty

            // A call whose self type is one of the item's own type params is not a call nobody
            // could work out: it is waiting for its caller, and it says so. Second, so that a
            // call the checker actually resolved wins over a deferral at the same node.
            let impls =
              resolutions
              |> List.fold
                (fun impls resolution ->
                  match resolution with
                  | Deferred(at, param) when not (Map.containsKey at impls) ->
                    Map.add at (PT.FQFnName.FromTypeParam param) impls
                  | _ -> impls)
                chosen

            // The other half: what this item's CALLS worked out for the bounds of the fns they
            // name. That is what makes a call into a bounded fn static: the callee's body defers
            // to its type param, and the call says which implementation that param implied.
            let! bounds =
              resolutions
              |> Ply.List.foldSequentially
                (fun bounds resolution ->
                  match resolution with
                  | CallerBound(at, param, traitHash, implHashes) ->
                    uply {
                      match! getTrait traitHash with
                      | Some trait_ ->
                        // One entry per method of the trait, so the callee's body finds a fn for
                        // whichever method it calls without reading the implementation item at
                        // run time.
                        return!
                          NEList.toList trait_.methods
                          |> Ply.List.foldSequentially
                            (fun bounds (m : PT.Trait.Method) ->
                              uply {
                                match! implFnFor m.name implHashes with
                                | Some implFn ->
                                  let entry : PT.FQFnName.BoundImpl =
                                    { param = param
                                      trait_ = traitHash
                                      method_ = m.name
                                      choice = PT.FQFnName.Chosen implFn }
                                  return addBound at entry bounds
                                | None -> return bounds
                              })
                            bounds
                      | None -> return bounds
                    }
                  | CallerBoundDeferred(at, param, traitHash, fromParam) ->
                    uply {
                      match! getTrait traitHash with
                      | Some trait_ ->
                        return
                          NEList.toList trait_.methods
                          |> List.fold
                            (fun bounds (m : PT.Trait.Method) ->
                              let entry : PT.FQFnName.BoundImpl =
                                { param = param
                                  trait_ = traitHash
                                  method_ = m.name
                                  choice = PT.FQFnName.FromTypeParam fromParam }
                              addBound at entry bounds)
                            bounds
                      | None -> return bounds
                    }
                  | _ -> uply { return bounds })
                Map.empty

            if Map.isEmpty impls && Map.isEmpty bounds then
              return None
            else
              return
                Some
                  { LibDB.AstTransformer.emptyMapping with
                      resolvedImpls = impls
                      boundImpls = bounds }
          }

        let! mappings =
          resolved
          |> Map.toList
          |> Ply.List.foldSequentially
            (fun mappings (itemHash, resolutions) ->
              uply {
                match! mappingOf resolutions with
                | Some mapping -> return Map.add itemHash mapping mappings
                | None -> return mappings
              })
            Map.empty

        let mappingFor (hash : PT.Hash) : Option<LibDB.AstTransformer.HashMapping> =
          Map.tryFind hash mappings

        let transformed =
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
            | PT.PackageOp.AddTest test ->
              match mappingFor test.hash with
              | Some mapping ->
                PT.PackageOp.AddTest(
                  LibDB.AstTransformer.transformTest mapping test
                )
              | None -> op
            | _ -> op)

        // Two items with one hash are one item, parsed twice. A resolution is keyed by node id
        // and the two parses have different ids, so only the copy the checker saw is resolved, and
        // which copy that is depends on how the batch was split. The unresolved copy then rehashes
        // apart from its twin, with every caller after it: loading the package tests alongside
        // the rest moved 340 production fns this way, `Cli.executeCliCommand` among them. So
        // the copy that was resolved stands for every copy; ids do not enter the hash.
        let bodyHash (op : PT.PackageOp) : Option<PT.Hash> =
          match op with
          | PT.PackageOp.AddFn fn -> Some fn.hash
          | PT.PackageOp.AddValue value -> Some value.hash
          | PT.PackageOp.AddTest test -> Some test.hash
          | _ -> None
        let rewrittenByHash =
          List.zip ops transformed
          |> List.choose (fun (before, after) ->
            // Structurally: the twin gets the same mapping, and comes back rebuilt but
            // unchanged, since none of the mapping's node ids are its own.
            if before = after then
              None
            else
              bodyHash before |> Option.map (fun h -> h, after))
          |> Map.ofList
        let rewritten =
          List.zip ops transformed
          |> List.map (fun (before, after) ->
            if before <> after then
              after
            else
              match
                bodyHash before
                |> Option.bind (fun h -> Map.tryFind h rewrittenByHash)
              with
              | Some rewritten -> rewritten
              | None -> after)
        return (rewritten, failed)
  }


// --------------------
// Saying what was left unresolved.
// --------------------

/// The calls in <param expr> that will be decided at run time rather than by this save: a trait
/// method or a trait operator still `Unknown`, and a call into a BOUNDED fn that recorded nothing
/// for its bounds (so the callee's deferral has nothing to read). <param isBounded> answers for a
/// callee by hash.
let rec private unresolved (isBounded : PT.Hash -> bool) (expr : PT.Expr) : int =
  let traitOperator (infix : PT.Infix) =
    match infix with
    | PT.InfixFnCall name -> Option.isSome (LibExecution.NumericTraits.ofInfix name)
    | PT.BinOp _ -> false
  let named (nr : PT.NameResolution<PT.FQFnName.FQFnName>) (recorded : bool) =
    match nr.resolved with
    | Ok { name = PT.FQFnName.TraitMethod { implFn = PT.FQFnName.Unknown } } -> 1
    | Ok { name = PT.FQFnName.Package h } when not recorded && isBounded h -> 1
    | _ -> 0
  let here =
    match expr with
    | PT.EFnName(_, nr, boundImpls, _) -> named nr (not (List.isEmpty boundImpls))
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
  here + (PTAst.subExprs expr |> List.sumBy (unresolved isBounded))


/// Write onto each trait-method call in <param ops> the implementation it resolves to, and on an
/// authoring save, say how many calls it could not.
///
/// The count is what makes a partly resolved save visible. A save the checker could not settle at
/// all printed a line; one it settled partly printed nothing, and its unsettled calls ran whichever
/// implementation was newest, which a later rival could change without anyone being told.
///
/// <param reload> is Some build on a package reload, None on a save: the hash of the build less
/// `outsideTheChecker` and of the files of theirs the checker runs (`CheckerSources`).
/// The caller passes it in because a builtin may not read the executable itself
/// (`tests/hostBoundary`).
let resolveTraitCalls
  (exeState : RT.ExecutionState)
  (branchId : PT.BranchId)
  (pm : PT.PackageManager)
  (reload : Option<string>)
  (ops : List<PT.PackageOp>)
  : Ply<List<PT.PackageOp>> =
  uply {
    let! (resolved, failed) = resolveIn exeState branchId pm reload ops

    // A reload has its own report (`Resolved trait calls: ...`); this is for a person saving.
    if Option.isNone reload then
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
          | PT.PackageOp.SetName(loc, PT.Reference.PackageValue h, _)
          | PT.PackageOp.SetName(loc, PT.Reference.PackageTest h, _) ->
            Some(h, String.concat "." (loc.owner :: loc.modules @ [ loc.name ]))
          | _ -> None)
        |> List.groupBy fst
        |> Map.map (List.map snd >> List.distinct)

      // Which callees are bounded, looked up once each.
      let callees =
        resolved
        |> List.collect (function
          | PT.PackageOp.AddFn fn -> calledPackageFns fn.body
          | PT.PackageOp.AddValue v -> calledPackageFns v.body
          | PT.PackageOp.AddTest t -> calledPackageFns t.body
          | _ -> [])
        |> List.distinct
      let! boundedCallees =
        callees
        |> Ply.List.filterSequentially (fun h ->
          match Map.tryFind h batchFns with
          | Some fn -> Ply(not (List.isEmpty fn.bounds))
          | None ->
            uply {
              match! pm.getFn h with
              | Some fn -> return not (List.isEmpty fn.bounds)
              | None -> return false
            })
      let bounded = Set.ofList boundedCallees
      let isBounded h = Set.contains h bounded

      // Items with identical bodies share a hash, so each hash is counted once and then
      // reported under every name the save gives it.
      let left =
        resolved
        |> List.choose (fun op ->
          match op with
          | PT.PackageOp.AddFn fn -> Some(fn.hash, unresolved isBounded fn.body)
          | PT.PackageOp.AddValue v -> Some(v.hash, unresolved isBounded v.body)
          | PT.PackageOp.AddTest t -> Some(t.hash, unresolved isBounded t.body)
          | _ -> None)
        |> List.distinctBy fst
        |> List.filter (fun (hash, count) ->
          count > 0 && not (Set.contains hash failed))
        |> List.collect (fun (hash, count) ->
          // A name when the save carries one; `dark fn` saves the body before its name.
          let (PT.Hash h) = hash
          Map.tryFind hash names
          |> Option.defaultValue
            [ if h.Length > 8 then h.Substring(0, 8) else "an item" ]
          |> List.map (fun name -> $"{name} ({count})"))

      if not (List.isEmpty left) then
        let total = List.length left
        let which = String.concat ", " left
        print
          $"  [traitcalls] trait calls this save could not resolve, in {total} item(s), will run whichever implementation is newest when they run: {which}"

    return resolved
  }
