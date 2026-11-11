/// Transitive permission requirements for package functions.
///
/// The package body is content-addressed, but builtin metadata is supplied by
/// the running host and can change between releases. Analysis therefore runs
/// against the current builtin map instead of reading a hash-only cache that
/// could contain a stale, narrower result.
module LibDB.PackagePermissions

open Prelude

module PT = LibExecution.ProgramTypes
module P = LibExecution.Permissions
module RT = LibExecution.RuntimeTypes
module Calls = LibExecution.CallGraph
module Requirements = LibExecution.CallGraph.Requirements

type CallEffectsFor = string * int -> Option<Set<LibExecution.Effects.Effect>>

/// How a package fn is fetched by hash. A parameter (rather than the package
/// DB directly) so review logic is testable against in-memory fns.
type LoadFn = PT.Hash -> Ply<Option<PT.PackageFn.PackageFn>>

/// How a package value's stored, evaluated value is fetched by hash. `None`
/// when there is no such value, when it has not been evaluated yet, or when it
/// could not be read; all three leave a reference to it incomplete.
type LoadValue = PT.Hash -> Ply<Option<RT.Dval>>

/// Everything the analysis reads from the store.
type Load = { fn : LoadFn; value : LoadValue }

module Load =
  /// The package store. A value's stored row is read as it is, so the answer
  /// for one fn can differ before and after `Seed.evaluateAllValues` has run:
  /// unevaluated, a value reference is incomplete.
  /// Function definitions use the package manager's shared cache.
  let fromStore : Load =
    { fn = LibDB.PackageManager.pt.getFn
      value =
        fun (PT.Hash h) ->
          uply {
            try
              let! found = LibDB.RuntimeTypes.Value.get (RT.Hash h)
              return found |> Option.map (fun v -> v.body)
            with _ ->
              return None
          } }

/// Whether each package value `expr` references holds only data, read
/// through `cache` so a value shared by several bodies is read once. Returns the
/// inert ones and the cache with this body's values added.
let private inertValuesIn
  (load : Load)
  (cache : Map<PT.Hash, bool>)
  (expr : PT.Expr)
  : Ply<Set<PT.Hash> * Map<PT.Hash, bool>> =
  Calls.valueRefs expr
  |> Set.toList
  |> Ply.List.foldSequentially
    (fun (inert, cache) value ->
      let keep isInert = if isInert then Set.add value inert else inert
      match Map.tryFind value cache with
      | Some known -> Ply((keep known, cache))
      | None ->
        uply {
          let! stored = load.value value
          let isInert =
            match stored with
            | Some dval -> LibExecution.Dval.isInertData dval
            | None -> false
          return (keep isInert, Map.add value isInert cache)
        })
    (Set.empty, cache)

/// What `loadClosureFrom` has seen so far. `visited` also holds hashes that
/// failed to load, so a missing dependency shared by several members is fetched
/// once; `valueIsInert` is whether each value referenced so far holds only data.
type private Walk =
  { loaded : Requirements.Closure
    visited : Set<PT.Hash>
    valueIsInert : Map<PT.Hash, bool> }

/// Load every package function reachable from the given roots, analyzing each
/// body once. Missing functions remain absent so requirements stay incomplete.
let private loadClosureFrom
  (load : Load)
  (roots : List<PT.Hash>)
  : Ply<Requirements.Closure> =
  let rec loadMember (walk : Walk) (h : PT.Hash) : Ply<Walk> =
    if Set.contains h walk.visited then
      Ply walk
    else
      uply {
        let walk = { walk with visited = Set.add h walk.visited }
        match! load.fn h with
        | None -> return walk
        | Some fn ->
          let! inertValues, valueIsInert =
            inertValuesIn load walk.valueIsInert fn.body
          let calls = Calls.analyzeFn inertValues fn
          let walk =
            { walk with
                loaded = Map.add h (fn, calls) walk.loaded
                valueIsInert = valueIsInert }
          return!
            calls.names
            |> Ply.List.foldSequentially
              (fun walk callee ->
                match callee with
                | PT.FQFnName.Package dependency -> loadMember walk dependency
                | PT.FQFnName.Builtin _ -> Ply walk
                // CallGraph reports a trait call as the implementation it resolved
                // to, or marks the analysis incomplete; none reach here.
                | PT.FQFnName.TraitMethod _ -> Ply walk)
              walk
      }

  roots
  |> Ply.List.foldSequentially
    loadMember
    { loaded = Map.empty; visited = Set.empty; valueIsInert = Map.empty }
  |> Ply.map (fun walk -> walk.loaded)

/// Load every package function reachable from `root`, analyze each body once,
/// and return the closure keyed by hash. Approval covers this whole closure so
/// dependencies remain available when entered at runtime.
let loadClosure (load : Load) (root : PT.Hash) : Ply<Requirements.Closure> =
  loadClosureFrom load [ root ]

/// Each body's own call analysis, in order, and the closure they share: every package fn any of
/// them reaches, loaded and analyzed once however many of them call it.
let analyzeExpressions
  (load : Load)
  (bodies : List<PT.Expr>)
  : Ply<List<Calls.Analysis> * Requirements.Closure> =
  uply {
    let! analyses, _ =
      bodies
      |> Ply.List.foldSequentially
        (fun (analyses, valueIsInert) body ->
          uply {
            let! inertValues, valueIsInert = inertValuesIn load valueIsInert body
            return
              (Calls.analyze Set.empty inertValues body :: analyses, valueIsInert)
          })
        ([], Map.empty)
    let analyses = List.rev analyses
    let roots =
      analyses
      |> List.collect (fun calls ->
        calls.names
        |> List.choose (function
          | PT.FQFnName.Package hash -> Some hash
          | PT.FQFnName.Builtin _
          | PT.FQFnName.TraitMethod _ -> None))
    let! closure = loadClosureFrom load roots
    return analyses, closure
  }

/// Check whether a passing test has no effects, unresolved calls, or known
/// branch-dependent calls. Assertion helpers are analyzed like any other call:
/// a passing test can inspect an assertion's failure message.
let cacheSafetyForExpression
  (load : Load)
  (callEffectsFor : CallEffectsFor)
  (body : PT.Expr)
  : Ply<Requirements.Result> =
  analyzeExpressions load [ body ]
  |> Ply.map (fun (analyses, closure) ->
    Requirements.forExpression callEffectsFor closure (List.exactlyOne analyses))

/// Analyze one immutable function using the explicit effect vocabulary.
let permissionRequirements
  (load : Load)
  (callEffectsFor : CallEffectsFor)
  (hashStr : string)
  : Ply<Requirements.Result> =
  uply {
    let root = PT.Hash hashStr
    let! closure = loadClosure load root
    return Requirements.forFunction callEffectsFor closure root
  }

/// Whether a package fn is pure for a list op deciding to spread it (`RT.Purity`), over its
/// whole closure: impure if anything it can reach is a builtin `Interpreter.spreadRefuses`, pure
/// if nothing is and the analysis is complete, unknown otherwise. The same classifier the spread
/// chunk refuses by, so a prediction and the refusal under it cannot disagree.
///
/// Its fn parameters do not make it unknown (`forFunctionWith true`): the list op checks the
/// arguments it applies the fn to.
///
/// Answered against this host's builtins and memoised by hash for the life of the process. Not
/// stored with the item, for the reason at the top of this module: a host upgrade can change a
/// builtin, and a verdict hashed into the item would go stale.
let purity
  (load : Load)
  (builtins :
    System.Collections.Generic.Dictionary<RT.FQFnName.Builtin, RT.BuiltInFn>)
  : RT.FQFnName.Package -> Ply<RT.Purity> =
  let memo =
    System.Collections.Concurrent.ConcurrentDictionary<RT.FQFnName.Package, RT.Purity>()
  // Any non-empty set stands for "refused"; which effect it names is not the question here.
  let refused = Set.singleton LibExecution.Effects.Effect.Native
  let classify (name : string, version : int) =
    let key : RT.FQFnName.Builtin = { name = name; version = version }
    match Lookup.tryFindV key builtins with
    | ValueSome found ->
      Some(
        if LibExecution.Interpreter.spreadRefuses found then refused else Set.empty
      )
    | ValueNone -> None
  fun (RT.Hash h as hash) ->
    match memo.TryGetValue hash with
    | true, known -> Ply known
    | false, _ ->
      uply {
        let root = PT.Hash h
        let! answer =
          uply {
            try
              let! closure = loadClosure load root
              let result = Requirements.forFunctionWith true classify closure root
              return
                if not (Set.isEmpty result.requiredEffects) then RT.Purity.Impure
                elif result.complete then RT.Purity.Pure
                else RT.Purity.Unknown
            with _ ->
              LibExecution.Spread.notePredictionFailure ()
              return RT.Purity.Unknown
          }
        memo[hash] <- answer
        return answer
      }

/// One root's closure, analyzed: what an approval reviews.
type ClosureAnalysis =
  { root : PT.PackageFn.PackageFn; rootRequirements : Requirements.Result }

/// Analyze every member of a root's closure from one load. `None` when the
/// root itself does not exist.
let analyzeClosure
  (load : Load)
  (callEffectsFor : CallEffectsFor)
  (rootHash : string)
  : Ply<Option<ClosureAnalysis>> =
  uply {
    let root = PT.Hash rootHash
    let! closure = loadClosure load root
    match Map.tryFind root closure with
    | None -> return None
    | Some(rootFn, _) ->
      let rootRequirements = Requirements.forFunction callEffectsFor closure root
      return Some { root = rootFn; rootRequirements = rootRequirements }
  }


// ── upgrade contracts ─────────────────────────────────────────────────────────

[<RequireQualifiedAccess>]
type ContractComparison =
  /// Callable type and complete transitive
  /// permission requirements all match.
  | Match
  /// Human-readable differences. Analysis gaps are listed here too: an
  /// unverifiable contract must be reviewed exactly like a changed one.
  | Changed of List<string>

let private signatureOf (fn : PT.PackageFn.PackageFn) =
  (fn.typeParams,
   fn.parameters |> NEList.toList |> List.map (fun p -> p.name, p.typ),
   fn.returnType)

let private permissionEffects (result : Requirements.Result) =
  LibExecution.Effects.permissionEffects result.requiredEffects

let private describeEffects (effects : Set<LibExecution.Effects.Effect>) : string =
  if Set.isEmpty effects then
    "effect-free"
  else
    effects
    |> Set.toList
    |> List.map LibExecution.Effects.name
    |> String.concat ", "

/// Compare the previously approved version (`oldHash`) with the new version.
/// Check their input and return types and permission requirements, including
/// those of their dependencies. Both analyses must be complete to report a match.
/// Each hash identifies fixed code, so the versions being compared cannot change.
let compareContracts
  (load : Load)
  (callEffectsFor : CallEffectsFor)
  (oldHash : string)
  (candidate : ClosureAnalysis)
  : Ply<ContractComparison> =
  uply {
    let oldRoot = PT.Hash oldHash
    let! oldClosure = loadClosure load oldRoot
    match Map.tryFind oldRoot oldClosure with
    | None ->
      return
        ContractComparison.Changed
          [ $"the previously approved version {oldHash} is no longer available for comparison" ]
    | Some(oldFn, _) ->
      let oldRequirements =
        Requirements.forFunction callEffectsFor oldClosure oldRoot
      let newRequirements = candidate.rootRequirements
      let differences =
        [ if signatureOf oldFn <> signatureOf candidate.root then
            "the callable type changed"
          if not (oldRequirements.complete && newRequirements.complete) then
            "permission analysis is incomplete"
          else
            let before = permissionEffects oldRequirements
            let after = permissionEffects newRequirements
            if before <> after then
              $"permission requirements changed from "
              + $"{describeEffects before} to "
              + $"{describeEffects after}" ]
      if List.isEmpty differences then
        return ContractComparison.Match
      else
        return ContractComparison.Changed differences
  }

/// What reviewing a candidate version found, before anything is stored.
[<RequireQualifiedAccess>]
type Review =
  /// The version may be approved: this is the entry policy to install
  /// together with its reviewed code identity.
  | Approvable of policies : List<string * P.Policy>
  /// The permission contract differs from the previous approval's (the
  /// human-readable differences) and the caller did not acknowledge a
  /// reviewed change. Nothing may be stored.
  | ContractChanged of List<string>

/// Analyze `hash`, reject unacknowledged incomplete analysis, and compare it
/// with `previousApproval` when replacing an approval. Explicit rules grant
/// exactly what the user requested, even when narrower than inferred effects.
let reviewVersion
  (load : Load)
  (callEffectsFor : CallEffectsFor)
  (previousApproval : Option<string>)
  (hash : string)
  (explicitPolicy : Option<P.Policy>)
  (acknowledgeIncomplete : bool)
  (acknowledgeContractChange : bool)
  : Ply<Result<Review, string>> =
  uply {
    match! analyzeClosure load callEffectsFor hash with
    | None -> return Error $"cannot approve unknown package-function hash: {hash}"
    | Some candidate ->
      // Check completeness for the approved root, not each dependency. A
      // dependency may pass on a callback supplied by its caller; that is only
      // incomplete when the dependency itself is the root being approved.
      match candidate.rootRequirements.complete with
      | false when explicitPolicy.IsNone && not acknowledgeIncomplete ->
        return
          Error
            "Effect analysis is incomplete. Supply explicit rules, or acknowledge an approval with no additional access."
      | _ ->
        let! comparison =
          match previousApproval with
          | Some existing when existing <> hash ->
            compareContracts load callEffectsFor existing candidate
          | _ -> Ply ContractComparison.Match
        match comparison with
        | ContractComparison.Changed differences when not acknowledgeContractChange ->
          return Ok(Review.ContractChanged differences)
        | _ ->
          let rootPolicy =
            match explicitPolicy with
            | Some policy -> policy
            | None ->
              if candidate.rootRequirements.complete then
                P.Policy.allowEffects (permissionEffects candidate.rootRequirements)
              else
                P.Policy.denyAll
          let policies = [ hash, rootPolicy ]
          return Ok(Review.Approvable policies)
  }

/// What [approveVersionForName] did with a request that storage accepted.
[<RequireQualifiedAccess>]
type ApprovalOutcome =
  /// The requested version now has remembered access for this name.
  | Approved
  /// The approval was left where it was; see [Review.ContractChanged]. The caller shows these,
  /// obtains the review, and calls again with the acknowledgment.
  | ContractChanged of List<string>

/// Review a named immutable version and, if it passes, approve it for that name -- atomically.
/// Analysis and contract checks happen before storage installs the closure approval.
let approveVersionForName
  (load : Load)
  (callEffectsFor : CallEffectsFor)
  (accountID : Option<System.Guid>)
  (location : string)
  (expected : string)
  (hash : string)
  (explicitPolicy : Option<P.Policy>)
  (acknowledgeIncomplete : bool)
  (acknowledgeContractChange : bool)
  (fingerprint : string)
  : Ply<Result<ApprovalOutcome, string>> =
  uply {
    let snapshot = LibDB.PolicyStore.get ()
    if LibDB.PolicyStore.reviewToken snapshot <> expected then
      return Error "Permissions changed during review. Review the approval again."
    else
      let current =
        Map.tryFind (accountID, location) snapshot.approvals |> Option.map _.hash
      match!
        reviewVersion
          load
          callEffectsFor
          current
          hash
          explicitPolicy
          acknowledgeIncomplete
          acknowledgeContractChange
      with
      | Error message -> return Error message
      | Ok(Review.ContractChanged differences) ->
        return Ok(ApprovalOutcome.ContractChanged differences)
      | Ok(Review.Approvable policies) ->
        return
          LibDB.PolicyStore.recordApproval
            accountID
            location
            expected
            { hash = hash
              policy =
                policies
                |> List.tryPick (fun (h, p) -> if h = hash then Some p else None)
                |> Option.defaultValue P.Policy.denyAll
              fingerprint = fingerprint
              explicitRules = explicitPolicy.IsSome }
          |> Result.map (fun () -> ApprovalOutcome.Approved)
  }

/// Explain each effect by showing the calls that lead to it. `Raise` follows `Raises.path`.
/// This does not grant access; permission checks use the actual resource.
let explain
  (load : Load)
  (callEffectsFor : CallEffectsFor)
  (hash : string)
  : Ply<Requirements.Result * List<LibExecution.Effects.Effect * List<string>>> =
  uply {
    let root = PT.Hash hash
    let! closure = loadClosure load root
    let labels = System.Collections.Generic.Dictionary<PT.Hash, string>()
    let label (nr : PT.NameResolution<PT.FQFnName.FQFnName>) =
      match nr.resolved with
      | Ok { name = PT.FQFnName.Package hash; location = Some loc } ->
        labels[hash] <- String.concat "." (loc.owner :: loc.modules @ [ loc.name ])
      | _ -> ()
    let rec scan expr =
      match expr with
      | PT.EFnName(_, nr, _, _) -> label nr
      | PT.EPipe(_, _, parts) ->
        for part in parts do
          match part with
          | PT.EPipeFnCall(_, nr, _, _, _) -> label nr
          | _ -> ()
      | _ -> ()
      LibExecution.ProgramTypesAst.subExprs expr |> List.iter scan
    for KeyValue(_, (fn, _)) in closure do
      scan fn.body
    let name (PT.Hash h as hash) =
      match labels.TryGetValue hash with
      | true, name -> name
      | _ -> "function " + h.Substring(0, min 12 h.Length)
    let rec visit visited path hash reasons =
      if Set.contains hash visited then
        visited, reasons
      else
        let visited = Set.add hash visited
        match Map.tryFind hash closure with
        | None -> visited, reasons
        | Some(_, calls) ->
          calls.names
          |> List.fold
            (fun (visited, reasons) called ->
              match called with
              | PT.FQFnName.Package child ->
                visit visited (path @ [ name child ]) child reasons
              | PT.FQFnName.Builtin b ->
                let effects =
                  callEffectsFor (b.name, b.version)
                  |> Option.defaultValue Set.empty
                let reasons =
                  effects
                  |> Set.fold
                    (fun reasons effect ->
                      if Map.containsKey effect reasons then
                        reasons
                      else
                        Map.add effect (path @ [ "Builtin." + b.name ]) reasons)
                    reasons
                visited, reasons
              | _ -> visited, reasons)
            (visited, reasons)
    let _, reasons = visit Set.empty [] root Map.empty
    let! names = Raises.fnNames ()
    let nameOf hash =
      Map.tryFind hash names |> Option.defaultWith (fun () -> name hash)
    let raisePath = Raises.path callEffectsFor nameOf closure root
    let reasons =
      match raisePath with
      | Some path -> Map.add LibExecution.Effects.Effect.Raise path reasons
      | None -> Map.remove LibExecution.Effects.Effect.Raise reasons
    return Requirements.forFunction callEffectsFor closure root, Map.toList reasons
  }
