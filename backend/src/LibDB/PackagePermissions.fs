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
module Calls = LibExecution.CallGraph
module Requirements = LibExecution.CallGraph.Requirements

type CallEffectsFor = string * int -> Option<Set<LibExecution.Effects.Effect>>

/// How a package fn is fetched by hash. A parameter (rather than the package
/// DB directly) so review logic is testable against in-memory fns; production
/// passes `LibDB.ProgramTypes.Fn.get`.
type LoadFn = PT.Hash -> Ply<Option<PT.PackageFn.PackageFn>>

/// Load every package function reachable from `root`, analyze each body once,
/// and return the closure keyed by hash. Approval covers this whole closure so
/// dependencies remain available when entered at runtime.
let loadClosure (loadFn : LoadFn) (root : PT.Hash) : Ply<Requirements.Closure> =
  uply {
    let loaded =
      System.Collections.Generic.Dictionary<PT.Hash, PT.PackageFn.PackageFn *
      Calls.Analysis>()
    // Also holds hashes that failed to load, so a missing dependency shared
    // by several members is fetched once.
    let visited = System.Collections.Generic.HashSet<PT.Hash>()

    let rec load (h : PT.Hash) : Ply<unit> =
      uply {
        if visited.Add h then
          match! loadFn h with
          | None -> ()
          | Some fn ->
            let calls = Calls.analyzeFn fn
            loaded[h] <- (fn, calls)
            for callee in calls.names do
              match callee with
              | PT.FQFnName.Package dependency -> do! load dependency
              | PT.FQFnName.Builtin _ -> ()
      }

    do! load root
    return loaded |> Seq.map (fun (KeyValue(h, entry)) -> h, entry) |> Map.ofSeq
  }

/// Analyze one immutable function using the explicit effect vocabulary.
let permissionRequirements
  (loadFn : LoadFn)
  (callEffectsFor : CallEffectsFor)
  (hashStr : string)
  : Ply<Requirements.Result> =
  uply {
    let root = PT.Hash hashStr
    let! closure = loadClosure loadFn root
    return Requirements.forFunction callEffectsFor closure root
  }

/// One root's closure, analyzed: what an approval reviews.
type ClosureAnalysis =
  {
    root : PT.PackageFn.PackageFn
    rootRequirements : Requirements.Result
    /// Every member (the root included) with its requirements, keyed by hash.
    members : List<string * Requirements.Result>
  }

/// Analyze every member of a root's closure from one load. `None` when the
/// root itself does not exist.
let analyzeClosure
  (loadFn : LoadFn)
  (callEffectsFor : CallEffectsFor)
  (rootHash : string)
  : Ply<Option<ClosureAnalysis>> =
  uply {
    let root = PT.Hash rootHash
    let! closure = loadClosure loadFn root
    match Map.tryFind root closure with
    | None -> return None
    | Some(rootFn, _) ->
      let members =
        closure
        |> Map.toList
        |> List.map (fun (PT.Hash hashStr as hash, _) ->
          hashStr, Requirements.forFunction callEffectsFor closure hash)
      let rootRequirements = Requirements.forFunction callEffectsFor closure root
      return
        Some
          { root = rootFn; rootRequirements = rootRequirements; members = members }
  }


// ── upgrade contracts ─────────────────────────────────────────────────────────

[<RequireQualifiedAccess>]
type ContractComparison =
  /// Callable type, author permission ceiling, and complete transitive
  /// permission requirements all match.
  | Match
  /// Human-readable differences. Analysis gaps are listed here too: an
  /// unverifiable contract must be reviewed exactly like a changed one.
  | Changed of List<string>

let private signatureOf (fn : PT.PackageFn.PackageFn) =
  (fn.typeParams,
   fn.parameters |> NEList.toList |> List.map (fun p -> p.name, p.typ),
   fn.returnType)

let private describeEffects (effects : Set<LibExecution.Effects.Effect>) : string =
  if Set.isEmpty effects then
    "effect-free"
  else
    effects
    |> Set.toList
    |> List.map LibExecution.Effects.name
    |> String.concat ", "

/// Compare the pinned version `oldHash` with an analyzed candidate: callable
/// type, author ceiling, and complete transitive requirements. Both versions
/// are content-addressed, so this comparison is stable for the two hashes.
let compareContracts
  (loadFn : LoadFn)
  (callEffectsFor : CallEffectsFor)
  (oldHash : string)
  (candidate : ClosureAnalysis)
  : Ply<ContractComparison> =
  uply {
    let oldRoot = PT.Hash oldHash
    let! oldClosure = loadClosure loadFn oldRoot
    match Map.tryFind oldRoot oldClosure with
    | None ->
      return
        ContractComparison.Changed
          [ $"the pinned version {oldHash} is no longer available for comparison" ]
    | Some(oldFn, _) ->
      let oldRequirements =
        Requirements.forFunction callEffectsFor oldClosure oldRoot
      let newRequirements = candidate.rootRequirements
      let differences = ResizeArray<string>()
      if signatureOf oldFn <> signatureOf candidate.root then
        differences.Add "the callable type changed"
      if oldFn.permissionCeiling <> candidate.root.permissionCeiling then
        differences.Add "the author's permission ceiling changed"
      if not (oldRequirements.complete && newRequirements.complete) then
        differences.Add "permission analysis is incomplete"
      elif oldRequirements.requiredEffects <> newRequirements.requiredEffects then
        differences.Add(
          $"permission requirements changed from "
          + $"{describeEffects oldRequirements.requiredEffects} to "
          + $"{describeEffects newRequirements.requiredEffects}"
        )
      if differences.Count = 0 then
        return ContractComparison.Match
      else
        return ContractComparison.Changed(List.ofSeq differences)
  }

/// Build the policy stored for each approved member. Complete analysis gets an
/// allowlist of its inferred effects. Incomplete analysis gets allow-all so
/// missed effects are not accidentally denied; instance and run policies still
/// bound it.
let private closurePolicies
  (members : List<string * Requirements.Result>)
  : List<string * P.Policy> =
  members
  |> List.map (fun (hash, result) ->
    let policy =
      if result.complete then
        P.Policy.allowEffects result.requiredEffects
      else
        P.Policy.allowAll
    hash, policy)

/// The effects `requirements` needs that `policy` can never allow, named for
/// the approval error. Empty when the policy is able to cover them all (an
/// `All` rule covers everything).
let private uncoveredEffects
  (policy : P.Policy)
  (requirements : Requirements.Result)
  : List<string> =
  match P.Policy.coverableEffects policy with
  | None -> []
  | Some coverable ->
    Set.difference requirements.requiredEffects coverable
    |> Set.toList
    |> List.map LibExecution.Effects.name

/// What reviewing a candidate version found, before anything is stored.
[<RequireQualifiedAccess>]
type Review =
  /// The version may be approved: these are the member policies to install
  /// together with the pin.
  | Approvable of policies : List<string * P.Policy>
  /// The permission contract differs from the current pin's (the
  /// human-readable differences) and the caller did not acknowledge a
  /// reviewed change. Nothing may be stored.
  | ContractChanged of List<string>

/// Analyze `hash`, reject unacknowledged incomplete analysis, and compare it
/// with `currentPin` when replacing a pinned version. With an explicit policy,
/// the root uses that policy while dependencies keep their own policies. The
/// explicit rules must cover the root's required effects.
let reviewVersion
  (loadFn : LoadFn)
  (callEffectsFor : CallEffectsFor)
  (currentPin : Option<string>)
  (hash : string)
  (explicitPolicy : Option<P.Policy>)
  (acknowledgeIncomplete : bool)
  (acknowledgeContractChange : bool)
  : Ply<Result<Review, string>> =
  uply {
    match! analyzeClosure loadFn callEffectsFor hash with
    | None -> return Error $"cannot approve unknown package-function hash: {hash}"
    | Some candidate ->
      // Check completeness for the approved root, not each dependency. A
      // dependency may pass on a callback supplied by its caller; that is only
      // incomplete when the dependency itself is the root being approved.
      let incomplete =
        if candidate.rootRequirements.complete then
          None
        else
          // Name a member when one is to blame; otherwise the root itself is.
          match candidate.members |> List.tryFind (fun (_, r) -> not r.complete) with
          | Some found -> Some found
          | None -> Some(hash, candidate.rootRequirements)
      let uncovered =
        match explicitPolicy with
        | Some policy -> uncoveredEffects policy candidate.rootRequirements
        | None -> []
      match incomplete, uncovered with
      | _, (_ :: _) ->
        let missing = String.concat ", " uncovered
        return
          Error
            $"cannot approve {hash} under these rules: they never allow {missing}, which it requires"
      | Some(incompleteHash, _), _ when
        explicitPolicy.IsNone && not acknowledgeIncomplete
        ->
        return
          Error
            $"cannot approve package-function {incompleteHash}: effect analysis is incomplete. Re-run with acknowledgement to approve the inferred lower bound."
      | _ ->
        let! comparison =
          match currentPin with
          | Some existing when existing <> hash ->
            compareContracts loadFn callEffectsFor existing candidate
          | _ -> Ply ContractComparison.Match
        match comparison with
        | ContractComparison.Changed differences when not acknowledgeContractChange ->
          return Ok(Review.ContractChanged differences)
        | _ ->
          let policies =
            match explicitPolicy with
            | Some policy ->
              closurePolicies candidate.members
              |> List.map (fun (h, derived) ->
                h, (if h = hash then policy else derived))
            | None -> closurePolicies candidate.members
          return Ok(Review.Approvable policies)
  }

/// What [approveAndPinFunctionVersion] did with a request that storage accepted.
[<RequireQualifiedAccess>]
type PinOutcome =
  /// The pin now points at the requested version.
  | Pinned
  /// The pin was left where it was; see [Review.ContractChanged]. The caller
  /// shows these, obtains the review, and calls again with the acknowledgment.
  | ContractChanged of List<string>

/// Review, approve, and pin a named immutable version atomically. Analysis and
/// contract checks happen before storage installs the closure approval and pin.
let approveAndPinFunctionVersion
  (loadFn : LoadFn)
  (callEffectsFor : CallEffectsFor)
  (accountID : Option<System.Guid>)
  (location : string)
  (hash : string)
  (explicitPolicy : Option<P.Policy>)
  (acknowledgeIncomplete : bool)
  (acknowledgeContractChange : bool)
  (fingerprint : string)
  : Ply<Result<PinOutcome, string>> =
  uply {
    let current = LibDB.PolicyStore.pinnedFunction accountID location
    match!
      reviewVersion
        loadFn
        callEffectsFor
        current
        hash
        explicitPolicy
        acknowledgeIncomplete
        acknowledgeContractChange
    with
    | Error message -> return Error message
    | Ok(Review.ContractChanged differences) ->
      return Ok(PinOutcome.ContractChanged differences)
    | Ok(Review.Approvable policies) ->
      return
        LibDB.PolicyStore.recordApprovalAndMovePin
          accountID
          hash
          policies
          fingerprint
          explicitPolicy
          location
          current
          hash
        |> Result.map (fun () -> PinOutcome.Pinned)
  }
