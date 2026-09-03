/// Static requirements analysis and the review that approves a version:
/// completeness, contracts, explicit rules.
module Tests.PackagePermissions

open Expecto
open Prelude
open TestUtils.PTShortcuts

module Effect = LibExecution.Effects
module Permission = LibExecution.Permissions
module Requirements = LibExecution.CallGraph.Requirements
module PT = LibExecution.ProgramTypes
module PackagePermissions = LibDB.PackagePermissions

let private only (item : 'a) : Permission.Scope<'a> = Permission.Scope.Only item

/// A `Unit -> Int` package fn with the given body.
let private unitFn (hash : string) (body : PT.Expr) : PT.PackageFn.PackageFn =
  { hash = PT.Hash hash
    body = body
    typeParams = []
    parameters = NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
    returnType = PT.TInt
    description = ""
    permissionCeiling = None }

/// A closure of in-memory fns, each body analyzed once, as
/// `PackagePermissions.loadClosure` produces.
let private closureOf (fns : List<PT.PackageFn.PackageFn>) =
  fns
  |> List.map (fun fn -> fn.hash, (fn, LibExecution.CallGraph.analyze fn.body))
  |> Map.ofList

/// Builtin effects for the review tests: `timeNowMs` is clock-only and
/// `fileRead` reads files; anything else is unknown (incomplete).
let private testEffects : PackagePermissions.CallEffectsFor =
  fun (name, _version) ->
    match name with
    | "timeNowMs" -> Some(Set.singleton Effect.Effect.Clock)
    | "fileRead" -> Some(Set.singleton Effect.Effect.FileRead)
    | _ -> None

let private callBuiltin (name : string) : PT.Expr =
  eApply (eBuiltinFn name 0) [] [ eUnit () ]

/// The review tests share one package universe: `v1` is the pinned version,
/// `v2` an identical-contract re-release, `wider` adds a file read, and
/// `dynamic` applies a callback the static walk cannot follow.
let private reviewUniverse =
  [ unitFn "v1" (callBuiltin "timeNowMs")
    unitFn "v2" (eLet (lpUnit ()) (eUnit ()) (callBuiltin "timeNowMs"))
    unitFn "wider" (eStatement (callBuiltin "fileRead") (callBuiltin "timeNowMs"))
    unitFn "dynamic" (eApply (eVar "callback") [] [ eUnit () ])
    // A root with a dependency: reads a file itself and calls `v1` for the clock.
    unitFn
      "caller"
      (eStatement (callBuiltin "fileRead") (eApply (ePackageFn "v1") [] [ eUnit () ])) ]
  |> List.map (fun fn -> fn.hash, fn)
  |> Map.ofList

let private loadFromUniverse : PackagePermissions.LoadFn =
  fun hash -> Ply(Map.tryFind hash reviewUniverse)

let private reviewUnder
  explicitPolicy
  currentPin
  hash
  acknowledgeIncomplete
  acknowledgeChange
  =
  PackagePermissions.reviewVersion
    loadFromUniverse
    testEffects
    currentPin
    hash
    explicitPolicy
    acknowledgeIncomplete
    acknowledgeChange
  |> Ply.toTask

let private review currentPin hash acknowledgeIncomplete acknowledgeChange =
  reviewUnder None currentPin hash acknowledgeIncomplete acknowledgeChange

/// `file read /data` plus the clock: the consumer's own resource rules.
let private consumerRules : Permission.Policy =
  Permission.Policy.create
    [ Permission.Rule.File(Permission.AccessKind.Read, Permission.Scope.Only "/data")
      Permission.Rule.Effect Effect.Effect.Clock ]
    []

let missingCodeIsIncomplete =
  test "missing package code makes permission analysis incomplete" {
    let result =
      Requirements.forFunction (fun _ -> None) Map.empty (PT.Hash "missing")

    Expect.isFalse result.complete "analysis reports incompleteness"
    Expect.isTrue
      (Set.isEmpty result.requiredEffects)
      "the effect set contains only behavior that was actually identified"
  }

let deferredCodeRequirementsAreIncluded =
  test "permission requirements include effects in a returned lambda" {
    let root = PT.Hash "returns-clock-callback"
    let fn =
      { unitFn
          "returns-clock-callback"
          (eLambda (gid ()) [ lpVar "unit" ] (callBuiltin "timeNowMs")) with
          returnType = PT.TFn(NEList.singleton PT.TUnit, PT.TInt) }
    let result = Requirements.forFunction testEffects (closureOf [ fn ]) root
    Expect.isTrue result.complete "the closed call graph is complete"
    Expect.equal
      result.requiredEffects
      (Set.singleton Effect.Effect.Clock)
      "deferred callback effects are part of the approval requirements"
  }

let packageValuesMakeAnalysisIncomplete =
  test "package values are conservative until their stored bodies are analyzed" {
    let root = PT.Hash "returns-package-value"
    let fn = unitFn "returns-package-value" (ePackageValue "stored-callback")
    let result = Requirements.forFunction testEffects (closureOf [ fn ]) root
    Expect.isFalse
      result.complete
      "an opaque package value may carry executable code"
    Expect.isEmpty
      result.requiredEffects
      "unknown effects are not invented, but the result is not effect-free"
  }

let reviewAcceptsAnUnchangedContract =
  testTask "an identical permission contract under a new hash is approvable" {
    let! reviewed = review (Some "v1") "v2" false false
    match reviewed with
    | Ok(PackagePermissions.Review.Approvable policies) ->
      Expect.equal
        (policies |> List.map fst)
        [ "v2" ]
        "the whole (one-member) closure is approved"
      Expect.equal
        (policies |> List.map snd)
        [ Permission.Policy.allowEffects (Set.singleton Effect.Effect.Clock) ]
        "a complete member is pinned to its exact effect set"
    | other -> failtest $"expected Approvable, got {other}"
  }

let reviewApprovesTheRootUnderExplicitRules =
  testTask
    "consumer rules confine the root; dependencies keep their derived policies" {
    let! reviewed = reviewUnder (Some consumerRules) None "caller" false false
    match reviewed with
    | Ok(PackagePermissions.Review.Approvable policies) ->
      Expect.equal
        (Map.ofList policies)
        (Map
          [ "caller", consumerRules
            "v1", Permission.Policy.allowEffects (Set.singleton Effect.Effect.Clock) ])
        "the root is stored under the rules written for it; the dependency it calls is not narrowed by them"
    | other -> failtest $"expected Approvable, got {other}"
    // Explicit rules bound an incomplete closure without an acknowledgement:
    // the consumer chose the bound rather than accepting an inferred one.
    let! dynamic = reviewUnder (Some consumerRules) None "dynamic" false false
    match dynamic with
    | Ok(PackagePermissions.Review.Approvable policies) ->
      Expect.equal policies [ "dynamic", consumerRules ] "bounded by the rules given"
    | other -> failtest $"expected Approvable, got {other}"
  }

let reviewRefusesRulesThatCannotCoverTheRequirements =
  testTask "rules that never allow a required effect are refused, naming it" {
    let clockOnly =
      Permission.Policy.allowEffects (Set.singleton Effect.Effect.Clock)
    let! reviewed = reviewUnder (Some clockOnly) None "caller" false false
    match reviewed with
    | Error message ->
      Expect.stringContains
        message
        "never allow file-read"
        "the missing effect is named at approval time, not by a runtime denial"
    | other -> failtest $"expected an error, got {other}"
    // Only which effects a rule can cover matters here; the exact resource
    // is decided at runtime, so a narrow file root still covers file-read.
    let! narrow = reviewUnder (Some consumerRules) None "wider" false false
    Expect.isOk narrow "a file root plus the clock covers file-read and clock"
    let! anything =
      reviewUnder (Some Permission.Policy.allowAll) None "wider" false false
    Expect.isOk anything "an `all` rule covers every effect"
  }

let reviewRefusesAWidenedContractWithoutAcknowledgment =
  testTask "a version whose requirements widen needs an explicit contract review" {
    let! reviewed = review (Some "v1") "wider" false false
    match reviewed with
    | Ok(PackagePermissions.Review.ContractChanged differences) ->
      Expect.equal
        differences
        [ "permission requirements changed from clock to file-read, clock" ]
        "the difference names the old and new requirement sets"
    | other -> failtest $"expected ContractChanged, got {other}"
    // The acknowledgment is the review: with it, the same version is approvable.
    let! acknowledged = review (Some "v1") "wider" false true
    match acknowledged with
    | Ok(PackagePermissions.Review.Approvable _) -> ()
    | other -> failtest $"expected Approvable after acknowledgment, got {other}"
  }

let reviewRejectsAnIncompleteClosure =
  testTask "an incomplete analysis is refused unless a human accepts the lower bound" {
    let! reviewed = review None "dynamic" false false
    Expect.isError reviewed "a dynamic call the walk cannot follow blocks approval"
    let! acknowledged = review None "dynamic" true false
    match acknowledged with
    | Ok(PackagePermissions.Review.Approvable policies) ->
      Expect.equal
        (policies |> List.map snd)
        [ Permission.Policy.allowAll ]
        "an acknowledged-incomplete member is allow-all, bounded by instance/run"
    | other -> failtest $"expected Approvable after acknowledgment, got {other}"
    let! unknown = review None "missing" true true
    Expect.isError unknown "an unknown hash cannot be approved"
  }

let tests =
  testList
    "packagePermissions"
    [ missingCodeIsIncomplete
      deferredCodeRequirementsAreIncluded
      packageValuesMakeAnalysisIncomplete
      reviewAcceptsAnUnchangedContract
      reviewApprovesTheRootUnderExplicitRules
      reviewRefusesRulesThatCannotCoverTheRequirements
      reviewRefusesAWidenedContractWithoutAcknowledgment
      reviewRejectsAnIncompleteClosure ]
