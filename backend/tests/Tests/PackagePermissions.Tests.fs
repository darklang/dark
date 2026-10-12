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
module RT = LibExecution.RuntimeTypes

let private only (item : 'a) : Permission.Scope<'a> = Permission.Scope.Only item

/// A `Unit -> Int` package fn with the given body.
let private unitFn (hash : string) (body : PT.Expr) : PT.PackageFn.PackageFn =
  { hash = PT.Hash hash
    body = body
    typeParams = []
    parameters = NEList.singleton { name = "unit"; typ = PT.TUnit; description = "" }
    returnType = PT.TInt
    description = ""
    permissionCeiling = None
    bounds = [] }

/// A `List<Int> -> (Int -> Int) -> List<Int>` fn: the shape that hands its own
/// callback parameter to something else.
let private higherOrderFn (hash : string) (body : PT.Expr) : PT.PackageFn.PackageFn =
  { hash = PT.Hash hash
    body = body
    typeParams = []
    parameters =
      NEList.ofList
        { name = "xs"; typ = PT.TList PT.TInt; description = "" }
        [ { name = "f"
            typ = PT.TFn(NEList.singleton PT.TInt, PT.TInt)
            description = "" } ]
    returnType = PT.TList PT.TInt
    description = ""
    permissionCeiling = None
    bounds = [] }

/// A closure of in-memory fns, each body analyzed once, as
/// `PackagePermissions.loadClosure` produces.
let private closureOf (fns : List<PT.PackageFn.PackageFn>) =
  fns
  |> List.map (fun fn ->
    fn.hash, (fn, LibExecution.CallGraph.analyzeFn Set.empty fn))
  |> Map.ofList

/// Builtin effects for the review tests: `timeNowMs` is clock-only and
/// `fileRead` reads files; anything else is unknown (incomplete).
let private testEffects : PackagePermissions.CallEffectsFor =
  fun (name, _version) ->
    match name with
    | "timeNowMs" -> Some(Set.singleton Effect.Effect.Clock)
    | "fileRead" -> Some(Set.singleton Effect.Effect.FileRead)
    // `listMap` is classified and has no host effects. `None` would mean that
    // the builtin is unclassified and make the test incomplete for the wrong
    // reason.
    | "listMap" -> Some Set.empty
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
      (eStatement (callBuiltin "fileRead") (eApply (ePackageFn "v1") [] [ eUnit () ]))
    // `Stdlib.List.map`'s shape: hands its own callback to a builtin, so on its
    // own it can say nothing about what runs there.
    higherOrderFn "map-like" (eApply (eBuiltinFn "listMap" 0) [] [ eArg 0; eArg 1 ])
    // A root that uses it, supplying the callback itself.
    unitFn
      "uses-map-like"
      (eApply
        (ePackageFn "map-like")
        []
        [ eArg 0; eLambda (gid ()) [ lpVar "x" ] (callBuiltin "timeNowMs") ]) ]
  |> List.map (fun fn -> fn.hash, fn)
  |> Map.ofList

let private loadFromUniverse : PackagePermissions.Load =
  { fn = fun hash -> Ply(Map.tryFind hash reviewUniverse)
    value = fun _ -> Ply None }

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

/// A reference to a package value is followed only when the value the store
/// holds is data all the way down. These go through `loadClosure`, so they
/// cover the store read as well as the rule.
module ValueReferences =
  let private typeName =
    RT.FQTypeName.fqPackage (LibExecution.PackageRefs.Type.Stdlib.option ())

  let private namedFn : RT.Dval =
    RT.DApplicable(
      RT.AppNamedFn
        { name = RT.FQFnName.fqBuiltin "fileRead" 0
          typeSymbolTable = RT.TST.empty
          typeArgs = []
          access = None
          argsSoFar = []
          boundImpls = [] }
    )

  /// A record of strings, `reset = "\u001b[0m"` and the like.
  let private colors : RT.Dval =
    RT.DRecord(
      typeName,
      typeName,
      [],
      Map.ofList [ "reset", RT.DString "\u001b[0m"; "red", RT.DString "\u001b[31m" ]
    )

  /// A list in a record in an enum: data, three levels down.
  let private nestedData : RT.Dval =
    RT.DEnum(
      typeName,
      typeName,
      [],
      "Some",
      [ RT.DRecord(
          typeName,
          typeName,
          [],
          Map.ofList
            [ "names",
              RT.DList(RT.ValueType.Unknown, [ RT.DString "a"; RT.DString "b" ]) ]
        ) ]
    )

  /// The same shape with a fn in place of one of the strings.
  let private nestedFn : RT.Dval =
    RT.DEnum(
      typeName,
      typeName,
      [],
      "Some",
      [ RT.DRecord(
          typeName,
          typeName,
          [],
          Map.ofList
            [ "names", RT.DList(RT.ValueType.Unknown, [ RT.DString "a"; namedFn ]) ]
        ) ]
    )

  let private values : Map<PT.Hash, RT.Dval> =
    Map.ofList
      [ PT.Hash "colors", colors
        PT.Hash "nested-data", nestedData
        PT.Hash "handler", namedFn
        PT.Hash "nested-fn", nestedFn ]

  /// The requirements of a fn whose body reads one package value. A hash not
  /// in `values` reads as unevaluated.
  let private readingValue (valueHash : string) : Requirements.Result =
    let root = unitFn "reads-value" (ePackageValue valueHash)
    let load : PackagePermissions.Load =
      { fn = fun h -> Ply(if h = root.hash then Some root else None)
        value = fun h -> Ply(Map.tryFind h values) }
    let closure =
      PackagePermissions.loadClosure load root.hash
      |> Ply.toTask
      |> Async.AwaitTask
      |> Async.RunSynchronously
    Requirements.forFunction testEffects closure root.hash

  let tests =
    [ test "a value holding a record of strings is followed" {
        let result = readingValue "colors"
        Expect.isTrue result.complete "a record of strings holds nothing to call"
        Expect.isEmpty result.requiredEffects "reading a stored value does nothing"
      }
      test "a value holding a list in a record in an enum is followed" {
        Expect.isTrue (readingValue "nested-data").complete "data at every depth"
      }
      test "a value holding a function stays incomplete" {
        Expect.isFalse
          (readingValue "handler").complete
          "the reader can call what the value hands back"
      }
      test "a function two levels down in a value still makes it incomplete" {
        Expect.isFalse
          (readingValue "nested-fn").complete
          "a shallow check would miss this one"
      }
      test "a value that has not been evaluated stays incomplete" {
        Expect.isFalse
          (readingValue "not-evaluated").complete
          "an unknown value is never treated as data"
      } ]

let passedCallbacksMakeAnalysisIncomplete =
  test "handing a callback parameter to another fn is not effect-free" {
    // `listMap` calls the supplied function, even though this body has no
    // direct call expression for `f`.
    let root = PT.Hash "passes-callback-along"
    let fn =
      higherOrderFn
        "passes-callback-along"
        // `xs` and `f` are parameters, so they lower to `EArg` positions;
        // position 1 is the function-typed one.
        (eApply (eBuiltinFn "listMap" 0) [] [ eArg 0; eArg 1 ])
    let result = Requirements.forFunction testEffects (closureOf [ fn ]) root
    Expect.isFalse
      result.complete
      "a callback the caller supplies is not knowable here"
    Expect.isEmpty
      result.requiredEffects
      "unknown effects are not invented, but the result is not effect-free"
  }

let unusedNonCallbackParamsStayComplete =
  test "an ordinary parameter does not make analysis incomplete" {
    // The guard is the parameter's declared type, not merely being a variable,
    // so ordinary data arguments must not poison every caller's analysis.
    let root = PT.Hash "ordinary-args"
    let fn = unitFn "ordinary-args" (eApply (eBuiltinFn "timeNowMs" 0) [] [ eArg 0 ])
    let result = Requirements.forFunction testEffects (closureOf [ fn ]) root
    Expect.isTrue result.complete "a non-function parameter is knowable"
    Expect.equal
      result.requiredEffects
      (Set.singleton Effect.Effect.Clock)
      "the builtin's own effects are still reported"
  }

let dependencyCallbacksDoNotMakeCallersIncomplete =
  test "a dependency's escaping callback does not spread to its caller" {
    // The helper's callback is supplied by its caller. The caller's lambda is
    // analyzed here, so the helper's uncertainty must not spread upward.
    let helper =
      higherOrderFn
        "escaping-helper"
        (eApply (eBuiltinFn "listMap" 0) [] [ eArg 0; eArg 1 ])
    let caller =
      unitFn
        "passes-a-literal-lambda"
        (eApply
          (ePackageFn "escaping-helper")
          []
          [ eArg 0; eLambda (gid ()) [ lpVar "x" ] (callBuiltin "timeNowMs") ])
    let result =
      Requirements.forFunction
        testEffects
        (closureOf [ helper; caller ])
        (PT.Hash "passes-a-literal-lambda")

    Expect.isTrue
      result.complete
      "the caller supplied the callback, so nothing here is unknown"
    Expect.equal
      result.requiredEffects
      (Set.singleton Effect.Effect.Clock)
      "the lambda it passed is read in full"
  }

let reviewApprovesACallerOfACallbackTakingDependency =
  testTask "a caller of a callback-taking dependency needs no acknowledgement" {
    // The dependency is incomplete as a root of its own; the caller is not,
    // because it passed the lambda. Judging completeness member by member
    // refused this and told the user to acknowledge a lower bound, while the
    // function's own badge said `complete` -- and it hit every caller of
    // `List.map`, which is most of the package tree.
    let! reviewed = review None "uses-map-like" false false
    match reviewed with
    | Ok(PackagePermissions.Review.Approvable _) -> ()
    | other -> failtest $"expected an approvable review, got {other}"
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

/// A call in a dictionary KEY. `subExprs` visited only the values, so this
/// analyzed as complete and effect-free, and could be approved as such.
let dictionaryKeysAreAnalyzed =
  test "a call in a dictionary key is part of the analysis" {
    let fn =
      unitFn
        "dict-key-effect"
        (eDictOf [ (eApply (eBuiltinFn "timeNowMs" 0) [] [ eUnit () ], eInt64 1L) ])
    let names = (LibExecution.CallGraph.analyzeFn Set.empty fn).names
    Expect.isTrue
      (names
       |> List.exists (fun name ->
         match name with
         | PT.FQFnName.Builtin b -> b.name = "timeNowMs"
         | _ -> false))
      "the clock call in the key is reachable"
  }

/// An operator runs the implementation its save recorded (`Add.add` for the
/// operand type), not just its two operands. Analyzing only the operands
/// reported `a + b` as complete and effect-free when `add` read a file.
module private Operators =
  let plus = PT.InfixFnCall PT.ArithmeticPlus

  let chosen (hash : string) : PT.FQFnName.ImplChoice =
    PT.FQFnName.Chosen { name = PT.Hash hash; location = None }

  /// An `add` implementation with an effect of its own.
  let effectfulAdd = unitFn "effectful-add" (callBuiltin "fileRead")

  let requirementsOf (fns : List<PT.PackageFn.PackageFn>) (root : string) =
    Requirements.forFunction testEffects (closureOf fns) (PT.Hash root)

  /// `display<'a: Show> v = Show.show v`: the call's implementation is the
  /// caller's type argument's.
  let showTrait = PT.Hash "show-trait"
  let deferringDisplay =
    unitFn
      "deferring-display"
      (eApply
        (PT.EFnName(
          gid (),
          PT.NameResolution.ok (
            PT.FQFnName.TraitMethod
              { trait_ = showTrait
                method_ = "show"
                implFn = PT.FQFnName.FromTypeParam "a" }
          ),
          []
        ))
        []
        [ eArg 0 ])

  let callDisplay (bounds : List<PT.FQFnName.BoundImpl>) =
    eApply
      (PT.EFnName(
        gid (),
        PT.NameResolution.ok (PT.FQFnName.fqPackage "deferring-display"),
        bounds
      ))
      []
      [ eUnit () ]

  let tests =
    [ test "an operator's chosen implementation is part of the analysis" {
        let caller =
          unitFn
            "adds"
            (PT.EInfix(gid (), plus, eInt64 1L, eInt64 2L, chosen "effectful-add"))
        let result = requirementsOf [ effectfulAdd; caller ] "adds"
        Expect.isTrue result.complete "the implementation was recorded at save"
        Expect.equal
          result.requiredEffects
          (Set.singleton Effect.Effect.FileRead)
          "the implementation's file read is required"
      }

      test "a piped operator's chosen implementation is part of the analysis" {
        let caller =
          unitFn
            "pipes-add"
            (ePipe
              (eInt64 1L)
              [ PT.EPipeInfix(gid (), plus, eInt64 2L, chosen "effectful-add") ])
        let result = requirementsOf [ effectfulAdd; caller ] "pipes-add"
        Expect.isTrue result.complete "the implementation was recorded at save"
        Expect.equal
          result.requiredEffects
          (Set.singleton Effect.Effect.FileRead)
          "the implementation's file read is required"
      }

      test "an operator whose implementation is unknown is incomplete" {
        // Resolved by the operand type at run time, so nothing here says what runs.
        let caller = unitFn "adds-unknown" (eInfix plus (eInt64 1L) (eInt64 2L))
        let result = requirementsOf [ caller ] "adds-unknown"
        Expect.isFalse result.complete "the store decides at run time"
      }

      test "a named trait call follows its chosen implementation" {
        // The same rule as the operator: `Add.add a b` and `a + b` must agree.
        let caller =
          unitFn
            "names-add"
            (eApply
              (PT.EFnName(
                gid (),
                PT.NameResolution.ok (
                  PT.FQFnName.TraitMethod
                    { trait_ = PT.Hash "add-trait"
                      method_ = "add"
                      implFn = chosen "effectful-add" }
                ),
                []
              ))
              []
              [ eInt64 1L; eInt64 2L ])
        let result = requirementsOf [ effectfulAdd; caller ] "names-add"
        Expect.isTrue result.complete "the implementation was recorded at save"
        Expect.equal
          result.requiredEffects
          (Set.singleton Effect.Effect.FileRead)
          "the implementation's file read is required"
      }

      test "a trait call deferred to a type param is the root's caller's to supply" {
        let result = requirementsOf [ deferringDisplay ] "deferring-display"
        Expect.isFalse result.complete "the caller's type argument decides"
      }

      test "a caller that recorded the bound supplies the implementation" {
        let caller =
          unitFn
            "displays-pinned"
            (callDisplay
              [ { param = "a"
                  trait_ = showTrait
                  method_ = "show"
                  choice = chosen "effectful-add" } ])
        let result =
          requirementsOf [ effectfulAdd; deferringDisplay; caller ] "displays-pinned"
        Expect.isTrue result.complete "the bound is recorded at the call"
        Expect.equal
          result.requiredEffects
          (Set.singleton Effect.Effect.FileRead)
          "the recorded implementation's file read is required"
      }

      test "a caller that recorded no bound leaves the deferred call unresolved" {
        // Nothing in the frame, so the callee asks the store at run time.
        let caller = unitFn "displays-unpinned" (callDisplay [])
        let result = requirementsOf [ deferringDisplay; caller ] "displays-unpinned"
        Expect.isFalse result.complete "the store decides at run time"
      } ]
let testBodiesUseTransitiveCacheSafetyAnalysis =
  testTask "cache safety checks a test body's transitive calls" {
    let analyze (body : PT.Expr) : System.Threading.Tasks.Task<Requirements.Result> =
      PackagePermissions.cacheSafetyForExpression loadFromUniverse testEffects body
      |> Ply.toTask

    let! (safe : Requirements.Result) = analyze (callBuiltin "listMap")
    Expect.isTrue safe.complete "the pure builtin is classified"
    Expect.isEmpty safe.requiredEffects "it requires no effects"

    let! (effectful : Requirements.Result) =
      analyze (eApply (ePackageFn "caller") [] [ eUnit () ])
    Expect.isTrue effectful.complete "the package dependency is resolved"
    Expect.equal
      effectful.requiredEffects
      (set [ Effect.Effect.Clock; Effect.Effect.FileRead ])
      "transitive effects are included"

    let! (dynamic : Requirements.Result) =
      analyze (eApply (eVar "callback") [] [ eUnit () ])
    Expect.isFalse dynamic.complete "dynamic calls cannot be certified cache-safe"

    let! (missing : Requirements.Result) =
      analyze (eApply (ePackageFn "missing") [] [ eUnit () ])
    Expect.isFalse
      missing.complete
      "missing dependencies cannot be certified cache-safe"
  }

let cacheSafetyRejectsBranchDependentRendering =
  testTask "a permission-free branch-dependent builtin is not cache-safe" {
    let callEffectsFor (name, version) =
      if name = "toRepr" then None else testEffects (name, version)
    let! (result : Requirements.Result) =
      PackagePermissions.cacheSafetyForExpression
        loadFromUniverse
        callEffectsFor
        (callBuiltin "toRepr")
      |> Ply.toTask
    Expect.isFalse result.complete "rendering depends on the current branch"
    Expect.isEmpty result.requiredEffects "this is not a permission effect"
  }

let tests =
  testList
    "packagePermissions"
    [ dictionaryKeysAreAnalyzed
      cacheSafetyRejectsBranchDependentRendering
      testBodiesUseTransitiveCacheSafetyAnalysis
      missingCodeIsIncomplete
      deferredCodeRequirementsAreIncluded
      packageValuesMakeAnalysisIncomplete
      passedCallbacksMakeAnalysisIncomplete
      unusedNonCallbackParamsStayComplete
      dependencyCallbacksDoNotMakeCallersIncomplete
      reviewApprovesACallerOfACallbackTakingDependency
      reviewAcceptsAnUnchangedContract
      reviewApprovesTheRootUnderExplicitRules
      reviewRefusesRulesThatCannotCoverTheRequirements
      reviewRefusesAWidenedContractWithoutAcknowledgment
      reviewRejectsAnIncompleteClosure
      testList "operators and trait calls" Operators.tests
      testList "package value references" ValueReferences.tests ]
