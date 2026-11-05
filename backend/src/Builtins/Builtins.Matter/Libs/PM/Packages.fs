/// Builtin functions for working with the dev-time Package Manager
///   (_not_ the run-time PM)
///
/// The PM is taken as a parameter so each runtime supplies its own. It backs the
/// parser flow and pretty-printing, which run constantly, so lookups need to be
/// reasonably fast.
module Builtins.Matter.Libs.PM.Packages

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module VT = LibExecution.ValueType
module NR = LibExecution.RuntimeTypes.NameResolution
module RTPM = LibDB.RuntimeTypes
module PMPT = LibDB.ProgramTypes
module Execution = LibExecution.Execution
module PackageRefs = LibExecution.PackageRefs


let private repointListKT =
  KTList(ValueType.Known(PT2DT.PropagateRepoint.knownType ()))


/// A branch parameter as the PM layer wants it.
///
/// The branch is a PARAMETER here rather than process state, so a caller can ask about a branch it isn't
/// sitting on -- which is what the LSP and any daemon need, and what stops a reader assuming the ambient
/// branch is the one they meant. Dark hands over a `Uuid`, so there is nothing to parse and nothing that
/// can arrive here not being an id; main is main's own uuid, like everywhere else.
let private branchOfParam (branchId : System.Guid) : PT.BranchId =
  PT.BranchId.Id branchId

/// The `branchId` parameter every branch-scoped builtin here takes.
let private branchParam : Param =
  Param.make
    "branchId"
    TUuid
    "the branch to resolve against; main is `SCM.Branch.mainBranchId`. Passed rather than ambient, so a caller can ask about a branch it is not sitting on"


/// Shared decoding for dependency reads, snapshots, and review seeds.
let private referenceSourcesFromDT (sources : List<Dval>) =
  sources
  |> List.map (function
    | DTuple(location, target, []) ->
      PT2DT.PackageLocation.fromDT location, PT2DT.Reference.fromDT target
    | value -> Exception.raiseInternal "Invalid reference source" [ "value", value ])

let private cutoffFromDT (value : Dval) =
  match value with
  | DEnum(_, _, _, "Some", [ DString stamp ]) -> Some stamp
  | DEnum(_, _, _, "None", []) -> None
  | value -> Exception.raiseInternal "Invalid history cutoff" [ "value", value ]

/// Persist a computed cascade on its branch and refresh the process overlay.
let private persistPropagation
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Ply<unit> =
  uply {
    do! LibDB.Queries.ensureAuthoringClockIsCurrent ()
    let! names = LibDB.ReferenceNames.captureForStorage branch ops
    let ops = ops @ names
    if branch.IsMain then
      // Marked as PROPAGATED, not authored. It's the only point at which the difference is known.
      let! _ = LibDB.Inserts.insertAndApplyPropagatedOps ops
      return ()
    else
      // On a branch the repoints are BRANCH ops: stored effective=0 and tagged to the frontier,
      // never folded into main's `locations`. That's the isolation guarantee -- a cascade that
      // leaked into main would be worse than one that didn't happen.
      //
      // A repoint can create a branch-local version of a MAIN item (main's `dep` gets a branch
      // copy pointing at the branch's `base`, main's copy untouched). Recording name bases for
      // them is what lets a later merge tell that apart from a divergence.
      // Marked as PROPAGATED on the branch's own record, as main's `locations.source` would be.
      let! _ = LibDB.Branches.storeDeltaOpsFrom "propagation" branch ops
      let! parentId = LibDB.Branches.parentOf branch
      do! LibDB.Branches.recordNameBases branch parentId ops
      // Fold the CONTENT (never the SetNames) so the new versions resolve and carry their
      // dependency edges, exactly as branch authoring does.
      let contentOps =
        ops
        |> List.filter (fun op ->
          match op with
          | PT.PackageOp.AddValue _
          | PT.PackageOp.AddFn _
          | PT.PackageOp.AddType _
          | PT.PackageOp.AddTrait _
          | PT.PackageOp.AddTraitImpl _
          | PT.PackageOp.AddTest _ -> true
          | _ -> false)
      if not (List.isEmpty contentOps) then
        do! LibDB.PackageOpPlayback.applyBranchContentOps contentOps
      // Refresh the process overlay so a later eval in THIS process sees the repoints.
      let! all = LibDB.Branches.loadDeltaOps branch
      LibDB.PackageManager.setBranchOverlay all
  }


/// The `kind` parameter of `pmFind` and `pmLocations`. A function, not a value: its type is a package
/// ref, which is not resolved yet when this module initializes.
let private kindParam () : Param =
  Param.make
    "kind"
    (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
    "fn, type, value, trait, impl or test"


/// `pmLocations`: every name a hash of <param kind> is bound to, seen from <param branchId>.
///
/// Three answers, in order, because a hash with no live name still has to render as something better
/// than `<hash:d6f972b3>`:
///
/// 1. main's `locations`, via the PM;
/// 2. the branch overlay -- a branch's SetNames never fold into `locations`, so a branch-authored item
///    has no row there at all;
/// 3. a name it USED to have, main's record first and then the branch's. Reached when viewing a
///    superseded version, whose live name has moved on to the newer one.
let private locationsFn (pm : PT.PackageManager) : BuiltInFn =
  { name = fn "pmLocations" 0
    typeParams = []
    parameters =
      [ branchParam
        kindParam ()
        Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
    returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
    description =
      "Returns all locations of a package item of the given kind, by its hash"
    fn =
      (function
      | _, _, _, [| DUuid branchId; kindDval; hashDval |] ->
        uply {
          let branch = branchOfParam branchId
          let kind = PT2DT.ItemKind.fromDT kindDval
          let hash = PT2DT.Hash.fromDT hashDval

          let fromMain, everNamedOnMain =
            match kind with
            | PT.ItemKind.Type ->
              pm.getTypeLocations, PMPT.Type.getLocationsEverNamed
            | PT.ItemKind.Value ->
              pm.getValueLocations, PMPT.Value.getLocationsEverNamed
            | PT.ItemKind.Fn -> pm.getFnLocations, PMPT.Fn.getLocationsEverNamed
            | PT.ItemKind.Trait ->
              pm.getTraitLocations, PMPT.Trait.getLocationsEverNamed
            | PT.ItemKind.TraitImpl ->
              pm.getTraitImplLocations, PMPT.TraitImpl.getLocationsEverNamed
            | PT.ItemKind.Test ->
              pm.getTestLocations, PMPT.Test.getLocationsEverNamed

          let! onMain = fromMain hash
          let named = LibDB.PackageManager.locationsFor branch kind hash onMain

          let! result =
            if List.isEmpty named then
              uply {
                match! everNamedOnMain hash with
                | [] ->
                  return
                    LibDB.PackageManager.branchLocationsEverNamed branch kind hash
                | everNamed -> return everNamed
              }
            else
              Ply named

          return
            result
            |> List.map PT2DT.PackageLocation.toDT
            |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
        }
      | _ -> incorrectArgs ())
    previewable = Impure
    callEffects = set [ Effect.PackageRead ]
    deprecated = NotDeprecated }


/// `pmFind`: location -> hash for an item of <param kind>, resolved against <param branchId>.
///
/// On a BRANCH, resolve against the overlay, so a name authored earlier on the same branch resolves. On
/// main, read the store directly: the overlay PM memoizes its base, which goes stale across a long-lived
/// process (the test harness shares one), and the direct finder (`PMPT.{Type,Value,Fn,...}.find`) has no
/// cache to go stale.
let private findFn () : BuiltInFn =
  { name = fn "pmFind" 0
    typeParams = []
    parameters =
      [ branchParam
        kindParam ()
        Param.make
          "location"
          (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
          "" ]
    returnType =
      TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
    description =
      "Tries to find a package item of the given kind, by location, and returns its hash if it exists"
    fn =
      (function
      | _, _, _, [| DUuid branchId; kindDval; location |] ->
        uply {
          let kind = PT2DT.ItemKind.fromDT kindDval
          let location = PT2DT.PackageLocation.fromDT location
          let branch = branchOfParam branchId
          let! result =
            if branch.IsMain then
              match kind with
              | PT.ItemKind.Type -> PMPT.Type.find location
              | PT.ItemKind.Value -> PMPT.Value.find location
              | PT.ItemKind.Fn -> PMPT.Fn.find location
              | PT.ItemKind.Trait -> PMPT.Trait.find location
              | PT.ItemKind.TraitImpl -> PMPT.TraitImpl.find location
              | PT.ItemKind.Test -> PMPT.Test.find location
            else
              let branchPM = LibDB.PackageManager.ptForBranch branch
              match kind with
              | PT.ItemKind.Type -> branchPM.findType location
              | PT.ItemKind.Value -> branchPM.findValue location
              | PT.ItemKind.Fn -> branchPM.findFn location
              | PT.ItemKind.Trait -> branchPM.findTrait location
              | PT.ItemKind.TraitImpl -> branchPM.findTraitImpl location
              | PT.ItemKind.Test -> branchPM.findTest location
          return
            result
            |> Option.map PT2DT.Hash.toDT
            |> Dval.option (PT2DT.Hash.knownType ())
        }
      | _ -> incorrectArgs ())
    previewable = Impure
    callEffects = set [ Effect.PackageRead ]
    deprecated = NotDeprecated }


/// `pmGet{Type,Value,Fn}`: hash -> the stored item, decoded to its Dark-side type.
let private getByHashFn
  (builtinName : string)
  (itemWord : string)
  (typeName : unit -> FQTypeName.FQTypeName)
  (get : PT.Hash -> Ply<Option<'a>>)
  (toDT : 'a -> Dval)
  : BuiltInFn =
  { name = fn builtinName 0
    typeParams = []
    parameters =
      [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
    returnType = TypeReference.option (TCustomType(NR.ok (typeName ()), []))
    description = $"Returns a package {itemWord}, by hash, if it exists"
    fn =
      (function
      | _, _, _, [| hashDval |] ->
        uply {
          let hash = PT2DT.Hash.fromDT hashDval
          let! result = get hash
          return
            result |> Option.map toDT |> Dval.option (KTCustomType(typeName (), []))
        }
      | _ -> incorrectArgs ())
    previewable = Impure
    callEffects = set [ Effect.PackageRead ]
    deprecated = NotDeprecated }


/// Mirrors `Query.getDirectSubmodules`: drop the query's current path, keep the next segment,
/// dedupe, sort. Reduced here rather than in Dark because at the root of a large store this is
/// hundreds of module paths collapsing to a handful of names, and the search already has them all.
let private directSubmodules
  (query : PT.Search.SearchQuery)
  (results : PT.Search.SearchResults)
  : List<string> =
  let depth = List.length query.currentModule
  // `List.skip` throws past the end; Dark's `List.drop` yields []. Match Dark.
  let rec dropN n (xs : List<string>) =
    if n <= 0 then
      xs
    else
      match xs with
      | [] -> []
      | _ :: rest -> dropN (n - 1) rest
  results.submodules
  |> List.choose (fun modulePath ->
    match dropN depth modulePath with
    | next :: _ when next <> "" -> Some next
    | _ -> None)
  |> List.distinct
  |> List.sort


/// Strings as a Dark `List<String>`.
let private toDList (xs : List<string>) : Dval =
  xs |> List.map DString |> Dval.list KTString


// TODO: review/reconsider the accessibility of these fns
let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ // location -> hash, and hash -> every location, for any item kind
    findFn ()
    locationsFn pm

    // types
    getByHashFn
      "pmGetType"
      "type"
      PT2DT.PackageType.typeName
      pm.getType
      PT2DT.PackageType.toDT


    // values
    getByHashFn
      "pmGetValue"
      "value"
      PT2DT.PackageValue.typeName
      pm.getValue
      PT2DT.PackageValue.toDT


    { name = fn "pmFindValuesByValueType" 0
      typeParams = []
      parameters =
        [ Param.make
            "valueType"
            (TCustomType(NR.ok (RT2DT.ValueType.typeName ()), []))
            "The ValueType to search for" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Returns a list of value hashes that have the given ValueType. "
        + "Uses exact match on the serialized type for efficient lookup."
      fn =
        (function
        | _, _, _, [| valueTypeDval |] ->
          uply {
            let vt = RT2DT.ValueType.fromDT valueTypeDval
            let! valueIds = RTPM.Value.findByValueType vt
            return
              DList(
                VT.known (PT2DT.Hash.knownType ()),
                valueIds |> List.map RT2DT.Hash.toDT
              )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Whether a stored value has its runtime value, so a save can say which values failed to
    // evaluate without Dark reading `package_values` itself.
    { name = fn "pmValueEvaluated" 0
      typeParams = []
      parameters =
        [ Param.make
            "valueHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package value" ]
      returnType = TBool
      description =
        "Returns true if the package value at this hash has been evaluated and its result "
        + "stored. False if its evaluation never ran or failed, or if there is no such value."
      fn =
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let (PT.Hash h) = PT2DT.Hash.fromDT hashDval
            let! evaluated = RTPM.Value.isEvaluated (Hash h)
            return DBool evaluated
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // traits and impls
    getByHashFn
      "pmGetTrait"
      "trait"
      PT2DT.Trait.typeName
      pm.getTrait
      PT2DT.Trait.toDT

    getByHashFn
      "pmGetTraitImpl"
      "impl"
      PT2DT.TraitImpl.typeName
      pm.getTraitImpl
      PT2DT.TraitImpl.toDT


    // Every impl of a trait visible on a branch, as dispatch sees them
    { name = fn "pmImplCandidates" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TUuid
            "The branch whose bindings decide what is visible"
          Param.make
            "traitHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "The trait" ]
      returnType = TList(TCustomType(NR.ok (RT2DT.ImplCandidate.typeName ()), []))
      description =
        "Returns the impls of a trait that are bound on the given branch: what "
        + "`Trait.method x` can dispatch to."
      fn =
        (function
        | exeState, _, _, [| DUuid branchId; traitHash |] ->
          uply {
            let (PT.Hash h) = PT2DT.Hash.fromDT traitHash
            let! candidates =
              exeState.fns.implCandidates
                (LibExecution.Branching.BranchId.Id branchId)
                (LibExecution.RuntimeTypes.Hash h)
            return
              DList(
                VT.known (RT2DT.ImplCandidate.knownType ()),
                candidates |> List.map RT2DT.ImplCandidate.toDT
              )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // The trait an operator IS, and the method it maps to. Its impls are `pmImplCandidates`.
    //
    // One table, in `NumericTraits`, which is also what the lowering uses: the at-rest
    // checker asking a second copy "which types support `+`" is how the checker and the runtime
    // come to disagree about what an operator means. Same reason `Lww` has exactly one copy.
    { name = fn "pmOperatorTrait" 0
      typeParams = []
      parameters =
        [ Param.make
            "operator"
            (TCustomType(NR.ok (PT2DT.Operator.typeName ()), []))
            "The operator: any infix one, unary minus, or `~`" ]
      returnType =
        TypeReference.option (
          TTuple(TCustomType(NR.ok (PT2DT.Hash.typeName ()), []), TString, [])
        )
      description =
        "The trait an operator is and the method it maps to. None for an operator that is "
        + "not a trait: `==` and `!=` are structural equality, and `++` is no longer in the "
        + "language. Also None while the trait hashes are not generated."
      fn =
        (function
        | _, _, _, [| operator |] ->
          let innerType =
            KTTuple(
              VT.known (KTCustomType(PT2DT.Hash.typeName (), [])),
              VT.known KTString,
              []
            )
          LibExecution.NumericTraits.ofOperator (PT2DT.Operator.fromDT operator)
          |> Option.map (fun (traitHash, methodName) ->
            DTuple(PT2DT.Hash.toDT (PT.Hash traitHash), DString methodName, []))
          |> Dval.option innerType
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Evaluate a package value by its UUID
    { name = fn "pmEvaluateValue" 0
      typeParams = []
      parameters =
        [ Param.make
            "valueHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the package value to evaluate" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "Evaluates a package value by its hash and returns the result. "
        + "Returns None if the value doesn't exist or fails to evaluate."
      fn =
        (function
        | exeState, _, _, [| hashDval |] ->
          uply {
            let (PT.Hash hash) = PT2DT.Hash.fromDT hashDval
            let valueName = FQValueName.Package(Hash hash)
            let instrs : Instructions =
              { registerCount = 1
                instructions = [ LoadValue(0, valueName) ]
                resultIn = 0 }

            let! result = Execution.executeExpr exeState instrs
            match result with
            | Ok dval ->
              match Dval.toValueType dval with
              | ValueType.Known kt -> return Dval.optionSome kt dval
              | ValueType.Unknown -> return Dval.optionSome KTUnit dval
            | Error _ -> return Dval.optionNone KTUnit
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.Native ]
      deprecated = NotDeprecated }


    // Functions
    getByHashFn
      "pmGetFn"
      "function"
      PT2DT.PackageFn.typeName
      pm.getFn
      PT2DT.PackageFn.toDT

    // The same fn with its body left out, before it becomes a Dark value: for a caller that reads
    // only the signature, which is how the at-rest checker loads a declaration's dependencies.
    getByHashFn
      "pmGetFnSignature"
      "function, without its body,"
      PT2DT.PackageFn.typeName
      pm.getFn
      (fun fn -> PT2DT.PackageFn.toDT { fn with body = PT.EUnit 0UL })


    // Resolve a package fn's dotted name to a callable value (Applicable), so a name that only exists as a
    // STRING (a CLI arg) can be passed as a function without eval'ing a source string. This is what lets
    // `dark serve` hand a router to `Stdlib.HttpServer.serve` directly (no `cliEvaluateExpression`).
    // CLEANUP(applicableByName): fold into eval and delete this builtin; wrinkle: an
    // expression that produces nothing must keep printing nothing.
    { name = fn "applicableByName" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make "name" TString "dotted package fn name, e.g. Stdlib.List.map" ]
      returnType =
        TypeReference.result
          (TFn(NEList.singleton (TVariable "a"), TVariable "b"))
          TString
      description =
        "Resolves a package function by its dotted <param name> to a callable value, "
        + "as a Result — Error (a plain-English message) if there's no such function. "
        + "Lets a caller (e.g. `dark serve`) report a bad name cleanly instead of "
        + "crashing."
      fn =
        (function
        | _, _, _, [| DUuid branchId; DString name |] ->
          uply {
            let okKT = KTFn(NEList.singleton ValueType.Unknown, ValueType.Unknown)
            let err (msg : string) = Dval.resultError okKT KTString (DString msg)
            // dotted name → owner.modules….fnName (native pattern-match; Prelude's List.last is Option-safe)
            match name.Split('.') |> Array.toList |> List.rev with
            | fnName :: revOwnerMods ->
              match List.rev revOwnerMods with
              | owner :: modules ->
                let location : PT.PackageLocation =
                  { owner = owner; modules = modules; name = fnName }
                // Through the branch overlay, like `pmSearch` and `pmFind`. The closure `pm` is
                // MAIN's package manager; asked about a router authored on a branch it answers "No
                // function named ..." about a fn that is plainly there.
                let branchPM =
                  LibDB.PackageManager.ptForBranch (branchOfParam branchId)
                match! branchPM.findFn location with
                | Some fqPkg ->
                  let rtName = FQFnName.Package(PT2RT.FQFnName.Package.toRT fqPkg)
                  let namedFn : ApplicableNamedFn =
                    { name = rtName
                      typeSymbolTable = TST.empty
                      typeArgs = []
                      boundImpls = []
                      // Do not capture the resolver's access here. The code
                      // that calls or receives this reference supplies its
                      // own frame access (for example, an HTTP server).
                      access = None
                      argsSoFar = [] }
                  return
                    Dval.resultOk okKT KTString (DApplicable(AppNamedFn namedFn))
                | None -> return err $"No function named {name}"
              | [] -> return err $"Not a package function name: {name}"
            | [] -> return err "Empty router name"
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // The by-hash twin of `applicableByName`, for a host that follows edits (`Stdlib.Live`). A name
    // resolves to whatever is bound NOW; a host that wants to keep running the last version that passed
    // its checks while a broken edit sits at the name needs to call by hash. Every version is still in
    // the store, so this is a lookup, not a resurrection.
    { name = fn "applicableByHash" 0
      typeParams = []
      parameters =
        [ Param.make
            "hash"
            TString
            "a package fn's content hash, as `Hash` carries it" ]
      // Any arity: the function is only known once the store answers.
      returnType = TypeReference.result (TVariable "f") TString
      description =
        "Resolves a package function by its content <param hash> to a callable value, "
        + "as a Result -- Error if no function has that hash."
      fn =
        (function
        | _, _, _, [| DString hash |] ->
          uply {
            let okKT = KTFn(NEList.singleton ValueType.Unknown, ValueType.Unknown)
            let err (msg : string) = Dval.resultError okKT KTString (DString msg)
            match! pm.getFn (PT.Hash hash) with
            | Some _ ->
              let namedFn : ApplicableNamedFn =
                { name = FQFnName.Package(Hash hash)
                  typeSymbolTable = TST.empty
                  typeArgs = []
                  // As in `applicableByName`: whoever applies this supplies the frame access.
                  access = None
                  argsSoFar = []
                  boundImpls = [] }
              return Dval.resultOk okKT KTString (DApplicable(AppNamedFn namedFn))
            | None -> return err $"No function with hash {hash}"
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Tests are a separate package namespace: discoverable and executable,
    // but never returned as a callable production function.
    getByHashFn
      "pmGetTest"
      "test"
      PT2DT.PackageTest.typeName
      pm.getTest
      PT2DT.PackageTest.toDT

    // Apply a fn a host resolved live, and get its failure back as a value. A host loop that shows
    // the last good frame with the error under it cannot let an RTE from user code unwind the loop;
    // this is the one place an RTE from an applied fn becomes a `Result`.
    //
    // The fn is the APPROVAL ROOT of the call, as a router handed to `serve` is: the host chose it
    // by name, and every version the name resolves to is the same choice, so a fresh version is not
    // asked to be approved again before it may print. The caller's own access still bounds it
    // (`constrainBy`), and the instance policy is the ceiling as everywhere. A lambda has no root
    // and runs under the caller's policies alone.
    { name = fn "applicableTryApply" 0
      typeParams = [ "a"; "b" ]
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton (TVariable "a"), TVariable "b"))
            "what to call"
            [ "arg" ]
          Param.make "arg" (TVariable "a") "" ]
      returnType = TypeReference.result (TVariable "b") TString
      description =
        "Calls <param fn> with <param arg>; `Error` with the failure's message instead of "
        + "raising, whatever the failure was."
      fn =
        (function
        | exeState, vm, _, [| DApplicable applicable; arg |] ->
          uply {
            let asRoot =
              LibDB.PolicyStore.rootState
                exeState
                vm.activeAccess
                (LibDB.PolicyStore.rootOf applicable)
            let ok (dv : Dval) =
              DEnum(
                Dval.resultType (),
                Dval.resultType (),
                [ ValueType.Unknown; ValueType.Known KTString ],
                "Ok",
                [ dv ]
              )
            let err (message : string) =
              DEnum(
                Dval.resultType (),
                Dval.resultType (),
                [ ValueType.Unknown; ValueType.Known KTString ],
                "Error",
                [ DString message ]
              )
            // Two kinds of failure, and both have to come back as a value. A runtime error is
            // the expected one. An F#-side raise -- `Exception.raiseInternal` from a builtin,
            // say, reporting a store that cannot answer -- is not, and it used to unwind
            // whoever called this: a live view whose `update` hit one lost the whole host loop
            // and the terminal with it, instead of showing the error in a band. The callers of
            // this builtin are precisely the ones that cannot let user code unwind them
            // (`Stdlib.Live.call`), so `applyCatching` catches both. Running out of memory is
            // the exception: it exits, as everywhere else in the runtime.
            match!
              Execution.applyCatching asRoot vm.threadID asRoot.access applicable arg
            with
            | Ok dv -> return ok dv
            | Error Execution.OutOfStack ->
              let! message =
                Execution.runtimeErrorMessage
                  asRoot
                  (RuntimeError.UncaughtException(Execution.outOfStackMessage, []))
              return err message
            | Error(Execution.Failed message) -> return err message
          }
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "pmSearch" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "query"
            (TCustomType(NR.ok (PT2DT.Search.SearchQuery.typeName ()), []))
            "" ]
      returnType = TCustomType(NR.ok (PT2DT.Search.SearchResults.typeName ()), [])
      description = "Search for packages based on the given query."
      fn =
        function
        | _, _, _, [| DUuid branchId; query as DRecord(_, _, _, _fields) |] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            // Through the branch overlay, so a branch's items show up in ls/view/tree/search, not just
            // eval. Main's overlay is main itself.
            let pm = LibDB.PackageManager.ptForBranch (branchOfParam branchId)
            let! results = pm.search searchQuery
            return PT2DT.Search.SearchResults.toDT results
          }
        | _ -> incorrectArgs ()
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "pmSearchNamesAndHashes" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "query"
            (TCustomType(NR.ok (PT2DT.Search.SearchQuery.typeName ()), []))
            "" ]
      returnType =
        let nameAndHash =
          TList(TTuple(TString, TCustomType(NR.ok (PT2DT.Hash.typeName ()), []), []))
        TTuple(
          TList TString,
          nameAndHash,
          [ nameAndHash; nameAndHash; nameAndHash; nameAndHash; nameAndHash ]
        )
      description =
        "Search, returning (direct submodules, types, values, fns, traits, impls, "
        + "tests) "
        + "as (name, hash) pairs. Submodules are already reduced to the direct "
        + "children of the query's module and sorted."
      fn =
        function
        | _, _, _, [| DUuid branchId; query as DRecord(_, _, _, _fields) |] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            // Through the branch overlay: a branch is its ops laid over main, not a scope to walk up.
            let pm = LibDB.PackageManager.ptForBranch (branchOfParam branchId)
            let! results = pm.search searchQuery

            let submodules = directSubmodules searchQuery results

            let pairKT =
              KTTuple(VT.string, ValueType.Known(PT2DT.Hash.knownType ()), [])

            let pairs
              (locations : List<PT.LocatedItem<'a>>)
              (hashOf : 'a -> PT.Hash)
              =
              locations
              |> List.map (fun i ->
                DTuple(
                  DString i.location.name,
                  PT2DT.Hash.toDT (hashOf i.entity),
                  []
                ))
              |> Dval.list pairKT

            return
              DTuple(
                toDList submodules,
                pairs results.types (fun (t : PT.PackageType.PackageType) -> t.hash),
                [ pairs results.values (fun (v : PT.PackageValue.PackageValue) ->
                    v.hash)
                  pairs results.fns (fun (f : PT.PackageFn.PackageFn) -> f.hash)
                  pairs results.traits (fun (t : PT.Trait.Trait) -> t.hash)
                  pairs results.impls (fun (i : PT.TraitImpl.TraitImpl) -> i.hash)
                  pairs results.tests (fun (test : PT.PackageTest.PackageTest) ->
                    test.hash) ]
              )
          }
        | _ -> incorrectArgs ()
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Hands the compiled instruction stream to Dark so it can be rendered there
    // (`PrettyPrinter.RuntimeTypes.instructions`). Data only: no formatting happens in F#, because a
    // disassembly listing is exactly the kind of thing Dark should own. The PT side needs no equivalent
    // builtin -- `pmGetFn` already returns the tree.
    { name = fn "pmFnInstructions" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (
          TCustomType(NR.ok (RT2DT.Instructions.typeName ()), [])
        )
      description =
        "Returns the register-machine instructions a package function compiles to, "
        + "or None if there's no such function."
      fn =
        (function
        | exeState, _, _, [| hashDval |] ->
          uply {
            let (PT.Hash hashStr) = PT2DT.Hash.fromDT hashDval
            match! exeState.fns.package (Hash hashStr) with
            | None -> return Dval.optionNone (RT2DT.Instructions.knownType ())
            | Some rtFn ->
              return
                RT2DT.Instructions.toDT rtFn.body
                |> Dval.optionSome (RT2DT.Instructions.knownType ())
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Restore a name to existing content with a distinct, stamped override op.
    // A bare SetName would deduplicate against its original binding and do nothing.
    { name = fn "pmSetName" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "The name to bind"
          Param.make
            "itemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, value, trait, impl or test"
          Param.make
            "hash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Existing content to bind the name to" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Binds a name to content already in the store, as a Decision/Override op. Errors if the content isn't there."
      fn =
        (function
        | _, _, _, [| DUuid branchGuid; locationDval; itemKindDval; hashDval |] ->
          uply {
            let loc = PT2DT.PackageLocation.fromDT locationDval
            let kind = PT2DT.ItemKind.fromDT itemKindDval
            let hash = PT2DT.Hash.fromDT hashDval

            // Refuse to bind a name to content that isn't there. A dangling binding folds into a location
            // row that resolves to nothing, which fails at CALL time, far from the cause.
            let! exists =
              uply {
                match kind with
                | PT.ItemKind.Type ->
                  let! t = LibDB.PackageManager.pt.getType hash
                  return Option.isSome t
                | PT.ItemKind.Fn ->
                  let! f = LibDB.PackageManager.pt.getFn hash
                  return Option.isSome f
                | PT.ItemKind.Value ->
                  let! v = LibDB.PackageManager.pt.getValue hash
                  return Option.isSome v
                | PT.ItemKind.Trait ->
                  let! t = LibDB.PackageManager.pt.getTrait hash
                  return Option.isSome t
                | PT.ItemKind.TraitImpl ->
                  let! i = LibDB.PackageManager.pt.getTraitImpl hash
                  return Option.isSome i
                | PT.ItemKind.Test ->
                  let! test = LibDB.PackageManager.pt.getTest hash
                  return Option.isSome test
              }

            if not exists then
              return
                DString "no item with that hash in the store"
                |> Dval.resultError KTUnit KTString
            else
              let reference = PT.Reference.fromHashAndKind (hash, kind)

              // A repeated restore is a new binding decision, even if its hash existed before.
              let decisionId =
                let mods = String.concat "." loc.modules
                let now = System.DateTime.UtcNow.ToString("o")
                $"rebind:{loc.owner}.{mods}.{loc.name}:{hash}:{now}"

              let ops =
                [ PT.PackageOp.Decision(
                    decisionId,
                    loc,
                    "restore version",
                    PT.DecisionKind.Override reference
                  ) ]

              let branchId = branchOfParam branchGuid

              if branchId.IsMain then
                let! _ = LibDB.Inserts.insertAndApplyOps ops
                ()
              else
                // A rebind a person forced, on the branch's own record; see `op_branches.source`.
                let! _ = LibDB.Branches.storeDeltaOpsFrom "resolution" branchId ops
                let! parentId = LibDB.Branches.parentOf branchId
                do! LibDB.Branches.recordNameBases branchId parentId ops

              return Dval.resultOk KTUnit KTString DUnit
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      // It authors an op. `Decision`/`Override` is a write whatever the name suggests, and this one
      // reaches `Inserts`, `storeDeltaOps` and `recordNameBases`.
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    // Dependency queries and propagation must agree on who calls what after a rename.
    // Share that logic here and fetch only the callers of the requested items.
    { name = fn "pmDependents" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "targets"
            (TList(
              TTuple(
                TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
                TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []),
                []
              )
            ))
            "Names to find callers for" ]
      returnType =
        TList(
          TTuple(
            TCustomType(NR.ok (PT2DT.Hash.typeName ()), []),
            TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
            [ TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []) ]
          )
        )
      description = "Find callers using this branch's reference names."
      fn =
        (function
        | _, _, _, [| DUuid branchGuid; DList(_, targets) |] ->
          uply {
            let branch = branchOfParam branchGuid
            let targets : List<LibDB.Queries.LocationTarget> =
              targets
              |> List.map (function
                | DTuple(location, kind, []) ->
                  { location = PT2DT.PackageLocation.fromDT location
                    itemKind = PT2DT.ItemKind.fromDT kind
                    hashes = [] }
                | v ->
                  Exception.raiseInternal "Invalid dependency target" [ "value", v ])
            let! state = LibDB.ReferenceNames.load branch
            let! bindings = LibDB.ReferenceNames.branchBindings branch
            let! dependents =
              LibDB.ReferenceDependencies.dependents state bindings targets
            let tupleKT =
              KTTuple(
                VT.known (PT2DT.Hash.knownType ()),
                VT.known (PT2DT.PackageLocation.knownType ()),
                [ VT.known (PT2DT.ItemKind.knownType ()) ]
              )
            return
              dependents
              |> List.map (fun d ->
                DTuple(
                  PT2DT.Hash.toDT d.itemHash,
                  PT2DT.PackageLocation.toDT d.itemLocation,
                  [ PT2DT.ItemKind.toDT d.itemKind ]
                ))
              |> Dval.list tupleKT
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmLiveReferenceSources" 0
      typeParams = []
      parameters = [ branchParam ]
      returnType =
        TList(
          TTuple(
            TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
            TCustomType(NR.ok (PT2DT.Reference.typeName ()), []),
            []
          )
        )
      description = "Read the declarations visible on a branch, including its draft."
      fn =
        (function
        | _, _, _, [| DUuid branch |] ->
          uply {
            let! bindings =
              LibDB.ReferenceDependencies.liveBindings (branchOfParam branch)
            return
              bindings
              |> Map.toList
              |> List.map (fun (location, target) ->
                DTuple(
                  PT2DT.PackageLocation.toDT location,
                  PT2DT.Reference.toDT target,
                  []
                ))
              |> Dval.list (
                KTTuple(
                  VT.known (PT2DT.PackageLocation.knownType ()),
                  VT.known (PT2DT.Reference.knownType ()),
                  []
                )
              )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    // Read the graph in one native batch so status and commit avoid per-reference interpreter work.
    { name = fn "pmNamedReferences" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "sources"
            (TList(
              TTuple(
                TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
                TCustomType(NR.ok (PT2DT.Reference.typeName ()), []),
                []
              )
            ))
            "Callers to read, or an empty list for all visible callers" ]
      returnType =
        TypeReference.result
          (TList(
            TTuple(
              TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
              TCustomType(NR.ok (PT2DT.Reference.typeName ()), []),
              [ TCustomType(NR.ok (PT2DT.Reference.typeName ()), [])
                TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []) ]
            )
          ))
          TString
      description = "Read callers' dependencies using this branch's reference names."
      fn =
        (function
        | _, _, _, [| DUuid branch; DList(_, sources) |] ->
          uply {
            let sources = referenceSourcesFromDT sources
            let! edges =
              LibDB.ReferenceDependencies.namedReferences
                (branchOfParam branch)
                sources
            let edgeKT =
              KTTuple(
                VT.known (PT2DT.PackageLocation.knownType ()),
                VT.known (PT2DT.Reference.knownType ()),
                [ VT.known (PT2DT.Reference.knownType ())
                  VT.known (PT2DT.PackageLocation.knownType ()) ]
              )
            let listKT = KTList(VT.known edgeKT)
            match edges with
            | Error message ->
              return Dval.resultError listKT KTString (DString message)
            | Ok edges ->
              let values =
                edges
                |> List.map (fun (location, caller, target, name) ->
                  DTuple(
                    PT2DT.PackageLocation.toDT location,
                    PT2DT.Reference.toDT caller,
                    [ PT2DT.Reference.toDT target; PT2DT.PackageLocation.toDT name ]
                  ))
              return Dval.resultOk listKT KTString (Dval.list edgeKT values)
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmDependencyReview" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "changed"
            (TList(
              TTuple(
                TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
                TCustomType(NR.ok (PT2DT.Reference.typeName ()), []),
                []
              )
            ))
            "Changed names and their current versions, in review order" ]
      returnType =
        let location = TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), [])
        let reference = TCustomType(NR.ok (PT2DT.Reference.typeName ()), [])
        TypeReference.result
          (TTuple(
            TList(TTuple(location, reference, [ reference; location; reference ])),
            TList(TTuple(location, location, [])),
            []
          ))
          TString
      description =
        "Review stale references and reachable caller updates using one branch snapshot."
      fn =
        (function
        | _, _, _, [| DUuid branch; DList(_, changed) |] ->
          uply {
            let changed = referenceSourcesFromDT changed
            let! result =
              LibDB.ReferenceDependencies.dependencyReview
                (branchOfParam branch)
                changed
            let location = VT.known (PT2DT.PackageLocation.knownType ())
            let reference = VT.known (PT2DT.Reference.knownType ())
            let staleKT =
              KTTuple(location, reference, [ reference; location; reference ])
            let repointKT = KTTuple(location, location, [])
            let resultKT =
              KTTuple(
                VT.known (KTList(VT.known staleKT)),
                VT.known (KTList(VT.known repointKT)),
                []
              )
            match result with
            | Error message ->
              return Dval.resultError resultKT KTString (DString message)
            | Ok(stale, repoints) ->
              let stale =
                stale
                |> List.map (fun ((usage, caller, target, dependency), current) ->
                  DTuple(
                    PT2DT.PackageLocation.toDT usage,
                    PT2DT.Reference.toDT caller,
                    [ PT2DT.Reference.toDT target
                      PT2DT.PackageLocation.toDT dependency
                      PT2DT.Reference.toDT current ]
                  ))
                |> Dval.list staleKT
              let repoints =
                repoints
                |> List.map (fun (usage, dependency) ->
                  DTuple(
                    PT2DT.PackageLocation.toDT usage,
                    PT2DT.PackageLocation.toDT dependency,
                    []
                  ))
                |> Dval.list repointKT
              return Dval.resultOk resultKT KTString (DTuple(stale, repoints, []))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmRevisionOperations" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make "inherited" TBool "Read only committed ancestor history"
          Param.make
            "before"
            (TypeReference.option TString)
            "Optional history cutoff" ]
      returnType =
        TList(
          TTuple(
            TString,
            TString,
            [ TCustomType(NR.ok (PT2DT.PackageOp.typeName ()), []) ]
          )
        )
      description = "Read one branch's operations for undo history."
      fn =
        (function
        | _, _, _, [| DUuid branchGuid; DBool inherited; before |] ->
          uply {
            let before = cutoffFromDT before
            let! operations =
              LibDB.OpHistory.revisionOperations
                (branchOfParam branchGuid)
                inherited
                before
            return
              operations
              |> List.map (fun (id, stamp, op) ->
                DTuple(DString id, DString stamp, [ PT2DT.PackageOp.toDT op ]))
              |> Dval.list (
                KTTuple(
                  VT.known KTString,
                  VT.known KTString,
                  [ VT.known (KTCustomType(PT2DT.PackageOp.typeName (), [])) ]
                )
              )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmReferenceNameOverrides" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "targets"
            (TList(TCustomType(NR.ok (PT2DT.Reference.typeName ()), [])))
            "Declaration versions to read saved reference names for"
          Param.make
            "before"
            (TypeReference.option TString)
            "Read names as they were at this timestamp, or use None for current names" ]
      returnType =
        TList(TCustomType(NR.ok (PT2DT.ReferenceNamesSnapshot.typeName ()), []))
      description = "Read this branch's saved reference names."
      fn =
        (function
        | _, _, _, [| DUuid branchGuid; DList(_, targets); before |] ->
          uply {
            let before = cutoffFromDT before
            let! snapshots =
              LibDB.ReferenceNames.overrides
                (branchOfParam branchGuid)
                (List.map PT2DT.Reference.fromDT targets)
                before
            return
              snapshots
              |> List.map PT2DT.ReferenceNamesSnapshot.toDT
              |> Dval.list (PT2DT.ReferenceNamesSnapshot.knownType ())
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmApplyReferenceNames" 0
      typeParams = []
      parameters =
        [ Param.make
            "snapshots"
            (TList(TCustomType(NR.ok (PT2DT.ReferenceNamesSnapshot.typeName ()), [])))
            "Saved reference names before this operation"
          Param.make
            "op"
            (TCustomType(NR.ok (PT2DT.PackageOp.typeName ()), []))
            "The original operation to apply to these reference names" ]
      returnType =
        TList(TCustomType(NR.ok (PT2DT.ReferenceNamesSnapshot.typeName ()), []))
      description = "Apply an operation's changes to saved reference names."
      fn =
        (function
        | _, _, _, [| DList(_, snapshots); operation |] ->
          match PT2DT.PackageOp.fromDT operation with
          | None -> incorrectArgs ()
          | Some op ->
            LibDB.ReferenceNames.applySnapshots
              (List.map PT2DT.ReferenceNamesSnapshot.fromDT snapshots)
              op
            |> List.map PT2DT.ReferenceNamesSnapshot.toDT
            |> Dval.list (PT2DT.ReferenceNamesSnapshot.knownType ())
            |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "pmStoredReferenceSnapshots" 0
      typeParams = []
      parameters =
        [ Param.make
            "sources"
            (TList(
              TTuple(
                TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
                TCustomType(NR.ok (PT2DT.Reference.typeName ()), []),
                []
              )
            ))
            "Locations and versions of the declarations to read" ]
      returnType =
        TList(TCustomType(NR.ok (PT2DT.ReferenceNamesSnapshot.typeName ()), []))
      description = "Read the reference names from stored declarations."
      fn =
        (function
        | _, _, _, [| DList(_, sources) |] ->
          uply {
            let sources = referenceSourcesFromDT sources
            let! snapshots = LibDB.ReferenceNamesContent.storedSnapshots sources
            return
              snapshots
              |> List.map PT2DT.ReferenceNamesSnapshot.toDT
              |> Dval.list (PT2DT.ReferenceNamesSnapshot.knownType ())
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    { name = fn "pmCommittedBindings" 0
      typeParams = []
      parameters =
        [ Param.make
            "locations"
            (TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), [])))
            "Names to resolve on committed main" ]
      returnType =
        TList(
          TTuple(TCustomType(NR.ok (PT2DT.PackageOp.typeName ()), []), TString, [])
        )
      description =
        "Find the operations that give these names their committed bindings on main, including their timestamps."
      fn =
        (function
        | _, _, _, [| DList(_, locations) |] ->
          uply {
            let! bindings =
              LibDB.Queries.committedBindingOperationsFor (
                List.map PT2DT.PackageLocation.fromDT locations
              )
            return
              bindings
              |> Map.toList
              |> List.map (fun (_, (op, stamp)) ->
                DTuple(PT2DT.PackageOp.toDT op, DString stamp, []))
              |> Dval.list (
                KTTuple(
                  VT.known (KTCustomType(PT2DT.PackageOp.typeName (), [])),
                  VT.known KTString,
                  []
                )
              )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    // Execute propagation of an update to all dependents
    { name = fn "pmPropagate" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "sourceLocation"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "Location of the updated item"
          Param.make
            "sourceItemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, value, trait, impl or test"
          Param.make
            "fromSourceHashes"
            (TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])))
            "All deprecated hashes at this location"
          Param.make
            "toSourceHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "New hash of the source item" ]
      returnType =
        TypeReference.result
          (TList(TCustomType(NR.ok (PT2DT.PropagateRepoint.typeName ()), [])))
          TString
      description =
        "Propagates an update to all dependents, creating new versions with updated references. Returns the repoints it made."
      fn =
        (function
        | _,
          _,
          _,
          [| DUuid branchId
             sourceLocation
             sourceItemKindDval
             DList(_, fromSourceHashDvals)
             toSourceHashDval |] ->
          uply {
            let sourceLocation = PT2DT.PackageLocation.fromDT sourceLocation
            let sourceItemKind = PT2DT.ItemKind.fromDT sourceItemKindDval
            let fromSourceHashes = fromSourceHashDvals |> List.map PT2DT.Hash.fromDT

            // The branch this propagation runs on, from the caller. Every Dark call site passes
            // `state.currentBranchId`, which is what keeps a branch's cascade off main.
            let branch = branchOfParam branchId

            let! result =
              LibDB.Propagation.propagate
                branch
                sourceLocation
                sourceItemKind
                fromSourceHashes
                (PT2DT.Hash.fromDT toSourceHashDval)

            match result with
            | Ok(Some(propagationResult, ops)) ->
              // The repoints are the answer either way; where the OPS land is what differs.
              let repointsDval =
                propagationResult.repoints
                |> List.map PT2DT.PropagateRepoint.toDT
                |> Dval.list (PT2DT.PropagateRepoint.knownType ())

              do! persistPropagation branch ops
              return Dval.resultOk repointListKT KTString repointsDval
            | Ok None ->
              // No dependents: nothing moved, which is a result, not an error.
              return
                Dval.resultOk
                  repointListKT
                  KTString
                  (Dval.list (PT2DT.PropagateRepoint.knownType ()) [])
            | Error errMsg ->
              return Dval.resultError repointListKT KTString (DString errMsg)
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects =
        set
          [ Effect.PackageRead
            Effect.PackageWrite
            // Generates a revert id via Guid.NewGuid (non-deterministic).
            Effect.Random ]
      deprecated = NotDeprecated }

    // Deprecation info used by ls/tree/search in a single DB round-trip:
    // (allDeprecatedHashes, hiddenHashes). Hidden = deprecated AND has no
    // live direct caller (a caller is "live" iff it's not itself deprecated).
    { name = fn "pmGetDeprecationSets" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TUuid
            "the branch to read as; main is `SCM.Branch.mainBranchId`" ]
      returnType =
        TTuple(
          TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])),
          TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])),
          []
        )
      description =
        "Tuple (allDeprecated, hidden) of package hashes, as <param branchId> sees them: "
        + "main's deprecations with the branch chain's own Deprecate/Undeprecate ops "
        + "layered over them. Deprecation keys on CONTENT, so it applies to a hash "
        + "wherever that hash is named. `hidden` is a subset of `allDeprecated`: the "
        + "deprecated items with no live direct caller."
      fn =
        (function
        | _, _, _, [| DUuid branchId |] ->
          uply {
            let! sets = LibDB.Queries.getDeprecationSetsFor (PT.BranchId.Id branchId)
            let hashListDval (hashes : Set<PT.Hash>) =
              hashes
              |> Set.toList
              |> List.map PT2DT.Hash.toDT
              |> Dval.list (PT2DT.Hash.knownType ())
            return
              DTuple(hashListDval sets.allDeprecated, hashListDval sets.hidden, [])
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Some (kind, message) returns the Dark-side DeprecationKind enum, so callers
    // format it however they want rather than parsing a string tag.
    { name = fn "pmGetCurrentDeprecation" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TUuid
            "the branch to read as; main is `SCM.Branch.mainBranchId`"
          Param.make "itemHash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) ""
          Param.make
            "itemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, value, trait, impl or test" ]
      returnType =
        TypeReference.option (
          TTuple(
            TCustomType(NR.ok (PT2DT.DeprecationKind.typeName ()), []),
            TString,
            []
          )
        )
      description =
        "Current deprecation state for an item, by hash, as <param branchId> sees it: main's "
        + "state with the branch chain's own Deprecate/Undeprecate ops layered over it. "
        + "Deprecation keys on CONTENT, so two names holding the same bytes are deprecated "
        + "together. None = not deprecated; Some (kind, message) otherwise."
      fn =
        (function
        | _, _, _, [| DUuid branchId; hashDval; itemKindDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let itemKind = PT2DT.ItemKind.fromDT itemKindDval
            let! result =
              LibDB.Queries.getCurrentDeprecationFor
                (PT.BranchId.Id branchId)
                hash
                itemKind
            let tupleKT =
              KTTuple(
                VT.known (PT2DT.DeprecationKind.knownType ()),
                VT.known KTString,
                []
              )
            match result with
            | None -> return Dval.optionNone tupleKT
            | Some(kind, message) ->
              return
                Dval.optionSome
                  tupleKT
                  (DTuple(PT2DT.DeprecationKind.toDT kind, DString message, []))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "pmDocsAt" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "" ]
      returnType =
        TList(
          TTuple(TCustomType(NR.ok (PT2DT.DocPart.typeName ()), []), TString, [])
        )
      description =
        "Every doc this NAME has of its own, by part. Empty for a name that has never had one "
        + "edited, which is most of them: the text a reader sees then comes from the declaration."
      fn =
        (function
        | _, _, _, [| DUuid branchId; location |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! docs = LibDB.Docs.docsAt (branchOfParam branchId) location

            let partKT = VT.known (PT2DT.DocPart.knownType ())

            return
              docs
              |> List.map (fun (part, text) ->
                DTuple(PT2DT.DocPart.toDT part, DString text, []))
              |> fun rows -> DList(VT.tuple partKT VT.string [], rows)
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
