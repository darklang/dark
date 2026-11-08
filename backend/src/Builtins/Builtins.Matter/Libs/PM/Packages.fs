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

/// Persist a computed cascade on its branch, marked as PROPAGATED, not authored: it's the only
/// point at which the difference is known.
let persistPropagation
  (branch : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Ply<unit> =
  uply {
    do! LibDB.Queries.ensureAuthoringClockIsCurrent ()
    let! names = LibDB.ReferenceNames.captureForStorage branch ops
    let ops = ops @ names
    let! _ = LibDB.Inserts.authorOn branch "propagation" ops
    return ()
  }


/// The `kind` parameter of `pmFind`, `pmLocations` and `pmGet`. A function, not a value: its type
/// is a package ref, which is not resolved yet when this module initializes.
let private kindParam () : Param =
  Param.make
    "kind"
    (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
    "fn, type, value, trait, impl or test"


/// What <param location> resolves to on <param branch>, for an item of <param kind>.
let findOn
  (branch : PT.BranchId)
  (kind : PT.ItemKind)
  (location : PT.PackageLocation)
  : Ply<Option<PT.Hash>> =
  match kind with
  | PT.ItemKind.Type -> PMPT.Type.find branch location
  | PT.ItemKind.Value -> PMPT.Value.find branch location
  | PT.ItemKind.Fn -> PMPT.Fn.find branch location
  | PT.ItemKind.Trait -> PMPT.Trait.find branch location
  | PT.ItemKind.TraitImpl -> PMPT.TraitImpl.find branch location
  | PT.ItemKind.Test -> PMPT.Test.find branch location


/// Bind <param location> to content already in the store, as a `Decision`/`Override` authored onto
/// the branch's draft with source 'resolution'. Refuses content that is not there,
/// since a dangling binding fails at CALL time, far from the cause.
let rebind
  (branch : PT.BranchId)
  (loc : PT.PackageLocation)
  (kind : PT.ItemKind)
  (hash : PT.Hash)
  : Ply<Result<unit, string>> =
  uply {
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
      return Error "no item with that hash in the store"
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

      // A rebind a person forced.
      let! _ = LibDB.Inserts.authorOn branch "resolution" ops

      return Ok()
  }


/// Repoint every caller of one of <param fromHashes> at <param toHash>, on <param branch>, persist
/// the repoints, and say which names moved. Then reassert any repointed binding that replaying an
/// existing op did not actually move: a repoint that dedupes into an op the log already holds folds
/// nothing, and the name would be left where it was.
let repoint
  (branch : PT.BranchId)
  (location : PT.PackageLocation)
  (kind : PT.ItemKind)
  (fromHashes : List<PT.Hash>)
  (toHash : PT.Hash)
  : Ply<Result<List<PT.PropagateRepoint>, string>> =
  uply {
    let! result = LibDB.Propagation.propagate branch location kind fromHashes toHash
    match result with
    | Error e -> return Error e
    | Ok None -> return Ok []
    | Ok(Some(propagationResult, ops)) ->
      do! persistPropagation branch ops
      let moved = propagationResult.repoints
      do!
        LibDB.Inserts.carryDependencyNames
          (fromHashes |> List.map (fun h -> (location, kind, h, toHash)))
          moved
      let! failure =
        moved
        |> Ply.List.foldSequentially
          (fun failure r ->
            match failure with
            | Some _ -> uply { return failure }
            | None ->
              uply {
                let! now = findOn branch r.toRef.kind r.location
                if now <> Some r.toRef.hash then
                  let! rebound = rebind branch r.location r.toRef.kind r.toRef.hash
                  match rebound with
                  | Error e -> return Some e
                  | Ok() -> return None
                else
                  return None
              })
          None
      match failure with
      | Some e -> return Error e
      | None -> return Ok moved
  }


/// `pmLocations`: every name a hash of <param kind> is bound to, seen from <param branchId>.
///
/// Two answers, in order, because a hash with no live name still has to render as something better
/// than `<hash:d6f972b3>`:
///
/// 1. the names the branch binds it at, main's first;
/// 2. a name it USED to have. Reached when viewing a superseded version, whose live name has moved
///    on to the newer one.
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

          // The caller's own manager first: it knows a script's declarations, which the store does
          // not, and main's names come before a branch's. Then the names the branch adds.
          let fromPm, onBranch, everNamed =
            match kind with
            | PT.ItemKind.Type ->
              pm.getTypeLocations,
              PMPT.Type.getLocations branch,
              PMPT.Type.getLocationsEverNamed branch
            | PT.ItemKind.Value ->
              pm.getValueLocations,
              PMPT.Value.getLocations branch,
              PMPT.Value.getLocationsEverNamed branch
            | PT.ItemKind.Fn ->
              pm.getFnLocations,
              PMPT.Fn.getLocations branch,
              PMPT.Fn.getLocationsEverNamed branch
            | PT.ItemKind.Trait ->
              pm.getTraitLocations,
              PMPT.Trait.getLocations branch,
              PMPT.Trait.getLocationsEverNamed branch
            | PT.ItemKind.TraitImpl ->
              pm.getTraitImplLocations,
              PMPT.TraitImpl.getLocations branch,
              PMPT.TraitImpl.getLocationsEverNamed branch
            | PT.ItemKind.Test ->
              pm.getTestLocations,
              PMPT.Test.getLocations branch,
              PMPT.Test.getLocationsEverNamed branch

          let! first = fromPm hash
          let! more = if branch.IsMain then Ply [] else onBranch hash
          let named =
            first @ (more |> List.filter (fun l -> not (List.contains l first)))
          let! result = if List.isEmpty named then everNamed hash else Ply named

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
/// Read in the branch's view through the direct finders (`PMPT.{Type,Value,Fn,...}.find`), which have
/// no cache to go stale across a long-lived process (the test harness shares one).
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
          let! result = findOn branch kind location
          return
            result
            |> Option.map PT2DT.Hash.toDT
            |> Dval.option (PT2DT.Hash.knownType ())
        }
      | _ -> incorrectArgs ())
    previewable = Impure
    callEffects = set [ Effect.PackageRead ]
    deprecated = NotDeprecated }


/// `pmBindingsNamed`: every binding <param branchId> sees whose name is in <param names>, and the
/// kind of item each holds. `pmFind` for a set of names in one read of the store.
let private bindingsNamedFn () : BuiltInFn =
  { name = fn "pmBindingsNamed" 0
    typeParams = []
    parameters = [ branchParam; Param.make "names" (TList TString) "" ]
    returnType =
      TList(
        TTuple(
          TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []),
          TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []),
          []
        )
      )
    description =
      "Every binding whose name is one of <param names>, with the kind of item it holds"
    fn =
      (function
      | _, _, _, [| DUuid branchId; DList(_, Items names) |] ->
        uply {
          let names =
            names
            |> List.choose (function
              | DString n -> Some n
              | _ -> None)
          let! found = PMPT.bindingsNamed (branchOfParam branchId) names
          return
            found
            |> List.map (fun (location, kind) ->
              DTuple(
                PT2DT.PackageLocation.toDT location,
                PT2DT.ItemKind.toDT kind,
                []
              ))
            |> Dval.list (
              KTTuple(
                ValueType.Known(PT2DT.PackageLocation.knownType ()),
                ValueType.Known(PT2DT.ItemKind.knownType ()),
                []
              )
            )
        }
      | _ -> incorrectArgs ())
    previewable = Impure
    callEffects = set [ Effect.PackageRead ]
    deprecated = NotDeprecated }


/// An item read by hash, as the Dark-side type <param typeName> names.
let private foundItem
  (typeName : FQTypeName.FQTypeName)
  (toDT : 'a -> Dval)
  (item : Option<'a>)
  : Dval =
  item |> Option.map toDT |> Dval.option (KTCustomType(typeName, []))


/// `pmGet`: hash -> the stored item of <param kind>, decoded to its Dark-side type.
let private getFn (pm : PT.PackageManager) : BuiltInFn =
  { name = fn "pmGet" 0
    typeParams = [ "a" ]
    parameters =
      [ kindParam ()
        Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
    // The kind decides the item's type; each Dark wrapper names it.
    returnType = TypeReference.option (TVariable "a")
    description = "Returns a package item of the given kind, by hash, if it exists"
    fn =
      (function
      | _, _, _, [| kindDval; hashDval |] ->
        uply {
          let hash = PT2DT.Hash.fromDT hashDval
          match PT2DT.ItemKind.fromDT kindDval with
          | PT.ItemKind.Type ->
            let! item = pm.getType hash
            return
              foundItem (PT2DT.PackageType.typeName ()) PT2DT.PackageType.toDT item
          | PT.ItemKind.Value ->
            let! item = pm.getValue hash
            return
              foundItem (PT2DT.PackageValue.typeName ()) PT2DT.PackageValue.toDT item
          | PT.ItemKind.Fn ->
            let! item = pm.getFn hash
            return foundItem (PT2DT.PackageFn.typeName ()) PT2DT.PackageFn.toDT item
          | PT.ItemKind.Trait ->
            let! item = pm.getTrait hash
            return foundItem (PT2DT.Trait.typeName ()) PT2DT.Trait.toDT item
          | PT.ItemKind.TraitImpl ->
            let! item = pm.getTraitImpl hash
            return foundItem (PT2DT.TraitImpl.typeName ()) PT2DT.TraitImpl.toDT item
          | PT.ItemKind.Test ->
            let! item = pm.getTest hash
            return
              foundItem (PT2DT.PackageTest.typeName ()) PT2DT.PackageTest.toDT item
        }
      | _ -> incorrectArgs ())
    previewable = Impure
    callEffects = set [ Effect.PackageRead ]
    deprecated = NotDeprecated }


/// Strings as a Dark `List<String>`.
let private toDList (xs : List<string>) : Dval =
  xs |> List.map DString |> Dval.list KTString


// TODO: review/reconsider the accessibility of these fns
let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ // location -> hash, hash -> every location, and hash -> item, for any item kind
    findFn ()
    bindingsNamedFn ()
    locationsFn pm
    getFn pm

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
                Items(valueIds |> List.map RT2DT.Hash.toDT)
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
                Items(candidates |> List.map RT2DT.ImplCandidate.toDT)
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


    // The same fn with its body left out, before it becomes a Dark value: for a caller that reads
    // only the signature, which is how the at-rest checker loads a declaration's dependencies.
    { name = fn "pmGetFnSignature" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.PackageFn.typeName ()), []))
      description =
        "Returns a package function, without its body, by hash, if it exists"
      fn =
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let! item = pm.getFn (PT2DT.Hash.fromDT hashDval)
            return
              foundItem
                (PT2DT.PackageFn.typeName ())
                (fun fn -> PT2DT.PackageFn.toDT { fn with body = PT.EUnit 0UL })
                item
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // A package fn by hash, for a host that follows edits (`LanguageTools.Live`). A name, which eval
    // resolves, is whatever is bound NOW; a host that wants to keep running the last version that
    // passed its checks while a broken edit sits at the name needs to call by hash. Every version is
    // still in the store, so this is a lookup, not a resurrection.
    { name = fn "applicableByHash" 0
      typeParams = []
      parameters =
        [ Param.make
            "hash"
            TString
            "a package fn's content hash, as `Hash` carries it"
          Param.make "name" TString "The logical entry point being followed" ]
      // Any arity: the function is only known once the store answers.
      returnType = TypeReference.result (TVariable "f") TString
      description =
        "Resolves a package function by its content <param hash> to a callable value, "
        + "as a Result -- Error if no function has that hash."
      fn =
        (function
        | _, _, _, [| DString hash; DString entryName |] ->
          uply {
            let okKT = KTFn(NEList.singleton ValueType.Unknown, ValueType.Unknown)
            let err (msg : string) = Dval.resultError okKT KTString (DString msg)
            match! pm.getFn (PT.Hash hash) with
            | Some _ ->
              let namedFn : ApplicableNamedFn =
                { name = FQFnName.Package(Hash hash)
                  typeSymbolTable = TST.empty
                  typeArgs = []
                  // Whoever applies this supplies the frame access.
                  approvalName = Some entryName
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


    // Apply a fn a host resolved live, and get its failure back as a value. A host loop that shows
    // the last good frame with the error under it cannot let an RTE from user code unwind the loop;
    // this is the one place an RTE from an applied fn becomes a `Result`.
    //
    // The fn is the APPROVAL ROOT of the call, as a router handed to `serve` is: the host chose it
    // by name. A new version needs review before it can use the named grant.
    // The caller's restrictions and explicit instance denies still apply. A lambda has no root
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
            // (`LanguageTools.Live.call`), so `applyCatching` catches both. Running out of memory is
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
            // Through the branch's view, so a branch's items show up in ls/view/tree/search, not just
            // eval.
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
            // Through the branch's view, not a scope to walk up. Straight to the store rather than
            // through `ptForBranch`'s `search`: that one reads and decodes every matched item's
            // definition and patches its docs, and this answer is names and hashes, which are the same
            // either way.
            let! results =
              PMPT.searchNamesAndHashes (branchOfParam branchId) searchQuery


            let pairKT =
              KTTuple(VT.string, ValueType.Known(PT2DT.Hash.knownType ()), [])

            let pairs (items : List<string * PT.Hash>) =
              items
              |> List.map (fun (name, hash) ->
                DTuple(DString name, PT2DT.Hash.toDT hash, []))
              |> Dval.list pairKT

            return
              DTuple(
                toDList results.submodules,
                pairs results.types,
                [ pairs results.values
                  pairs results.fns
                  pairs results.traits
                  pairs results.impls
                  pairs results.tests ]
              )
          }
        | _ -> incorrectArgs ()
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Hands the compiled instruction stream to Dark so it can be rendered there
    // (`PrettyPrinter.RuntimeTypes.instructions`). Data only: no formatting happens in F#, because a
    // disassembly listing is exactly the kind of thing Dark should own. The PT side needs no equivalent
    // builtin -- `pmGet` already returns the tree.
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
            match! rebind (branchOfParam branchGuid) loc kind hash with
            | Ok() -> return Dval.resultOk KTUnit KTString DUnit
            | Error e -> return DString e |> Dval.resultError KTUnit KTString
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      // It authors an op. `Decision`/`Override` is a write whatever the name suggests, and this one
      // reaches `Inserts`.
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
        | _, _, _, [| DUuid branchGuid; DList(_, Items(targets)) |] ->
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
            let! dependents =
              LibDB.ReferenceDependencies.dependents state branch targets
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
        | _, _, _, [| DUuid branch; DList(_, Items(sources)) |] ->
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
        | _, _, _, [| DUuid branch; DList(_, Items(changed)) |] ->
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
        | _, _, _, [| DUuid branchGuid; DList(_, Items(targets)); before |] ->
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
        | _, _, _, [| DList(_, Items(snapshots)); operation |] ->
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
        | _, _, _, [| DList(_, Items(sources)) |] ->
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
        "Propagates an update to all dependents, creating new versions with updated references, and reasserts any repointed binding that replaying an existing op left where it was. Evaluates the new versions of values. Returns the repoints it made."
      fn =
        (function
        | exeState,
          _,
          _,
          [| DUuid branchId
             sourceLocation
             sourceItemKindDval
             DList(_, Items(fromSourceHashDvals))
             toSourceHashDval |] ->
          uply {
            let sourceLocation = PT2DT.PackageLocation.fromDT sourceLocation
            let sourceItemKind = PT2DT.ItemKind.fromDT sourceItemKindDval
            let fromSourceHashes = fromSourceHashDvals |> List.map PT2DT.Hash.fromDT

            // The branch this propagation runs on, from the caller. Every Dark call site passes
            // `state.currentBranchId`, which is what keeps a branch's cascade off main.
            let! result =
              repoint
                (branchOfParam branchId)
                sourceLocation
                sourceItemKind
                fromSourceHashes
                (PT2DT.Hash.fromDT toSourceHashDval)

            match result with
            | Ok repoints ->
              // A repointed value is a new version with no result yet, and a value with no
              // result reads as missing. Authoring evaluates the values it lands
              // (`PackageOps.authorOps`); the versions a cascade makes need the same, or a
              // live caller that follows the cascade fails until something else evaluates them.
              if not (List.isEmpty repoints) then
                do! PackageOps.evaluateLandedValues exeState
              let repointsDval =
                repoints
                |> List.map PT2DT.PropagateRepoint.toDT
                |> Dval.list (PT2DT.PropagateRepoint.knownType ())
              return Dval.resultOk repointListKT KTString repointsDval
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
          [ TList TString ]
        )
      description =
        "Tuple (allDeprecated, hidden, names) as <param branchId> sees them, the first two of "
        + "package hashes: "
        + "main's deprecations with the branch chain's own Deprecate/Undeprecate ops "
        + "layered over them. A deprecation is said at a name, so a hash counts here only "
        + "when no name holds it undeprecated. `hidden` is a subset of `allDeprecated`: the "
        + "deprecated items with no live direct caller. `names` is every deprecated name, as "
        + "`Owner.Modules.name#hash`."
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
              DTuple(
                hashListDval sets.allDeprecated,
                hashListDval sets.hidden,
                [ sets.names |> Set.toList |> List.map DString |> Dval.list KTString ]
              )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "pmImplPathsBelow" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make "modulePath" (TList TString) "the module to look below" ]
      returnType = TList TString
      description =
        "The full path of every impl at or below <param modulePath>, such as "
        + "`Darklang.Json.ParseError.Display`, without reading any definition."
      fn =
        (function
        | _, _, _, [| DUuid branchId; DList(_, Items(modulePath)) |] ->
          uply {
            let modulePath =
              modulePath
              |> List.map (function
                | DString s -> s
                | _ -> incorrectArgs ())
            let! paths = PMPT.implPathsBelow (branchOfParam branchId) modulePath
            return toDList paths
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "pmCallerCounts" 0
      typeParams = []
      parameters =
        [ Param.make
            "hashes"
            (TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])))
            "the items to count callers of" ]
      returnType =
        TList(TTuple(TCustomType(NR.ok (PT2DT.Hash.typeName ()), []), TInt64, []))
      description =
        "How many items call each of <param hashes>: (hash, callers) pairs, leaving out "
        + "the hashes nothing calls. A caller counts when a name holds it now and it is "
        + "not a test or in a `Tests` module. Search ranks by it."
      fn =
        (function
        | _, _, _, [| DList(_, Items(hashes)) |] ->
          uply {
            let! counts =
              hashes |> List.map PT2DT.Hash.fromDT |> LibDB.Queries.callerCounts
            return
              counts
              |> List.map (fun (hash, n) ->
                DTuple(PT2DT.Hash.toDT hash, DInt64 n, []))
              |> Dval.list (
                KTTuple(VT.known (PT2DT.Hash.knownType ()), VT.int64, [])
              )
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
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "the name the item is deprecated at"
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
        "Current deprecation state for an item at a name, as <param branchId> sees it: main's "
        + "state with the branch chain's own Deprecate/Undeprecate ops layered over it. "
        + "Deprecation is said at a name, so another name holding the same content has its "
        + "own answer. None = not deprecated; Some (kind, message) otherwise."
      fn =
        (function
        | _, _, _, [| DUuid branchId; locationDval; hashDval; itemKindDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let itemKind = PT2DT.ItemKind.fromDT itemKindDval
            let! result =
              LibDB.Queries.getCurrentDeprecationFor
                (PT.BranchId.Id branchId)
                (PT2DT.PackageLocation.fromDT locationDval)
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
              |> fun rows -> DList(VT.tuple partKT VT.string [], Items(rows))
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
