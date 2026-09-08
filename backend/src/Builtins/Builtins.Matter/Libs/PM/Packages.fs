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


/// `pmGetLocationsBy{Type,Value,Fn}`: every name a hash is bound to, seen from <param branchId>.
///
/// One shape, three item kinds. Three answers, in order, because a hash with no live name still has to
/// render as something better than `<hash:d6f972b3>`:
///
/// 1. main's `locations`, via the PM;
/// 2. the branch overlay -- a branch's SetNames never fold into `locations`, so a branch-authored item
///    has no row there at all;
/// 3. a name it USED to have, main's record first and then the branch's. Reached when viewing a
///    superseded version, whose live name has moved on to the newer one.
let private locationsByHashFn
  (builtinName : string)
  (itemWord : string)
  (kind : PT.ItemKind)
  (fromMain : PT.Hash -> Ply<List<PT.PackageLocation>>)
  (everNamedOnMain : PT.Hash -> Ply<List<PT.PackageLocation>>)
  : BuiltInFn =
  { name = fn builtinName 0
    typeParams = []
    parameters =
      [ branchParam
        Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
    returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
    description = $"Returns all locations of a package {itemWord} by its hash"
    fn =
      (function
      | _, _, _, [| DUuid branchId; hashDval |] ->
        uply {
          let branch = branchOfParam branchId
          let hash = PT2DT.Hash.fromDT hashDval

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
    sqlSpec = NotQueryable
    previewable = Impure
    callEffects = set [ Effect.PackageRead ]
    deprecated = NotDeprecated }


/// `pmFind{Type,Value,Fn}`: location -> hash, resolved against <param branchId>.
///
/// One shape, three item kinds. On a BRANCH, resolve against the overlay, so a name authored
/// earlier on the same branch resolves. On main, read the store directly: the overlay PM memoizes
/// its base, which goes stale across a long-lived process (the test harness shares one), and the
/// direct finder (`PMPT.{Type,Value,Fn}.find`) has no cache to go stale.
let private findByLocationFn
  (builtinName : string)
  (itemWord : string)
  (findOnMain : PT.PackageLocation -> Ply<Option<PT.Hash>>)
  (findOnBranch : PT.PackageManager -> PT.PackageLocation -> Ply<Option<PT.Hash>>)
  : BuiltInFn =
  { name = fn builtinName 0
    typeParams = []
    parameters =
      [ branchParam
        Param.make
          "location"
          (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
          "" ]
    returnType =
      TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
    description =
      $"Tries to find a package {itemWord}, by location, and returns the ID if it exists"
    fn =
      (function
      | _, _, _, [| DUuid branchId; location |] ->
        uply {
          let location = PT2DT.PackageLocation.fromDT location
          let! result =
            let branch = branchOfParam branchId

            if branch.IsMain then
              findOnMain location
            else
              findOnBranch (LibDB.PackageManager.ptForBranch branch) location
          return
            result
            |> Option.map PT2DT.Hash.toDT
            |> Dval.option (PT2DT.Hash.knownType ())
        }
      | _ -> incorrectArgs ())
    sqlSpec = NotQueryable
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
    sqlSpec = NotQueryable
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
  [ // types
    findByLocationFn "pmFindType" "type" PMPT.Type.find (fun branchPM loc ->
      branchPM.findType loc)

    getByHashFn
      "pmGetType"
      "type"
      PT2DT.PackageType.typeName
      pm.getType
      PT2DT.PackageType.toDT


    // values
    findByLocationFn "pmFindValue" "value" PMPT.Value.find (fun branchPM loc ->
      branchPM.findValue loc)

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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.Native ]
      deprecated = NotDeprecated }


    // Functions
    findByLocationFn "pmFindFn" "function" PMPT.Fn.find (fun branchPM loc ->
      branchPM.findFn loc)

    getByHashFn
      "pmGetFn"
      "function"
      PT2DT.PackageFn.typeName
      pm.getFn
      PT2DT.PackageFn.toDT


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
                // Through the branch overlay, like `pmSearch` and the `pmFind*` trio. The closure `pm` is
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "pmOwnerHasItems" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to look on"
          Param.make "owner" TString "Owner to test" ]
      returnType = TBool
      description =
        "Whether this owner has any listed item at all. A search would answer "
        + "the same question by scanning `locations` four times; this is an "
        + "equality seek on the owner index."
      fn =
        function
        | _, _, _, [| DUuid branchId; DString owner |] ->
          uply {
            // `locations` has no branch column (a branch is an overlay), so the seek answers about
            // MAIN. Ask the branch too when it says no, or the first item someone authors on a branch
            // does not count as having any.
            let! found = PMPT.ownerHasItems owner
            if found then
              return DBool true
            else
              return
                DBool(
                  LibDB.PackageManager.branchOwnerHasItems
                    (branchOfParam branchId)
                    owner
                )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "pmSearchNames" 0
      typeParams = []
      parameters =
        [ branchParam
          Param.make
            "query"
            (TCustomType(NR.ok (PT2DT.Search.SearchQuery.typeName ()), []))
            "" ]
      returnType =
        TTuple(TList TString, TList TString, [ TList TString; TList TString ])
      description =
        "Search, returning only names: (direct submodules, types, values, fns). "
        + "Submodules are already reduced to the direct children of the query's "
        + "module and sorted."
      fn =
        function
        | _, _, _, [| DUuid branchId; query as DRecord(_, _, _, _fields) |] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            // Through the branch overlay: a branch is its ops laid over main, not a scope to walk up.
            let pm = LibDB.PackageManager.ptForBranch (branchOfParam branchId)
            let! results = pm.search searchQuery

            let submodules = directSubmodules searchQuery results

            let names (locations : List<PT.LocatedItem<'a>>) =
              locations |> List.map (fun i -> i.location.name)

            return
              DTuple(
                toDList submodules,
                toDList (names results.types),
                [ toDList (names results.values); toDList (names results.fns) ]
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
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
        TTuple(TList TString, nameAndHash, [ nameAndHash; nameAndHash ])
      description =
        "Search, returning (direct submodules, types, values, fns) as (name, "
        + "hash) pairs. Like pmSearchNames but keeps each item's hash, which "
        + "listings need for deprecation marks."
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
                  pairs results.fns (fun (f : PT.PackageFn.PackageFn) -> f.hash) ]
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Location lookups — returns ALL locations for a hash
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    locationsByHashFn
      "pmGetLocationsByType"
      "type"
      PT.ItemKind.Type
      pm.getTypeLocations
      PMPT.Type.getLocationsEverNamed

    locationsByHashFn
      "pmGetLocationsByValue"
      "value"
      PT.ItemKind.Value
      pm.getValueLocations
      PMPT.Value.getLocationsEverNamed

    locationsByHashFn
      "pmGetLocationsByFn"
      "function"
      PT.ItemKind.Fn
      pm.getFnLocations
      PMPT.Fn.getLocationsEverNamed


    // Bind a name back to content that ALREADY exists in the store.
    //
    // This is the primitive under the propagation toggle. Propagation runs on every edit, so pinning
    // something is usually a request to UNDO a repoint that already happened -- and undoing it means
    // pointing the name back at the version it had, which is still in the store because nothing is ever
    // deleted.
    //
    // Despite the name, it emits a `Decision`/`Override` op rather than a `SetName`, and that is not an
    // implementation detail. Ops are content-addressed, so `SetName(name -> the old hash)` is
    // byte-identical to the op that first bound it: it INSERT-OR-IGNOREs and folds NOTHING, so the
    // rollback silently doesn't happen. `Override` exists precisely to say "this binding again, but now I
    // mean it", and a pin is the same act as a conflict override -- a human overruling what the machine
    // picked.
    //
    // Emitting an op rather than touching `locations` is what makes the undo sync, audit and conflict like
    // any other authoring. Deleting ops instead would do none of that, and would be unsafe besides: a pinned
    // dependent may be the only thing still referencing the version being deleted.
    { name = fn "pmSetName" 0
      typeParams = []
      parameters =
        [ Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "The name to bind"
          Param.make
            "itemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, or value"
          Param.make
            "hash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Existing content to bind the name to" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Binds a name to content already in the store, as a Decision/Override op. Errors if the content isn't there."
      fn =
        (function
        | _, _, _, [| locationDval; itemKindDval; hashDval |] ->
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
              }

            if not exists then
              return
                DString "no item with that hash in the store"
                |> Dval.resultError KTUnit KTString
            else
              let reference =
                match kind with
                | PT.ItemKind.Type -> PT.Reference.PackageType hash
                | PT.ItemKind.Fn -> PT.Reference.PackageFn hash
                | PT.ItemKind.Value -> PT.Reference.PackageValue hash

              // The decision id makes the op distinct from the SetName that originally created this
              // binding. It's provenance, never a lookup key -- the fold ignores it.
              //
              // It carries a TIMESTAMP, so pinning the same name back to the same hash twice produces two
              // ops rather than one. That's deliberate and it's where a pin differs from a conflict
              // resolution: resolving conflict #7 the same way twice is one decision stated twice, but
              // pin -> follow -> pin is genuinely three, and the third has to fold or the rollback silently
              // doesn't happen.
              let decisionId =
                let mods = String.concat "." loc.modules
                let now = System.DateTime.UtcNow.ToString("o")
                $"pin:{loc.owner}.{mods}.{loc.name}:{hash}:{now}"

              let ops =
                [ PT.PackageOp.Decision(
                    decisionId,
                    loc,
                    "pinned",
                    PT.DecisionKind.Override reference
                  ) ]

              // Same branch source authoring uses, so a rebind lands where the edit that caused it landed.
              //
              // This one takes no branch parameter and uses the process default deliberately: `pmSetName`
              // is called as part of authoring, which is already happening on whatever branch the caller is
              // on. A parameter here would be a second way to say the same thing, and the two could differ.
              let branchId = LibDB.PackageManager.currentBranchId ()

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
      sqlSpec = NotQueryable
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
            "fn, type, or value"
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

              if branch.IsMain then
                // Marked as PROPAGATED, not authored. It's the only point at which the difference is known.
                let! _ = LibDB.Inserts.insertAndApplyPropagatedOps ops
                return Dval.resultOk repointListKT KTString repointsDval
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
                    | PT.PackageOp.AddType _ -> true
                    | _ -> false)
                if not (List.isEmpty contentOps) then
                  do! LibDB.PackageOpPlayback.applyBranchContentOps contentOps
                // Refresh the process overlay so a later eval in THIS process sees the repoints.
                let! all = LibDB.Branches.loadDeltaOps branch
                LibDB.PackageManager.setBranchOverlay all

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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
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
            "fn, type, or value" ]
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
