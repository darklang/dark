/// Builtin functions for working with the dev-time Package Manager
///   (_not_ the run-time PM)
///
/// A ProgramTypes Package Manager is taken as a parameter, so that:
/// - the Cloud runtime can use the Cloud PM (just accessing DB directly)
/// - the CLI runtime can use the HTTP-bound PM
///   (which calls upon endpoints in the dark-packages canvas)
///
/// Previously, the non-Cloud package manager was supported by Dark package fns that
/// made HTTPClient calls to the Cloud-hosted PM, but: since Darklang doesn't really
/// have a caching mechanism, it made more sense to have the HTTP-access be inside
/// of builtin functions.
///
/// At run-time, we use the PM to support the parser flow, and pretty-printing (i.e.
/// to grab the name of a package type). We do those operations quite a lot, so it's
/// important that the operations are reasonably fast, which we can't curently do
/// without some sort of such caching.
module Builtins.Matter.Libs.PM.Packages

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module D = LibExecution.DvalDecoder
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PackageRefs = LibExecution.PackageRefs
module C2DT = LibExecution.CommonToDarkTypes
module VT = LibExecution.ValueType
module NR = LibExecution.RuntimeTypes.NameResolution
module RTPM = LibDB.RuntimeTypes
module PMPT = LibDB.ProgramTypes
module Branches = LibDB.Branches
module Execution = LibExecution.Execution

let statsTypeName () = FQTypeName.fqPackage (PackageRefs.Type.DarkPackages.stats ())

let private repointListKT =
  KTList(ValueType.Known(PT2DT.PropagateRepoint.knownType ()))


// TODO: review/reconsider the accessibility of these fns
let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "pmGetStats" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(NR.ok (statsTypeName ()), [])
      description = "Returns high-level stats of what's in the Package Manager"
      fn =
        function
        | _, _, _, [| DUnit |] ->
          uply {
            let! stats = LibDB.Stats.get ()

            return
              DRecord(
                statsTypeName (),
                statsTypeName (),
                [],
                [ "types", Dval.int (bigint stats.types)
                  "values", Dval.int (bigint stats.values)
                  "fns", Dval.int (bigint stats.fns) ]
                |> Map
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // types
    { name = fn "pmFindType" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Tries to find a package type, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [| DUuid branchId; location |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            // Do a fresh lookup using the branchId to get the current branch chain.
            // This ensures newly-created types on the branch are visible.
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Type.find branchChain location
            return
              result
              |> Option.map PT2DT.Hash.toDT
              |> Dval.option (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetType" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.PackageType.typeName ()), []))
      description = "Returns a package type, by hash, if it exists"
      fn =
        let optType = KTCustomType((PT2DT.PackageType.typeName ()), [])
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getType hash
            return result |> Option.map PT2DT.PackageType.toDT |> Dval.option optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // values
    { name = fn "pmFindValue" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Tries to find a package value, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [| DUuid branchId; location |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Value.find branchChain location
            return
              result
              |> Option.map PT2DT.Hash.toDT
              |> Dval.option (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetValue" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (
          TCustomType(NR.ok (PT2DT.PackageValue.typeName ()), [])
        )
      description = "Returns a package value, by hash, if it exists"
      fn =
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getValue hash
            return
              result
              |> Option.map PT2DT.PackageValue.toDT
              |> Dval.option (KTCustomType((PT2DT.PackageValue.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Find all value IDs that have a specific ValueType
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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Functions
    { name = fn "pmFindFn" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Tries to find a package function, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [| DUuid branchId; location |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Fn.find branchChain location
            return
              result
              |> Option.map PT2DT.Hash.toDT
              |> Dval.option (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetFn" 0
      typeParams = []
      parameters =
        [ Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (PT2DT.PackageFn.typeName ()), []))
      description = "Returns a package function, by hash, if it exists"
      fn =
        (function
        | _, _, _, [| hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getFn hash
            return
              result
              |> Option.map PT2DT.PackageFn.toDT
              |> Dval.option (KTCustomType((PT2DT.PackageFn.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Resolve a package fn's dotted name to a callable value (Applicable), so a name that only exists as a
    // STRING (a CLI arg) can be passed as a function without eval'ing a source string. This is what lets
    // `dark serve` hand a router to `Stdlib.HttpServer.serve` directly (no `cliEvaluateExpression`).
    // CLEANUP(applicableByName): `dark eval` already resolves a dotted name to a runnable value, so this builtin
    // can eventually fold into eval and be deleted (one fewer builtin — the fewer-builtins rule). ~1hr; the one
    // wrinkle is keeping an expression that produces nothing printing nothing. Left for a tidy pass.
    { name = fn "applicableByName" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "the branch to resolve on"
          Param.make
            "name"
            TString
            "dotted package fn name, e.g. Darklang.Sync.Server.router" ]
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
                match! pm.findFn (branchId, location) with
                | Some fqPkg ->
                  let rtName = FQFnName.Package(PT2RT.FQFnName.Package.toRT fqPkg)
                  let namedFn : ApplicableNamedFn =
                    { name = rtName
                      typeSymbolTable = TST.empty
                      typeArgs = []
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSearch" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
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
            let! branchChain = Branches.getBranchChain branchId
            let! results = PMPT.search branchChain searchQuery
            return PT2DT.Search.SearchResults.toDT results
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
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
            let! branchChain = Branches.getBranchChain branchId
            let! found = PMPT.ownerHasItems branchChain owner
            return DBool found
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSearchNames" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
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
            let! branchChain = Branches.getBranchChain branchId
            let! results = PMPT.search branchChain searchQuery

            // Mirrors `Query.getDirectSubmodules`: drop the current path, keep the next segment, dedupe,
            // sort. Reduced here rather than in Dark because at the root of a large store this is hundreds
            // of module paths collapsing to a handful of names, and the search already has them all.
            let depth = List.length searchQuery.currentModule
            // `List.skip` throws past the end; Dark's `List.drop` yields []. Match Dark.
            let rec dropN n (xs : List<string>) =
              if n <= 0 then
                xs
              else
                match xs with
                | [] -> []
                | _ :: rest -> dropN (n - 1) rest
            let submodules =
              results.submodules
              |> List.choose (fun modulePath ->
                match dropN depth modulePath with
                | next :: _ when next <> "" -> Some next
                | _ -> None)
              |> List.distinct
              |> List.sort

            let names (locations : List<PT.LocatedItem<'a>>) =
              locations |> List.map (fun i -> i.location.name)

            let toDList (xs : List<string>) =
              xs |> List.map DString |> Dval.list KTString

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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmSearchNamesAndHashes" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
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
            let! branchChain = Branches.getBranchChain branchId
            let! results = PMPT.search branchChain searchQuery

            let depth = List.length searchQuery.currentModule
            // `List.skip` throws past the end; Dark's `List.drop` yields []. Match Dark.
            let rec dropN n (xs : List<string>) =
              if n <= 0 then
                xs
              else
                match xs with
                | [] -> []
                | _ :: rest -> dropN (n - 1) rest
            let submodules =
              results.submodules
              |> List.choose (fun modulePath ->
                match dropN depth modulePath with
                | next :: _ when next <> "" -> Some next
                | _ -> None)
              |> List.distinct
              |> List.sort

            let pairKT =
              KTTuple(VT.string, ValueType.Known(PT2DT.Hash.knownType ()), [])

            let toDList (xs : List<string>) =
              xs |> List.map DString |> Dval.list KTString

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
      capabilities = LibExecution.Capabilities.noCaps
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByType" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
          Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
      description = "Returns all locations of a package type by its hash"
      fn =
        (function
        | _, _, _, [| DUuid branchId; hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getTypeLocations branchId hash
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByValue" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
          Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
      description = "Returns all locations of a package value by its hash"
      fn =
        (function
        | _, _, _, [| DUuid branchId; hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getValueLocations branchId hash
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByFn" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
          Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
      description = "Returns all locations of a package function by its hash"
      fn =
        (function
        | _, _, _, [| DUuid branchId; hashDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let! result = pm.getFnLocations branchId hash
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Get ALL previous (deprecated) hashes at a location - used for propagation
    { name = fn "pmGetAllPreviousHashes" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
          Param.make
            "location"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            ""
          Param.make
            "itemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, or value" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
      description =
        "Returns all hashes that have ever been at a location across the branch chain"
      fn =
        (function
        | _, _, _, [| DUuid branchId; location; itemKindDval |] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let itemKind = PT2DT.ItemKind.fromDT itemKindDval
            let modulesStr = location.modules |> String.concat "."
            let! branchChain = Branches.getBranchChain branchId
            let! result =
              LibDB.Queries.getAllPreviousHashes
                branchChain
                location.owner
                modulesStr
                location.name
                (itemKind.toString ())
            return
              result
              |> List.map PT2DT.Hash.toDT
              |> Dval.list (PT2DT.Hash.knownType ())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Execute propagation of an update to all dependents
    { name = fn "pmPropagate" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
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
          (TTuple(
            TUuid,
            TList(TCustomType(NR.ok (PT2DT.PropagateRepoint.typeName ()), [])),
            []
          ))
          TString
      description =
        "Propagates an update to all dependents, creating new versions with updated references. Returns (propagationId, repoints)."
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

            let! result =
              LibDB.Propagation.propagate
                branchId
                sourceLocation
                sourceItemKind
                fromSourceHashes
                (PT2DT.Hash.fromDT toSourceHashDval)

            let tupleKT =
              KTTuple(ValueType.Known KTUuid, ValueType.Known repointListKT, [])

            match result with
            | Ok(Some(propagationResult, ops)) ->
              let! _ = LibDB.Inserts.insertAndApplyOps branchId None ops

              let repointsDval =
                propagationResult.repoints
                |> List.map PT2DT.PropagateRepoint.toDT
                |> Dval.list (PT2DT.PropagateRepoint.knownType ())

              let resultTuple =
                DTuple(DUuid propagationResult.propagationId, repointsDval, [])
              return Dval.resultOk tupleKT KTString resultTuple
            | Ok None ->
              // No dependents found - return empty result with empty UUID
              let resultTuple =
                DTuple(
                  DUuid System.Guid.Empty,
                  Dval.list (PT2DT.PropagateRepoint.knownType ()) [],
                  []
                )
              return Dval.resultOk tupleKT KTString resultTuple
            | Error errMsg ->
              return Dval.resultError tupleKT KTString (DString errMsg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Atomic undo: revert repoints + restore source version in one operation
    // Supports incremental undo: targetHash specifies which version to restore.
    // If targetHash is None, finds and restores the committed version (final step).
    { name = fn "pmAtomicUndo" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
          Param.make
            "revertableRepoints"
            (TList(TCustomType(NR.ok (PT2DT.PropagateRepoint.typeName ()), [])))
            "Repoints to revert directly"
          Param.make
            "sourceLocation"
            (TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
            "Location of the item being undone"
          Param.make
            "sourceItemKind"
            (TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []))
            "fn, type, or value"
          Param.make "propagationIds" (TList TUuid) "Propagation IDs being reverted"
          Param.make
            "targetHash"
            (TypeReference.option (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])))
            "Hash to restore. None = find committed hash" ]
      returnType =
        TypeReference.result
          (TTuple(TUuid, TCustomType(NR.ok (PT2DT.Hash.typeName ()), []), []))
          TString
      description =
        "Atomically reverts repoints and restores a source version. "
        + "If targetHash is Some, restores that specific version. "
        + "If targetHash is None, finds and restores the committed version. "
        + "Creates a RevertPropagation op that persists in the op log. "
        + "Returns (revertId, restoredHash) on success."
      fn =
        (function
        | _,
          _,
          _,
          [| DUuid branchId
             DList(_, repoints)
             sourceLocation
             sourceItemKindDval
             DList(_, propagationIds)
             targetHashDval |] ->
          uply {
            let repoints = repoints |> List.map PT2DT.PropagateRepoint.fromDT
            let sourceLocation = PT2DT.PackageLocation.fromDT sourceLocation
            let sourceItemKind = PT2DT.ItemKind.fromDT sourceItemKindDval
            let propagationIds = propagationIds |> List.map D.uuid
            let modulesStr = sourceLocation.modules |> String.concat "."

            let tupleKT =
              KTTuple(
                ValueType.Known KTUuid,
                ValueType.Known(PT2DT.Hash.knownType ()),
                []
              )

            // Determine what to restore: an explicit target hash (whose kind is the source's — the caller
            // named it), or whatever is committed at the name. In the latter case use the kind actually
            // FOUND there, not the source's: one name holds one item, so the name may have been rebound to
            // a different kind since, and labelling the restored hash with the wrong kind builds a
            // Reference that points at nothing.
            let! restoredResult =
              match C2DT.Option.fromDT PT2DT.Hash.fromDT targetHashDval with
              | Some targetHash -> uply { return Ok(targetHash, sourceItemKind) }
              | None ->
                uply {
                  let! result =
                    LibDB.Inserts.findCommittedHash
                      branchId
                      sourceLocation.owner
                      modulesStr
                      sourceLocation.name
                  return
                    result
                    |> Result.map (fun ((hash, itemType), _) ->
                      (hash, PT.ItemKind.fromString itemType))
                }

            match restoredResult with
            | Error errMsg ->
              return Dval.resultError tupleKT KTString (DString errMsg)
            | Ok(restoredHash, restoredKind) ->
              let revertId = System.Guid.NewGuid()

              let restoredSourceRef =
                PT.Reference.fromHashAndKind (restoredHash, restoredKind)

              let revertOp =
                PT.PackageOp.RevertPropagation(
                  revertId,
                  propagationIds,
                  sourceLocation,
                  restoredSourceRef,
                  repoints
                )

              let! _ = LibDB.Inserts.insertAndApplyOps branchId None [ revertOp ]

              let resultTuple =
                DTuple(DUuid revertId, PT2DT.Hash.toDT restoredHash, [])
              return Dval.resultOk tupleKT KTString resultTuple
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Deprecation info used by ls/tree/search in a single DB round-trip:
    // (allDeprecatedHashes, hiddenHashes). Hidden = deprecated AND has no
    // live direct caller (a caller is "live" iff it's not itself deprecated).
    { name = fn "pmGetDeprecationSets" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch context" ]
      returnType =
        TTuple(
          TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])),
          TList(TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])),
          []
        )
      description =
        "Tuple (allDeprecated, hidden) of package hashes for the branch. "
        + "hidden ⊆ allDeprecated — deprecated items with no live direct caller."
      fn =
        (function
        | _, _, _, [| DUuid branchId |] ->
          uply {
            let! branchChain = Branches.getBranchChain branchId
            let! sets = LibDB.Queries.getDeprecationSets branchChain
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Current deprecation state for a package item on a branch chain.
    // None = not deprecated on this chain (or explicitly un-deprecated by a
    // child branch). Some (kind, message) returns the Dark-side
    // DeprecationKind enum so callers can format it however they want.
    { name = fn "pmGetCurrentDeprecation" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch context"
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
        "Current deprecation state for an item on the branch chain. "
        + "None = not deprecated. Some ((kind, message)) otherwise."
      fn =
        (function
        | _, _, _, [| DUuid branchId; hashDval; itemKindDval |] ->
          uply {
            let hash = PT2DT.Hash.fromDT hashDval
            let itemKind = PT2DT.ItemKind.fromDT itemKindDval
            let! branchChain = Branches.getBranchChain branchId
            let! result =
              LibDB.Queries.getCurrentDeprecation branchChain hash itemKind
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
