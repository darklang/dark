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
module BuiltinPM.Libs.Packages

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

open Fumble
open LibDB.Db

module Dval = LibExecution.Dval
module D = LibExecution.DvalDecoder
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PackageRefs = LibExecution.PackageRefs
module C2DT = LibExecution.CommonToDarkTypes
module VT = LibExecution.ValueType
module NR = LibExecution.RuntimeTypes.NameResolution
module RTPM = LibPackageManager.RuntimeTypes
module PMPT = LibPackageManager.ProgramTypes
module Branches = LibPackageManager.Branches
module Execution = LibExecution.Execution

let statsTypeName = FQTypeName.fqPackage PackageRefs.Type.DarkPackages.stats

let private repointListKT = KTList(ValueType.Known PT2DT.PropagateRepoint.knownType)


// TODO: review/reconsider the accessibility of these fns
let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "pmGetStats" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(NR.ok statsTypeName, [])
      description = "Returns high-level stats of what's in the Package Manager"
      fn =
        function
        | _, _, _, [ DUnit ] ->
          uply {
            let! stats = LibPackageManager.Stats.get ()

            return
              DRecord(
                statsTypeName,
                statsTypeName,
                [],
                [ "types", DInt64 stats.types
                  "values", DInt64 stats.values
                  "fns", DInt64 stats.fns ]
                |> Map
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // types
    { name = fn "pmFindType" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
          Param.make
            "location"
            (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TString
      description =
        "Tries to find a package type, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            // Do a fresh lookup using the branchId to get the current branch chain.
            // This ensures newly-created types on the branch are visible.
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Type.find branchChain location
            return
              result
              |> Option.map (fun (ContentHash h) -> DString h)
              |> Dval.option KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetType" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok PT2DT.PackageType.typeName, []))
      description = "Returns a package type, by hash, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageType.typeName, [])
        (function
        | _, _, _, [ DString hash ] ->
          uply {
            let! result = pm.getType (ContentHash hash)
            return result |> Option.map PT2DT.PackageType.toDT |> Dval.option optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // values
    { name = fn "pmFindValue" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
          Param.make
            "location"
            (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TString
      description =
        "Tries to find a package value, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Value.find branchChain location
            return
              result
              |> Option.map (fun (ContentHash h) -> DString h)
              |> Dval.option KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetValue" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok PT2DT.PackageValue.typeName, []))
      description = "Returns a package value, by hash, if it exists"
      fn =
        (function
        | _, _, _, [ DString hash ] ->
          uply {
            let! result = pm.getValue (ContentHash hash)
            return
              result
              |> Option.map PT2DT.PackageValue.toDT
              |> Dval.option (KTCustomType(PT2DT.PackageValue.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Find all value IDs that have a specific ValueType
    { name = fn "pmFindValuesByValueType" 0
      typeParams = []
      parameters =
        [ Param.make
            "valueType"
            (TCustomType(NR.ok RT2DT.ValueType.typeName, []))
            "The ValueType to search for" ]
      returnType = TList TString
      description =
        "Returns a list of value hashes that have the given ValueType. "
        + "Uses exact match on the serialized type for efficient lookup."
      fn =
        (function
        | _, _, _, [ valueTypeDval ] ->
          uply {
            let vt = RT2DT.ValueType.fromDT valueTypeDval
            let! valueIds = RTPM.Value.findByValueType vt
            return
              DList(
                VT.string,
                valueIds |> List.map (fun (ContentHash h) -> DString h)
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Evaluate a package value by its UUID
    { name = fn "pmEvaluateValue" 0
      typeParams = []
      parameters =
        [ Param.make "valueHash" TString "Hash of the package value to evaluate" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "Evaluates a package value by its hash and returns the result. "
        + "Returns None if the value doesn't exist or fails to evaluate."
      fn =
        (function
        | exeState, _, _, [ DString hash ] ->
          uply {
            let valueName = FQValueName.Package(ContentHash hash)
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
      deprecated = NotDeprecated }


    // Functions
    { name = fn "pmFindFn" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
          Param.make
            "location"
            (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TString
      description =
        "Tries to find a package function, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Fn.find branchChain location
            return
              result
              |> Option.map (fun (ContentHash h) -> DString h)
              |> Dval.option KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetFn" 0
      typeParams = []
      parameters = [ Param.make "hash" TString "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok PT2DT.PackageFn.typeName, []))
      description = "Returns a package function, by hash, if it exists"
      fn =
        (function
        | _, _, _, [ DString hash ] ->
          uply {
            let! result = pm.getFn (ContentHash hash)
            return
              result
              |> Option.map PT2DT.PackageFn.toDT
              |> Dval.option (KTCustomType(PT2DT.PackageFn.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmSearch" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to search on"
          Param.make
            "query"
            (TCustomType(NR.ok PT2DT.Search.SearchQuery.typeName, []))
            "" ]
      returnType = TCustomType(NR.ok PT2DT.Search.SearchResults.typeName, [])
      description = "Search for packages based on the given query."
      fn =
        function
        | _, _, _, [ DUuid branchId; query as DRecord(_, _, _, _fields) ] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            let! branchChain = Branches.getBranchChain branchId
            let! results = PMPT.search branchChain searchQuery
            return PT2DT.Search.SearchResults.toDT results
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Location lookups (singular — returns first match)
    { name = fn "pmGetLocationByType" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid ""; Param.make "hash" TString "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package type by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString hash ] ->
          uply {
            let! result = pm.getTypeLocation branchId (ContentHash hash)
            return
              result
              |> Option.map PT2DT.PackageLocation.toDT
              |> Dval.option (KTCustomType(PT2DT.PackageLocation.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationByValue" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid ""; Param.make "hash" TString "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package value by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString hash ] ->
          uply {
            let! result = pm.getValueLocation branchId (ContentHash hash)
            return
              result
              |> Option.map PT2DT.PackageLocation.toDT
              |> Dval.option (KTCustomType(PT2DT.PackageLocation.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationByFn" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid ""; Param.make "hash" TString "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
      description =
        "Returns the location of a package function by its hash, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString hash ] ->
          uply {
            let! result = pm.getFnLocation branchId (ContentHash hash)
            return
              result
              |> Option.map PT2DT.PackageLocation.toDT
              |> Dval.option (KTCustomType(PT2DT.PackageLocation.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Location lookups (plural — returns ALL locations for ambiguity detection)
    { name = fn "pmGetLocationsByType" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid ""; Param.make "hash" TString "" ]
      returnType = TList(TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
      description = "Returns all locations of a package type by its hash"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString hash ] ->
          uply {
            let! result = pm.getTypeLocations branchId (ContentHash hash)
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType(PT2DT.PackageLocation.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByValue" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid ""; Param.make "hash" TString "" ]
      returnType = TList(TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
      description = "Returns all locations of a package value by its hash"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString hash ] ->
          uply {
            let! result = pm.getValueLocations branchId (ContentHash hash)
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType(PT2DT.PackageLocation.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationsByFn" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid ""; Param.make "hash" TString "" ]
      returnType = TList(TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
      description = "Returns all locations of a package function by its hash"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DString hash ] ->
          uply {
            let! result = pm.getFnLocations branchId (ContentHash hash)
            return
              result
              |> List.map PT2DT.PackageLocation.toDT
              |> Dval.list (KTCustomType(PT2DT.PackageLocation.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Get ALL previous (deprecated) hashes at a location - used for propagation
    { name = fn "pmGetAllPreviousHashes" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
          Param.make
            "location"
            (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
            ""
          Param.make
            "itemKind"
            (TCustomType(NR.ok PT2DT.ItemKind.typeName, []))
            "fn, type, or value" ]
      returnType = TList TString
      description =
        "Returns all hashes that have ever been at a location across the branch chain"
      fn =
        (function
        | _, _, _, [ DUuid branchId; location; itemKindDval ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let itemKind = PT2DT.ItemKind.fromDT itemKindDval
            let modulesStr = location.modules |> String.concat "."
            let! branchChain = Branches.getBranchChain branchId
            let! result =
              LibPackageManager.Queries.getAllPreviousHashes
                branchChain
                location.owner
                modulesStr
                location.name
                (itemKind.toString ())
            return
              result
              |> List.map (fun (ContentHash h) -> DString h)
              |> Dval.list KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Execute propagation of an update to all dependents
    { name = fn "pmPropagate" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
          Param.make
            "sourceLocation"
            (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
            "Location of the updated item"
          Param.make
            "sourceItemKind"
            (TCustomType(NR.ok PT2DT.ItemKind.typeName, []))
            "fn, type, or value"
          Param.make
            "fromSourceHashes"
            (TList TString)
            "All deprecated hashes at this location"
          Param.make "toSourceHash" TString "New hash of the source item" ]
      returnType =
        TypeReference.result
          (TTuple(
            TUuid,
            TList(TCustomType(NR.ok PT2DT.PropagateRepoint.typeName, [])),
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
          [ DUuid branchId
            sourceLocation
            sourceItemKindDval
            DList(_, fromSourceHashes)
            DString toSourceHashStr ] ->
          uply {
            let sourceLocation = PT2DT.PackageLocation.fromDT sourceLocation
            let sourceItemKind = PT2DT.ItemKind.fromDT sourceItemKindDval
            let fromSourceHashes =
              fromSourceHashes
              |> List.map (fun dv ->
                match dv with
                | DString s -> ContentHash s
                | _ -> ContentHash "")

            let! result =
              LibPackageManager.Propagation.propagate
                branchId
                sourceLocation
                sourceItemKind
                fromSourceHashes
                (ContentHash toSourceHashStr)

            let tupleKT =
              KTTuple(ValueType.Known KTUuid, ValueType.Known repointListKT, [])

            match result with
            | Ok(Some(propagationResult, ops)) ->
              let! _ = LibPackageManager.Inserts.insertAndApplyOps branchId None ops
              ()

              let repointsDval =
                propagationResult.repoints
                |> List.map PT2DT.PropagateRepoint.toDT
                |> Dval.list PT2DT.PropagateRepoint.knownType

              let resultTuple =
                DTuple(DUuid propagationResult.propagationId, repointsDval, [])
              return Dval.resultOk tupleKT KTString resultTuple
            | Ok None ->
              // No dependents found - return empty result with empty UUID
              let resultTuple =
                DTuple(
                  DUuid System.Guid.Empty,
                  Dval.list PT2DT.PropagateRepoint.knownType [],
                  []
                )
              return Dval.resultOk tupleKT KTString resultTuple
            | Error errMsg ->
              return Dval.resultError tupleKT KTString (DString errMsg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
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
            (TList(TCustomType(NR.ok PT2DT.PropagateRepoint.typeName, [])))
            "Repoints to revert directly"
          Param.make
            "sourceLocation"
            (TCustomType(NR.ok PT2DT.PackageLocation.typeName, []))
            "Location of the item being undone"
          Param.make
            "sourceItemKind"
            (TCustomType(NR.ok PT2DT.ItemKind.typeName, []))
            "fn, type, or value"
          Param.make "propagationIds" (TList TUuid) "Propagation IDs being reverted"
          Param.make
            "targetHash"
            (TypeReference.option TString)
            "Hash to restore. None = find committed hash" ]
      returnType = TypeReference.result (TTuple(TUuid, TString, [])) TString
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
          [ DUuid branchId
            DList(_, repoints)
            sourceLocation
            sourceItemKindDval
            DList(_, propagationIds)
            targetHashDval ] ->
          uply {
            let repoints = repoints |> List.map PT2DT.PropagateRepoint.fromDT
            let sourceLocation = PT2DT.PackageLocation.fromDT sourceLocation
            let sourceItemKind = PT2DT.ItemKind.fromDT sourceItemKindDval
            let propagationIds = propagationIds |> List.map D.uuid
            let modulesStr = sourceLocation.modules |> String.concat "."

            let tupleKT =
              KTTuple(ValueType.Known KTUuid, ValueType.Known KTString, [])

            // Determine the hash to restore: explicit target or find committed
            let! restoredHashResult =
              match C2DT.Option.fromDT D.string targetHashDval with
              | Some targetHashStr -> uply { return Ok(ContentHash targetHashStr) }
              | None ->
                uply {
                  let! result =
                    LibPackageManager.Inserts.findCommittedHash
                      branchId
                      sourceLocation.owner
                      modulesStr
                      sourceLocation.name
                      (sourceItemKind.toString ())
                  return result |> Result.map fst
                }

            match restoredHashResult with
            | Error errMsg ->
              return Dval.resultError tupleKT KTString (DString errMsg)
            | Ok restoredHash ->
              let revertId = System.Guid.NewGuid()

              let revertOp =
                PT.PackageOp.RevertPropagation(
                  revertId,
                  propagationIds,
                  sourceLocation,
                  sourceItemKind,
                  restoredHash,
                  repoints
                )

              let! _ =
                LibPackageManager.Inserts.insertAndApplyOps
                  branchId
                  None
                  [ revertOp ]

              let resultTuple =
                DTuple(
                  DUuid revertId,
                  DString(ContentHash.toHexString restoredHash),
                  []
                )
              return Dval.resultOk tupleKT KTString resultTuple
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
