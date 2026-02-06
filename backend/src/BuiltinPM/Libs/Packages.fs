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

module Dval = LibExecution.Dval
module D = LibExecution.DvalDecoder
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module PackageIDs = LibExecution.PackageIDs
module C2DT = LibExecution.CommonToDarkTypes
module VT = LibExecution.ValueType
module RTPM = LibPackageManager.RuntimeTypes
module PMPT = LibPackageManager.ProgramTypes
module Branches = LibPackageManager.Branches
module Execution = LibExecution.Execution

let statsTypeName = FQTypeName.fqPackage PackageIDs.Type.DarkPackages.stats


// TODO: review/reconsider the accessibility of these fns
let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "pmGetStats" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok statsTypeName, [])
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
            (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TUuid
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
            return result |> Option.map DUuid |> Dval.option KTUuid
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetType" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageType.typeName, []))
      description = "Returns a package type, by id, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageType.typeName, [])
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = pm.getType id
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
            (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package value, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Value.find branchChain location
            return result |> Option.map DUuid |> Dval.option KTUuid
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetValue" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageValue.typeName, []))
      description = "Returns a package value, by id, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = pm.getValue id
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
            (TCustomType(Ok RT2DT.ValueType.typeName, []))
            "The ValueType to search for" ]
      returnType = TList TUuid
      description =
        "Returns a list of value UUIDs that have the given ValueType. "
        + "Uses exact match on the serialized type for efficient lookup."
      fn =
        (function
        | _, _, _, [ valueTypeDval ] ->
          uply {
            let vt = RT2DT.ValueType.fromDT valueTypeDval
            let! valueIds = RTPM.Value.findByValueType vt
            return DList(VT.uuid, valueIds |> List.map DUuid)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Evaluate a package value by its UUID
    { name = fn "pmEvaluateValue" 0
      typeParams = []
      parameters =
        [ Param.make "valueId" TUuid "UUID of the package value to evaluate" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "Evaluates a package value by its UUID and returns the result. "
        + "Returns None if the value doesn't exist or fails to evaluate."
      fn =
        (function
        | exeState, _, _, [ DUuid valueId ] ->
          uply {
            let valueName = FQValueName.Package valueId
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
            (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package function, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! branchChain = Branches.getBranchChain branchId
            let! result = PMPT.Fn.find branchChain location
            return result |> Option.map DUuid |> Dval.option KTUuid
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetFn" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageFn.typeName, []))
      description = "Returns a package function, by id, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = pm.getFn id
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
            (TCustomType(Ok PT2DT.Search.SearchQuery.typeName, []))
            "" ]
      returnType = TCustomType(Ok PT2DT.Search.SearchResults.typeName, [])
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


    // Location lookups
    { name = fn "pmGetLocationByType" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid ""; Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package type by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DUuid id ] ->
          uply {
            let! result = pm.getTypeLocation branchId id
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
      parameters = [ Param.make "branchId" TUuid ""; Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package value by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DUuid id ] ->
          uply {
            let! result = pm.getValueLocation branchId id
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
      parameters = [ Param.make "branchId" TUuid ""; Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description =
        "Returns the location of a package function by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid branchId; DUuid id ] ->
          uply {
            let! result = pm.getFnLocation branchId id
            return
              result
              |> Option.map PT2DT.PackageLocation.toDT
              |> Dval.option (KTCustomType(PT2DT.PackageLocation.typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
