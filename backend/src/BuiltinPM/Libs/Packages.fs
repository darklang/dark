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
module PackageIDs = LibExecution.PackageIDs
module C2DT = LibExecution.CommonToDarkTypes

let statsTypeName = FQTypeName.fqPackage PackageIDs.Type.DarkPackages.stats


// TODO: Reconsider which of these functions should be public vs admin-only:
// - All pmGet*, pmFind*, pmSearch functions: Read-only lookups, should be public
// - pmGetStats: Read-only stats, should be public
// These are core package manager query functions needed by all users
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
            let fields =
              Map
                [ "types", DInt64 stats.types
                  "values", DInt64 stats.values
                  "fns", DInt64 stats.fns ]
            return DRecord(statsTypeName, statsTypeName, [], fields)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // types
    { name = fn "pmFindType" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "location" (TCustomType(Ok PT2DT.PackageLocation.typeName, [])) "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package type, by location, and returns the ID if it exists"
      fn =
        let optType = KTUuid
        (function
        | _, _, _, [ branchIdDval; locationDval ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdDval
            let location = PT2DT.PackageLocation.fromDT locationDval
            match! pm.findType(branchId, location) with
            | Some id -> return DUuid id |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
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
            match! pm.getType id with
            | Some f -> return f |> PT2DT.PackageType.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // values
    { name = fn "pmFindValue" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "location" (TCustomType(Ok PT2DT.PackageLocation.typeName, [])) "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package value, by location, and returns the ID if it exists"
      fn =
        let optType = KTUuid
        (function
        | _, _, _, [ branchIdDval; locationDval ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdDval
            let location = PT2DT.PackageLocation.fromDT locationDval
            match! pm.findValue(branchId, location) with
            | Some id -> return DUuid id |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
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
        let optType = KTCustomType(PT2DT.PackageValue.typeName, [])
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            match! pm.getValue id with
            | Some f ->
              return f |> PT2DT.PackageValue.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Functions
    { name = fn "pmFindFn" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "location" (TCustomType(Ok PT2DT.PackageLocation.typeName, [])) "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package function, by location, and returns the ID if it exists"
      fn =
        let optType = KTUuid
        (function
        | _, _, _, [ branchIdDval; locationDval ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdDval
            let location = PT2DT.PackageLocation.fromDT locationDval
            match! pm.findFn(branchId, location) with
            | Some id -> return DUuid id |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
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
        let optType = KTCustomType(PT2DT.PackageFn.typeName, [])
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            match! pm.getFn id with
            | Some f -> return f |> PT2DT.PackageFn.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmSearch" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make
            "query"
            (TCustomType(Ok PT2DT.Search.SearchQuery.typeName, []))
            "" ]
      returnType = TCustomType(Ok PT2DT.Search.SearchResults.typeName, [])
      description = "Search for packages based on the given query"
      fn =
        function
        | _, _, _, [ branchIdDval; query as DRecord(_, _, _, _fields) ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdDval
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query

            let! results = pm.search(branchId, searchQuery)

            let submodules =
              results.submodules
              |> List.map (fun modules ->
                modules |> List.map (fun m -> DString m) |> Dval.list KTString)
              |> Dval.list KTString

            let types =
              results.types
              |> List.map (PT2DT.LocatedItem.toDT PT2DT.PackageType.toDT)
              |> Dval.list (PT2DT.LocatedItem.knownType (KTCustomType(PT2DT.PackageType.typeName, [])))

            let values =
              results.values
              |> List.map (PT2DT.LocatedItem.toDT PT2DT.PackageValue.toDT)
              |> Dval.list (PT2DT.LocatedItem.knownType (KTCustomType(PT2DT.PackageValue.typeName, [])))

            let fns =
              results.fns
              |> List.map (PT2DT.LocatedItem.toDT PT2DT.PackageFn.toDT)
              |> Dval.list (PT2DT.LocatedItem.knownType (KTCustomType(PT2DT.PackageFn.typeName, [])))

            let resultFields =
              [ "submodules", submodules
                "types", types
                "values", values
                "fns", fns ]

            return
              DRecord(
                PT2DT.Search.SearchResults.typeName,
                PT2DT.Search.SearchResults.typeName,
                [],
                Map resultFields
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Location lookups
    // Note that there _could_ be multiple names for the same type/fn/etc
    // TODO deal with that... somehow.
    { name = fn "pmGetLocationByType" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package type by its ID, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageLocation.typeName, [])
        (function
        | _, _, _, [ branchIdDval; DUuid id ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdDval
            match! pm.getTypeLocation(branchId, id) with
            | Some location ->
              return location |> PT2DT.PackageLocation.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationByValue" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package value by its ID, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageLocation.typeName, [])
        (function
        | _, _, _, [ branchIdDval; DUuid id ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdDval
            match! pm.getValueLocation(branchId, id) with
            | Some location ->
              return location |> PT2DT.PackageLocation.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "pmGetLocationByFn" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" (TypeReference.option TUuid) ""
          Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package function by its ID, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageLocation.typeName, [])
        (function
        | _, _, _, [ branchIdDval; DUuid id ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchIdDval
            match! pm.getFnLocation(branchId, id) with
            | Some location ->
              return location |> PT2DT.PackageLocation.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
