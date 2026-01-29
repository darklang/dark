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
        [ Param.make
            "location"
            (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package type, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! result = pm.findType location
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
        [ Param.make
            "location"
            (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package value, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! result = pm.findValue location
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


    // Functions
    { name = fn "pmFindFn" 0
      typeParams = []
      parameters =
        [ Param.make
            "location"
            (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
            "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package function, by location, and returns the ID if it exists"
      fn =
        (function
        | _, _, _, [ location ] ->
          uply {
            let location = PT2DT.PackageLocation.fromDT location
            let! result = pm.findFn location
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
        [ Param.make
            "query"
            (TCustomType(Ok PT2DT.Search.SearchQuery.typeName, []))
            "" ]
      returnType = TCustomType(Ok PT2DT.Search.SearchResults.typeName, [])
      description = "Search for packages based on the given query"
      fn =
        function
        | _, _, _, [ query as DRecord(_, _, _, _fields) ] ->
          uply {
            let searchQuery = PT2DT.Search.SearchQuery.fromDT query
            let! results = pm.search searchQuery
            return PT2DT.Search.SearchResults.toDT results
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Location lookups
    { name = fn "pmGetLocationByType" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package type by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = pm.getTypeLocation id
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
      parameters = [ Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description = "Returns the location of a package value by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = pm.getValueLocation id
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
      parameters = [ Param.make "id" TUuid "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageLocation.typeName, []))
      description =
        "Returns the location of a package function by its ID, if it exists"
      fn =
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = pm.getFnLocation id
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
