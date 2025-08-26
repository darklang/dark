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
module BuiltinExecution.Libs.Packages

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PackageIDs = LibExecution.PackageIDs

let statsTypeName = FQTypeName.fqPackage PackageIDs.Type.DarkPackages.stats


// CLEANUP adjust the fns to take in proper PackageType.Name, etc,
// rather than a simple String.
type GenericName = { owner : string; modules : List<string>; name : string }

let parseGenericName (name : string) : GenericName =
  match name |> String.split "." with
  | owner :: modulesAndName ->
    match List.rev modulesAndName with
    | name :: modulesInReverse ->
      { owner = owner; modules = List.rev modulesInReverse; name = name }
    | [] -> Exception.raiseInternal "Invalid name (no name)" [ "name", name ]
  | [] -> Exception.raiseInternal "Invalid name (no owner)" [ "name", name ]


let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "packageManagerGetStats" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok statsTypeName, [])
      description = "Returns high-level stats of what's in the Package Manager"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            // TODO: real #s (requires updates in RuntimeTypes and some other places, I think?)
            let fields = [ "types", DInt64 0; "values", DInt64 0; "fns", DInt64 0 ]
            return DRecord(statsTypeName, statsTypeName, [], Map fields)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // types
    { name = fn "packageManagerFindType" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package type, by name, and returns the ID if it exists"
      fn =
        let optType = KTUuid
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let n = parseGenericName name
            let name = PT.PackageType.name n.owner n.modules n.name
            match! pm.findType name with
            | Some id -> return DUuid id |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "packageManagerGetType" 0
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
    { name = fn "packageManagerFindValue" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package value, by name, and returns the ID if it exists"
      fn =
        let optType = KTUuid
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let n = parseGenericName name
            let name = PT.PackageValue.name n.owner n.modules n.name
            match! pm.findValue name with
            | Some id -> return DUuid id |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "packageManagerGetValue" 0
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
    { name = fn "packageManagerFindFn" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TUuid
      description =
        "Tries to find a package function, by name, and returns the ID if it exists"
      fn =
        let optType = KTUuid
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let n = parseGenericName name
            let name = PT.PackageFn.name n.owner n.modules n.name
            match! pm.findFn name with
            | Some id -> return DUuid id |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "packageManagerGetFn" 0
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


    { name = fn "packageManagerSearch" 0
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

            let submodules =
              results.submodules
              |> List.map (fun modules ->
                modules |> List.map (fun m -> DString m) |> Dval.list KTString)
              |> Dval.list KTString

            let types =
              results.types
              |> List.map PT2DT.PackageType.toDT
              |> Dval.list (KTCustomType(PT2DT.PackageType.typeName, []))

            let values =
              results.values
              |> List.map PT2DT.PackageValue.toDT
              |> Dval.list (KTCustomType(PT2DT.PackageValue.typeName, []))

            let fns =
              results.fns
              |> List.map PT2DT.PackageFn.toDT
              |> Dval.list (KTCustomType(PT2DT.PackageFn.typeName, []))

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
      deprecated = NotDeprecated } ]

let builtins ptPM = LibExecution.Builtin.make [] (fns ptPM)
