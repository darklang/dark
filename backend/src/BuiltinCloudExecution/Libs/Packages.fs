/// Builtin functions for working with the Dark-cloud-hosted Package Manager
module BuiltinCloudExecution.Libs.Packages

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module PM = LibCloud.PackageManager

let statsTypeName = FQTypeName.fqPackage "Darklang" [ "DarkPackages" ] "Stats"


// TODO: consider doing the name-parsing in Dark instead

type GenericName = { owner : string; modules : List<string>; name : string }

let parseGenericName (name : string) : GenericName =
  match name |> String.split "." with
  | owner :: modulesAndName ->
    match List.rev modulesAndName with
    | name :: modulesInReverse ->
      { owner = owner; modules = List.rev modulesInReverse; name = name }
    | [] -> failwithf "Invalid name (no name): %s" name
  | [] -> failwithf "Invalid name (no owner): %s" name


let fns : List<BuiltInFn> =
  [ { name = fn "packageManagerGetStats" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok statsTypeName, [])
      description = "Returns high-level stats of what's in the Package Manager"
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            // TODO: real #s (requires updates in RuntimeTypes and some other places, I think?)
            let fields =
              [ "types", DInt64 0; "constants", DInt64 0; "fns", DInt64 0 ]
            return DRecord(statsTypeName, statsTypeName, [], Map fields)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // types
    { name = fn "packageManagerGetTypeByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageType.typeName, []))
      description = "Returns a package type, by name, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageType.typeName, [])
        (function
        | _, _, [ DString name ] ->
          uply {
            let n = parseGenericName name
            let name = PT.FQTypeName.package n.owner n.modules n.name
            match! PM.getType name with
            | Some t -> return t |> PT2DT.PackageType.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // constants
    { name = fn "packageManagerGetConstantByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageConstant.typeName, []))
      description = "Returns a package constant, by name, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageConstant.typeName, [])
        (function
        | _, _, [ DString name ] ->
          uply {
            let n = parseGenericName name
            let name = PT.FQConstantName.package n.owner n.modules n.name
            match! PM.getConstant name with
            | Some c ->
              return c |> PT2DT.PackageConstant.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Functions
    { name = fn "packageManagerGetFnByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageFn.typeName, []))
      description = "Returns a package function, by name, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageFn.typeName, [])
        (function
        | _, _, [ DString name ] ->
          uply {
            let n = parseGenericName name
            let name = PT.FQFnName.package n.owner n.modules n.name
            match! PM.getFn name with
            | Some f -> return f |> PT2DT.PackageFn.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "packageManagerGetFnByTlid" 0
      typeParams = []
      parameters = [ Param.make "tlid" TUInt64 "" ]
      returnType =
        TypeReference.option (TCustomType(Ok PT2DT.PackageFn.typeName, []))
      description = "Returns a package function, by name, if it exists"
      fn =
        let optType = KTCustomType(PT2DT.PackageFn.typeName, [])
        (function
        | _, _, [ DUInt64 tlid ] ->
          uply {
            match! PM.getFnByTLID tlid with
            | Some f -> return f |> PT2DT.PackageFn.toDT |> Dval.optionSome optType
            | None -> return Dval.optionNone optType
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
