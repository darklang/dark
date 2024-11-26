module BuiltinExecution.Libs.LanguageTools

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


let builtinConstant =
  FQTypeName.fqPackage PackageIDs.Type.LanguageTools.builtinConstant

let builtinFnParam =
  FQTypeName.fqPackage PackageIDs.Type.LanguageTools.builtinFnParam
let builtinFn = FQTypeName.fqPackage PackageIDs.Type.LanguageTools.builtinFn

let fns : List<BuiltInFn> =
  [ { name = fn "languageToolsAllBuiltinConstants" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok builtinConstant, []) |> TList
      description =
        "Returns a list of the Builtin constants (usually not to be accessed directly)."
      fn =
        (function
        | exeState, _, _, [ DUnit ] ->
          let consts =
            exeState.constants.builtIn
            |> Map.toList
            |> List.map (fun (name, data) ->
              let fields =
                [ "name", RT2DT.FQConstantName.Builtin.toDT name
                  "description", DString data.description
                  "returnType", RT2DT.TypeReference.toDT data.typ ]

              DRecord(builtinConstant, builtinConstant, [], Map fields))

          DList(VT.customType builtinConstant [], consts) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "languageToolsAllBuiltinFns" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok builtinFn, []) |> TList
      description =
        "Returns a list of the Builtin functions (usually not to be accessed directly)."
      fn =
        (function
        | exeState, _, _, [ DUnit ] ->
          let fns =
            exeState.fns.builtIn
            |> Map.toList
            |> List.map (fun (name, data) ->
              let parameters =
                data.parameters
                |> List.map (fun p ->
                  let fields =
                    [ "name", DString p.name
                      "type", RT2DT.TypeReference.toDT p.typ ]
                  DRecord(builtinFnParam, builtinFnParam, [], Map fields))
                |> Dval.list (KTCustomType(builtinFnParam, []))

              let fields =
                [ "name", RT2DT.FQFnName.Builtin.toDT name
                  "description", DString data.description
                  "parameters", parameters
                  "returnType", RT2DT.TypeReference.toDT data.returnType ]

              DRecord(builtinFn, builtinFn, [], Map fields))

          DList(VT.customType builtinFn [], fns) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // This exists because the above-defined fn returns a big list,
    // and List.filter fails on such a big list due to stack overflow
    // (a bug to fix...) TODO
    { name = fn "languageToolsBuiltinFnExists" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "version" TInt64 "" ]
      returnType = TBool
      description = "Returns whether or not some builtin fn exists"
      fn =
        (function
        | exeState, _, _, [ DString name; DInt64 version ] ->
          let name : FQFnName.Builtin = { name = name; version = int version }

          let found = exeState.fns.builtIn |> Map.find name |> Option.isSome

          DBool found |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
