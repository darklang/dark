module BuiltinExecution.Libs.LanguageTools

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


let builtinValue = FQTypeName.fqPackage PackageIDs.Type.LanguageTools.builtinValue

let builtinFnParam =
  FQTypeName.fqPackage PackageIDs.Type.LanguageTools.builtinFnParam
let builtinFn = FQTypeName.fqPackage PackageIDs.Type.LanguageTools.builtinFn

let fns : List<BuiltInFn> =
  [ { name = fn "languageToolsAllBuiltinValues" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok builtinValue, []) |> TList
      description =
        "Returns a list of the Builtin values (usually not to be accessed directly)."
      fn =
        (function
        | exeState, _, _, [ DUnit ] ->
          let vals =
            exeState.values.builtIn
            |> Map.toList
            |> List.map (fun (name, (data : BuiltInValue)) ->
              let fields =
                [ "name", RT2DT.FQValueName.Builtin.toDT name
                  "description", DString data.description
                  "returnType", RT2DT.TypeReference.toDT data.typ ]

              DRecord(builtinValue, builtinValue, [], Map fields))

          DList(VT.customType builtinValue [], vals) |> Ply
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
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
