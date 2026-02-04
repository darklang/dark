module BuiltinExecution.Libs.Builtins

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


let fns : List<BuiltInFn> =
  [ { name = fn "getBuiltins" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            Ok(FQTypeName.fqPackage PackageIDs.Type.Builtins.functionInfo),
            []
          )
        )
      description = "Returns a list of all builtin functions with their metadata"
      fn =
        (function
        | exeState, _, [], [ DUnit ] ->
          uply {
            let builtinFnTypeName =
              FQTypeName.fqPackage PackageIDs.Type.Builtins.functionInfo

            let paramTypeName =
              FQTypeName.fqPackage PackageIDs.Type.Builtins.paramInfo

            let paramToRecord (param : BuiltInParam) =
              let fields =
                [ ("name", DString param.name)
                  ("typ", RT2DT.TypeReference.toDT param.typ)
                  ("description", DString param.description) ]
                |> Map.ofList
              DRecord(paramTypeName, paramTypeName, [], fields)

            let previewableToString (p : Previewable) : string =
              match p with
              | Pure -> "pure"
              | ImpurePreviewable -> "impure-previewable"
              | Impure -> "impure"

            let fnToRecord (fn : BuiltInFn) =
              let params' =
                fn.parameters
                |> List.map paramToRecord
                |> Dval.list (VT.customType paramTypeName [])
              let fields =
                [ ("name", DString fn.name.name)
                  ("version", DInt64(int64 fn.name.version))
                  ("parameters", params')
                  ("returnType", RT2DT.TypeReference.toDT fn.returnType)
                  ("description", DString fn.description)
                  ("purity", DString(previewableToString fn.previewable)) ]
                |> Map.ofList
              DRecord(builtinFnTypeName, builtinFnTypeName, [], fields)

            let fns =
              exeState.fns.builtIn
              |> Map.values
              |> List.filter (fun fn ->
                match fn.deprecated with
                | NotDeprecated -> true
                | _ -> false)
              |> List.sortBy (fun fn -> fn.name.name)

            let builtins =
              fns
              |> List.map fnToRecord
              |> Dval.list (VT.customType builtinFnTypeName [])

            return builtins
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
