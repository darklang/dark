module BuiltinExecution.Libs.Builtins

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Exe = LibExecution.Execution
module PackageIDs = LibExecution.PackageIDs


let fns : List<BuiltInFn> =
  [ { name = fn "getBuiltins" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.Builtins.functionInfo),
            []
          )
        )
      description = "Returns a list of all builtin functions with their metadata"
      fn =
        (function
        | exeState, _, [], [ DUnit ] ->
          uply {
            let builtinFnTypeName =
              FQTypeName.fqPackage PackageIDs.Type.Stdlib.Builtins.functionInfo

            let paramTypeName =
              FQTypeName.fqPackage PackageIDs.Type.Stdlib.Builtins.paramInfo

            let paramToRecord (param : BuiltInParam) =
              uply {
                let! typStr = Exe.typeRefToString exeState param.typ
                let fields =
                  [ ("name", DString param.name)
                    ("typ", DString typStr)
                    ("description", DString param.description) ]
                  |> Map.ofList
                return DRecord(paramTypeName, paramTypeName, [], fields)
              }

            let previewableToString (p : Previewable) : string =
              match p with
              | Pure -> "pure"
              | ImpurePreviewable -> "impure-previewable"
              | Impure -> "impure"

            let fnToRecord (fn : BuiltInFn) =
              uply {
                let! params' = Ply.List.mapSequentially paramToRecord fn.parameters
                let params' = DList(VT.customType paramTypeName [], params')
                let! returnType = Exe.typeRefToString exeState fn.returnType
                let fields =
                  [ ("name", DString fn.name.name)
                    ("version", DInt64(int64 fn.name.version))
                    ("parameters", params')
                    ("returnType", DString returnType)
                    ("description", DString fn.description)
                    ("purity", DString(previewableToString fn.previewable)) ]
                  |> Map.ofList
                return DRecord(builtinFnTypeName, builtinFnTypeName, [], fields)
              }

            let fns =
              exeState.fns.builtIn
              |> Map.values
              |> List.filter (fun fn ->
                match fn.deprecated with
                | NotDeprecated -> true
                | _ -> false)
              |> List.sortBy (fun fn -> fn.name.name)

            let! builtins = Ply.List.mapSequentially fnToRecord fns

            return DList(VT.customType builtinFnTypeName [], builtins)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
