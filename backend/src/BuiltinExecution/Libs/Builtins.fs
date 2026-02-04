module BuiltinExecution.Libs.Builtins

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


/// Converters from F# builtin types to Dark types
module ToDarkTypes =
  module Purity =
    let typeName = FQTypeName.fqPackage PackageIDs.Type.Builtins.purity

    let toDT (p : Previewable) : Dval =
      let caseName =
        match p with
        | Pure -> "Pure"
        | ImpurePreviewable -> "ImpurePreviewable"
        | Impure -> "Impure"
      DEnum(typeName, typeName, [], caseName, [])

  module ParamInfo =
    let typeName = FQTypeName.fqPackage PackageIDs.Type.Builtins.paramInfo

    let toDT (param : BuiltInParam) : Dval =
      let fields =
        [ ("name", DString param.name)
          ("typ", RT2DT.TypeReference.toDT param.typ)
          ("description", DString param.description) ]
        |> Map.ofList
      DRecord(typeName, typeName, [], fields)

  module FunctionInfo =
    let typeName = FQTypeName.fqPackage PackageIDs.Type.Builtins.functionInfo

    let toDT (fn : BuiltInFn) : Dval =
      let params' =
        fn.parameters
        |> List.map ParamInfo.toDT
        |> Dval.list (KTCustomType(ParamInfo.typeName, []))
      let fields =
        [ ("name", DString fn.name.name)
          ("version", DInt64(int64 fn.name.version))
          ("parameters", params')
          ("returnType", RT2DT.TypeReference.toDT fn.returnType)
          ("description", DString fn.description)
          ("purity", Purity.toDT fn.previewable) ]
        |> Map.ofList
      DRecord(typeName, typeName, [], fields)


let fns : List<BuiltInFn> =
  [ { name = fn "getBuiltins" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(Ok(ToDarkTypes.FunctionInfo.typeName), []))
      description = "Returns a list of all builtin functions with their metadata"
      fn =
        (function
        | exeState, _, [], [ DUnit ] ->
          uply {
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
              |> List.map ToDarkTypes.FunctionInfo.toDT
              |> Dval.list (KTCustomType(ToDarkTypes.FunctionInfo.typeName, []))

            return builtins
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
