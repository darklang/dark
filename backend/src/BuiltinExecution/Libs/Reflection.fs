module BuiltinExecution.Libs.Reflection

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


let fns : List<BuiltInFn> =
  [ { name = fn "reflect" 0
      typeParams = []
      parameters = [ Param.make "dv" (TVariable "a") "" ]
      returnType =
        TCustomType(
          Ok(
            FQTypeName.fqPackage
              LibExecution.PackageHashes.Type.LanguageTools.RuntimeTypes.dval
          ),
          []
        )
      description = "Returns a meta representation of the real underlying dval"
      fn =
        function
        | _, _, _, [ dv ] ->
          dv |> LibExecution.RuntimeTypesToDarkTypes.Dval.toDT |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
