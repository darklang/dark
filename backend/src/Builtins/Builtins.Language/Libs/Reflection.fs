module Builtins.Language.Libs.Reflection

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PackageRefs = LibExecution.PackageRefs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes


let fns () : List<BuiltInFn> =
  [ { name = fn "reflect" 0
      typeParams = []
      parameters = [ Param.make "dv" (TVariable "a") "" ]
      returnType =
        TCustomType(
          { originalName = []
            resolved =
              Ok(
                FQTypeName.fqPackage (
                  PackageRefs.Type.LanguageTools.RuntimeTypes.dval ()
                )
              ) },
          []
        )
      description = "Returns a meta representation of the real underlying dval"
      fn =
        function
        | _, _, _, [ dv ] -> dv |> RT2DT.Dval.toDT |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated
      accessibility = Any } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
