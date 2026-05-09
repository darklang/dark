module Builtins.Language.Libs.Reflection

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PackageRefs = LibExecution.PackageRefs
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module Exe = LibExecution.Execution


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
      deprecated = NotDeprecated }


    { name = fn "toRepr" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "a") "The value to convert." ]
      returnType = TString
      description = "Returns a string representation of the given <param value>"
      fn =
        (function
        | exeState, _, _, [ value ] ->
          uply {
            let! repr = Exe.dvalToRepr exeState value
            return DString repr
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
