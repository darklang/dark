module Builtins.Pure.Libs.Int32

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "int32ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TFloat
      description = "Converts an <type Int32> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt32 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DInt32 int |] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
