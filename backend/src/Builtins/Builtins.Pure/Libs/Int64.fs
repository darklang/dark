module Builtins.Pure.Libs.Int64

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "int64ToFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TFloat64
      description = "Converts an <type Int64> to a <type Float64>"
      fn =
        (function
        | _, _, _, [| DInt64 a |] -> Ply(DFloat64(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int64ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt64 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DInt64 int |] -> Ply(DString(Dval.int64ToString int))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
