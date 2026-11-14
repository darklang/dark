module Builtins.Pure.Libs.UInt64

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint64ToFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TFloat64
      description = "Converts an <type UInt64> to a <type Float64>"
      fn =
        (function
        | _, _, _, [| DUInt64 a |] -> Ply(DFloat64(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint64ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TUInt64 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DUInt64 int |] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
