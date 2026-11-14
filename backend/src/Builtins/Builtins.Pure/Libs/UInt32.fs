module Builtins.Pure.Libs.UInt32

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint32ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TString
      description = "Stringify <param uint32>"
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint32ToFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TFloat64
      description = "Converts an <type UInt32> to a <type Float64>"
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Ply(DFloat64(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
