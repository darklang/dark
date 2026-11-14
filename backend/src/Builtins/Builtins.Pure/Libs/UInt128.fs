module Builtins.Pure.Libs.UInt128

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint128ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TString
      description = "Converts an <type UInt128> to a <type String>"
      fn =
        (function
        | _, _, _, [| DUInt128 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint128ToFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TFloat64
      description = "Converts an <type UInt128> to a <type Float64>"
      fn =
        (function
        | _, _, _, [| DUInt128 a |] -> Ply(DFloat64(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
