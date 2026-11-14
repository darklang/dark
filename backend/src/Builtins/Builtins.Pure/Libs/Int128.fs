module Builtins.Pure.Libs.Int128

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "int128ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TString
      description = "Converts an <type Int128> to a <type String>"
      fn =
        (function
        | _, _, _, [| DInt128 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int128ToFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TFloat64
      description = "Converts an <type Int128> to a <type Float64>"
      fn =
        (function
        | _, _, _, [| DInt128 a |] -> Ply(DFloat64(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
