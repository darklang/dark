module Builtins.Pure.Libs.Int16

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError

let fns () : List<BuiltInFn> =
  [ { name = fn "int16ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TString
      description = "Stringify <param int16>"
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int16ToFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TFloat64
      description = "Converts an <type Int16> to a <type Float64>"
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Ply(DFloat64(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
