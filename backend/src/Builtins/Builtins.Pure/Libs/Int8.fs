module Builtins.Pure.Libs.Int8

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "int8ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TString
      description = "Converts an <type Int8> to a <type String>"
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TFloat
      description = "Converts an <type Int8> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
