module Builtins.Pure.Libs.UInt16

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint16ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TString
      description = "Stringify <param uint16>"
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TFloat
      description = "Converts an <type UInt16> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
