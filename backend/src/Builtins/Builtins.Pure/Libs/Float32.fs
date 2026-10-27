/// The two conversions Float32 cannot do in Dark: everything else about it is in
/// `stdlib/float32.dark`, as Float operations between a widen and a narrow.
module Builtins.Pure.Libs.Float32

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "float32FromFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TFloat32
      description =
        "Rounds <param a> to the nearest <type Float32>, ties to even. Out of range is infinity."
      fn =
        (function
        | _, _, _, [| DFloat a |] -> Ply(DFloat32(float32 a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float32ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat32 "" ]
      returnType = TFloat
      description =
        "Widens <param a> to a <type Float>. Exact: every Float32 is a Float."
      fn =
        (function
        | _, _, _, [| DFloat32 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
