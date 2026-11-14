/// The parts of Float32 that need .NET's binary32: converting, printing, parsing and the bit
/// pattern. The arithmetic is the polymorphic operators in `NoModule`; the rest is
/// `stdlib/float32.dark`.
module Builtins.Pure.Libs.Float32

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval


let fns () : List<BuiltInFn> =
  [ { name = fn "float32FromFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat64 "" ]
      returnType = TFloat32
      description =
        "Rounds <param a> to the nearest <type Float32>, ties to even. Out of range is infinity."
      fn =
        (function
        | _, _, _, [| DFloat64 a |] -> Ply(DFloat32(float32 a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float32ToFloat64" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat32 "" ]
      returnType = TFloat64
      description =
        "Widens <param a> to a <type Float64>. Exact: every Float32 is a Float64."
      fn =
        (function
        | _, _, _, [| DFloat32 a |] -> Ply(DFloat64(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float32FromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TFloat32
      description =
        "The <type Float32> nearest <param a>, ties to even, rounded once rather than through a Float64."
      fn =
        (function
        | _, _, _, [| DInt64 a |] -> Ply(DFloat32(float32 a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float32ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat32 "" ]
      returnType = TString
      description =
        "The shortest decimal that parses back to the same <type Float32>, e.g. `0.1`, `1e+16`."
      fn =
        (function
        | _, _, _, [| DFloat32 a |] ->
          let result =
            if System.Single.IsPositiveInfinity a then "Infinity"
            else if System.Single.IsNegativeInfinity a then "-Infinity"
            else if System.Single.IsNaN a then "NaN"
            else float32ToShortestString a
          Ply(DString result)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float32Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.option TFloat32
      description =
        "The <type Float32> nearest the number in <param s>, rounded once from the decimal, or {{None}}."
      fn =
        (function
        | _, _, _, [| DString s |] ->
          try
            float32 s |> DFloat32 |> Dval.optionSome KTFloat32 |> Ply
          with :? System.FormatException ->
            Dval.optionNone KTFloat32 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float32ToBits" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat32 "" ]
      returnType = TUInt32
      description = "The IEEE 754 binary32 bit pattern of <param a>."
      fn =
        (function
        | _, _, _, [| DFloat32 a |] ->
          System.BitConverter.SingleToUInt32Bits a |> DUInt32 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "float32FromBits" 0
      typeParams = []
      parameters = [ Param.make "bits" TUInt32 "" ]
      returnType = TFloat32
      description =
        "The <type Float32> whose IEEE 754 binary32 bit pattern is <param bits>."
      fn =
        (function
        | _, _, _, [| DUInt32 bits |] ->
          System.BitConverter.UInt32BitsToSingle bits |> DFloat32 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
