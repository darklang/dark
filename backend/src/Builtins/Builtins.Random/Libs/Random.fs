/// Impure randomness builtins. Pure numeric/list ops live in their
/// type-specific files under `Builtins.Pure/Libs/`.
module Builtins.Random.Libs.Random

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module TypeChecker = LibExecution.TypeChecker


let fns () : List<BuiltInFn> =
  [ { name = fn "int8Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt8 ""; Param.make "end" TInt8 "" ]
      returnType = TInt8
      description =
        "Returns a random 8-bit signed integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DInt8 a; DInt8 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          let lowerBound = max lower -128y
          let upperBound = min upper 127y
          let int8Range = int upperBound - int lowerBound + 1
          let resultInt = randomSeeded().Next(int8Range)
          let int8Result = lowerBound + (int8 resultInt)
          int8Result |> DInt8 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "uint8Random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt8 ""; Param.make "end" TUInt8 "" ]
      returnType = TUInt8
      description =
        "Returns a random 8-bit unsigned integer (uint8) between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DUInt8 a; DUInt8 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          let lowerBound = max lower 0uy
          let upperBound = min upper 255uy
          let uint8Range = int upperBound - int lowerBound + 1
          let resultInt = randomSeeded().Next(uint8Range)
          let uint8Result = lowerBound + (uint8 resultInt)
          uint8Result |> DUInt8 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "int16Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt16 ""; Param.make "end" TInt16 "" ]
      returnType = TInt16
      description =
        "Returns a random integer16 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DInt16 a; DInt16 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          let correctRange = 1
          int lower + randomSeeded().Next(int upper - int lower + correctRange)
          |> int16
          |> DInt16
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "uint16Random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt16 ""; Param.make "end" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Returns a random integer16 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DUInt16 a; DUInt16 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          let lowerBound = max lower 0us
          let upperBound = min upper 65535us
          let correctRange = 1
          let uint16Range = int upperBound - int lowerBound + correctRange
          let resultInt = randomSeeded().Next(uint16Range)
          let uint16Result = lowerBound + (uint16 resultInt)
          Ply(DUInt16(uint16Result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "int32Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt32 ""; Param.make "end" TInt32 "" ]
      returnType = TInt32
      description =
        "Returns a random integer32 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DInt32 a; DInt32 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          let correction : int32 = 1
          lower + randomSeeded().Next(upper - lower + correction) |> DInt32 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "uint32Random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt32 ""; Param.make "end" TUInt32 "" ]
      returnType = TUInt32
      description =
        "Returns a random integer32 between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DUInt32 a; DUInt32 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          let lowerBound = max lower 0ul
          let upperBound = min upper 4294967295ul
          let correctRange = 1
          let uint32Range = int upperBound - int lowerBound + correctRange
          let resultInt = randomSeeded().Next(uint32Range)
          let uint32Result = lowerBound + (uint32 resultInt)
          Ply(DUInt32(uint32Result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "int64Random" 0
      typeParams = []
      parameters = [ Param.make "start" TInt64 ""; Param.make "end" TInt64 "" ]
      returnType = TInt64
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DInt64 a; DInt64 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          // .NET's "nextInt64" is exclusive, but we want inclusive.
          let correction : int64 = 1
          lower + randomSeeded().NextInt64(upper - lower + correction)
          |> DInt64
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "uint64Random" 0
      typeParams = []
      parameters = [ Param.make "start" TUInt64 ""; Param.make "end" TUInt64 "" ]
      returnType = TUInt64
      description =
        "Returns a random integer between <param start> and <param end> (inclusive)"
      fn =
        (function
        | _, _, _, [ DUInt64 a; DUInt64 b ] ->
          let lower, upper = if a > b then (b, a) else (a, b)
          let correction : int = 1
          let lowerBound = max lower 0UL
          let upperBound = min upper (uint64 System.UInt64.MaxValue)
          let uint64Range = int upperBound - int lowerBound + correction
          let resultInt = randomSeeded().Next(uint64Range)
          let uint64Result = lowerBound + (uint64 resultInt)
          Ply(DUInt64(uint64Result))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "listRandomElement" 0
      typeParams = []
      parameters = [ Param.make "list" (TList(TVariable "a")) "" ]
      returnType = TypeReference.option (TVariable "a")
      description =
        "Returns {{Some <var randomValue>}}, where <var randomValue> is a randomly
         selected value in <param list>. Returns {{None}} if <param list> is
         empty."
      fn =
        let optType = VT.unknownTODO
        (function
        | _, _, _, [ DList(_, []) ] ->
          TypeChecker.DvalCreator.optionNone optType |> Ply
        | _, vm, _, [ DList(_, l) ] ->
          // Will return <= (length - 1).
          // Maximum value is Int64.MaxValue (half of UInt64.MaxValue),
          // not a real concern at expected list sizes.
          let index = RNG.GetInt32(l.Length)
          (List.tryItem index l)
          |> TypeChecker.DvalCreator.option vm.threadID optType
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
