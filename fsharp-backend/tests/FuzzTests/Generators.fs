/// Generators
module FuzzTests.Generators

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils
open NodaTime
open System

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated

/// List of all a..z, A..Z, 0..9, and _ characters
let alphaNumericCharacters =
  List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ]

/// Generates a string that 'normalizes' successfully
let safeUnicodeString =
  /// We disallow `\u0000` because Postgres doesn't like it
  let isSafeString (s : string) : bool = s <> null && not (s.Contains('\u0000'))

  let normalizesSuccessfully (s : string) : bool =
    try
      String.normalize s |> ignore<string>
      true
    with
    | e ->
      // debuG
      //   "Failed to normalize :"
      //   $"{e}\n '{s}': (len {s.Length}, {System.BitConverter.ToString(toBytes s)})"

      false

  Arb.generate<UnicodeString>
  |> Gen.map (fun (UnicodeString s) -> s)
  |> Gen.filter normalizesSuccessfully
  // Now that we know it can be normalized, actually normalize it
  |> Gen.map String.normalize
  |> Gen.filter isSafeString

let SafeUnicodeString = safeUnicodeString |> Arb.fromGen

let char : Gen<string> =
  safeUnicodeString
  |> Gen.map String.toEgcSeq
  |> Gen.map Seq.toList
  |> Gen.map List.head
  |> Gen.filter Option.isSome
  |> Gen.map (Option.defaultValue "")
  |> Gen.filter ((<>) "")

/// Generates an `int` >= 0
let nonNegativeInt =
  gen {
    let! (NonNegativeInt i) = Arb.generate<NonNegativeInt>
    return i
  }

let safeFloat =
  gen {
    let specials = interestingFloats |> List.map Tuple2.second |> Gen.elements

    return! Gen.frequency [ (5, specials); (5, Arb.generate<float>) ]
  }

let SafeFloat = Arb.fromGen safeFloat

let safeInt64 =
  gen {
    let specials = interestingInts |> List.map Tuple2.second |> Gen.elements

    return! Gen.frequency [ (5, specials); (5, Arb.generate<int64>) ]
  }

let SafeInt64 = Arb.fromGen safeInt64

module NodaTime =
  let instant =
    Arb.generate<System.DateTime>
    |> Gen.map (fun dt -> dt.ToUniversalTime())
    |> Gen.map (fun dt -> Instant.FromDateTimeUtc dt)

  let localDateTime : Gen<NodaTime.LocalDateTime> =
    Arb.generate<System.DateTime> |> Gen.map NodaTime.LocalDateTime.FromDateTime

  let Instant = instant |> Arb.fromGen
  let LocalDateTime = localDateTime |> Arb.fromGen

module RuntimeTypes =
  let Dval =
    Arb.Default.Derive()
    |> Arb.filter (fun dval ->
      match dval with
      // These all break the serialization to OCaml
      // TODO allow all Dvals to be generated
      | RT.DPassword _ -> false
      | RT.DFnVal _ -> false
      | _ -> true)

  let DType =
    let rec isSupportedType dtype =
      match dtype with
      | RT.TInt
      | RT.TStr
      | RT.TVariable _
      | RT.TFloat
      | RT.TBool
      | RT.TNull
      | RT.TNull
      | RT.TDate
      | RT.TChar
      | RT.TUuid
      | RT.TBytes
      | RT.TError
      | RT.TDB (RT.TUserType _)
      | RT.TDB (RT.TRecord _)
      | RT.TUserType _ -> true
      | RT.TList t
      | RT.TDict t
      | RT.TOption t
      | RT.THttpResponse t -> isSupportedType t
      | RT.TTuple (first, second, theRest) ->
        List.all isSupportedType ([ first; second ] @ theRest)
      | RT.TResult (t1, t2) -> isSupportedType t1 && isSupportedType t2
      | RT.TFn (ts, rt) -> isSupportedType rt && List.all isSupportedType ts
      | RT.TRecord (pairs) ->
        pairs |> List.map Tuple2.second |> List.all isSupportedType

      // FSTODO: support all types
      | RT.TDB _
      | RT.TIncomplete
      | RT.TPassword
      | RT.TErrorRail -> false

    Arb.Default.Derive() |> Arb.filter isSupportedType

  let dType = DType.Generator
