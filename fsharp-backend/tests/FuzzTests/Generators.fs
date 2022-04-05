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
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal

let isSafeOCamlString (s : string) : bool = s <> null && not (s.Contains('\u0000'))

/// We disallow `\u0000` in OCaml because Postgres doesn't like it; see `of_utf8_encoded_string.ml`
/// FSTODO: add in unicode
let safeOCamlString = Arb.Default.String() |> Arb.filter isSafeOCamlString

/// List of all a..z, A..Z, 0..9, and _ characters
let alphaNumericCharacters =
  List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ]

let alphaNumericString =
  let charGen = alphaNumericCharacters |> Gen.elements
  Gen.arrayOf charGen |> Gen.map (fun cs -> new String(cs))

let AlphaNumericString = Arb.fromGen alphaNumericString

/// Generates a string that 'normalizes' successfully,
/// and is safe for use in OCaml
let ocamlSafeString =
  let isValid (s : string) : bool =
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
  |> Gen.filter isValid
  // Now that we know it can be normalized, actually normalize it
  |> Gen.map String.normalize
  |> Gen.filter isSafeOCamlString

let OCamlSafeString = ocamlSafeString |> Arb.fromGen

// FSTODO The above string generators yield strings that result in inconsistent
// behaviour between OCaml and F# backends. This should be resolved. That said,
// to test functionality outside of that issue, locally toggling the above
// generator/arb for the below pair is recommended.

// let ocamlSafeString = alphaNumericString
// let OCamlSafeString = AlphaNumericString

let char : Gen<string> =
  ocamlSafeString
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

let ocamlSafeFloat =
  gen {
    let specials = interestingFloats |> List.map Tuple2.second |> Gen.elements

    return! Gen.frequency [ (5, specials); (5, Arb.generate<float>) ]
  }

let OCamlSafeFloat = Arb.fromGen ocamlSafeFloat

/// Ensure we only work with OCaml-friendly integers
let isValidOCamlInt (i : int64) : bool =
  let ocamlIntUpperLimit = 4611686018427387903L
  let ocamlIntLowerLimit = -4611686018427387904L

  i <= ocamlIntUpperLimit && i >= ocamlIntLowerLimit

let ocamlSafeInt64 =
  gen {
    let specials =
      interestingInts
      |> List.map Tuple2.second
      |> List.filter isValidOCamlInt
      |> Gen.elements

    let v = Gen.frequency [ (5, specials); (5, Arb.generate<int64>) ]
    return! Gen.filter isValidOCamlInt v
  }

let OCamlSafeInt64 = Arb.fromGen ocamlSafeInt64

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
  /// Used to avoid `toString` on Dvals that contains bytes,
  /// as OCaml backend raises an exception when attempted.
  /// CLEANUP can be removed with OCaml
  let rec containsBytes (dv : RT.Dval) =
    match dv with
    | RT.DBytes _ -> true

    | RT.DDB _
    | RT.DInt _
    | RT.DBool _
    | RT.DFloat _
    | RT.DNull
    | RT.DStr _
    | RT.DChar _
    | RT.DIncomplete _
    | RT.DFnVal _
    | RT.DError _
    | RT.DDate _
    | RT.DPassword _
    | RT.DUuid _
    | RT.DHttpResponse (RT.Redirect _)
    | RT.DOption None -> false

    | RT.DList dv -> List.any containsBytes dv
    | RT.DObj o -> o |> Map.values |> List.any containsBytes

    | RT.DHttpResponse (RT.Response (_, _, dv))
    | RT.DOption (Some dv)
    | RT.DErrorRail dv
    | RT.DResult (Ok dv)
    | RT.DResult (Error dv) -> containsBytes dv

  let Dval =
    Arb.Default.Derive()
    |> Arb.filter (function
      // These all break the serialization to OCaml
      // CLEANUP allow all Dvals to be generated
      | RT.DPassword _ -> false
      | RT.DFnVal _ -> false
      | _ -> true)

  let DType =
    let rec isSupportedType =
      function
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

  let dType : Gen<RT.DType> = DType.Generator

module Certificates =
  let rsaPrivateKey = Gen.constant "-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ\nd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN\nc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2\nuUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B\nCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW\nDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa\noNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq\np0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+\n1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ\nXUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv\nvz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua\n6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509\nrWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz\nNBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj\namEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr\nr48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5\nPJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J\nhpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g\nMcgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn\n4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV\nctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf\nn8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9\n5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb\nT/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I\nFW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=\n-----END RSA PRIVATE KEY-----"
