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

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal

let isSafeOCamlString (s : string) : bool =
  s <> null && not (s.Contains('\u0000'))

/// We disallow `\u0000` in OCaml because Postgres doesn't like it; see `of_utf8_encoded_string.ml`
/// FSTODO: add in unicode
let safeOCamlString = Arb.Default.String() |> Arb.filter isSafeOCamlString

/// List of all a..z, A..Z, 0..9, and _ characters
let alphaNumericString =
  List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ]

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

let char () : Gen<string> =
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

module NodaTime =
  let instant =
    Arb.generate<System.DateTime>
    |> Gen.map (fun dt -> dt.ToUniversalTime())
    |> Gen.map (fun dt -> Instant.FromDateTimeUtc dt)

  let localDateTime : Gen<NodaTime.LocalDateTime> =
    Arb.generate<System.DateTime> |> Gen.map NodaTime.LocalDateTime.FromDateTime

  type All =
    static member Instant() = instant |> Arb.fromGen
    static member LocalDateTime() = localDateTime |> Arb.fromGen
