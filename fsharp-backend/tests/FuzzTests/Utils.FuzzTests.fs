/// Utilities useful for writing and running FuzzTests
module FuzzTests.Utils

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

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal

/// Extracts the result from a task
let result (t : Task<'a>) : 'a = t.Result

let (.=.) actual expected : bool =
  if actual = expected then
    Expect.equal actual expected ""
    true
  else
    let o = string actual |> UTF8.toBytes
    let e = string expected |> UTF8.toBytes
    Expect.equal (actual, o) (expected, e) ""
    false

let baseConfig : FsCheckConfig =
  { FsCheckConfig.defaultConfig with maxTest = 100000 }

let baseConfigWithGenerator (typ : System.Type) : FsCheckConfig =
  { baseConfig with arbitrary = [ typ ] }

let testProperty (name : string) (x : 'a) : Test =
  testPropertyWithConfig baseConfig name x

let testPropertyWithGenerator (typ : System.Type) (name : string) (x : 'a) : Test =
  testPropertyWithConfig (baseConfigWithGenerator typ) name x

/// Lower-level generators
module Generators =
  /// We disallow `\u0000` in OCaml because Postgres doesn't like it; see `of_utf8_encoded_string.ml`
  let safeOCamlString (s : string) : bool = s <> null && not (s.Contains('\u0000'))

  /// List of all a..z, A..Z, 0..9, and _ characters
  let alphaNumericString =
    List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ]

  /// Generates a string that 'normalizes' successfully,
  /// and is safe for use in OCaml
  let string () =
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
    |> Gen.filter safeOCamlString

  /// Generates an `int` >= 0
  let nonNegativeInt () =
    gen {
      let! (NonNegativeInt i) = Arb.generate<NonNegativeInt>
      return i
    }

  let char () : Gen<string> =
    string ()
    |> Gen.map String.toEgcSeq
    |> Gen.map Seq.toList
    |> Gen.map List.head
    |> Gen.filter Option.isSome
    |> Gen.map (Option.defaultValue "")
    |> Gen.filter ((<>) "")
