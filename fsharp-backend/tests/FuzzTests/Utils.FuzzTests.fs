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


module Generators =
  let nonNullString (s : string) : bool = s <> null

  let safeOCamlString (s : string) : bool =
    // We disallow \u0000 in OCaml because postgres doesn't like it, see of_utf8_encoded_string
    s <> null && not (s.Contains('\u0000'))

  let alphaNumericString =
    (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ])

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
    |> Gen.map (fun s -> s.Normalize())
    |> Gen.filter safeOCamlString

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
    |> Gen.filter ((<>) None)
    |> Gen.map (Option.defaultValue "")
    |> Gen.filter ((<>) "")
