module FuzzTests.DvalRepr

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open TestUtils.TestUtils
open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal

let tpwg = testPropertyWithGenerator

module DeveloperRepr =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.fromGen (Generators.string ())

    // The format here is only used for errors so it doesn't matter all the
    // much. These are places where we've manually checked the differing
    // outputs are fine.

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | RT.DFloat 0.0 -> false
        | RT.DFloat infinity -> false
        | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    DvalReprExternal.toDeveloperReprV0 dv
    .=. (OCamlInterop.toDeveloperRepr dv).Result

  let tests =
    testList "toDeveloperRepr" [ tpwg typeof<Generator> "roundtripping" equalsOCaml ]

module EndUserReadable =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.fromGen (Generators.string ())

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  // The format here is used to show users so it has to be exact
  let equalsOCaml (dv : RT.Dval) : bool =
    DvalReprExternal.toEnduserReadableTextV0 dv
    .=. (OCamlInterop.toEnduserReadableTextV0 dv).Result

  let tests =
    testList
      "toEnduserReadable"
      [ tpwg typeof<Generator> "roundtripping" equalsOCaml ]

module Hashing =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.fromGen (Generators.string ())

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        // not supported in OCaml
        | RT.DFnVal _ -> false
        | _ -> true)

  // The format here is used to get values from the DB, so this has to be 100% identical
  let equalsOCamlToHashable (dv : RT.Dval) : bool =
    let ocamlVersion = (OCamlInterop.toHashableRepr dv).Result
    let fsharpVersion =
      DvalReprInternal.toHashableRepr 0 false dv |> UTF8.ofBytesUnsafe
    ocamlVersion .=. fsharpVersion

  let equalsOCamlV0 (l : List<RT.Dval>) : bool =
    DvalReprInternal.hash 0 l .=. (OCamlInterop.hashV0 l).Result

  let equalsOCamlV1 (l : List<RT.Dval>) : bool =
    let ocamlVersion = (OCamlInterop.hashV1 l).Result
    let fsharpVersion = DvalReprInternal.hash 1 l
    ocamlVersion .=. fsharpVersion

  let tests =
    testList
      "hash"
      [ tpwg typeof<Generator> "toHashableRepr" equalsOCamlToHashable
        tpwg typeof<Generator> "hashv0" equalsOCamlV0
        tpwg typeof<Generator> "hashv1" equalsOCamlV1 ]
