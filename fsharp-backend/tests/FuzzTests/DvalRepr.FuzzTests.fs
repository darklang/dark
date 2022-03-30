/// Generators and FuzzTests to ensure consistent behaviour
/// of Runtime Dvals across OCaml and F# backends
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

/// Ensure text representation of Dvals meant to be read by Dark users
/// is produced consistently across OCaml and F# backends
module DeveloperRepr =
  type Generator =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      Generators.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> =
      Generators.NodaTime.Instant

    static member String() : Arbitrary<string> = Generators.OCamlSafeString

    // The format here is only used for errors so it doesn't matter all that
    // much. These are places where we've manually checked the differing
    // outputs are fine.

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | RT.DFloat 0.0 -> false
        | RT.DFloat infinity -> false // TODO I don't think this is doing what we want it to!!
        | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    DvalReprExternal.toDeveloperReprV0 dv
    .=. (OCamlInterop.toDeveloperRepr dv).Result

  let tests config =
    testList
      "toDeveloperRepr"
      [ testProperty config typeof<Generator> "roundtripping" equalsOCaml ]

/// Ensure text representation of DVals meant to be read by "end users"
/// is produced consistently across OCaml and F# backends
module EnduserReadable =
  type Generator =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      Generators.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> =
      Generators.NodaTime.Instant

    static member String() : Arbitrary<string> = Generators.OCamlSafeString

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  // The format here is shown to users, so it has to be exact
  let equalsOCaml (dv : RT.Dval) : bool =
    DvalReprExternal.toEnduserReadableTextV0 dv
    .=. (OCamlInterop.toEnduserReadableTextV0 dv).Result

  let tests config =
    testList
      "toEnduserReadable"
      [ testProperty config typeof<Generator> "roundtripping" equalsOCaml ]

/// Ensure hashing of RT DVals is consistent across OCaml and F# backends
module Hashing =
  type Generator =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      Generators.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> =
      Generators.NodaTime.Instant

    static member String() : Arbitrary<string> = Generators.OCamlSafeString

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

  let tests config =
    testList
      "hash"
      [ testProperty config typeof<Generator> "toHashableRepr" equalsOCamlToHashable
        testProperty config typeof<Generator> "hashv0" equalsOCamlV0
        testProperty config typeof<Generator> "hashv1" equalsOCamlV1 ]
