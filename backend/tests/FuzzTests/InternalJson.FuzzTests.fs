/// Generators and FuzzTests for DvalReprInternal
module FuzzTests.InternalJson

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
open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated
module DvalReprInternalNew = LibExecution.DvalReprInternalNew
module G = Generators

type Generator =
  static member LocalDateTime() = G.NodaTime.LocalDateTime
  static member Instant() = G.NodaTime.Instant

  static member String() : Arbitrary<string> = G.SafeUnicodeString

  static member Expr() =
    Arb.Default.Derive()
    |> Arb.filter (fun expr ->
      match expr with
      // characters are not supported in OCaml
      // CLEANUP can be removed once OCaml gone
      | PT.ECharacter _ -> false
      | _ -> true)

  static member MatchPattern() =
    Arb.Default.Derive()
    |> Arb.filter (fun pattern ->
      match pattern with
      // characters are not supported in OCaml
      // CLEANUP can be removed once OCaml gone
      | PT.MPCharacter _ -> false
      | _ -> true)


module Roundtrippable =
  type Generator =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      G.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant

    static member String() : Arbitrary<string> = G.SafeUnicodeString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> = Arb.Default.Derive()

  type GeneratorWithBugs =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      G.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant

    static member String() : Arbitrary<string> = G.SafeUnicodeString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> = Arb.Default.Derive()

  let roundtripsSuccessfully (dv : RT.Dval) : bool =
    dv
    |> DvalReprInternalNew.toRoundtrippableJsonV0
    |> DvalReprInternalNew.parseRoundtrippableJsonV0
    |> Expect.dvalEquality dv

  let tests config =
    testList
      "roundtrippable"
      [ testProperty
          config
          typeof<Generator>
          "roundtripping works properly"
          roundtripsSuccessfully ]


module Queryable =
  type Generator =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      G.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant

    static member String() : Arbitrary<string> = G.SafeUnicodeString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter DvalReprInternalDeprecated.Test.isQueryableDval

  let canV1Roundtrip (dv : RT.Dval) : bool =
    let dvm = (Map.ofList [ "field", dv ])

    dvm
    |> DvalReprInternalDeprecated.toInternalQueryableV1
    |> DvalReprInternalDeprecated.ofInternalQueryableV1
    |> Expect.dvalEquality (RT.DObj dvm)

  let tests config =
    let tp f = testProperty config typeof<Generator> f

    testList "InternalQueryable" [ tp "roundtripping v1" canV1Roundtrip ]
