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
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
module G = Generators

type Generator =
  static member LocalDateTime() = G.NodaTime.LocalDateTime
  static member Instant() = G.NodaTime.Instant

  static member String() : Arbitrary<string> = G.SafeUnicodeString

  static member Expr() = Arb.Default.Derive()


  static member MatchPattern() = Arb.Default.Derive()



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
    |> DvalReprInternalRoundtrippable.toJsonV0
    |> DvalReprInternalRoundtrippable.parseJsonV0
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
      |> Arb.filter DvalReprInternalQueryable.Test.isQueryableDval

  let canV1Roundtrip (dv : RT.Dval) : bool =
    let dvm = (Map.ofList [ "field", dv ])

    dvm
    |> DvalReprInternalQueryable.toJsonStringV0
    |> DvalReprInternalQueryable.parseJsonV0
    |> Expect.dvalEquality (RT.DObj dvm)

  let tests config =
    let tp f = testProperty config typeof<Generator> f

    testList "InternalQueryable" [ tp "roundtripping v1" canV1Roundtrip ]
