/// Generators and FuzzTests ensuring consistent
/// behaviour across F# and OCaml backends
module FuzzTests.OCamlInterop

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
module OCamlInterop = TestUtils.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated
module G = Generators

open LibExecution.OCamlTypes.Convert
open OCamlInterop
open Json.OCamlCompatible

let isInteroperableWithOCamlBackend
  (ocamlToString : 'a -> Task<string>)
  (ocamlOfString : string -> Task<'a>)
  (fsToString : 'a -> string)
  (fsOfString : string -> 'a)
  (equality : 'a -> 'a -> bool)
  (v : 'a)
  : bool =
  try
    // What does it mean to interoperate? Ideally, the F# impl would be able
    // to read what the OCaml impl sends it and vice versa. However, because
    // the OCaml side is buggy, and we want to reproduce those bugs exactly
    // (for now), that isn't sufficient. We actually just want to make sure
    // we produce the same thing as they do for the same value. BUT, we don't
    // actually produce the exact same thing, and it's hard to do that for
    // the edge cases we've found. So really we just want to make sure that
    // whatever either side produces, both sides are able to read it and get
    // the same result.
    let bothCanRead str = (ocamlOfString str).Result |> equality (fsOfString str)
    let bothCanReadOCamlString = bothCanRead (ocamlToString v).Result
    let bothCanReadFSharpString = bothCanRead (fsToString v)

    if bothCanReadFSharpString && bothCanReadOCamlString then
      true
    else
      print (
        $"ocamlStringReadable: {bothCanReadOCamlString}\n"
        + $"fsharpStringReadable: {bothCanReadFSharpString}\n"
      )

      false
  with
  | e ->
    print $"Exception while fuzzing {e}"
    reraise ()

type Generator =
  static member LocalDateTime() = G.NodaTime.LocalDateTime
  static member Instant() = G.NodaTime.Instant

  static member String() : Arbitrary<string> = G.OCamlSafeUnicodeString

  static member Expr() =
    Arb.Default.Derive()
    |> Arb.filter (fun expr ->
      match expr with
      // characters are not supported in OCaml
      // CLEANUP can be removed once OCaml gone
      | PT.ECharacter _ -> false
      | _ -> true)

  static member Pattern() =
    Arb.Default.Derive()
    |> Arb.filter (fun pattern ->
      match pattern with
      // characters are not supported in OCaml
      // CLEANUP can be removed once OCaml gone
      | PT.PCharacter _ -> false
      | _ -> true)

let yojsonExprRoundtrip (a : PT.Expr) : bool =
  a
  |> pt2ocamlExpr
  |> serialize
  |> deserialize
  |> ocamlExpr2PT
  |> serialize
  |> deserialize
  |> pt2ocamlExpr
  |> serialize
  |> deserialize
  |> ocamlExpr2PT
  |> serialize
  |> deserialize
  .=. a

let yojsonHandlerRoundtrip (a : PT.Handler.T) : bool =
  a
  |> pt2ocamlHandler
  |> serialize
  |> deserialize
  |> ocamlHandler2PT { x = a.pos.x; y = a.pos.y }
  |> serialize
  |> deserialize
  |> pt2ocamlHandler
  |> serialize
  |> deserialize
  |> ocamlHandler2PT { x = a.pos.x; y = a.pos.y }
  |> serialize
  |> deserialize
  .=. a

let binaryExprRoundtrip (pair : PT.Expr * tlid) : bool =
  pair
  |> exprTLIDPairToCachedBinary
  |> result
  |> exprTLIDPairOfCachedBinary
  |> result
  .=. pair

let tests config =
  let tp f = testProperty config typeof<Generator> f

  testList
    "OcamlInterop"
    [ tp "roundtripping OCamlInteropBinaryExpr" binaryExprRoundtrip
      tp "roundtripping OCamlInteropYojsonHandler" yojsonHandlerRoundtrip
      tp "roundtripping OCamlInteropYojsonExpr" yojsonExprRoundtrip ]


module Roundtrippable =
  type Generator =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      G.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant

    static member String() : Arbitrary<string> = G.OCamlSafeUnicodeString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (DvalReprInternalDeprecated.isRoundtrippableDval false)

  type GeneratorWithBugs =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      G.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant

    static member String() : Arbitrary<string> = G.OCamlSafeUnicodeString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (DvalReprInternalDeprecated.isRoundtrippableDval true)

  let roundtripsSuccessfully (dv : RT.Dval) : bool =
    dv
    |> DvalReprInternalDeprecated.toInternalRoundtrippableV0
    |> DvalReprInternalDeprecated.ofInternalRoundtrippableV0
    |> Expect.dvalEquality dv

  let isInteroperableV0 dv =
    if containsPassword dv then
      true
    else
      isInteroperableWithOCamlBackend
        OCamlInterop.toInternalRoundtrippableV0
        OCamlInterop.ofInternalRoundtrippableV0
        DvalReprInternalDeprecated.toInternalRoundtrippableV0
        DvalReprInternalDeprecated.ofInternalRoundtrippableV0
        Expect.dvalEquality
        dv

  let tests config =
    testList
      "roundtrippable"
      [ testProperty
          config
          typeof<Generator>
          "roundtripping works properly"
          roundtripsSuccessfully
        testProperty
          config
          typeof<GeneratorWithBugs>
          "roundtrippable is interoperable"
          isInteroperableV0 ]


module Queryable =
  type Generator =
    static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
      G.NodaTime.LocalDateTime
    static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant

    static member String() : Arbitrary<string> = G.OCamlSafeUnicodeString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter DvalReprInternalDeprecated.isQueryableDval

  let canV1Roundtrip (dv : RT.Dval) : bool =
    let dvm = (Map.ofList [ "field", dv ])

    dvm
    |> DvalReprInternalDeprecated.toInternalQueryableV1
    |> DvalReprInternalDeprecated.ofInternalQueryableV1
    |> Expect.dvalEquality (RT.DObj dvm)

  let isInteroperableV1 (dv : RT.Dval) =
    // redacted passwords are created on the OCaml side and hard to remove
    if containsPassword dv then
      true
    else
      let dvm = (Map.ofList [ "field", dv ])

      isInteroperableWithOCamlBackend
        OCamlInterop.toInternalQueryableV1
        OCamlInterop.ofInternalQueryableV1
        (fun dval ->
          match dval with
          | RT.DObj dvm -> DvalReprInternalDeprecated.toInternalQueryableV1 dvm
          | dv -> Exception.raiseInternal "not an obj" [ "dval", dv ])
        DvalReprInternalDeprecated.ofInternalQueryableV1
        Expect.dvalEquality
        (RT.DObj dvm)

  let tests config =
    let tp f = testProperty config typeof<Generator> f

    testList
      "InternalQueryable"
      [ tp "roundtripping v1" canV1Roundtrip
        tp "interoperable v1" isInteroperableV1 ]
