module FuzzTests.All

// This aims to find test cases that violate certain properties that we expect.
// Desired properties include that OCaml Dark programs and functions work the
// same as F# ones, and things related to serialization and output.

open Expecto
open Expecto.ExpectoFsCheck

open Prelude
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalRepr = LibExecution.DvalRepr

open FsCheck

let (.=.) actual expected : bool =
  if actual = expected then
    Expect.equal actual expected ""
    true
  else
    let o = actual.ToString() |> toBytes
    let e = expected.ToString() |> toBytes
    Expect.equal (actual, o) (expected, e) ""
    false


module GeneratorUtils =
  let nonNullString (s : string) : bool = s <> null

  let safeOCamlString (s : string) : bool =
    // We disallow \u0000 in OCaml because postgres doesn't like it, see of_utf8_encoded_string
    s <> null && not (s.Contains('\u0000'))

open GeneratorUtils

let baseConfig : FsCheckConfig = { FsCheckConfig.defaultConfig with maxTest = 10000 }

let baseConfigWithGenerator (typ : System.Type) : FsCheckConfig =
  { baseConfig with arbitrary = [ typ ] }

let testProperty (name : string) (x : 'a) : Test =
  testPropertyWithConfig baseConfig name x

let testPropertyWithGenerator (typ : System.Type) (name : string) (x : 'a) : Test =
  testPropertyWithConfig (baseConfigWithGenerator typ) name x



module FQFnName =
  let nameGenerator (first : char list) (other : char list) : Gen<string> =
    gen {
      let! length = Gen.choose (0, 20)
      let! head = Gen.elements first
      let! tail = Gen.arrayOfLength length (Gen.elements other)
      return System.String(Array.append [| head |] tail)
    }

  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter nonNullString

    static member FQFnName() : Arbitrary<PT.FQFnName.T> =
      let alphaNumeric =
        (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ])

      let ownerName : Gen<string> =
        nameGenerator [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

      let packageName = ownerName
      let modName : Gen<string> = nameGenerator [ 'A' .. 'Z' ] alphaNumeric
      let fnName : Gen<string> = nameGenerator [ 'a' .. 'z' ] alphaNumeric
      { new Arbitrary<PT.FQFnName.T>() with
          member x.Generator =
            gen {
              let! owner = ownerName
              let! package = packageName
              let! module_ = modName
              let! function_ = fnName
              let! NonNegativeInt version = Arb.generate<NonNegativeInt>

              return
                { owner = owner
                  package = package
                  module_ = module_
                  function_ = function_
                  version = version }
            } }

  let roundtrip (a : PT.FQFnName.T) : bool = a.ToString() |> PT.FQFnName.parse .=. a

  let tests =
    [ testPropertyWithGenerator typeof<Generator> "roundtripping FQFnName" roundtrip ]


module OCamlInterop =
  open OCamlInterop.Convert
  open OCamlInterop
  open Json.AutoSerialize

  type Generator =
    static member Expr() =
      Arb.Default.Derive()
      |> Arb.mapFilter
           (function
           // make sure we get numbers in our floats
           | other -> other)
           (function
           // characters are not yet supported in OCaml
           | PT.ECharacter _ -> false
           | other -> true)

    static member Pattern() =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           // characters are not yet supported in OCaml
           | PT.PCharacter _ -> false
           | _ -> true)

    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter nonNullString


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
    |> ocamlHandler2PT a.pos
    |> serialize
    |> deserialize
    |> pt2ocamlHandler
    |> serialize
    |> deserialize
    |> ocamlHandler2PT a.pos
    |> serialize
    |> deserialize
    .=. a

  let binaryHandlerRoundtrip (a : PT.Handler.T) : bool =
    let h = PT.TLHandler a

    h |> toplevelToCachedBinary |> (fun bin -> bin, None) |> toplevelOfCachedBinary
    .=. h

  let binaryExprRoundtrip (pair : PT.Expr * tlid) : bool =
    pair |> exprTLIDPairToCachedBinary |> exprTLIDPairOfCachedBinary .=. pair

  let tests =
    let tp f = testPropertyWithGenerator typeof<Generator> f

    [ tp "roundtripping OCamlInteropBinaryHandler" binaryHandlerRoundtrip
      tp "roundtripping OCamlInteropBinaryExpr" binaryExprRoundtrip
      tp "roundtripping OCamlInteropYojsonHandler" yojsonHandlerRoundtrip
      tp "roundtripping OCamlInteropYojsonExpr" yojsonExprRoundtrip ]

module RoundtrippableDval =
  type StrictGenerator =
    static member String() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter (DvalRepr.isRoundtrippableDval true)

  type NonStrictGenerator =
    static member String() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter (DvalRepr.isRoundtrippableDval false)

  let roundtrip (dv : RT.Dval) : bool =
    dv
    |> DvalRepr.toInternalRoundtrippableV0
    |> DvalRepr.ofInternalRoundtrippableV0
    |> dvalEquality dv

  let roundtrippableWorks (dv : RT.Dval) : bool =
    try
      // here we need to be able to read the data OCaml generates and OCaml needs to be able to read the data F# generates. But, both of those are buggy. So if they produce the same string it's fine at least.
      // either: we get the same string both ways, or we can read in both directions
      let fsString = dv |> DvalRepr.toInternalRoundtrippableV0
      let ocamlString = dv |> OCamlInterop.toInternalRoundtrippableV0

      let fsCanReadOCaml =
        fsString |> OCamlInterop.ofInternalRoundtrippableV0 |> dvalEquality dv

      let ocamlCanReadFS =
        ocamlString |> DvalRepr.ofInternalRoundtrippableV0 |> dvalEquality dv

      let theyCanReadEachOthersText = ocamlCanReadFS && fsCanReadOCaml

      let theyMakeTheSameMistakes =
        OCamlInterop.ofInternalRoundtrippableV0 ocamlString
        |> dvalEquality (DvalRepr.ofInternalRoundtrippableV0 fsString)

      if theyCanReadEachOthersText || theyMakeTheSameMistakes then
        true
      else
        printfn
          "%s"
          ($"theyCanReadEachOthersText: {theyCanReadEachOthersText}\n"
           + $"theyMakeTheSameMistakes: {theyMakeTheSameMistakes}\n")

        false
    with e ->
      printfn $"Cause exception while fuzzing {e}"
      reraise ()

  let tests =
    [ testPropertyWithGenerator
        typeof<StrictGenerator>
        "roundtripping works properly"
        roundtrip
      testPropertyWithGenerator
        typeof<NonStrictGenerator>
        "roundtrippable works the same as the OCaml version"
        roundtrippableWorks ]


module Queryable =
  open FsCheck

  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter DvalRepr.isQueryableDval

  let dvalReprInternalQueryableV0Roundtrip (dv : RT.Dval) : bool =
    dv
    |> DvalRepr.toInternalQueryableV0
    |> DvalRepr.ofInternalQueryableV0
    |> dvalEquality dv

  let dvalReprInternalQueryableV1Roundtrip (dvm : RT.DvalMap) : bool =
    dvm
    |> DvalRepr.toInternalQueryableV1
    |> DvalRepr.ofInternalQueryableV1
    |> dvalEquality (RT.DObj dvm)

  let tests =
    let tp f = testPropertyWithGenerator typeof<Generator> f

    [ tp "roundtripping InternalQueryable v0" dvalReprInternalQueryableV0Roundtrip
      tp "roundtripping InternalQueryable v1" dvalReprInternalQueryableV1Roundtrip ]

module DeveloperRepr =
  open FsCheck

  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    // The format here is only used for errors so it doesn't matter all the
    // much. These are places where we've manually checked the differing
    // outputs are fine.

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           | RT.DFnVal _ -> false
           | RT.DFloat 0.0 -> false
           | RT.DFloat infinity -> false
           | _ -> true)


  let equalsOCaml (dv : RT.Dval) : bool =
    DvalRepr.toDeveloperReprV0 dv .=. OCamlInterop.toDeveloperRepr dv

  let tests =
    [ testPropertyWithGenerator
        typeof<Generator>
        "roundtripping toDeveloperRepr"
        equalsOCaml ]



let tests =
  testList
    "FuzzTests"
    (List.concat [ FQFnName.tests
                   OCamlInterop.tests
                   RoundtrippableDval.tests
                   Queryable.tests
                   DeveloperRepr.tests ])

[<EntryPoint>]
let main args =
  LibBackend.OCamlInterop.Binary.init ()
  runTestsWithCLIArgs [] args tests
