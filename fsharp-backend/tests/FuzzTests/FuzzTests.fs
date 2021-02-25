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

let (.=.) actual expected : bool =
  (if actual = expected then
     true
   else
     printfn $"Expected:\n{expected}\n but got:\n{actual}"
     false)


// This allows us to control the values of the types that are generated. We can
// write our own generators, or filter existing ones. To add new type
// generators, add new static members
module DarkFsCheck =
  open FsCheck

  let nameGenerator (first : char list) (other : char list) : Gen<string> =
    gen {
      let! length = Gen.choose (0, 20)
      let! head = Gen.elements first
      let! tail = Gen.arrayOfLength length (Gen.elements other)
      return System.String(Array.append [| head |] tail)
    }

  type MyGenerators =
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
      Arb.Default.String() |> Arb.filter (fun (s : string) -> s <> null)

    static member FQFnType() =
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

let config : FsCheckConfig =
  { FsCheckConfig.defaultConfig with
      maxTest = 10000
      arbitrary = [ typeof<DarkFsCheck.MyGenerators> ] }

let configWithGenerator (typ : System.Type) : FsCheckConfig =
  { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [ typ ] }

let testProperty (name : string) (x : 'a) : Test =
  testPropertyWithConfig config name x

let testPropertyWithGenerator (typ : System.Type) (name : string) (x : 'a) : Test =
  testPropertyWithConfig (configWithGenerator typ) name x

// Tests
// These tests are like this so they can be reused from LibBackend.Tests

let fqFnNameRoundtrip (a : PT.FQFnName.T) : bool =
  a.ToString() |> PT.FQFnName.parse .=. a

let ocamlInteropYojsonExprRoundtrip (a : PT.Expr) : bool =
  a
  |> OCamlInterop.Convert.pt2ocamlExpr
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> OCamlInterop.Convert.ocamlExpr2PT
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> OCamlInterop.Convert.pt2ocamlExpr
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> OCamlInterop.Convert.ocamlExpr2PT
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  .=. a

let ocamlInteropYojsonHandlerRoundtrip (a : PT.Handler.T) : bool =
  a
  |> OCamlInterop.Convert.pt2ocamlHandler
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> OCamlInterop.Convert.ocamlHandler2PT a.pos
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> OCamlInterop.Convert.pt2ocamlHandler
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> OCamlInterop.Convert.ocamlHandler2PT a.pos
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  .=. a

let ocamlInteropBinaryHandlerRoundtrip (a : PT.Handler.T) : bool =
  let h = PT.TLHandler a

  h
  |> OCamlInterop.toplevelToCachedBinary
  |> fun bin -> bin, None
  |> OCamlInterop.toplevelOfCachedBinary
  .=. h

let ocamlInteropBinaryExprRoundtrip (pair : PT.Expr * tlid) : bool =
  pair
  |> OCamlInterop.exprTLIDPairToCachedBinary
  |> OCamlInterop.exprTLIDPairOfCachedBinary
  .=. pair

module RoundtrippableDval =
  open FsCheck

  type RoundtrippableDvalGenerator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter (fun (s : string) -> s <> null)

    static member RoundtrippableDvals() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           | RT.DFnVal _ -> false
           | RT.DChar "" -> false // Invalid value
           | _ -> true)

  let dvalReprInternalRoundtrippableV1Roundtrip (dv : RT.Dval) : bool =
    dv
    |> DvalRepr.toInternalRoundtrippableV0
    |> DvalRepr.ofInternalRoundtrippableV0
    |> dvalEquality dv

  let roundtrippableWorks (dv : RT.Dval) : bool =
    try
      // here we need to be able to read the data OCaml generates and OCaml needs to be able to read the data F# generates. But, both of those are buggy. So if they produce the same string it's fine at least.
      // either: we get the same string both ways, or we can read in both directions
      let fsString =
        dv
        |> debug "fuzzing roundtrippable works on value"
        |> DvalRepr.toInternalRoundtrippableV0
        |> debug "as string by f#"

      let fsCanReadOCaml =
        fsString
        |> OCamlInterop.ofInternalRoundtrippableV0
        |> debug "converted by OCaml"
        |> dvalEquality dv

      let ocamlString =
        dv |> OCamlInterop.toInternalRoundtrippableV0 |> debug "as string by ocaml"

      let ocamlCanReadFS =
        ocamlString
        |> DvalRepr.ofInternalRoundtrippableV0
        |> debug "converted by F#"
        |> dvalEquality dv

      let theyMakeTheSameMistakes =
        OCamlInterop.ofInternalRoundtrippableV0 ocamlString
        |> dvalEquality (DvalRepr.ofInternalRoundtrippableV0 fsString)

      let theyCanReadEachOthersText = ocamlCanReadFS && fsCanReadOCaml
      let theyGenerateTheSameString = fsString = ocamlString

      if theyCanReadEachOthersText
         || theyGenerateTheSameString
         || theyMakeTheSameMistakes then
        true
      else
        printfn
          "%s"
          ($"theyCanReadEachOthersText: {theyCanReadEachOthersText}\n"
           + $"theyGenerateTheSameString: {theyGenerateTheSameString}\n"
           + $"theyMakeTheSameMistakes: {theyMakeTheSameMistakes}\n")

        false
    with e ->
      printfn $"Cause exception while fuzzing {e}"
      reraise ()




  let tests =
    [ testPropertyWithGenerator
        typeof<RoundtrippableDvalGenerator>
        "roundtripping InternalRoundtrippable v0"
        dvalReprInternalRoundtrippableV1Roundtrip
      testPropertyWithGenerator
        typeof<RoundtrippableDvalGenerator>
        "roundtrippable works"
        roundtrippableWorks ]




let roundtrips =
  testList
    "roundtripping"
    ([ testProperty
         "roundtripping OCamlInteropBinaryHandler"
         ocamlInteropBinaryHandlerRoundtrip
       testProperty
         "roundtripping OCamlInteropBinaryExpr"
         ocamlInteropBinaryExprRoundtrip
       testProperty
         "roundtripping OCamlInteropYojsonHandler"
         ocamlInteropYojsonHandlerRoundtrip
       testProperty
         "roundtripping OCamlInteropYojsonExpr"
         ocamlInteropYojsonExprRoundtrip
       testProperty "roundtripping FQFnName" fqFnNameRoundtrip ]
     @ RoundtrippableDval.tests)

let tests = testList "FuzzTests" [ roundtrips ]

[<EntryPoint>]
let main args =
  LibBackend.OCamlInterop.Binary.init ()
  runTestsWithCLIArgs [] args tests
