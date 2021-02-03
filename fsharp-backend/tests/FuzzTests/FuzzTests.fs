module FuzzTests.All

// This aims to find test cases that violate certain properties that we expect.
// Desired properties include that OCaml Dark programs and functions work the
// same as F# ones, and things related to serialization and output.

open Expecto
open Expecto.ExpectoFsCheck

open Prelude

module PT = LibBackend.ProgramSerialization.ProgramTypes

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

let testProperty (name : string) (x : 'a) : Test =
  testPropertyWithConfig config name x


// Tests
// These tests are like this so they can be reused from LibBackend.Tests

let fqFnNameRoundtrip (a : PT.FQFnName.T) : bool =
  a.ToString() |> PT.FQFnName.parse .=. a

let ocamlInteropYojsonExprRoundtrip (a : PT.Expr) : bool =
  a
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.pt2ocamlExpr
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.ocamlExpr2PT
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.pt2ocamlExpr
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.ocamlExpr2PT
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  .=. a

let ocamlInteropYojsonHandlerRoundtrip (a : PT.Handler.T) : bool =
  a
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.pt2ocamlHandler
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.ocamlHandler2PT a.pos
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.pt2ocamlHandler
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  |> LibBackend.ProgramSerialization.OCamlInterop.Convert.ocamlHandler2PT a.pos
  |> Json.AutoSerialize.serialize
  |> Json.AutoSerialize.deserialize
  .=. a

let ocamlInteropBinaryHandlerRoundtrip (a : PT.Handler.T) : bool =
  let h = PT.TLHandler a

  h
  |> LibBackend.ProgramSerialization.OCamlInterop.toplevelToCachedBinary
  |> fun bin -> bin, None
  |> LibBackend.ProgramSerialization.OCamlInterop.toplevelOfCachedBinary
  .=. h

let ocamlInteropBinaryExprRoundtrip (pair : PT.Expr * tlid) : bool =
  pair
  |> LibBackend.ProgramSerialization.OCamlInterop.exprTLIDPairToCachedBinary
  |> LibBackend.ProgramSerialization.OCamlInterop.exprTLIDPairOfCachedBinary
  .=. pair




let roundtrips =
  testList
    "roundtripping"
    [ testProperty
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

let tests = testList "FuzzTests" [ roundtrips ]

[<EntryPoint>]
let main args =
  LibBackend.ProgramSerialization.OCamlInterop.Binary.init ()
  runTestsWithCLIArgs [] args tests
