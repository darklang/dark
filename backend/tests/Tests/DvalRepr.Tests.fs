module Tests.DvalRepr

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes

module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
module DvalReprInternalHash = LibExecution.DvalReprInternalHash
module S = TestUtils.RTShortcuts


let bogusCallStack = RT.CallStack.fromEntryPoint RT.Script

let defaultTypes () = { RT.Types.empty with package = packageManager.getType }

let roundtrippableRoundtripsSuccessfully (dv : RT.Dval) : bool =
  dv
  |> DvalReprInternalRoundtrippable.toJsonV0
  |> DvalReprInternalRoundtrippable.parseJsonV0
  |> Expect.dvalEquality dv

let queryableRoundtripsSuccessfullyInRecord
  (
    dv : RT.Dval,
    fieldTyp : RT.TypeReference
  ) : Task<bool> =

  task {
    let typeName = S.packageTypeName "Tests" [] "MyType"
    let record =
      RT.DRecord(
        RT.FQTypeName.Package typeName,
        RT.FQTypeName.Package typeName,
        [],
        Map.ofList [ "field", dv ]
      )
    let typeRef = S.packageTypeReference "Tests" [] "MyType"

    let types : RT.Types =
      { typeSymbolTable = Map.empty
        package =
          fun name ->
            if name = typeName then
              let packageType : RT.PackageType.T =
                { name = typeName
                  id = System.Guid.NewGuid()
                  declaration = S.customTypeRecord [ "field", fieldTyp ] }
              packageType |> Some |> Ply
            else
              packageManager.getType name }

    let! roundtripped =
      record
      |> DvalReprInternalQueryable.toJsonStringV0 bogusCallStack types typeRef
      |> Ply.bind (
        DvalReprInternalQueryable.parseJsonV0 bogusCallStack types typeRef
      )

    return Expect.dvalEquality record roundtripped
  }

let queryableRoundtripsSuccessfully
  (
    dv : RT.Dval,
    typ : RT.TypeReference
  ) : Task<bool> =
  task {
    let! serialized =
      DvalReprInternalQueryable.toJsonStringV0
        bogusCallStack
        (defaultTypes ())
        typ
        dv
    let! roundtripped =
      DvalReprInternalQueryable.parseJsonV0
        bogusCallStack
        (defaultTypes ())
        typ
        serialized
    return Expect.dvalEquality dv roundtripped
  }


let testDvalRoundtrippableRoundtrips =
  testMany
    "special roundtrippable dvals roundtrip"
    roundtrippableRoundtripsSuccessfully
    [ RT.DDict(
        VT.float,
        Map [ ("", RT.DFloat 1.797693135e+308); ("a", RT.DFloat nan) ]
      ),
      true ]


let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        DvalReprDeveloper.toRepr
        [ RT.DFloat(-0.0), "-0.0"
          RT.DFloat(infinity), "Infinity"
          RT.DTuple(RT.DInt64 1, RT.DInt64 2, [ RT.DInt64 3 ]), "(1, 2, 3)"
          RT.DDict(VT.unit, Map [ "", RT.DUnit ]), "{\n  : ()\n}"
          RT.DList(VT.unit, [ RT.DUnit ]), "[\n  ()\n]" ] ]

module ToHashableRepr =
  open LibExecution.RuntimeTypes

  let testHashV2 =
    let t (l : NEList<Dval>) (expected : string) : Test =
      testTask $"hashV2: {l}" {
        let actual = DvalReprInternalHash.hash 2 l

        if actual <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print $"expected: {p expected}"
          print $"fsharp  : {p actual}"

        Expect.equal actual expected "bad fsharp impl"
      }

    testList
      "hashv2"
      [ t (NEList.singleton (DList(VT.uint8, []))) "DEux3mJnJPs"
        t
          (NEList.singleton (
            DList(VT.uint8, List.map (fun i -> DUInt8(uint8 i)) [ 128uy ])
          ))
          "cE2FaQ8GKZU" ]

  let tests = testList "hashing" [ testHashV2 ]


let allRoundtrips =
  let dvs (filter : RT.Dval -> bool) : List<string * (RT.Dval * RT.TypeReference)> =
    List.filter (fun (_, (dv, _)) -> filter dv) sampleDvals

  testList
    "roundtrips"
    [ testListUsingProperty
        "roundtrippable"
        roundtrippableRoundtripsSuccessfully
        (dvs DvalReprInternalRoundtrippable.Test.isRoundtrippableDval
         |> List.map (fun (name, (v, _)) -> name, v))

      testListUsingPropertyAsync
        "queryable v0"
        queryableRoundtripsSuccessfully
        (dvs DvalReprInternalQueryable.Test.isQueryableDval)

      testListUsingPropertyAsync
        "queryable record v0"
        queryableRoundtripsSuccessfullyInRecord
        (dvs DvalReprInternalQueryable.Test.isQueryableDval) ]


let testInternalRoundtrippableNew =
  testList
    "internalNew"
    [ test "tuples serialize correctly" {
        let expected =
          """{"DTuple":[{"DInt64":[1]},{"DInt64":[2]},[{"DInt64":[3]}]]}"""

        let actual =
          RT.DTuple(RT.DInt64 1, RT.DInt64 2, [ RT.DInt64 3 ])
          |> DvalReprInternalRoundtrippable.toJsonV0

        Expect.equal actual expected ""
      } ]

let tests =
  testList
    "dvalRepr"
    [ testDvalRoundtrippableRoundtrips
      testToDeveloperRepr
      ToHashableRepr.tests
      testInternalRoundtrippableNew
      allRoundtrips ]
