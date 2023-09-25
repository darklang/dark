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
    let typeName = S.userTypeName [] "MyType" 0
    let record =
      RT.DRecord(
        RT.FQName.UserProgram typeName,
        RT.FQName.UserProgram typeName,
        VT.typeArgsTODO,
        Map.ofList [ "field", dv ]
      )
    let typeRef = S.userTypeReference [] "MyType" 0

    let types : RT.Types =
      { defaultTypes () with
          userProgram =
            Map
              [ typeName,
                { name = typeName
                  tlid = 8UL
                  declaration = S.customTypeRecord [ "field", fieldTyp ] } ] }


    let! roundtripped =
      record
      |> DvalReprInternalQueryable.toJsonStringV0 types typeRef
      |> Ply.bind (DvalReprInternalQueryable.parseJsonV0 types typeRef)

    return Expect.dvalEquality record roundtripped
  }

let queryableRoundtripsSuccessfully
  (
    dv : RT.Dval,
    typ : RT.TypeReference
  ) : Task<bool> =
  task {
    let! serialized =
      DvalReprInternalQueryable.toJsonStringV0 (defaultTypes ()) typ dv
    let! roundtripped =
      DvalReprInternalQueryable.parseJsonV0 (defaultTypes ()) typ serialized
    return Expect.dvalEquality dv roundtripped
  }


let testDvalRoundtrippableRoundtrips =
  testMany
    "special roundtrippable dvals roundtrip"
    roundtrippableRoundtripsSuccessfully
    [ Dval.dict
        VT.unknownTODO
        [ ("", RT.DFloat 1.797693135e+308); ("a", RT.DFloat nan) ],
      true ]


let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        DvalReprDeveloper.toRepr
        [ RT.DFloat(-0.0), "-0.0"
          RT.DFloat(infinity), "Infinity"
          RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ]), "(1, 2, 3)"
          Dval.dict VT.unknownTODO [ "", RT.DUnit ], "{\n  : ()\n}"
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
      [ t (NEList.singleton (DBytes [||])) "X1YnxJLFsVg"
        t (NEList.singleton (DBytes [| 128uy |])) "Hj0nqyrvXis" ]

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
        let expected = """{"DTuple":[{"DInt":[1]},{"DInt":[2]},[{"DInt":[3]}]]}"""

        let actual =
          RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ])
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
