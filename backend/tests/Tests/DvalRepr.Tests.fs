module Tests.DvalRepr

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module DvalUtils = LibExecution.DvalUtils
module PT = LibExecution.ProgramTypes

module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
module DvalReprInternalHash = LibExecution.DvalReprInternalHash
module Errors = LibExecution.Errors
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
        RT.valueTypesTODO,
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
    [ DvalUtils.dict [ ("", RT.DFloat 1.797693135e+308); ("a", RT.DFloat nan) ], true ]


let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        DvalReprDeveloper.toRepr
        [ RT.DFloat(-0.0), "-0.0"
          RT.DFloat(infinity), "Infinity"
          RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ]), "(1, 2, 3)"
          DvalUtils.dict [ "", RT.DUnit ], "{\n  : ()\n}"
          RT.DList(RT.ValueType.Known RT.KTUnit, [ RT.DUnit ]), "[\n  ()\n]" ] ]

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

module Password =
  let testJsonRoundtripForwards =
    test "json roundtrips forward" {
      let password = RT.DPassword(Password(UTF8.toBytes "x"))

      Expect.equalDval
        password
        (password
         |> DvalReprInternalRoundtrippable.toJsonV0
         |> DvalReprInternalRoundtrippable.parseJsonV0
         |> DvalReprInternalRoundtrippable.toJsonV0
         |> DvalReprInternalRoundtrippable.parseJsonV0)
        "Passwords serialize and deserialize if there's no redaction."
    }

  let testSerialization =
    test "password serialization" {
      let testSerialize shouldRedact name f =
        let bytes = UTF8.toBytes "encryptedbytes"
        let password = RT.DPassword(Password bytes)
        let allowed =
          if shouldRedact then
            "should redact password but doesn't"
          else
            "shouldn't redact password but does"

        Expect.equal
          shouldRedact
          (String.contains
            ("encryptedbytes" |> UTF8.toBytes |> Base64.urlEncodeToString)
            (f password)
           |> not)
          $"{name} {allowed}"

      let doesntRedact = testSerialize false
      let doesRedact = testSerialize true

      let roundtrips name serialize deserialize =
        let bytes = UTF8.toBytes "encryptedbytes"
        let password = RT.DPassword(Password bytes)
        Expect.equalDval
          password
          (password |> serialize |> deserialize |> serialize |> deserialize)
          $"Passwords serialize in non-redaction function: {name}"

      // doesn't redact
      doesntRedact
        "toInternalRoundtrippableV0"
        DvalReprInternalRoundtrippable.toJsonV0

      // roundtrips
      roundtrips
        "toInternalRoundtrippableV0 roundtrips"
        DvalReprInternalRoundtrippable.toJsonV0
        DvalReprInternalRoundtrippable.parseJsonV0

      // redacting
      doesRedact "toDeveloperReprV0" DvalReprDeveloper.toRepr
    }

  let testSerialization2 =
    testTask "serialization in object" {
      let bytes = UTF8.toBytes "encryptedbytes"
      let typeName = S.userTypeName [] "MyType" 0
      let password =
        RT.DRecord(
          RT.FQName.UserProgram typeName,
          RT.FQName.UserProgram typeName,
          RT.valueTypesTODO,
          Map.ofList [ "x", RT.DPassword(Password bytes) ]
        )

      let typeRef = S.userTypeReference [] "MyType" 0

      let availableTypes =
        { RT.Types.empty with
            userProgram =
              Map
                [ typeName,
                  { tlid = 8UL
                    name = typeName
                    declaration = S.customTypeRecord [ "x", RT.TPassword ] } ] }


      let serialize = DvalReprInternalQueryable.toJsonStringV0 availableTypes typeRef
      let deserialize = DvalReprInternalQueryable.parseJsonV0 availableTypes typeRef
      let! roundtripped =
        task {
          let! serialized1 = serialize password
          let! deserialized1 = deserialize serialized1
          let! serialized2 = serialize deserialized1
          let! deserialized2 = deserialize serialized2
          return deserialized2
        }

      Expect.equalDval
        password
        roundtripped
        "Passwords serialize in non-redaction function: toInternalQueryableV1"
    }



  let tests =
    testList
      "password"
      [ testJsonRoundtripForwards; testSerialization; testSerialization2 ]

let tests =
  testList
    "dvalRepr"
    [ testDvalRoundtrippableRoundtrips
      testToDeveloperRepr
      ToHashableRepr.tests
      testInternalRoundtrippableNew
      Password.tests
      allRoundtrips ]
