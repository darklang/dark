module Tests.DvalRepr

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module CRT = LibAnalysis.ClientRuntimeTypes

module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module DvalReprInternalQueryable = LibExecution.DvalReprInternalQueryable
module DvalReprInternalRoundtrippable = LibExecution.DvalReprInternalRoundtrippable
module DvalReprInternalHash = LibExecution.DvalReprInternalHash
module Errors = LibExecution.Errors
module S = TestUtils.RTShortcuts

let roundtrippableRoundtripsSuccessfully (dv : RT.Dval) : bool =
  dv
  |> DvalReprInternalRoundtrippable.toJsonV0
  |> DvalReprInternalRoundtrippable.parseJsonV0
  |> Expect.dvalEquality dv

let queryableRoundtripsSuccessfullyInRecord
  (
    dv : RT.Dval,
    fieldTyp : RT.TypeReference
  ) : bool =

  let typeName = S.userTypeName [] "MyType" 0
  let record = RT.DRecord(typeName, Map.ofList [ "field", dv ])
  let typeRef = S.userTypeReference [] "MyType" 0

  let availableTypes = Map [ typeName, S.customTypeRecord [ "field", fieldTyp ] ]

  record
  |> DvalReprInternalQueryable.toJsonStringV0 availableTypes typeRef
  |> DvalReprInternalQueryable.parseJsonV0 availableTypes typeRef
  |> Expect.dvalEquality record

let queryableRoundtripsSuccessfully (dv : RT.Dval, typ : RT.TypeReference) : bool =
  dv
  |> DvalReprInternalQueryable.toJsonStringV0 Map.empty typ
  |> DvalReprInternalQueryable.parseJsonV0 Map.empty typ
  |> Expect.dvalEquality dv

let testDvalRoundtrippableRoundtrips =

  testMany
    "special roundtrippable dvals roundtrip"
    roundtrippableRoundtripsSuccessfully
    [ RT.DDict(Map.ofList [ ("", RT.DFloat 1.797693135e+308); ("a", RT.DFloat nan) ]),
      true ]


let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        DvalReprDeveloper.toRepr
        [ RT.DFloat(-0.0), "-0.0"
          RT.DFloat(infinity), "Infinity"
          RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ]), "(\n  1, 2, 3\n)"
          RT.DDict(Map.ofList [ "", RT.DUnit ]), "{\n  : unit\n}"
          RT.DList [ RT.DUnit ], "[\n  unit\n]" ] ]

// We used a System.Text.Json converter supplied by a NuGet package for a bit,
// but found that it was incompatible with the OCamlCompatible serializer. We
// have since adjusted `Vanilla` to use a custom converter, and this test is to
// ensure values serialized during the time where the NuGet package's converter
// are able to be deserialized.
let testPreviousDateSerializionCompatibility =
  test "previous date serialization compatible" {
    let expected =
      CRT.Dval.DDateTime(NodaTime.Instant.UnixEpoch.toUtcLocalTimeZone ())
    let actual =
      Json.Vanilla.deserialize<CRT.Dval.T> """["DDateTime","1970-01-01T00:00:00"]"""
    Expect.equal expected actual "not deserializing correctly"
  }

module ToHashableRepr =
  open LibExecution.RuntimeTypes

  let testHashV2 =
    let t (l : List<Dval>) (expected : string) : Test =
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
      [ t [ DBytes [||] ] "diEjLGQC8oE"; t [ DBytes [| 128uy |] ] "kQHs0urT3N4" ]

  let tests = testList "hashing" [ testHashV2 ]


let allRoundtrips =
  let t = testListUsingProperty

  let dvs (filter : RT.Dval -> bool) : List<string * (RT.Dval * RT.TypeReference)> =
    List.filter (fun (_, (dv, _)) -> filter dv) sampleDvals

  testList
    "roundtrips"
    [ t
        "roundtrippable"
        roundtrippableRoundtripsSuccessfully
        (dvs DvalReprInternalRoundtrippable.Test.isRoundtrippableDval
         |> List.map (fun (name, (v, _)) -> name, v))
      t
        "queryable v0"
        queryableRoundtripsSuccessfully
        (dvs DvalReprInternalQueryable.Test.isQueryableDval)
      t
        "queryable record v0"
        queryableRoundtripsSuccessfullyInRecord
        (dvs DvalReprInternalQueryable.Test.isQueryableDval)
      t
        "vanilla"
        (fun (dv, _) ->
          dv
          |> CRT.Dval.toCT
          |> Json.Vanilla.serialize
          |> Json.Vanilla.deserialize
          |> CRT.Dval.fromCT
          |> Expect.dvalEquality dv)
        (dvs (function
          | RT.DPassword _ -> false
          | _ -> true)) ]


let testInternalRoundtrippableNew =
  testList
    "internalNew"
    [ test "tuples serialize correctly" {
        let expected = """["DTuple",["DInt",1],["DInt",2],[["DInt",3]]]"""

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
          (String.includes
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
      doesRedact "Json.Vanilla.serialize" (fun dv ->
        dv |> CRT.Dval.toCT |> Json.Vanilla.serialize)
      ()
    }

  let testSerialization2 =
    test "serialization in object" {
      let bytes = UTF8.toBytes "encryptedbytes"
      let typeName = S.userTypeName [] "MyType" 0
      let password =
        RT.DRecord(typeName, Map.ofList [ "x", RT.DPassword(Password bytes) ])

      let typeRef = S.userTypeReference [] "MyType" 0

      let availableTypes = Map [ typeName, S.customTypeRecord [ "x", RT.TPassword ] ]


      let serialize = DvalReprInternalQueryable.toJsonStringV0 availableTypes typeRef
      let deserialize = DvalReprInternalQueryable.parseJsonV0 availableTypes typeRef
      Expect.equalDval
        password
        (password |> serialize |> deserialize |> serialize |> deserialize)
        "Passwords serialize in non-redaction function: toInternalQueryableV1"
    }

  let testNoAutoSerialization =
    testList
      "no auto serialization of passwords"
      [ test "vanilla" {
          let password =
            CRT.Dval.DPassword(Password(UTF8.toBytes "some password"))
            |> Json.Vanilla.serialize
            |> Json.Vanilla.deserialize

          Expect.equal
            password
            (CRT.Dval.DPassword(Password(UTF8.toBytes "Redacted")))
            "should be redacted"
        } ]



  let tests =
    testList
      "password"
      [ testJsonRoundtripForwards
        testSerialization
        testSerialization2
        testNoAutoSerialization ]

let tests =
  testList
    "dvalRepr"
    [ testDvalRoundtrippableRoundtrips
      testToDeveloperRepr
      ToHashableRepr.tests
      testInternalRoundtrippableNew
      Password.tests
      testPreviousDateSerializionCompatibility
      allRoundtrips ]
