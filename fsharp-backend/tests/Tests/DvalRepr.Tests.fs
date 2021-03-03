module Tests.DvalRepr

open Expecto
open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr


let testInternalRoundtrippableDoesntCareAboutOrder =
  test "internal_roundtrippable doesn't care about key order" {
    Expect.equal
      (DvalRepr.ofInternalRoundtrippableV0
        "{
           \"type\": \"option\",
           \"value\": 5
          }")
      (DvalRepr.ofInternalRoundtrippableV0
        "{
           \"value\": 5,
           \"type\": \"option\"
          }")
      ""
  }


let testDvalRoundtrippableRoundtrips =
  testMany
    "special roundtrippable dvals roundtrip"
    FuzzTests.All.Roundtrippable.roundtrip
    [ RT.DObj(
        Map.ofList [ ("", RT.DFloat 1.797693135e+308)
                     ("a", RT.DErrorRail(RT.DFloat nan)) ]
      ),
      true ]


let testDvalOptionQueryableSpecialCase =
  test "dval Option Queryable Special Case" {
    let dvm = Map.ofList [ ("type", RT.DStr "option"); ("value", RT.DInt 5I) ]

    Expect.equal
      (RT.DObj dvm)
      (dvm |> DvalRepr.toInternalQueryableV1 |> DvalRepr.ofInternalQueryableV1)
      "extra"
  }

let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        DvalRepr.toDeveloperReprV0
        // Most of this is just the OCaml output and not really what the output should be
        [ RT.DHttpResponse(RT.Response(0, []), RT.DNull), "0 {  }\nnull"
          RT.DFloat(-0.0), "-0."
          RT.DFloat(infinity), "inf"
          RT.DObj(Map.ofList [ "", RT.DNull ]), "{ \n  : null\n}"
          RT.DList [ RT.DNull ], "[ \n  null\n]" ] ]

let testToEnduserReadable =
  testList
    "enduserReadable"
    [ testMany
        "toEnduserReadable string"
        DvalRepr.toEnduserReadableTextV0
        // Most of this is just the OCaml output and not really what the output should be
        [ RT.DFloat(0.0), "0." // this type of thing in particular is ridic
          RT.DFloat(-0.0), "-0."
          RT.DFloat(5.0), "5."
          RT.DFloat(5.1), "5.1"
          RT.DFloat(-5.0), "-5."
          RT.DFloat(-5.1), "-5.1"
          RT.DError(RT.SourceNone, "Some message"), "Error: Some message"
          RT.DHttpResponse(RT.Redirect("some url"), RT.DNull), "302 some url\nnull"
          RT.DHttpResponse(RT.Response(0, [ "a header", "something" ]), RT.DNull),
          "0 { a header: something }\nnull" ] ]

module F = FuzzTests.All

let allRoundtrips =
  let t = testListUsingProperty

  let all =
    // interoperable tests do not support passwords because it's very
    // hard/risky to get ocaml to roundtrip them correctly without compromising
    // the redaction protections. We do password tests in the rest of the file
    // so lets not confuse these tests.
    TestUtils.sampleDvals
    |> List.filter
         (function
         | (_, RT.DPassword _) -> false
         | _ -> true)

  let dvs (filter : RT.Dval -> bool) = List.filter (fun (_, dv) -> filter dv) all

  testList
    "roundtrips"
    [ t
        "roundtrippable"
        F.Roundtrippable.roundtrip
        (dvs (DvalRepr.isRoundtrippableDval false))
      t
        "roundtrippable interop"
        F.Roundtrippable.isInteroperableV0
        (dvs (DvalRepr.isRoundtrippableDval false))
      t "queryable v0" F.Queryable.v1Roundtrip (dvs DvalRepr.isQueryableDval)
      t
        "queryable interop v0"
        F.Queryable.isInteroperableV0
        (dvs (DvalRepr.isQueryableDval))
      t
        "queryable interop v1"
        F.Queryable.isInteroperableV1
        (dvs DvalRepr.isQueryableDval)
      t "enduserReadable" F.EndUserReadable.equalsOCaml all
      t "developerRepr" F.DeveloperRepr.equalsOCaml all
      t "prettyMachineJson" F.PrettyMachineJson.equalsOCaml all ]

// let testDateMigrationHasCorrectFormats () =
//   let str = "2019-03-08T08:26:14Z" in
//   let date = RT.DDate(System.DateTime.ofIsoString str) in
//   let oldFormat = $"{{ \"type\": \"date\", \"value\": \"{str}\"}}"
//   Expect.equal (Legacy.toPrettyMachineJsonStringV0 date) oldFormat "old version"
//   Expect.equal (DvalRepr.toPrettyMachineJsonStringV1 date) $"\"{str}\"" "new version"
//

module Password =
  let testJsonRoundtripForwards =
    test "json roundtrips forward" {
      let password = RT.DPassword(Password (toBytes "x"))

      Expect.equalDval
        password
        (password
         |> DvalRepr.toInternalRoundtrippableV0
         |> DvalRepr.ofInternalRoundtrippableV0
         |> DvalRepr.toInternalRoundtrippableV0
         |> DvalRepr.ofInternalRoundtrippableV0)
        "Passwords serialize and deserialize if there's no redaction."
    }

  let testSerialization =
    test "password serialization" {
      let testSerialize expected name f =
        let bytes = toBytes "encryptedbytes"
        let password = RT.DPassword (Password bytes)

        Expect.equal
          expected
          (String.includes ("encryptedbytes" |> toBytes |> base64Encode) (f password))
          ($"Passwords serialize in non-redaction function: {name}")

      let doesSerialize = testSerialize true
      let doesntSerialize = testSerialize false

      let roundtrips name serialize deserialize =
        let bytes = toBytes "encryptedbytes"
        let password = RT.DPassword (Password bytes)

        Expect.equalDval
          password
          (password |> serialize |> deserialize |> serialize |> deserialize)
          $"Passwords serialize in non-redaction function: {name}"

      // doesn't redact
      doesSerialize "toInternalRoundtrippableV0" DvalRepr.toInternalRoundtrippableV0

      // roundtrips
      roundtrips
        "toInternalRoundtrippableV0 roundtrips"
        DvalRepr.toInternalRoundtrippableV0
        DvalRepr.ofInternalRoundtrippableV0

      // redacting
      doesntSerialize "toEnduserReadableTextV0" DvalRepr.toEnduserReadableTextV0
      doesntSerialize "toDeveloperReprV0" DvalRepr.toDeveloperReprV0
      doesntSerialize "toPrettyMachineJsonV1" DvalRepr.toPrettyMachineJsonStringV1
    // FSTODO
    //   doesSerialize
    //     "toPrettyRequestJsonV0"
    //     false
    //     Libexecution.Legacy.PrettyRequestJsonV0.toPrettyRequestJsonV0 ;
    //   doesSerialize
    //     "toPrettyResponseJsonV1"
    //     false
    //     Libexecution.Legacy.PrettyResponseJsonV0.toPrettyResponseJsonV0 ;
    //   ()
    }

  let testSerialization2 =
    test "serialization in object" {
      let roundtrips name serialize deserialize =
        let bytes = toBytes "encryptedbytes" in
        let password = RT.DObj(Map.ofList [ "x", RT.DPassword (Password bytes) ])

        let wrappedSerialize dval =
          dval
          |> (function
          | RT.DObj dvalMap -> dvalMap
          | _ -> failwith "dobj only here")
          |> serialize

        Expect.equalDval
          password
          (password
           |> wrappedSerialize
           |> deserialize
           |> wrappedSerialize
           |> deserialize)
          $"Passwords serialize in non-redaction function: {name}"
      // roundtrips
      roundtrips
        "toInternalQueryableV1"
        DvalRepr.toInternalQueryableV1
        DvalRepr.ofInternalQueryableV1
    }

  let testNoAutoSerialization =
    test "no auto serialization of passwords" {
      let mutable success = false

      try
        Json.AutoSerialize.serialize (RT.DPassword (Password [||])) |> ignore
      with e -> success <- true

      Expect.equal success true "success should be true"
    }

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
      testInternalRoundtrippableDoesntCareAboutOrder
      testDvalOptionQueryableSpecialCase
      testToDeveloperRepr
      testToEnduserReadable
      Password.tests
      allRoundtrips ]
