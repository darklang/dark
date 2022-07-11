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

module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated
module DvalReprInternalNew = LibExecution.DvalReprInternalNew
module Errors = LibExecution.Errors

let testInternalRoundtrippableDoesntCareAboutOrder =
  test "internal_roundtrippable doesn't care about key order" {
    Expect.equal
      (DvalReprInternalDeprecated.ofInternalRoundtrippableV0
        "{
           \"type\": \"option\",
           \"value\": 5
          }")
      (DvalReprInternalDeprecated.ofInternalRoundtrippableV0
        "{
           \"value\": 5,
           \"type\": \"option\"
          }")
      ""
  }




let testDvalRoundtrippableRoundtrips =
  testMany
    "special roundtrippable dvals roundtrip"
    FuzzTests.InternalJson.Roundtrippable.roundtripsSuccessfully
    [ RT.DObj(
        Map.ofList [ ("", RT.DFloat 1.797693135e+308)
                     ("a", RT.DErrorRail(RT.DFloat nan)) ]
      ),
      true ]


let testDvalOptionQueryableSpecialCase =
  test "dval Option Queryable Special Case" {
    let dvm = Map.ofList [ ("type", RT.DStr "option"); ("value", RT.DInt 5L) ]

    Expect.equal
      (RT.DObj dvm)
      (dvm
       |> DvalReprInternalDeprecated.toInternalQueryableV1
       |> DvalReprInternalDeprecated.ofInternalQueryableV1)
      "extra"
  }

let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        DvalReprDeveloper.toRepr
        // Most of this is just the OCaml output and not really what the output should be
        [ RT.DHttpResponse(RT.Response(0L, [], RT.DNull)), "0 {  }\nnull"
          RT.DFloat(-0.0), "-0."
          RT.DFloat(infinity), "inf"
          RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ]), "(\n  1, 2, 3\n)"
          RT.DObj(Map.ofList [ "", RT.DNull ]), "{ \n  : null\n}"
          RT.DList [ RT.DNull ], "[ \n  null\n]" ] ]

let testToEnduserReadable =
  testMany
    "toEnduserReadable string"
    DvalReprLegacyExternal.toEnduserReadableTextV0
    // Most of this is just the OCaml output and not really what the output should be
    [ RT.DFloat(0.0), "0." // this type of thing in particular is ridic
      RT.DFloat(-0.0), "-0."
      RT.DFloat(5.0), "5."
      RT.DFloat(5.1), "5.1"
      RT.DFloat(-5.0), "-5."
      RT.DFloat(-5.1), "-5.1"
      RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ]), "(\n  1, 2, 3\n)"
      RT.DError(RT.SourceNone, "Some message"), "Error"
      RT.DHttpResponse(RT.Redirect("some url")), "302 some url\nnull"
      RT.DHttpResponse(RT.Response(0L, [ "a header", "something" ], RT.DNull)),
      "0 { a header: something }\nnull" ]

let testToPrettyResponseJson =
  testMany
    "toPrettyResponseJson"
    LibExecutionStdLib.LibObject.PrettyResponseJsonV0.toPrettyResponseJsonV0
    [ RT.DBytes [| 00uy |], "{\n  \"type\": \"bytes\",\n  \"value\": \"\\u0000\"\n}"

      RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ]), "[\n  1,\n  2,\n  3\n]" ]


let testDateMigrationHasCorrectFormats =
  test "date migration has correct formats" {
    let str = "2019-03-08T08:26:14Z"
    let date =
      str |> NodaTime.Instant.ofIsoString |> RT.DDateTime.fromInstant |> RT.DDate
    let oldActual =
      LibExecutionStdLib.LibObject.PrettyResponseJsonV0.toPrettyResponseJsonV0 date
    let oldExpected =
      "{\n  \"type\": \"date\",\n  \"value\": \"2019-03-08T08:26:14Z\"\n}"
    Expect.equal oldActual oldExpected "old format"
    let newActual = DvalReprLegacyExternal.toPrettyMachineJsonStringV1 date
    Expect.equal newActual $"\"{str}\"" "old format"
  }

// We used a System.Text.Json converter supplied by a NuGet package for a bit,
// but found that it was incompatible with the OCamlCompatible serializer. We
// have since adjusted `Vanilla` to use a custom converter, and this test is to
// ensure values serialized during the time where the NuGet package's converter
// are able to be deserialized. The value here
let testPreviousDateSerializionCompatibility =
  test "previous date serialization compatible" {
    let expected = RT.DDate(NodaTime.Instant.UnixEpoch.toUtcLocalTimeZone ())
    let actual =
      Json.Vanilla.deserialize<RT.Dval> """["DDate","1970-01-01T00:00:00"]"""
    Expect.equal expected actual "not deserializing correctly"
  }

let testToPrettyRequestJson =
  testMany
    "toPrettyRequestJson"
    (fun v ->
      try
        BackendOnlyStdLib.LibHttpClient0.PrettyRequestJson.toPrettyRequestJson v
      with
      | e -> e.Message)
    [ RT.DErrorRail(RT.DResult(Ok RT.DNull)),
      "Unknown Err: (Failure \"printing an unprintable value:<result>\")"

      RT.DError(RT.SourceNone, "some message"), "<error: error>"

      RT.DIncomplete RT.SourceNone, "<incomplete: <incomplete>>"

      RT.DDB "my dbstore", "<datastore: my dbstore>"

      RT.DUuid(System.Guid.Parse "1271ebde-7d15-327d-9a36-f9bee0ac22e7"),
      "<uuid: 1271ebde-7d15-327d-9a36-f9bee0ac22e7>"

      RT.DPassword(Password [| 76uy; 13uy |]), "<password: <password>>"

      RT.DBytes [||],
      "Unknown Err: (Failure \"printing an unprintable value:<bytes>\")"

      RT.DDate(
        NodaTime.Instant.parse "2019-07-28T22:42:36Z" |> RT.DDateTime.fromInstant
      ),
      "<date: 2019-07-28T22:42:36Z>"

      (RT.DErrorRail(RT.DHttpResponse(RT.Redirect("some url"))),
       "ErrorRail: 302 some url\n  null")

      (RT.DHttpResponse(RT.Redirect("some url")), "302 some url\nnull")

      (RT.DHttpResponse(RT.Response(200L, [], RT.DStr "some url"))),
      "200 {  }\n\"some url\""

      (RT.DHttpResponse(RT.Response(200L, [ "header", "value" ], RT.DStr "some url"))),
      "200 { header: value }\n\"some url\""

      RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ]), "(\n  1, 2, 3\n)" ]

module ToHashableRepr =
  open LibExecution.RuntimeTypes

  let testToHashableRepr =
    let t (dv : Dval) (expected : string) : Test =
      testTask $"toHashableRepr: {dv}" {
        let fsharpVersion =
          DvalReprInternalDeprecated.toHashableRepr 0 false dv |> UTF8.ofBytesUnsafe

        Expect.equal fsharpVersion expected "bad fsharp impl"
      }


    testList
      "toHashableRepr string"
      [ t (DHttpResponse(Redirect "")) "302 \nnull"
        t (DFloat 0.0) "0."
        t (DTuple(DInt 1, DStr "two", [ DFloat 3.14 ])) "(\n  1, \"two\", 3.14\n)"
        t
          (DObj(
            Map.ofList [ ("", DNull)
                         ("-", DInt 0L)
                         ("j", DFloat -1.797693135e+308) ]
          ))
          "{ \n  j: -inf,\n  -: 0,\n  : null\n}"
        t (DIncomplete(SourceID(2UL, 1UL))) "<incomplete: <incomplete>>"
        t (DOption(Some(DPassword(Password [||])))) "Just <password: <password>>"
        t
          (DResult(Error(DResult(Error(DFloat -0.03902435513)))))
          "ResultError ResultError -0.03902435513"
        t
          (DList [ DUuid(System.Guid.Parse "3e64631e-f455-5d61-30f7-2be5794ebb19")
                   DStr "6"
                   DResult(Ok(DHttpResponse(Response(0L, [], DChar "")))) ])
          "[ \n  <uuid: 3e64631e-f455-5d61-30f7-2be5794ebb19>, \"6\", ResultOk 0 {  }\n    ''\n]"
        t
          (DBytes [| 148uy; 96uy; 130uy; 71uy |])
          "HnXEOfyd6X-BKhAPIBY6kHcrYLxO44nHCshZShS12Qy2qbnLc6vvrQnU4bjTiewW" ]

  let testHashV0 =
    let t (l : List<Dval>) (expected : string) : Test =
      testTask $"hashV0: {l}" {
        let fsharpVersion = DvalReprInternalDeprecated.hash 0 l

        if fsharpVersion <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print "expected: {p expected}"
          print "fsharp  : {p fsharpVersion}"

        Expect.equal fsharpVersion expected "bad fsharp impl"
      }

    testList
      "hashv0"
      [ t
          [ DBytes [||] ]
          "OLBgp1GsljhM2TJ-sbHjaiH9txEUvgdDTAzHv2P24donTt6_529l-9Ua0vFImLlb"
        t
          [ DBytes [| 128uy |] ]
          "jbYwswNvQOKapMlePAFW9VpZO_AF_EJZNtITSk_AuFW7SrR2fdSwsd0mHNERWY09" ]

  let testHashV1 =
    let t (l : List<Dval>) (expected : string) : Test =
      testTask $"hashV1: {l}" {
        let fsharpVersion = DvalReprInternalDeprecated.hash 1 l

        if fsharpVersion <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print $"expected: {p expected}"
          print $"fsharp  : {p fsharpVersion}"

        Expect.equal fsharpVersion expected "bad fsharp impl"
      }

    testList
      "hashv1"
      [ t
          [ DBytes [||] ]
          "JEK8_Gubug09wt7BUWWIPypb2yoMYI4TjzCWqbGWWrK6mNP4I-vszXmZNlDjX2ig"
        t
          [ DHttpResponse(Redirect "H"); DStr "\""; DIncomplete SourceNone ]
          "koFt73igAWTI4-ROoi18TnrSKAN7RDuYjiWD43HGXy7qL9s7LlKbSUjSZeGV6Gt6"
        t
          [ DBytes [| 128uy |] ]
          "EYSh9xozHYAoaIUeS40e25VqvD1K7cA72JhEKbAmMtj6xhN02H7nouKqx4GCtvo_" ]

  let tests = testList "hashing" [ testToHashableRepr; testHashV0; testHashV1 ]


let allRoundtrips =
  let t = testListUsingProperty

  let all =
    sampleDvals
    |> List.filter (fun (_, dval) ->
      match dval with
      // interoperable tests do not support passwords because it's very
      // hard/risky to get legacyserver to roundtrip them correctly without
      // compromising the redaction protections. We do password tests in the
      // rest of the file so lets not confuse these tests.
      | RT.DPassword _ -> false
      // These can't be parsed by the roundtrip tests so skip
      | RT.DInt i -> i > -4611686018427387904L && i < 4611686018427387904L
      | _ -> true)

  let dvs (filter : RT.Dval -> bool) = List.filter (fun (_, dv) -> filter dv) all

  testList
    "roundtrips"
    [ t
        "roundtrippable"
        FuzzTests.InternalJson.Roundtrippable.roundtripsSuccessfully
        (dvs (DvalReprInternalDeprecated.isRoundtrippableDval false))
      t
        "queryable v0"
        FuzzTests.InternalJson.Queryable.canV1Roundtrip
        (dvs DvalReprInternalDeprecated.Test.isQueryableDval) ]

let testInternalRoundtrippableV0 =
  testList
    "tuples"
    [ test "serializes correctly" {
        let expected = """{"type":"tuple","first":1,"second":2,"theRest":[3]}"""

        let actual =
          RT.Dval.DTuple(RT.Dval.DInt 1, RT.Dval.DInt 2, [ RT.Dval.DInt 3 ])
          |> DvalReprInternalDeprecated.toInternalRoundtrippableV0

        Expect.equal actual expected ""
      }

      test "roundtrips successfully" {
        let tpl = RT.Dval.DTuple(RT.Dval.DInt 1, RT.Dval.DInt 2, [ RT.Dval.DInt 3 ])

        let roundtripped =
          tpl
          |> DvalReprInternalDeprecated.toInternalRoundtrippableV0
          |> DvalReprInternalDeprecated.ofInternalRoundtrippableV0

        Expect.equal tpl roundtripped ""
      } ]

let testInternalRoundtrippableNew =
  testList
    "internalNew"
    [ test "tuples serialize correctly" {
        let expected = """["DTuple",["DInt",1],["DInt",2],[["DInt",3]]]"""

        let actual =
          RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ])
          |> DvalReprInternalNew.toRoundtrippableJsonV0

        Expect.equal actual expected ""
      } ]

let testToPrettyMachineJsonStringV1 =
  testList
    "toPrettyMachineJsonStringV1"
    [ test "tuples serialize correctly" {
        let expected = "[1,2,3\n]"

        let actual =
          RT.DTuple(RT.DInt 1, RT.DInt 2, [ RT.DInt 3 ])
          |> DvalReprLegacyExternal.toPrettyMachineJsonStringV1

        Expect.equal actual expected ""
      } ]

module Password =
  let testJsonRoundtripForwards =
    test "json roundtrips forward" {
      let password = RT.DPassword(Password(UTF8.toBytes "x"))

      Expect.equalDval
        password
        (password
         |> DvalReprInternalDeprecated.toInternalRoundtrippableV0
         |> DvalReprInternalDeprecated.ofInternalRoundtrippableV0
         |> DvalReprInternalDeprecated.toInternalRoundtrippableV0
         |> DvalReprInternalDeprecated.ofInternalRoundtrippableV0)
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
            ("encryptedbytes" |> UTF8.toBytes |> Base64.defaultEncodeToString)
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
        DvalReprInternalDeprecated.toInternalRoundtrippableV0

      // roundtrips
      roundtrips
        "toInternalRoundtrippableV0 roundtrips"
        DvalReprInternalDeprecated.toInternalRoundtrippableV0
        DvalReprInternalDeprecated.ofInternalRoundtrippableV0

      // redacting
      doesRedact
        "toEnduserReadableTextV0"
        DvalReprLegacyExternal.toEnduserReadableTextV0
      doesRedact "toDeveloperReprV0" DvalReprDeveloper.toRepr
      doesRedact
        "toPrettyMachineJsonStringV1"
        DvalReprLegacyExternal.toPrettyMachineJsonStringV1
      doesRedact
        "toPrettyRequestJsonV0"
        BackendOnlyStdLib.LibHttpClient0.PrettyRequestJson.toPrettyRequestJson
      doesRedact
        "toPrettyResponseJsonV1"
        LibExecutionStdLib.LibObject.PrettyResponseJsonV0.toPrettyResponseJsonV0
      doesRedact "Json.OCamlCompatible.serialize" Json.OCamlCompatible.serialize
      doesRedact "Json.Vanilla.serialize" Json.Vanilla.serialize
      // These test that serializing via ocaml types will also omit the password.
      // This wasn't the case because these types are used for two contradictory
      // purposes: to communicate with the legacy server (where redacting passwords
      // is bad), and to comminucate with the client (where redacting is good)
      doesRedact "ocaml Json.OCamlCompatible.serialize" (fun dv ->
        dv
        |> LibExecution.OCamlTypes.Convert.rt2ocamlDval
        |> Json.OCamlCompatible.serialize)
      doesRedact "ocaml Json.Vanilla.serialize" (fun dv ->
        dv |> LibExecution.OCamlTypes.Convert.rt2ocamlDval |> Json.Vanilla.serialize)
      ()
    }

  let testSerialization2 =
    test "serialization in object" {
      let roundtrips name serialize deserialize =
        let bytes = UTF8.toBytes "encryptedbytes"
        let password = RT.DObj(Map.ofList [ "x", RT.DPassword(Password bytes) ])

        let wrappedSerialize dval =
          dval
          |> (fun dval ->
            match dval with
            | RT.DObj dvalMap -> dvalMap
            | _ -> Exception.raiseInternal "dobj only here" [])
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
        DvalReprInternalDeprecated.toInternalQueryableV1
        DvalReprInternalDeprecated.ofInternalQueryableV1
    }

  let testNoAutoSerialization =
    testList
      "no auto serialization of passwords"
      [ test "vanilla" {
          let password =
            RT.DPassword(Password(UTF8.toBytes "some password"))
            |> Json.Vanilla.serialize
            |> Json.Vanilla.deserialize

          Expect.equal
            password
            (RT.DPassword(Password(UTF8.toBytes "Redacted")))
            "should be redacted"
        }
        test "ocamlcompatible" {
          let password =
            RT.DPassword(Password(UTF8.toBytes "some password"))
            |> Json.OCamlCompatible.serialize
            |> Json.OCamlCompatible.deserialize

          Expect.equal
            password
            (RT.DPassword(Password(UTF8.toBytes "Redacted")))
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
      testInternalRoundtrippableDoesntCareAboutOrder
      testDvalOptionQueryableSpecialCase
      testToDeveloperRepr
      testToEnduserReadable
      testToPrettyRequestJson
      testToPrettyResponseJson
      testDateMigrationHasCorrectFormats
      ToHashableRepr.tests
      testInternalRoundtrippableNew
      testToPrettyMachineJsonStringV1
      Password.tests
      testInternalRoundtrippableV0
      testPreviousDateSerializionCompatibility
      allRoundtrips ]
