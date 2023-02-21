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
module CTRuntime = ClientTypes.Runtime
module CT2Runtime = ClientTypes2ExecutionTypes.Runtime

module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated
module DvalReprInternalNew = LibExecution.DvalReprInternalNew
module DvalReprInternalHash = LibExecution.DvalReprInternalHash
module Errors = LibExecution.Errors


let testDvalRoundtrippableRoundtrips =
  testMany
    "special roundtrippable dvals roundtrip"
    FuzzTests.InternalJson.Roundtrippable.roundtripsSuccessfully
    [ RT.DObj(
        Map.ofList [ ("", RT.DFloat 1.797693135e+308)
                     ("a", RT.DErrorRail(RT.DFloat nan)) ]
      ),
      true ]


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
          RT.DObj(Map.ofList [ "", RT.DNull ]), "{\n  : null\n}"
          RT.DList [ RT.DNull ], "[\n  null\n]" ] ]

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

// We used a System.Text.Json converter supplied by a NuGet package for a bit,
// but found that it was incompatible with the OCamlCompatible serializer. We
// have since adjusted `Vanilla` to use a custom converter, and this test is to
// ensure values serialized during the time where the NuGet package's converter
// are able to be deserialized.
let testPreviousDateSerializionCompatibility =
  test "previous date serialization compatible" {
    let expected =
      CTRuntime.Dval.DDate(NodaTime.Instant.UnixEpoch.toUtcLocalTimeZone ())
    let actual =
      Json.Vanilla.deserialize<CTRuntime.Dval.T>
        """["DDate","1970-01-01T00:00:00"]"""
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
      [ t [ DBytes [||] ] "diEjLGQC8oE"
        t
          [ DHttpResponse(Redirect "H"); DStr "\""; DIncomplete SourceNone ]
          "lqp206BU8hI"
        t [ DBytes [| 128uy |] ] "kQHs0urT3N4" ]

  let tests = testList "hashing" [ testHashV2 ]


let allRoundtrips =
  let t = testListUsingProperty

  let all = sampleDvals
  let dvs (filter : RT.Dval -> bool) = List.filter (fun (_, dv) -> filter dv) all

  testList
    "roundtrips"
    [ t
        "roundtrippable"
        FuzzTests.InternalJson.Roundtrippable.roundtripsSuccessfully
        (dvs (DvalReprInternalNew.Test.isRoundtrippableDval))
      t
        "queryable v0"
        FuzzTests.InternalJson.Queryable.canV1Roundtrip
        (dvs DvalReprInternalDeprecated.Test.isQueryableDval)
      t
        "vanilla"
        (fun dv ->
          dv
          |> CT2Runtime.Dval.toCT
          |> Json.Vanilla.serialize
          |> Json.Vanilla.deserialize
          |> CT2Runtime.Dval.fromCT
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
          |> DvalReprInternalNew.toRoundtrippableJsonV0

        Expect.equal actual expected ""
      } ]

module Password =
  let testJsonRoundtripForwards =
    test "json roundtrips forward" {
      let password = RT.DPassword(Password(UTF8.toBytes "x"))

      Expect.equalDval
        password
        (password
         |> DvalReprInternalNew.toRoundtrippableJsonV0
         |> DvalReprInternalNew.parseRoundtrippableJsonV0
         |> DvalReprInternalNew.toRoundtrippableJsonV0
         |> DvalReprInternalNew.parseRoundtrippableJsonV0)
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
        DvalReprInternalNew.toRoundtrippableJsonV0

      // roundtrips
      roundtrips
        "toInternalRoundtrippableV0 roundtrips"
        DvalReprInternalNew.toRoundtrippableJsonV0
        DvalReprInternalNew.parseRoundtrippableJsonV0

      // redacting
      doesRedact
        "toEnduserReadableTextV0"
        DvalReprLegacyExternal.toEnduserReadableTextV0
      doesRedact "toDeveloperReprV0" DvalReprDeveloper.toRepr
      doesRedact "Json.Vanilla.serialize" (fun dv ->
        dv |> CT2Runtime.Dval.toCT |> Json.Vanilla.serialize)
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
            CTRuntime.Dval.DPassword(Password(UTF8.toBytes "some password"))
            |> Json.Vanilla.serialize
            |> Json.Vanilla.deserialize

          Expect.equal
            password
            (CTRuntime.Dval.DPassword(Password(UTF8.toBytes "Redacted")))
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
      testToEnduserReadable
      ToHashableRepr.tests
      testInternalRoundtrippableNew
      Password.tests
      testPreviousDateSerializionCompatibility
      allRoundtrips ]
