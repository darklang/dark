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
        let actual = DvalReprInternalHash.hash 0 l

        if actual <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print $"expected: {p expected}"
          print $"actual  : {p actual}"

        Expect.equal actual expected "bad fsharp impl"
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
        let actual = DvalReprInternalHash.hash 1 l

        if actual <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print $"expected: {p expected}"
          print $"fsharp  : {p actual}"

        Expect.equal actual expected "bad fsharp impl"
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

  let tests =
    testList "hashing" [ testToHashableRepr; testHashV0; testHashV1; testHashV2 ]


let allRoundtrips =
  let t = testListUsingProperty

  let all = sampleDvals
  let dvs (filter : RT.Dval -> bool) = List.filter (fun (_, dv) -> filter dv) all

  testList
    "roundtrips"
    [ t
        "roundtrippable"
        FuzzTests.InternalJson.Roundtrippable.roundtripsSuccessfully
        (dvs (fun _ -> true))
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
      doesRedact
        "toPrettyMachineJsonStringV1"
        DvalReprLegacyExternal.toPrettyMachineJsonStringV1
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
      testDvalOptionQueryableSpecialCase
      testToDeveloperRepr
      testToEnduserReadable
      ToHashableRepr.tests
      testInternalRoundtrippableNew
      testToPrettyMachineJsonStringV1
      Password.tests
      testPreviousDateSerializionCompatibility
      allRoundtrips ]
