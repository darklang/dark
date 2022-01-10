module Tests.DvalReprExternal

open Expecto
open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal


let testInternalRoundtrippableDoesntCareAboutOrder =
  test "internal_roundtrippable doesn't care about key order" {
    Expect.equal
      (DvalReprInternal.ofInternalRoundtrippableV0
        "{
           \"type\": \"option\",
           \"value\": 5
          }")
      (DvalReprInternal.ofInternalRoundtrippableV0
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
    let dvm = Map.ofList [ ("type", RT.DStr "option"); ("value", RT.DInt 5L) ]

    Expect.equal
      (RT.DObj dvm)
      (dvm
       |> DvalReprInternal.toInternalQueryableV1
       |> DvalReprInternal.ofInternalQueryableV1)
      "extra"
  }

let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        DvalReprExternal.toDeveloperReprV0
        // Most of this is just the OCaml output and not really what the output should be
        [ RT.DHttpResponse(RT.Response(0L, [], RT.DNull)), "0 {  }\nnull"
          RT.DFloat(-0.0), "-0."
          RT.DFloat(infinity), "inf"
          RT.DObj(Map.ofList [ "", RT.DNull ]), "{ \n  : null\n}"
          RT.DList [ RT.DNull ], "[ \n  null\n]" ] ]

let testToEnduserReadable =
  testMany
    "toEnduserReadable string"
    DvalReprExternal.toEnduserReadableTextV0
    // Most of this is just the OCaml output and not really what the output should be
    [ RT.DFloat(0.0), "0." // this type of thing in particular is ridic
      RT.DFloat(-0.0), "-0."
      RT.DFloat(5.0), "5."
      RT.DFloat(5.1), "5.1"
      RT.DFloat(-5.0), "-5."
      RT.DFloat(-5.1), "-5.1"
      RT.DError(RT.SourceNone, "Some message"), "Error"
      RT.DHttpResponse(RT.Redirect("some url")), "302 some url\nnull"
      RT.DHttpResponse(RT.Response(0L, [ "a header", "something" ], RT.DNull)),
      "0 { a header: something }\nnull" ]

module ToHashableRepr =
  open LibExecution.RuntimeTypes

  let testToHashableRepr =
    let t (dv : Dval) (expected : string) : Test =
      testTask $"toHashableRepr: {dv}" {
        let! ocamlVersion = LibBackend.OCamlInterop.toHashableRepr dv
        let fsharpVersion =
          DvalReprInternal.toHashableRepr 0 false dv |> UTF8.ofBytesUnsafe

        if ocamlVersion <> expected || fsharpVersion <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print "expected: {p expected}"
          print "ocaml   : {p ocamlVersion}"
          print "fsharp  : {p fsharpVersion}"

        Expect.equal ocamlVersion expected "wrong test value"
        Expect.equal fsharpVersion expected "bad fsharp impl"
      }


    testList
      "toHashableRepr string"
      [ t (DHttpResponse(Redirect "")) "302 \nnull"
        t (DFloat 0.0) "0."
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
        let! ocamlVersion = LibBackend.OCamlInterop.hashV0 l
        let fsharpVersion = DvalReprInternal.hash 0 l

        if ocamlVersion <> expected || fsharpVersion <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print "expected: {p expected}"
          print "ocaml   : {p ocamlVersion}"
          print "fsharp  : {p fsharpVersion}"

        Expect.equal ocamlVersion expected "wrong test value"
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
        let! ocamlVersion = LibBackend.OCamlInterop.hashV1 l
        let fsharpVersion = DvalReprInternal.hash 1 l

        if ocamlVersion <> expected || fsharpVersion <> expected then
          let p str = str |> UTF8.toBytes |> System.BitConverter.ToString
          print $"expected: {p expected}"
          print $"ocaml   : {p ocamlVersion}"
          print $"fsharp  : {p fsharpVersion}"

        Expect.equal ocamlVersion expected "wrong test value"
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


module F = FuzzTests.All

let allRoundtrips =
  let t = testListUsingProperty

  let all =
    sampleDvals
    |> List.filter (function
      // interoperable tests do not support passwords because it's very
      // hard/risky to get legacyserver to roundtrip them correctly without
      // compromising the redaction protections. We do password tests in the
      // rest of the file so lets not confuse these tests.
      | (_, RT.DPassword _) -> false
      // These can't be parsed by the roundtrip tests so skip
      | (_, RT.DInt i) -> i > -4611686018427387904L && i < 4611686018427387904L
      | _ -> true)

  let dvs (filter : RT.Dval -> bool) = List.filter (fun (_, dv) -> filter dv) all

  testList
    "roundtrips"
    [ t
        "roundtrippable"
        F.Roundtrippable.roundtrip
        (dvs (DvalReprInternal.isRoundtrippableDval false))
      t
        "roundtrippable interop"
        F.Roundtrippable.isInteroperableV0
        (dvs (DvalReprInternal.isRoundtrippableDval false))
      t "queryable v0" F.Queryable.v1Roundtrip (dvs DvalReprInternal.isQueryableDval)
      t
        "queryable interop v1"
        F.Queryable.isInteroperableV1
        (dvs DvalReprInternal.isQueryableDval)
      t "enduserReadable" F.EndUserReadable.equalsOCaml all
      t "developerRepr" F.DeveloperRepr.equalsOCaml all
      t "prettyMachineJson" F.PrettyMachineJson.equalsOCaml all ]

module Date =
  let testDateMigrationHasCorrectFormats =
    test "date migration has correct formats" {
      let str = "2019-03-08T08:26:14Z" in
      let date = RT.DDate(System.DateTime.ofIsoString str) in
      let oldFormat = $"{{ \"type\": \"date\", \"value\": \"{str}\"}}"
      // FSTODO
      // Expect.equal (Legacy.toPrettyMachineJsonStringV0 date) oldFormat "old version"
      Expect.equal
        (DvalReprExternal.toPrettyMachineJsonStringV1 date)
        $"\"{str}\""
        "new version"
    }

  let tests = testList "date" [ testDateMigrationHasCorrectFormats ]




module Password =
  let testJsonRoundtripForwards =
    test "json roundtrips forward" {
      let password = RT.DPassword(Password(UTF8.toBytes "x"))

      Expect.equalDval
        password
        (password
         |> DvalReprInternal.toInternalRoundtrippableV0
         |> DvalReprInternal.ofInternalRoundtrippableV0
         |> DvalReprInternal.toInternalRoundtrippableV0
         |> DvalReprInternal.ofInternalRoundtrippableV0)
        "Passwords serialize and deserialize if there's no redaction."
    }

  let testSerialization =
    test "password serialization" {
      let testSerialize expected name f =
        let bytes = UTF8.toBytes "encryptedbytes"
        let password = RT.DPassword(Password bytes)

        Expect.equal
          expected
          (String.includes
            ("encryptedbytes" |> UTF8.toBytes |> Base64.defaultEncodeToString)
            (f password))
          ($"Passwords serialize in non-redaction function: {name}")

      let doesSerialize = testSerialize true
      let doesntSerialize = testSerialize false

      let roundtrips name serialize deserialize =
        let bytes = UTF8.toBytes "encryptedbytes"
        let password = RT.DPassword(Password bytes)

        Expect.equalDval
          password
          (password |> serialize |> deserialize |> serialize |> deserialize)
          $"Passwords serialize in non-redaction function: {name}"

      // doesn't redact
      doesSerialize
        "toInternalRoundtrippableV0"
        DvalReprInternal.toInternalRoundtrippableV0

      // roundtrips
      roundtrips
        "toInternalRoundtrippableV0 roundtrips"
        DvalReprInternal.toInternalRoundtrippableV0
        DvalReprInternal.ofInternalRoundtrippableV0

      // redacting
      doesntSerialize
        "toEnduserReadableTextV0"
        DvalReprExternal.toEnduserReadableTextV0
      doesntSerialize "toDeveloperReprV0" DvalReprExternal.toDeveloperReprV0
      doesntSerialize
        "toPrettyMachineJsonV1"
        DvalReprExternal.toPrettyMachineJsonStringV1
      // FSTODO
      // doesntSerialize
      //   "toPrettyRequestJsonV0"
      //   LibExecution.Legacy.PrettyRequestJsonV0.toPrettyRequestJsonV0
      doesntSerialize
        "toPrettyResponseJsonV1"
        LibExecutionStdLib.LibObject.PrettyResponseJsonV0.toPrettyResponseJsonV0
      ()
    }

  let testSerialization2 =
    test "serialization in object" {
      let roundtrips name serialize deserialize =
        let bytes = UTF8.toBytes "encryptedbytes" in
        let password = RT.DObj(Map.ofList [ "x", RT.DPassword(Password bytes) ])

        let wrappedSerialize dval =
          dval
          |> (function
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
        DvalReprInternal.toInternalQueryableV1
        DvalReprInternal.ofInternalQueryableV1
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
            (RT.DPassword(Password(UTF8.toBytes "Redacted")))
            password
            "should be redacted"

        }
        test "ocamlcompatible" {
          let password =
            RT.DPassword(Password(UTF8.toBytes "some password"))
            |> Json.OCamlCompatible.serialize
            |> Json.OCamlCompatible.deserialize

          Expect.equal
            (RT.DPassword(Password(UTF8.toBytes "Redacted")))
            password
            "should be redacted"
        } ]



  let tests =
    testList
      "password"
      [ testJsonRoundtripForwards
        testSerialization
        testSerialization2
        testNoAutoSerialization ]

module LibJwt =
  let testJsonSameOnBoth =
    testMany
      "LibJwt json toString works same on both"
      FuzzTests.All.LibJwtJson.equalsOCaml
      ([ RT.DObj(
           Map.ofList [ ("", RT.DFloat 1.797693135e+308)
                        ("a", RT.DErrorRail(RT.DFloat nan)) ]
         )
         RT.DDate(System.DateTime.Parse "7/29/2028 12:00:00 AM")
         RT.DStr "痃"
         RT.DError(RT.SourceNone, "ܱ")
         RT.DDB "ϴ"
         RT.DStr "\u000f"
         RT.DFloat 1.7976931348623157e+308
         RT.DObj(Map [ ("鳉", RT.DChar "\u001e") ])
         RT.DObj(
           Map [ ("", RT.DPassword(Password [||]))
                 ("伯",
                  RT.DUuid(System.Guid.Parse "1cfb3de5-4350-2a1c-3e03-7945672ca26e")) ]
         ) ]
       @ (sampleDvals
          |> List.map Tuple2.second
          |> List.filter ((<>) (RT.DInt 4611686018427387904L)))
       |> List.map (fun x -> x, true))


let tests =
  testList
    "dvalRepr"
    [ testDvalRoundtrippableRoundtrips
      testInternalRoundtrippableDoesntCareAboutOrder
      testDvalOptionQueryableSpecialCase
      testToDeveloperRepr
      testToEnduserReadable
      ToHashableRepr.tests
      Password.tests
      Date.tests
      LibJwt.testJsonSameOnBoth
      allRoundtrips ]
