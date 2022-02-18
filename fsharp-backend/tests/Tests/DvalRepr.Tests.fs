module Tests.DvalReprExternal

open System.Threading.Tasks
open FSharp.Control.Tasks

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
    FuzzTests.OCamlInterop.Roundtrippable.roundtrip
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

let testToPrettyResponseJson =
  testMany
    "toPrettyResponseJson"
    LibExecutionStdLib.LibObject.PrettyResponseJsonV0.toPrettyResponseJsonV0
    [ RT.DBytes [| 00uy |], "{\n  \"type\": \"bytes\",\n  \"value\": \"\\u0000\"\n}" ]

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
      RT.DDate(System.DateTime.Parse "2019-07-28T22:42:36Z"),
      "<date: 2019-07-28T22:42:36Z>"
      (RT.DErrorRail(RT.DHttpResponse(RT.Redirect("some url"))),
       "ErrorRail: 302 some url\n  null")
      (RT.DHttpResponse(RT.Redirect("some url")), "302 some url\nnull")
      (RT.DHttpResponse(RT.Response(200L, [], RT.DStr "some url"))),
      "200 {  }\n\"some url\""
      (RT.DHttpResponse(RT.Response(200L, [ "header", "value" ], RT.DStr "some url"))),
      "200 { header: value }\n\"some url\"" ]

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
        FuzzTests.OCamlInterop.Roundtrippable.roundtrip
        (dvs (DvalReprInternal.isRoundtrippableDval false))
      t
        "roundtrippable interop"
        FuzzTests.OCamlInterop.Roundtrippable.isInteroperableV0
        (dvs (DvalReprInternal.isRoundtrippableDval false))
      t
        "queryable v0"
        FuzzTests.OCamlInterop.Queryable.v1Roundtrip
        (dvs DvalReprInternal.isQueryableDval)
      t
        "queryable interop v1"
        FuzzTests.OCamlInterop.Queryable.isInteroperableV1
        (dvs DvalReprInternal.isQueryableDval)
      t "enduserReadable" FuzzTests.DvalRepr.EndUserReadable.equalsOCaml all
      t "developerRepr" FuzzTests.DvalRepr.equalsOCaml all
      t "prettyMachineJson" FuzzTests.Json.PrettyMachineJson.equalsOCaml all ]




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
        DvalReprInternal.toInternalRoundtrippableV0

      // roundtrips
      roundtrips
        "toInternalRoundtrippableV0 roundtrips"
        DvalReprInternal.toInternalRoundtrippableV0
        DvalReprInternal.ofInternalRoundtrippableV0

      // redacting
      doesRedact "toEnduserReadableTextV0" DvalReprExternal.toEnduserReadableTextV0
      doesRedact "toDeveloperReprV0" DvalReprExternal.toDeveloperReprV0
      doesRedact "toPrettyMachineJsonV1" DvalReprExternal.toPrettyMachineJsonStringV1
      doesRedact
        "toPrettyRequestJsonV0"
        BackendOnlyStdLib.LibHttpClient0.PrettyRequestJson.toPrettyRequestJson
      doesRedact
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
      FuzzTests.Json.LibJwtJson.equalsOCaml
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

module ParsingMinefield =

  // Run the ofUnknownJson functions against a repo of JSON edge cases, and make sure
  // they generate the same.

  let functionsEqual
    (filename : string)
    (ocamlFn : string -> Task<RT.Dval>)
    (fsharpFn : string -> Result<RT.Dval, string>)
    : Result<unit, string> =
    let str = System.IO.File.ReadAllText(filename, System.Text.Encoding.UTF8)
    let actual = fsharpFn str
    let expected =
      try
        Ok (ocamlFn str).Result
      with
      | :? System.AggregateException as e -> Error e.InnerException.Message
      | e -> Error e.Message

    let str = if String.length str > 1000 then String.take 1000 str else str

    match actual, expected with
    | Ok a, Ok e ->
      Expect.equalDval a e $"{filename} equals dval"
      Ok()
    | Error _, Error _ -> Ok()
    | Ok dv, Error msg ->
      Error $"F# parsed '{filename}' ({str}) while ocaml errored: {msg}"
    | Error msg, Ok dv ->
      Error $"OCaml parsed '{filename}' ({str}) while F# errored: {msg}"


  let testFiles () =
    let dir = "tests/json-test-suite/"
    System.IO.Directory.GetFiles(dir, "*.json")
    |> Array.filter ((<>) "README.md")
    |> Array.filter ((<>) "LICENSE")
    |> Array.filter ((<>) ".gitattributes")
    |> Array.toList
    |> List.sort

  let testOfUnknownJsonV0SameInOCaml =
    // We ran all the tests then manually checked out the differences, documenting
    // them in the changelog, then adding them here.
    let knownDifferences =
      [ "i_number_too_big_neg_int"
        "i_number_too_big_pos_int"
        "i_number_very_big_negative_int"
        "i_object_key_lone_2nd_surrogate"
        "n_number_NaN"
        "n_number_infinity"
        "n_number_minus_infinity"
        "n_object_repeated_null_null"
        "n_object_unquoted_key"
        "n_string_unescaped_newline"
        "n_string_unescaped_tab"
        "n_string_unescaped_tab"
        "y_string_null_escape" ]
      |> List.map (fun name -> $"tests/json-test-suite/{name}.json")
      |> Set

    let unknownV0Equal (filename : string) : Result<unit, string> =
      functionsEqual filename LibBackend.OCamlInterop.ofUnknownJsonV0 (fun str ->
        try
          Ok(DvalReprExternal.unsafeOfUnknownJsonV0 str)
        with
        | e -> Error e.Message)

    testMany
      "ofUnknownJsonV0"
      unknownV0Equal
      (testFiles ()
       |> List.filterMap (fun filename ->
         if Set.contains filename knownDifferences then
           None
         else
           Some((filename), Ok())))

  let testOfUnknownJsonV1SameInOCaml =
    // We ran all the tests then manually checked out the differences, documenting
    // them in the changelog, then adding them here.
    let knownDifferences =
      [ "i_number_too_big_neg_int"
        "i_number_too_big_pos_int"
        "i_number_very_big_negative_int"
        "i_object_key_lone_2nd_surrogate"
        "n_number_NaN"
        "n_number_infinity"
        "n_number_minus_infinity"
        "n_object_repeated_null_null"
        "n_object_unquoted_key"
        "n_string_unescaped_newline"
        "n_string_unescaped_tab"
        "n_string_unescaped_tab"
        "y_string_null_escape" ]
      |> List.map (fun name -> $"tests/json-test-suite/{name}.json")
      |> Set

    let unknownV1Equal (filename : string) : Result<unit, string> =
      functionsEqual
        filename
        LibBackend.OCamlInterop.ofUnknownJsonV1
        DvalReprExternal.ofUnknownJsonV1

    testMany
      "ofUnknownJsonV1"
      unknownV1Equal
      (testFiles ()
       |> List.filterMap (fun filename ->
         if Set.contains filename knownDifferences then
           None
         else
           Some(filename, Ok())))




  let tests =
    testList
      "minefield"
      [ testOfUnknownJsonV0SameInOCaml; testOfUnknownJsonV1SameInOCaml ]


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
      ToHashableRepr.tests
      Password.tests
      LibJwt.testJsonSameOnBoth
      ParsingMinefield.tests
      allRoundtrips ]
