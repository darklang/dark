module FuzzTests.All

// This aims to find test cases that violate certain properties that we expect.
// Desired properties include that OCaml Dark programs and functions work the
// same as F# ones, and things related to serialization and output.

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal

let result (t : Task<'a>) : 'a = t.Result

let (.=.) actual expected : bool =
  if actual = expected then
    Expect.equal actual expected ""
    true
  else
    let o = string actual |> UTF8.toBytes
    let e = string expected |> UTF8.toBytes
    Expect.equal (actual, o) (expected, e) ""
    false


let baseConfig : FsCheckConfig =
  { FsCheckConfig.defaultConfig with maxTest = 100000 }

let baseConfigWithGenerator (typ : System.Type) : FsCheckConfig =
  { baseConfig with arbitrary = [ typ ] }

let testProperty (name : string) (x : 'a) : Test =
  testPropertyWithConfig baseConfig name x

let testPropertyWithGenerator (typ : System.Type) (name : string) (x : 'a) : Test =
  testPropertyWithConfig (baseConfigWithGenerator typ) name x

let tpwg = testPropertyWithGenerator

module Generators =
  let nonNullString (s : string) : bool = s <> null

  let safeOCamlString (s : string) : bool =
    // We disallow \u0000 in OCaml because postgres doesn't like it, see of_utf8_encoded_string
    s <> null && not (s.Contains('\u0000'))

  let alphaNumericString =
    (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ])

  let string () =
    let isValid (s : string) : bool =
      try
        String.normalize s |> ignore<string>
        true
      with
      | e ->
        // debuG
        //   "Failed to normalize :"
        //   $"{e}\n '{s}': (len {s.Length}, {System.BitConverter.ToString(toBytes s)})"

        false

    Arb.generate<UnicodeString>
    |> Gen.map (fun (UnicodeString s) -> s)
    |> Gen.filter isValid
    // Now that we know it can be normalized, actually normalize it
    |> Gen.map (fun s -> s.Normalize())
    |> Gen.filter safeOCamlString



  let nonNegativeInt () =
    gen {
      let! (NonNegativeInt i) = Arb.generate<NonNegativeInt>
      return i
    }

  let char () : Gen<string> =
    string ()
    |> Gen.map String.toEgcSeq
    |> Gen.map Seq.toList
    |> Gen.map List.head
    |> Gen.filter ((<>) None)
    |> Gen.map (Option.defaultValue "")
    |> Gen.filter ((<>) "")


module G = Generators



module FQFnName =
  let nameGenerator (first : char list) (other : char list) : Gen<string> =
    gen {
      let! length = Gen.choose (0, 20)
      let! head = Gen.elements first
      let! tail = Gen.arrayOfLength length (Gen.elements other)
      return System.String(Array.append [| head |] tail)
    }

  let ownerName : Gen<string> =
    nameGenerator [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

  let packageName = ownerName
  let modName : Gen<string> = nameGenerator [ 'A' .. 'Z' ] G.alphaNumericString
  let fnName : Gen<string> = nameGenerator [ 'a' .. 'z' ] G.alphaNumericString

  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.fromGenShrink (G.string (), Arb.shrink<string>)

    static member PTFQFnName() : Arbitrary<PT.FQFnName.T> =
      { new Arbitrary<PT.FQFnName.T>() with
          member x.Generator =
            let stdlib =
              gen {
                let! module_ = modName
                let! function_ = fnName
                let! version = G.nonNegativeInt ()
                return PT.FQFnName.stdlibFqName module_ function_ version
              }

            let user = Gen.map PT.FQFnName.userFqName fnName

            let package =
              gen {
                let! owner = ownerName
                let! package = packageName
                let! module_ = modName
                let! function_ = fnName
                let! version = G.nonNegativeInt ()

                return
                  PT.FQFnName.packageFqName owner package module_ function_ version
              }

            Gen.oneof [ stdlib; user; package ] }

    static member RTFQFnName() : Arbitrary<RT.FQFnName.T> =
      { new Arbitrary<RT.FQFnName.T>() with
          member x.Generator = Generator.PTFQFnName().Generator }

  let ptRoundtrip (a : PT.FQFnName.T) : bool = string a |> PT.FQFnName.parse .=. a

  let tests =
    testList "PT.FQFnName" [ tpwg typeof<Generator> "roundtripping" ptRoundtrip ]


module OCamlInterop =
  open LibExecution.OCamlTypes.Convert
  open OCamlInterop
  open Json.OCamlCompatible

  let isInteroperable
    (ocamlToString : 'a -> Task<string>)
    (ocamlOfString : string -> Task<'a>)
    (fsToString : 'a -> string)
    (fsOfString : string -> 'a)
    (equality : 'a -> 'a -> bool)
    (v : 'a)
    : bool =
    try
      // What does it mean to interoperate? Ideally, the F# impl would be able
      // to read what the OCaml impl sends it and vice versa. However, because
      // the OCaml side is buggy, and we want to reproduce those bugs exactly
      // (for now), that isn't sufficient. We actually just want to make sure
      // we produce the same thing as they do for the same value. BUT, we don't
      // actually produce the exact same thing, and it's hard to do that for
      // the edge cases we've found. So really we just want to make sure that
      // whatever either side produces, both sides are able to read it and get
      // the same result.
      let bothCanRead str = (ocamlOfString str).Result |> equality (fsOfString str)
      let bothCanReadOCamlString = bothCanRead (ocamlToString v).Result
      let bothCanReadFSharpString = bothCanRead (fsToString v)

      if bothCanReadFSharpString && bothCanReadOCamlString then
        true
      else
        print (
          $"ocamlStringReadable: {bothCanReadOCamlString}\n"
          + $"fsharpStringReadable: {bothCanReadFSharpString}\n"
        )

        false
    with
    | e ->
      print $"Cause exception while fuzzing {e}"
      reraise ()

  type Generator =
    static member Expr() =
      Arb.Default.Derive()
      |> Arb.filter (function
        // characters are not yet supported in OCaml
        | PT.ECharacter _ -> false
        | _ -> true)

    static member Pattern() =
      Arb.Default.Derive()
      |> Arb.filter (function
        // characters are not yet supported in OCaml
        | PT.PCharacter _ -> false
        | _ -> true)

    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())


  let yojsonExprRoundtrip (a : PT.Expr) : bool =
    a
    |> pt2ocamlExpr
    |> serialize
    |> deserialize
    |> ocamlExpr2PT
    |> serialize
    |> deserialize
    |> pt2ocamlExpr
    |> serialize
    |> deserialize
    |> ocamlExpr2PT
    |> serialize
    |> deserialize
    .=. a

  let yojsonHandlerRoundtrip (a : PT.Handler.T) : bool =
    a
    |> pt2ocamlHandler
    |> serialize
    |> deserialize
    |> ocamlHandler2PT a.pos
    |> serialize
    |> deserialize
    |> pt2ocamlHandler
    |> serialize
    |> deserialize
    |> ocamlHandler2PT a.pos
    |> serialize
    |> deserialize
    .=. a

  let binaryHandlerRoundtrip (a : PT.Handler.T) : bool =
    let h = PT.TLHandler a

    h
    |> toplevelToCachedBinary
    |> result
    |> (fun bin -> bin, None)
    |> toplevelBin2Json
    |> result
    .=. h

  let binaryExprRoundtrip (pair : PT.Expr * tlid) : bool =
    pair
    |> exprTLIDPairToCachedBinary
    |> result
    |> exprTLIDPairOfCachedBinary
    |> result
    .=. pair

  let tests =
    let tp f = tpwg typeof<Generator> f

    testList
      "OcamlInterop"
      [ tp "roundtripping OCamlInteropBinaryHandler" binaryHandlerRoundtrip
        tp "roundtripping OCamlInteropBinaryExpr" binaryExprRoundtrip
        tp "roundtripping OCamlInteropYojsonHandler" yojsonHandlerRoundtrip
        tp "roundtripping OCamlInteropYojsonExpr" yojsonExprRoundtrip ]

module Roundtrippable =
  type Generator =
    static member String() : Arbitrary<string> = Arb.fromGen (G.string ())

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (DvalReprInternal.isRoundtrippableDval false)

  type GeneratorWithBugs =
    static member String() : Arbitrary<string> = Arb.fromGen (G.string ())

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter (DvalReprInternal.isRoundtrippableDval true)

  let roundtrip (dv : RT.Dval) : bool =
    dv
    |> DvalReprInternal.toInternalRoundtrippableV0
    |> DvalReprInternal.ofInternalRoundtrippableV0
    |> dvalEquality dv

  let isInteroperableV0 dv =
    if containsPassword dv then
      true
    else
      OCamlInterop.isInteroperable
        OCamlInterop.toInternalRoundtrippableV0
        OCamlInterop.ofInternalRoundtrippableV0
        DvalReprInternal.toInternalRoundtrippableV0
        DvalReprInternal.ofInternalRoundtrippableV0
        dvalEquality
        dv

  let tests =
    testList
      "roundtrippable"
      [ tpwg typeof<Generator> "roundtripping works properly" roundtrip
        tpwg
          typeof<GeneratorWithBugs>
          "roundtrippable is interoperable"
          isInteroperableV0 ]


module Queryable =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter DvalReprInternal.isQueryableDval

  let v1Roundtrip (dv : RT.Dval) : bool =
    let dvm = (Map.ofList [ "field", dv ])

    dvm
    |> DvalReprInternal.toInternalQueryableV1
    |> DvalReprInternal.ofInternalQueryableV1
    |> dvalEquality (RT.DObj dvm)

  let isInteroperableV1 (dv : RT.Dval) =
    // redacted passwords are created on the OCaml side and hard to remove
    if containsPassword dv then
      true
    else
      let dvm = (Map.ofList [ "field", dv ])

      OCamlInterop.isInteroperable
        OCamlInterop.toInternalQueryableV1
        OCamlInterop.ofInternalQueryableV1
        (function
        | RT.DObj dvm -> DvalReprInternal.toInternalQueryableV1 dvm
        | dv -> Exception.raiseInternal "not an obj" [ "dval", dv ])
        DvalReprInternal.ofInternalQueryableV1
        dvalEquality
        (RT.DObj dvm)

  let tests =
    let tp f = tpwg typeof<Generator> f

    testList
      "InternalQueryable"
      [ tp "roundtripping v1" v1Roundtrip; tp "interoperable v1" isInteroperableV1 ]

module DeveloperRepr =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    // The format here is only used for errors so it doesn't matter all the
    // much. These are places where we've manually checked the differing
    // outputs are fine.

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | RT.DFloat 0.0 -> false
        | RT.DFloat infinity -> false
        | _ -> true)


  let equalsOCaml (dv : RT.Dval) : bool =
    DvalReprExternal.toDeveloperReprV0 dv
    .=. (OCamlInterop.toDeveloperRepr dv).Result

  let tests =
    testList "toDeveloperRepr" [ tpwg typeof<Generator> "roundtripping" equalsOCaml ]

module HttpClient =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      // FSTODO: add in unicode
      // G.string () |> Arb.fromGen
      Arb.Default.String() |> Arb.filter G.safeOCamlString

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  type QueryStringGenerator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter G.safeOCamlString
    static member String() : Arbitrary<string> =
      Gen.listOf (Gen.listOf (G.string ()))
      |> Gen.map (List.map (String.concat "="))
      |> Gen.map (String.concat "&")
      |> Arb.fromGen


  let dvalToUrlStringExn (l : List<string * RT.Dval>) : bool =
    let dv = RT.DObj(Map l)

    DvalReprExternal.toUrlString dv .=. (OCamlInterop.toUrlString dv).Result

  let dvalToQuery (l : List<string * RT.Dval>) : bool =
    let dv = RT.DObj(Map l)
    DvalReprExternal.toQuery dv |> Result.unwrapUnsafe
    .=. (OCamlInterop.dvalToQuery dv).Result

  let dvalToFormEncoding (l : List<string * RT.Dval>) : bool =
    let dv = RT.DObj(Map l)
    (DvalReprExternal.toFormEncoding dv).ToString()
    .=. (OCamlInterop.dvalToFormEncoding dv).Result

  let queryStringToParams (s : string) : bool =
    DvalReprExternal.parseQueryString s
    .=. (OCamlInterop.queryStringToParams s).Result


  let queryToDval (q : List<string * List<string>>) : bool =
    DvalReprExternal.ofQuery q .=. (OCamlInterop.queryToDval q).Result

  let queryToEncodedString (q : List<string * List<string>>) : bool =
    DvalReprExternal.queryToEncodedString q
    .=. (OCamlInterop.paramsToQueryString q).Result

  let tests =
    let test name fn = tpwg typeof<Generator> name fn
    testList
      "FuzzHttpClient"
      [ test "dvalToUrlStringExn" dvalToUrlStringExn // FSTODO: unicode
        test "dvalToQuery" dvalToQuery
        test "dvalToFormEncoding" dvalToFormEncoding
        tpwg typeof<QueryStringGenerator> "queryStringToParams" queryStringToParams // only &=& fails
        test "queryToDval" queryToDval
        test "queryToEncodedString" queryToEncodedString ]


module EndUserReadable =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  // The format here is used to show users so it has to be exact
  let equalsOCaml (dv : RT.Dval) : bool =
    DvalReprExternal.toEnduserReadableTextV0 dv
    .=. (OCamlInterop.toEnduserReadableTextV0 dv).Result

  let tests =
    testList
      "toEnduserReadable"
      [ tpwg typeof<Generator> "roundtripping" equalsOCaml ]

module Hashing =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        // not supported in OCaml
        | RT.DFnVal _ -> false
        | _ -> true)

  // The format here is used to get values from the DB, so this has to be 100% identical
  let equalsOCamlToHashable (dv : RT.Dval) : bool =
    let ocamlVersion = (OCamlInterop.toHashableRepr dv).Result
    let fsharpVersion =
      DvalReprInternal.toHashableRepr 0 false dv |> UTF8.ofBytesUnsafe
    ocamlVersion .=. fsharpVersion

  let equalsOCamlV0 (l : List<RT.Dval>) : bool =
    DvalReprInternal.hash 0 l .=. (OCamlInterop.hashV0 l).Result

  let equalsOCamlV1 (l : List<RT.Dval>) : bool =
    let ocamlVersion = (OCamlInterop.hashV1 l).Result
    let fsharpVersion = DvalReprInternal.hash 1 l
    ocamlVersion .=. fsharpVersion

  let tests =
    testList
      "hash"
      [ tpwg typeof<Generator> "toHashableRepr" equalsOCamlToHashable
        tpwg typeof<Generator> "hashv0" equalsOCamlV0
        tpwg typeof<Generator> "hashv1" equalsOCamlV1 ]


module PrettyMachineJson =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    // This should produce identical JSON to the OCaml function or customers will have an unexpected change
    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    let actual =
      dv
      |> DvalReprExternal.toPrettyMachineJsonStringV1
      |> Newtonsoft.Json.Linq.JToken.Parse
      |> string

    let expected =
      (OCamlInterop.toPrettyMachineJsonV1 dv).Result
      |> Newtonsoft.Json.Linq.JToken.Parse
      |> string

    actual .=. expected

  let tests =
    testList
      "prettyMachineJson"
      [ tpwg typeof<Generator> "roundtripping prettyMachineJson" equalsOCaml ]


module PrettyResponseJson =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    // This should produce identical JSON to the OCaml function or customers will have an unexpected change
    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    // The json generated is not identical, so check that it parses to the same thing
    let actual =
      try
        dv
        |> LibExecutionStdLib.LibObject.PrettyResponseJsonV0.toPrettyResponseJsonV0
        |> Newtonsoft.Json.Linq.JToken.Parse
        |> string
      with
      | e -> e.Message

    let expected =
      try
        (OCamlInterop.toPrettyResponseJson dv).Result
        |> Newtonsoft.Json.Linq.JToken.Parse
        |> string
      with
      // Task error
      | :? System.AggregateException as e -> e.InnerException.Message
      | e -> e.Message

    actual .=. expected

  let tests =
    testList
      "prettyResponseJson"
      [ tpwg typeof<Generator> "compare to ocaml" equalsOCaml ]


module PrettyRequestJson =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    // This should produce identical JSON to the OCaml function or customers will have an unexpected change
    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        | RT.DFnVal _ -> false
        | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    let actual =
      try
        dv |> BackendOnlyStdLib.LibHttpClient0.PrettyRequestJson.toPrettyRequestJson
      with
      | e -> e.Message

    let expected =
      try
        (OCamlInterop.toPrettyRequestJson dv).Result
      with
      // Task error
      | :? System.AggregateException as e -> e.InnerException.Message
      | e -> e.Message

    actual .=. expected

  let tests =
    testList
      "prettyRequestJson"
      [ tpwg typeof<Generator> "compare to ocaml" equalsOCaml ]

module LibJwtJson =
  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        // They're all printed as blocks, but the OCamlInterop doesn't work great - no point in fixing though
        | RT.DFnVal _ -> false
        | _ -> true)

  // This should produce absolutely identical JSON to the OCaml function or customers will have an unexpected change
  let equalsOCaml (dv : RT.Dval) : bool =
    let actual =
      dv
      |> BackendOnlyStdLib.LibJwt.Legacy.toYojson
      |> BackendOnlyStdLib.LibJwt.Legacy.toString

    let expected = (OCamlInterop.toSafePrettyMachineYojsonV1 dv).Result

    actual .=. expected

  let roundtripV1 (dv : RT.Dval) : bool =
    let t =
      task {
        let! meta = initializeTestCanvas "jwt-roundtrip-v1"

        let privateKey =
          "-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ\nd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN\nc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2\nuUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B\nCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW\nDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa\noNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq\np0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+\n1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ\nXUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv\nvz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua\n6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509\nrWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz\nNBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj\namEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr\nr48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5\nPJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J\nhpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g\nMcgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn\n4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV\nctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf\nn8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9\n5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb\nT/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I\nFW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=\n-----END RSA PRIVATE KEY-----"
        let publicKey =
          "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvxW2wuTTK2d0ob5mu/AS\nJ9vYDc/SXy06QAIepF9x9eoVZZVZd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqL\nn9/0Ag9ua4ml/ft7COprfEYA7klNc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGW\nCV7+3DF2RvDV2okk3x1ZKyBy2Rw2uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTw\nMlHbmVv9QMY5UetA9o05uPaAXH4BCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p\n4Ur22mtma+6ree45gsdnzlj1OASWDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ4\n4wIDAQAB\n-----END PUBLIC KEY-----"

        let callWithBoth code symtable =
          task {
            let ast = FSharpToExpr.parsePTExpr code
            let! expected =
              OCamlInterop.execute meta.owner meta.id ast symtable [] []

            let! state = executionStateFor meta Map.empty Map.empty
            let! actual =
              LibExecution.Execution.executeExpr
                state
                symtable
                (ast.toRuntimeType ())
            return (expected, actual)
          }

        // Encode it with v1 first
        let code = "JWT.signAndEncode_v1_ster priv payload"
        let symtable = Map [ "priv", RT.DStr privateKey; "payload", dv ]
        let! (expectedEncoded, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expectedEncoded) false "isn't an error"
        Expect.equalDval actual expectedEncoded "signed string matches"

        // check it can be read with v1 decode function
        let code = "JWT.verifyAndExtract_v1_ster pub encoded"
        let symtable = Map [ "pub", RT.DStr publicKey; "encoded", expectedEncoded ]
        let! (expected, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expected) false "isn't on the error rail"
        Expect.equalDval actual expected "extracted matches ocaml"

        return true
      }
    try
      if containsPassword dv || containsFakeDval dv then true else t.Result
    with
    | :? System.AggregateException as e ->
      print e.Message
      print e.StackTrace
      false



  let roundtripV0 (dv : RT.Dval) : bool =
    let t =
      task {
        let! meta = initializeTestCanvas "jwt-roundtrip-v0"

        let privateKey =
          "-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ\nd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN\nc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2\nuUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B\nCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW\nDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa\noNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq\np0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+\n1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ\nXUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv\nvz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua\n6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509\nrWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz\nNBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj\namEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr\nr48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5\nPJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J\nhpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g\nMcgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn\n4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV\nctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf\nn8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9\n5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb\nT/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I\nFW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=\n-----END RSA PRIVATE KEY-----"
        let publicKey =
          "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvxW2wuTTK2d0ob5mu/AS\nJ9vYDc/SXy06QAIepF9x9eoVZZVZd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqL\nn9/0Ag9ua4ml/ft7COprfEYA7klNc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGW\nCV7+3DF2RvDV2okk3x1ZKyBy2Rw2uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTw\nMlHbmVv9QMY5UetA9o05uPaAXH4BCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p\n4Ur22mtma+6ree45gsdnzlj1OASWDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ4\n4wIDAQAB\n-----END PUBLIC KEY-----"

        let callWithBoth code symtable =
          task {
            let ast = FSharpToExpr.parsePTExpr code
            let! expected =
              OCamlInterop.execute meta.owner meta.id ast symtable [] []

            let! state = executionStateFor meta Map.empty Map.empty
            let! actual =
              LibExecution.Execution.executeExpr
                state
                symtable
                (ast.toRuntimeType ())
            return (expected, actual)
          }

        // Encode it with v0 first
        let code = "JWT.signAndEncode_v0 priv payload"
        let symtable = Map [ "priv", RT.DStr privateKey; "payload", dv ]
        let! (expectedEncoded, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expectedEncoded) false "isn't an error"
        Expect.equalDval actual expectedEncoded "signed string matches"

        // check it can be read with v0 decode functions
        let code = "JWT.verifyAndExtract_v0_ster pub encoded"
        let symtable = Map [ "pub", RT.DStr publicKey; "encoded", expectedEncoded ]
        let! (expected, actual) = callWithBoth code symtable

        Expect.equal (RT.Dval.isFake expected) false "isn't on the error rail"
        Expect.equalDval actual expected "extracted matches ocaml"

        // For v0, the extracted often does not match the original
        // match actual with
        // | RT.DObj map ->
        //   Expect.equalDval map["payload"] dv "extracted matches original"
        // | _ -> Exception.raiseInternal "doesnt match" []

        return true
      }
    try
      if containsPassword dv || containsFakeDval dv then true else t.Result
    with
    | :? System.AggregateException as e ->
      print e.Message
      print e.StackTrace
      false



  let tests =
    testList
      "jwtJson"
      [ tpwg typeof<Generator> "roundtrip jwt v0" roundtripV0
        tpwg typeof<Generator> "roundtrip jwt v1" roundtripV1
        tpwg typeof<Generator> "comparing jwt json" equalsOCaml ]

module Passwords =

  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

  let passwordChecks (rawPassword : string) : bool =
    let t =
      task {
        let! meta = initializeTestCanvas "executePure"
        let! state = executionStateFor meta Map.empty Map.empty

        let ast =
          $"Password.check_v0 (Password.hash_v0 \"{rawPassword}\") \"{rawPassword}\""
          |> FSharpToExpr.parseRTExpr

        match! LibExecution.Execution.executeExpr state Map.empty ast with
        | RT.DBool true -> return true
        | _ -> return false
      }
    t.Result

  let tests =
    testList
      "password"
      [ tpwg typeof<Generator> "comparing passwords" passwordChecks ]

module BytesToString =

  type Generator =
    static member SafeString() : Arbitrary<string> = Arb.fromGen (G.string ())

  let toStringTest (bytes : byte []) : bool =
    let t =
      task {
        let! meta = initializeTestCanvas "bytes-to-string"

        let ast = $"toString_v0 myValue" |> FSharpToExpr.parsePTExpr
        let symtable = Map [ "myvalue", RT.DBytes bytes ]

        let! expected = OCamlInterop.execute meta.owner meta.id ast symtable [] []

        let! state = executionStateFor meta Map.empty Map.empty
        let! actual =
          LibExecution.Execution.executeExpr state symtable (ast.toRuntimeType ())

        if dvalEquality actual expected then return true else return false
      }
    t.Result

  let tests =
    testList
      "bytesToString"
      [ tpwg typeof<Generator> "comparing bytesToString" toStringTest ]


module Date =

  let roundtrip (date : System.DateTime) : bool =
    let truncate (d : System.DateTime) =
      d.AddTicks(-(d.Ticks % System.TimeSpan.TicksPerSecond))
    let date = truncate date
    let roundTripped =
      date.toIsoString ()
      |> System.DateTime.ofIsoString
      |> fun d -> d.toIsoString ()
      |> System.DateTime.ofIsoString
    roundTripped = date

  let tests = testList "date" [ testProperty "roundtrip" roundtrip ]


module ExecutePureFunctions =
  open LibExecution.ProgramTypes.Shortcuts

  let filterFloat (f : float) : bool =
    match f with
    | System.Double.PositiveInfinity -> false
    | System.Double.NegativeInfinity -> false
    | f when System.Double.IsNaN f -> false
    | f when f <= -1e+308 -> false
    | f when f >= 1e+308 -> false
    | _ -> true

  let ocamlIntUpperLimit = 4611686018427387903L

  let ocamlIntLowerLimit = -4611686018427387904L

  let isValidOCamlInt (i : int64) : bool =
    i <= ocamlIntUpperLimit && i >= ocamlIntLowerLimit


  type AllowedFuzzerErrorFileStructure =
    { functionToTest : Option<string>
      knownDifferingFunctions : Set<string>
      knownErrors : List<List<string>> }

  // Keep a list of allowed errors where we can edit it without recompiling
  let readAllowedErrors () =
    use r = new System.IO.StreamReader "tests/allowedFuzzerErrors.jsonc"
    let json = r.ReadToEnd()

    Json.Vanilla.deserializeWithComments<AllowedFuzzerErrorFileStructure> json

  let allowedErrors = readAllowedErrors ()

  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.fromGenShrink (G.string (), Arb.shrink<string>)

    static member Float() : Arbitrary<float> =
      Arb.fromGenShrink (
        gen {
          let specials =
            interestingFloats
            |> List.map Tuple2.second
            |> List.filter filterFloat
            |> List.map Gen.constant
            |> Gen.oneof

          let v = Gen.frequency [ (5, specials); (5, Arb.generate<float>) ]
          return! Gen.filter filterFloat v
        },
        Arb.shrinkNumber
      )

    static member Int64() : Arbitrary<int64> =
      Arb.fromGenShrink (
        gen {
          let specials =
            interestingInts
            |> List.map Tuple2.second
            |> List.filter isValidOCamlInt
            |> List.map Gen.constant
            |> Gen.oneof

          let v = Gen.frequency [ (5, specials); (5, Arb.generate<int64>) ]
          return! Gen.filter isValidOCamlInt v
        },
        Arb.shrinkNumber
      )

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter (function
        // These all break the serialization to OCaml
        | RT.DPassword _ -> false
        | RT.DFnVal _ -> false
        | RT.DFloat f -> filterFloat f
        | _ -> true)

    static member DType() : Arbitrary<RT.DType> =
      let rec isSupportedType =
        (function
        | RT.TInt
        | RT.TStr
        | RT.TVariable _
        | RT.TFloat
        | RT.TBool
        | RT.TNull
        | RT.TNull
        | RT.TDate
        | RT.TChar
        | RT.TUuid
        | RT.TBytes
        | RT.TError
        | RT.TDB (RT.TUserType _)
        | RT.TDB (RT.TRecord _)
        | RT.TUserType _ -> true
        | RT.TList t
        | RT.TDict t
        | RT.TOption t
        | RT.THttpResponse t -> isSupportedType t
        | RT.TResult (t1, t2) -> isSupportedType t1 && isSupportedType t2
        | RT.TFn (ts, rt) -> isSupportedType rt && List.all isSupportedType ts
        | RT.TRecord (pairs) ->
          pairs |> List.map Tuple2.second |> List.all isSupportedType
        | _ -> false) // FSTODO: expand list and support all types

      Arb.Default.Derive() |> Arb.filter isSupportedType


    static member Fn() : Arbitrary<PT.FQFnName.StdlibFnName * List<RT.Dval>> =

      // Ensure we pick a type instead of having heterogeneous lists
      let rec selectNestedType (typ : RT.DType) : Gen<RT.DType> =
        gen {
          match typ with
          | RT.TVariable name ->
            // Generally return a homogenous list. We'll sometimes get a
            // TVariable which will give us a heterogenous list. It's fine to
            // do that occasionally
            return! Arb.generate<RT.DType>
          | typ -> return typ
        }

      let genExpr (typ' : RT.DType) : Gen<RT.Expr> =
        let rec genExpr' typ s =
          let call mod_ fn version args =
            let call =
              RT.EFQFnValue(
                gid (),
                RT.FQFnName.Stdlib(RT.FQFnName.stdlibFnName mod_ fn version)
              )

            RT.EApply(gid (), call, args, RT.NotInPipe, RT.NoRail)

          gen {
            match typ with
            | RT.TInt ->
              let! v = Arb.generate<int64>
              return RT.EInteger(gid (), v)
            | RT.TStr ->
              let! v = Generators.string ()
              return RT.EString(gid (), v)
            | RT.TChar ->
              // We don't have a construct for characters, so create code to generate the character
              let! v = G.char ()
              return call "String" "toChar" 0 [ RT.EString(gid (), v) ]
            // Don't generate a random value as some random values are invalid
            // (eg constructor outside certain names). Ints should be fine for
            // whatever purpose there is here
            | RT.TVariable _ -> return! genExpr' RT.TInt s
            | RT.TFloat ->
              let! v = Arb.generate<float>
              return RT.EFloat(gid (), v)
            | RT.TBool ->
              let! v = Arb.generate<bool>
              return RT.EBool(gid (), v)
            | RT.TNull -> return RT.ENull(gid ())
            | RT.TList typ ->
              let! typ = selectNestedType typ
              let! v = (Gen.listOfLength s (genExpr' typ (s / 2)))
              return RT.EList(gid (), v)
            | RT.TDict typ ->
              let! typ = selectNestedType typ

              return!
                Gen.map
                  (fun l -> RT.ERecord(gid (), l))
                  (Gen.listOfLength
                    s
                    (Gen.zip (Generators.string ()) (genExpr' typ (s / 2))))
            | RT.TUserType (_name, _version) ->
              let! typ = Arb.generate<RT.DType>

              return!
                Gen.map
                  (fun l -> RT.ERecord(gid (), l))
                  (Gen.listOfLength
                    s
                    (Gen.zip (Generators.string ()) (genExpr' typ (s / 2))))

            | RT.TRecord pairs ->
              let! entries =
                List.fold
                  (Gen.constant [])
                  (fun (l : Gen<List<string * RT.Expr>>) ((k, t) : string * RT.DType) ->
                    gen {
                      let! l = l
                      let! v = genExpr' t s
                      return (k, v) :: l
                    })
                  pairs
                |> Gen.map List.reverse

              return RT.ERecord(gid (), entries)
            | RT.TOption typ ->
              match! Gen.optionOf (genExpr' typ s) with
              | Some v -> return RT.EConstructor(gid (), "Just", [ v ])
              | None -> return RT.EConstructor(gid (), "Nothing", [])
            | RT.TResult (okType, errType) ->
              let! v =
                Gen.oneof [ Gen.map Ok (genExpr' okType s)
                            Gen.map Error (genExpr' errType s) ]

              match v with
              | Ok v -> return RT.EConstructor(gid (), "Ok", [ v ])
              | Error v -> return RT.EConstructor(gid (), "Error", [ v ])

            | RT.TFn (paramTypes, returnType) ->
              let parameters =
                List.mapi
                  (fun i (v : RT.DType) ->
                    (id i, $"{v.toOldString().ToLower()}_{i}"))
                  paramTypes

              // FSTODO: occasionally use the wrong return type
              // FSTODO: can we use the argument to get this type?
              let! body = genExpr' returnType s
              return RT.ELambda(gid (), parameters, body)
            | RT.TBytes ->
              // FSTODO: this doesn't really do anything useful
              let! bytes = Arb.generate<byte []>
              let v = RT.EString(gid (), Base64.defaultEncodeToString bytes)
              return call "String" "toBytes" 0 [ v ]
            | RT.TDB _ ->
              let! name = Generators.string ()
              let ti = System.Globalization.CultureInfo.InvariantCulture.TextInfo
              let name = ti.ToTitleCase name
              return RT.EVariable(gid (), name)
            | RT.TDate ->
              let! d = Arb.generate<System.DateTime>
              return call "Date" "parse" 0 [ RT.EString(gid (), d.toIsoString ()) ]
            | RT.TUuid ->
              let! u = Arb.generate<System.Guid>
              return call "String" "toUUID" 0 [ RT.EString(gid (), string u) ]
            | RT.THttpResponse typ ->
              let! code = genExpr' RT.TInt s
              let! body = genExpr' typ s
              return call "Http" "response" 0 [ body; code ]
            | RT.TError ->
              let! msg = genExpr' RT.TStr s
              return call "Test" "typeError" 0 [ msg ]

            | _ ->
              return
                Exception.raiseInternal $"Unsupported type (yet!)" [ "typ", typ ]
          }

        Gen.sized (genExpr' typ')


      let genDval (typ' : RT.DType) : Gen<RT.Dval> =

        let rec genDval' typ s : Gen<RT.Dval> =
          gen {
            match typ with
            | RT.TInt ->
              let! v = Arb.generate<int64>
              return RT.DInt v
            | RT.TStr ->
              let! v = Generators.string ()
              return RT.DStr v
            | RT.TVariable _ ->
              let! newtyp = Arb.generate<RT.DType>
              return! genDval' newtyp s
            | RT.TFloat ->
              let! v = Arb.generate<float>
              return RT.DFloat v
            | RT.TBool -> return! Gen.map RT.DBool Arb.generate<bool>
            | RT.TNull -> return RT.DNull
            | RT.TList typ ->
              let! typ = selectNestedType typ
              return! Gen.map RT.DList (Gen.listOfLength s (genDval' typ (s / 2)))
            | RT.TDict typ ->
              let! typ = selectNestedType typ

              return!
                Gen.map
                  (fun l -> RT.DObj(Map.ofList l))
                  (Gen.listOfLength
                    s
                    (Gen.zip (Generators.string ()) (genDval' typ (s / 2))))
            // | RT.TIncomplete -> return! Gen.map RT.TIncomplete Arb.generate<incomplete>
            // | RT.TError -> return! Gen.map RT.TError Arb.generate<error>
            | RT.TDB _ -> return! Gen.map RT.DDB (Generators.string ())
            | RT.TDate ->
              return!
                Gen.map
                  (fun (dt : System.DateTime) ->
                    // Set milliseconds to zero
                    let dt = (dt.AddMilliseconds(-(double dt.Millisecond)))
                    RT.DDate dt)
                  Arb.generate<System.DateTime>
            | RT.TChar ->
              let! v = G.char ()
              return RT.DChar v
            // | RT.TPassword -> return! Gen.map RT.TPassword Arb.generate<password>
            | RT.TUuid -> return! Gen.map RT.DUuid Arb.generate<System.Guid>
            | RT.TOption typ ->
              return! Gen.map RT.DOption (Gen.optionOf (genDval' typ s))
            | RT.TBytes ->
              let! v = Arb.generate<byte []>
              return RT.DBytes v
            | RT.TResult (okType, errType) ->
              return!
                Gen.map
                  RT.DResult
                  (Gen.oneof [ Gen.map Ok (genDval' okType s)
                               Gen.map Error (genDval' errType s) ])
            | RT.TFn (paramTypes, returnType) ->
              let parameters =
                List.mapi
                  (fun i (v : RT.DType) -> (id i, $"{v.toOldString ()}_{i}"))
                  paramTypes

              let! body = genExpr returnType

              return
                (RT.DFnVal(
                  RT.Lambda
                    { parameters = parameters; symtable = Map.empty; body = body }
                ))
            | RT.TError ->
              let! source = Arb.generate<RT.DvalSource>
              let! str = Arb.generate<string>
              return RT.DError(source, str)
            | RT.TUserType (_name, _version) ->
              let! list =
                Gen.listOfLength
                  s
                  (Gen.zip (Generators.string ()) (genDval' typ (s / 2)))

              return RT.DObj(Map list)
            | RT.TRecord (pairs) ->
              let map =
                List.fold
                  (Gen.constant Map.empty)
                  (fun (m : Gen<RT.DvalMap>) ((k, t) : string * RT.DType) ->
                    gen {
                      let! m = m
                      let! v = genDval' t s
                      return Map.add k v m
                    })
                  pairs

              return! Gen.map RT.DObj map
            | RT.THttpResponse typ ->
              let! url = Arb.generate<string>
              let! code = Arb.generate<int64>
              let! headers = Arb.generate<List<string * string>>
              let! body = genDval' typ s

              return!
                Gen.oneof [ Gen.constant (RT.Response(code, headers, body))
                            Gen.constant (RT.Redirect url) ]
                |> Gen.map RT.DHttpResponse
            | RT.TErrorRail ->
              let! typ = Arb.generate<RT.DType>
              return! Gen.map RT.DErrorRail (genDval' typ s)
            | _ ->
              return Exception.raiseInternal "Type not supported yet" [ "type", typ ]
          }

        Gen.sized (genDval' typ')

      { new Arbitrary<PT.FQFnName.StdlibFnName * List<RT.Dval>>() with
          member x.Generator =
            gen {
              let fns =
                LibRealExecution.RealExecution.stdlibFns
                |> Lazy.force
                |> Map.values
                |> List.filter (fun fn ->
                  let name = string fn.name
                  let has set = Set.contains name set
                  let different = has allowedErrors.knownDifferingFunctions
                  let fsOnly = has (ApiServer.Functions.fsharpOnlyFns.Force())

                  if different || fsOnly then
                    false
                  elif allowedErrors.functionToTest = None then
                    // FSTODO: Add JWT and X509 functions here
                    fn.previewable = RT.Pure || fn.previewable = RT.ImpurePreviewable
                  elif Some name = allowedErrors.functionToTest then
                    true
                  else
                    false)

              let! fnIndex = Gen.choose (0, List.length fns - 1)
              let name = fns[fnIndex].name
              let signature = fns[fnIndex].parameters

              let unifiesWith (typ : RT.DType) =
                (fun dv ->
                  dv |> LibExecution.TypeChecker.unify (Map.empty) typ |> Result.isOk)

              let rec containsBytes (dv : RT.Dval) =
                match dv with
                | RT.DDB _
                | RT.DInt _
                | RT.DBool _
                | RT.DFloat _
                | RT.DNull
                | RT.DStr _
                | RT.DChar _
                | RT.DIncomplete _
                | RT.DFnVal _
                | RT.DError _
                | RT.DDate _
                | RT.DPassword _
                | RT.DUuid _
                | RT.DHttpResponse (RT.Redirect _)
                | RT.DOption None -> false
                | RT.DList l -> List.any containsBytes l
                | RT.DObj o -> o |> Map.values |> List.any containsBytes
                | RT.DHttpResponse (RT.Response (_, _, dv))
                | RT.DOption (Some dv)
                | RT.DErrorRail dv
                | RT.DResult (Ok dv)
                | RT.DResult (Error dv) -> containsBytes dv
                | RT.DBytes _ -> true

              let arg (i : int) (prevArgs : List<RT.Dval>) =
                // If the parameters need to be in a particular format to get
                // meaningful testing, generate them here.
                let specific =
                  gen {
                    match string name, i with
                    | "String::toInt_v1", 0
                    | "String::toInt", 0 ->
                      let! v = Arb.generate<int64>
                      return v |> string |> RT.DStr
                    | "String::toFloat", 0 ->
                      let! v = Arb.generate<float>
                      return v |> string |> RT.DStr
                    | "String::toUUID", 0 ->
                      let! v = Arb.generate<System.Guid>
                      return v |> string |> RT.DStr
                    | "String::padStart", 1
                    | "String::padEnd", 1 ->
                      let! v = Generators.char ()
                      return RT.DStr v
                    | _ -> return! genDval signature[i].typ
                  }
                // Still throw in random data occasionally test errors, edge-cases, etc.
                let randomValue =
                  gen {
                    let! typ = Arb.generate<RT.DType>
                    return! genDval typ
                  }

                Gen.frequency [ (1, randomValue); (99, specific) ]
                |> Gen.filter (fun dv ->
                  // Avoid triggering known errors in OCaml
                  match (i, dv, prevArgs, name.module_, name.function_, name.version)
                    with
                  // Specific OCaml exception (use `when`s here)
                  | 1, RT.DStr s, _, "String", "split", 0 when s = "" -> false
                  | 1, RT.DStr s, _, "String", "replaceAll", 0 when s = "" -> false
                  | 1, RT.DInt i, _, "Int", "power", 0
                  | 1, RT.DInt i, _, "", "^", 0 when i < 0L -> false
                  // Int Overflow
                  | 1, RT.DInt i, [ RT.DInt e ], "Int", "power", 0
                  | 1, RT.DInt i, [ RT.DInt e ], "", "^", 0 ->
                    i <> 1L
                    && i <> (-1L)
                    && isValidOCamlInt i
                    && i <= 2000L
                    && isValidOCamlInt (int64 (bigint e ** (int i)))
                  | 1, RT.DInt i, [ RT.DInt e ], "", "*", 0
                  | 1, RT.DInt i, [ RT.DInt e ], "Int", "multiply", 0 ->
                    isValidOCamlInt (e * i)
                  | 1, RT.DInt i, [ RT.DInt e ], "", "+", 0
                  | 1, RT.DInt i, [ RT.DInt e ], "Int", "add", 0 ->
                    isValidOCamlInt (e + i)
                  | 1, RT.DInt i, [ RT.DInt e ], "", "-", 0
                  | 1, RT.DInt i, [ RT.DInt e ], "Int", "subtract", 0 ->
                    isValidOCamlInt (e - i)
                  | 0, RT.DList l, _, "Int", "sum", 0 ->
                    l
                    |> List.map (function
                      | RT.DInt i -> i
                      | _ -> 0L)
                    |> List.fold 0L (+)
                    |> isValidOCamlInt
                  // Int overflow converting from Floats
                  | 0, RT.DFloat f, _, "Float", "floor", 0
                  | 0, RT.DFloat f, _, "Float", "roundDown", 0
                  | 0, RT.DFloat f, _, "Float", "roundTowardsZero", 0
                  | 0, RT.DFloat f, _, "Float", "round", 0
                  | 0, RT.DFloat f, _, "Float", "ceiling", 0
                  | 0, RT.DFloat f, _, "Float", "roundUp", 0
                  | 0, RT.DFloat f, _, "Float", "truncate", 0 ->
                    f |> int64 |> isValidOCamlInt
                  // gmtime out of range
                  | 1, RT.DInt i, _, "Date", "sub", 0
                  | 1, RT.DInt i, _, "Date", "subtract", 0
                  | 1, RT.DInt i, _, "Date", "add", 0
                  | 0, RT.DInt i, _, "Date", "fromSeconds", 0 -> i < 10000000L
                  // Out of memory
                  | _, RT.DInt i, _, "List", "range", 0
                  | 0, RT.DInt i, _, "List", "repeat", 0
                  | 2, RT.DInt i, _, "String", "padEnd", 0
                  | 2, RT.DInt i, _, "String", "padStart", 0 -> i < 10000L
                  // Exception
                  | 0, _, _, "", "toString", 0 -> not (containsBytes dv)
                  | _ -> true)

              match List.length signature with
              | 0 -> return (name, [])
              | 1 ->
                let! arg0 = arg 0 []
                return (name, [ arg0 ])
              | 2 ->
                let! arg0 = arg 0 []
                let! arg1 = arg 1 [ arg0 ]
                return (name, [ arg0; arg1 ])
              | 3 ->
                let! arg0 = arg 0 []
                let! arg1 = arg 1 [ arg0 ]
                let! arg2 = arg 2 [ arg0; arg1 ]
                return (name, [ arg0; arg1; arg2 ])
              | 4 ->
                let! arg0 = arg 0 []
                let! arg1 = arg 1 [ arg0 ]
                let! arg2 = arg 2 [ arg0; arg1 ]
                let! arg3 = arg 3 [ arg0; arg1; arg2 ]
                return (name, [ arg0; arg1; arg2; arg3 ])
              | _ ->
                Exception.raiseInternal
                  "No support for generating functions with over 4 parameters yet"
                  []

                return (name, [])
            } }


  let equalsOCaml ((fn, args) : (PT.FQFnName.StdlibFnName * List<RT.Dval>)) : bool =
    let t =
      task {
        let! meta = initializeTestCanvas "ExecutePureFunction"
        let args = List.mapi (fun i arg -> ($"v{i}", arg)) args
        let fnArgList = List.map (fun (name, _) -> eVar name) args

        let ast = PT.EFnCall(gid (), RT.FQFnName.Stdlib fn, fnArgList, PT.NoRail)
        let st = Map.ofList args

        let! expected = OCamlInterop.execute meta.owner meta.id ast st [] []

        let! state = executionStateFor meta Map.empty Map.empty

        let! actual =
          LibExecution.Execution.executeExpr state st (ast.toRuntimeType ())

        // Error messages are not required to be directly the same between
        // old and new implementations. However, this can hide errors, so we
        // manually verify them all to make sure we didn't miss any.
        let errorAllowed (debug : bool) (actualMsg : string) (expectedMsg : string) =
          let expectedMsg =
            // Some OCaml errors are in a JSON struct, so get the message and compare that
            try
              let mutable options = System.Text.Json.JsonDocumentOptions()
              options.CommentHandling <- System.Text.Json.JsonCommentHandling.Skip

              let jsonDocument =
                System.Text.Json.JsonDocument.Parse(expectedMsg, options)

              let mutable elem = System.Text.Json.JsonElement()

              if jsonDocument.RootElement.TryGetProperty("short", &elem) then
                elem.GetString()
              else
                expectedMsg
            with
            | _ -> expectedMsg

          if actualMsg = expectedMsg then
            true
          else
            // enable to allow dynamically updating without restarting
            // let allowedErrors = readAllowedErrors ()

            List.any
              (function
              | [ namePat; actualPat; expectedPat ] ->
                let regexMatch str regex =
                  Regex.Match(str, regex, RegexOptions.Singleline)

                let nameMatches = (regexMatch (string fn) namePat).Success
                let actualMatch = regexMatch actualMsg actualPat
                let expectedMatch = regexMatch expectedMsg expectedPat

                // Not only should we check that the error message matches,
                // but also that the captures match in both.
                let sameGroups =
                  actualMatch.Groups.Count = expectedMatch.Groups.Count

                let actualGroupMatches = Dictionary.empty ()
                let expectedGroupMatches = Dictionary.empty ()

                if sameGroups && actualMatch.Groups.Count > 1 then
                  // start at 1, because 0 is the whole match
                  for i = 1 to actualMatch.Groups.Count - 1 do
                    let group = actualMatch.Groups[i]
                    actualGroupMatches.Add(group.Name, group.Value)
                    let group = expectedMatch.Groups[i]
                    expectedGroupMatches.Add(group.Name, group.Value)

                let dToL d = Dictionary.toList d |> List.sortBy Tuple2.first

                let groupsMatch =
                  (dToL actualGroupMatches) = (dToL expectedGroupMatches)

                if nameMatches && debug then
                  print "\n\n\n======================"
                  print (if nameMatches then "✅" else "❌")
                  print (if actualMatch.Success then "✅" else "❌")
                  print (if expectedMatch.Success then "✅" else "❌")
                  print (if groupsMatch then "✅" else "❌")
                  print $"{string fn}"
                  print $"{actualMsg}"
                  print $"{expectedMsg}\n\n"
                  print $"{namePat}"
                  print $"{actualPat}"
                  print $"{expectedPat}"
                  print $"actualGroupMatches: {dToL actualGroupMatches}"
                  print $"expectedGroupMatches: {dToL expectedGroupMatches}"

                nameMatches
                && actualMatch.Success
                && expectedMatch.Success
                && groupsMatch
              | _ ->
                Exception.raiseInternal
                  "Invalid json in tests/allowedFuzzerErrors.json"
                  [])
              allowedErrors.knownErrors


        let debugFn () =
          debuG "\n\n\nfn" fn
          debuG "args" (List.map (fun (_, v) -> debugDval v) args)

        if not (Expect.isCanonical expected) then
          debugFn ()
          debuG "ocaml (expected) is not normalized" (debugDval expected)
          return false
        elif not (Expect.isCanonical actual) then
          debugFn ()
          debuG "fsharp (actual) is not normalized" (debugDval actual)
          return false
        elif dvalEquality actual expected then
          return true
        else
          match actual, expected with
          | RT.DError (_, aMsg), RT.DError (_, eMsg) ->
            let allowed = errorAllowed false aMsg eMsg
            // For easier debugging. Check once then step through
            let allowed2 =
              if not allowed then errorAllowed true aMsg eMsg else allowed

            if not allowed2 then
              debugFn ()

              print $"Got different error msgs:\n\"{aMsg}\"\n\nvs\n\"{eMsg}\"\n\n"

            return allowed
          | RT.DResult (Error (RT.DStr aMsg)), RT.DResult (Error (RT.DStr eMsg)) ->
            let allowed = errorAllowed false aMsg eMsg
            // For easier debugging. Check once then step through
            let allowed2 =
              if not allowed then errorAllowed true aMsg eMsg else allowed

            if not allowed2 then
              debugFn ()

              print $"Got different DError msgs:\n\"{aMsg}\"\n\nvs\n\"{eMsg}\"\n\n"

            return allowed
          | _ ->
            debugFn ()
            debuG "ocaml (expected)" (debugDval expected)
            debuG "fsharp (actual) " (debugDval actual)
            return false
      }

    Task.WaitAll [| t :> Task |]
    t.Result

  let tests =
    testList
      "executePureFunctions"
      [ tpwg typeof<Generator> "equalsOCaml" equalsOCaml ]


let stillBuggy = testList "still buggy" [ OCamlInterop.tests; FQFnName.tests ]

let knownGood =
  testList
    "known good"
    ([ Roundtrippable.tests
       Queryable.tests
       DeveloperRepr.tests
       PrettyRequestJson.tests
       PrettyResponseJson.tests
       HttpClient.tests
       EndUserReadable.tests
       Hashing.tests
       PrettyMachineJson.tests
       LibJwtJson.tests
       Passwords.tests
       BytesToString.tests
       Date.tests
       ExecutePureFunctions.tests ])

let tests = testList "FuzzTests" [ knownGood; stillBuggy ]

// FSTODO: add fuzz test that running analysis gets the same results for different exprs

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
