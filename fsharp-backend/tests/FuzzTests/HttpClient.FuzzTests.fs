/// Generators and FuzzTests that ensure HttpClient functionality
/// is consistent across OCaml and F# backends
module FuzzTests.HttpClient

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open Prelude.Tablecloth
open Tablecloth

open TestUtils.TestUtils
open FuzzTests.Utils

module RT = LibExecution.RuntimeTypes
module OCamlInterop = TestUtils.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module HttpQueryEncoding = BackendOnlyStdLib.HttpQueryEncoding
module G = Generators

type Generator =
  static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
    G.NodaTime.LocalDateTime
  static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant

  static member String() : Arbitrary<string> = G.OCamlSafeUnicodeString

  static member Dval() : Arbitrary<RT.Dval> =
    Arb.Default.Derive()
    |> Arb.filter (fun dval ->
      match dval with
      | RT.DFnVal _ -> false
      | _ -> true)

type QueryStringGenerator =
  static member String() : Arbitrary<string> =
    Gen.listOf (Gen.listOf (G.ocamlSafeUnicodeString))
    |> Gen.map (List.map (String.concat "="))
    |> Gen.map (String.concat "&")
    |> Arb.fromGen

/// Checks that a Dval is consistently converted
/// to a URL-safe string across OCaml and F# backends
let dvalToUrlStringExn (l : List<string * RT.Dval>) : bool =
  let dv = RT.DObj(Map l)

  HttpQueryEncoding.toUrlString dv .=. (OCamlInterop.toUrlString dv).Result

/// Checks that a Dval is consistently converted
/// to a querystring-safe string across OCaml and F# backends
let dvalToQuery (l : List<string * RT.Dval>) : bool =
  let dv = RT.DObj(Map l)
  (HttpQueryEncoding.toQuery dv |> Exception.unwrapResultInternal [ "dv", dv ])
  .=. (OCamlInterop.dvalToQuery dv).Result

/// Checks that a Dval is consistently converted
/// to a form-encoding-safe string across OCaml and F# backends
let dvalToFormEncoding (l : List<string * RT.Dval>) : bool =
  let dv = RT.DObj(Map l)
  (HttpQueryEncoding.toFormEncoding dv
   |> Exception.unwrapResultInternal [ "dv", dv ])
  .=. (OCamlInterop.dvalToFormEncoding dv).Result

/// Checks that provided query strings are parsed as URL route parameters
/// consistently across OCaml and F# backends
let queryStringToParams (s : string) : bool =
  HttpQueryEncoding.parseQueryString_ s
  .=. (OCamlInterop.queryStringToParams s).Result

let queryToDval (q : List<string * List<string>>) : bool =
  HttpQueryEncoding.ofQuery q .=. (OCamlInterop.queryToDval q).Result

let queryToEncodedString (q : List<string * List<string>>) : bool =
  HttpQueryEncoding.queryToEncodedString_ q
  .=. (OCamlInterop.paramsToQueryString q).Result

// FSTODO replace with simple `let tests = ...` once issues resolved
module Tests =
  let knownGood config =
    let test name fn = testProperty config typeof<Generator> name fn
    testList
      "HttpClient, known good"
      [ test "dvalToUrlStringExn" dvalToUrlStringExn
        test "dvalToQuery" dvalToQuery
        test "dvalToFormEncoding" dvalToFormEncoding
        test "queryToDval" queryToDval
        test "queryToEncodedString" queryToEncodedString ]

  let knownBad config =
    testList
      "HttpClient, known bad"
      [ testProperty
          config
          typeof<QueryStringGenerator>
          "queryStringToParams"
          queryStringToParams ] // fails on "&=&"
