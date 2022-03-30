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
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module G = Generators


type Generator =
  inherit G.NodaTime.All

  static member String() : Arbitrary<string> =
    // FSTODO: add in unicode
    // Generators.ocamlSafeString |> Arb.fromGen
    Arb.Default.String() |> Arb.filter G.isSafeOCamlString

  static member Dval() : Arbitrary<RT.Dval> =
    Arb.Default.Derive()
    |> Arb.filter (function
      | RT.DFnVal _ -> false
      | _ -> true)

type QueryStringGenerator =
  static member String() : Arbitrary<string> =
    Gen.listOf (Gen.listOf (Generators.ocamlSafeString))
    |> Gen.map (List.map (String.concat "="))
    |> Gen.map (String.concat "&")
    |> Arb.fromGen

/// Checks that a Dval is consistently converted
/// to a URL-safe string across OCaml and F# backends
let dvalToUrlStringExn (l : List<string * RT.Dval>) : bool =
  let dv = RT.DObj(Map l)

  DvalReprExternal.toUrlString dv .=. (OCamlInterop.toUrlString dv).Result

/// Checks that a Dval is consistently converted
/// to a querystring-safe string across OCaml and F# backends
let dvalToQuery (l : List<string * RT.Dval>) : bool =
  let dv = RT.DObj(Map l)
  DvalReprExternal.toQuery dv |> Result.unwrapUnsafe
  .=. (OCamlInterop.dvalToQuery dv).Result

/// Checks that a Dval is consistently converted
/// to a form-encoding-safe string across OCaml and F# backends
let dvalToFormEncoding (l : List<string * RT.Dval>) : bool =
  let dv = RT.DObj(Map l)
  (DvalReprExternal.toFormEncoding dv |> Result.unwrapUnsafe)
  .=. (OCamlInterop.dvalToFormEncoding dv).Result

/// Checks that provided query strings are parsed as URL route parameters
/// consistently across OCaml and F# backends
let queryStringToParams (s : string) : bool =
  DvalReprExternal.parseQueryString s
  .=. (OCamlInterop.queryStringToParams s).Result

let queryToDval (q : List<string * List<string>>) : bool =
  DvalReprExternal.ofQuery q .=. (OCamlInterop.queryToDval q).Result

let queryToEncodedString (q : List<string * List<string>>) : bool =
  DvalReprExternal.queryToEncodedString q
  .=. (OCamlInterop.paramsToQueryString q).Result

let tests =
  let test name fn = testProperty typeof<Generator> name fn
  testList
    "HttpClient"
    [ test "dvalToUrlStringExn" dvalToUrlStringExn // FSTODO: unicode
      test "dvalToQuery" dvalToQuery
      test "dvalToFormEncoding" dvalToFormEncoding
      testProperty
        typeof<QueryStringGenerator>
        "queryStringToParams"
        queryStringToParams // only &=& fails
      test "queryToDval" queryToDval
      test "queryToEncodedString" queryToEncodedString ]
