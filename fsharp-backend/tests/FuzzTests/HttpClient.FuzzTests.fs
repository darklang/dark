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

let tpwg = testPropertyWithGenerator


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
  (DvalReprExternal.toFormEncoding dv |> Result.unwrapUnsafe)
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
