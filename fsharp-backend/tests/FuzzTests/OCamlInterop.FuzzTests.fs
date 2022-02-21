module FuzzTests.OCamlInterop

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
open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalReprExternal = LibExecution.DvalReprExternal
module DvalReprInternal = LibExecution.DvalReprInternal
module G = Generators

let tpwg = testPropertyWithGenerator

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

let binaryExprRoundtrip (pair : PT.Expr * tlid) : bool =
  pair
  |> exprTLIDPairToCachedBinary
  |> result
  |> exprTLIDPairOfCachedBinary
  |> result
  .=. pair

let binaryHandlerRoundtrip (a : PT.Handler.T) : bool =
  let h = PT.TLHandler a

  h
  |> toplevelToCachedBinary
  |> result
  |> (fun bin -> bin, None)
  |> toplevelBin2Json
  |> result
  .=. h

let binaryToplevelRoundtrip (tl : PT.Toplevel) : bool =
  tl
  |> toplevelToCachedBinary
  |> result
  |> (fun bin -> bin, None)
  |> toplevelBin2Json
  |> result
  .=. tl

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
      isInteroperable
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

      isInteroperable
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
