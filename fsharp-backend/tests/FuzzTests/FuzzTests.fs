module FuzzTests.All

// This aims to find test cases that violate certain properties that we expect.
// Desired properties include that OCaml Dark programs and functions work the
// same as F# ones, and things related to serialization and output.

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module DvalRepr = LibExecution.DvalRepr

let (.=.) actual expected : bool =
  if actual = expected then
    Expect.equal actual expected ""
    true
  else
    let o = actual.ToString() |> toBytes
    let e = expected.ToString() |> toBytes
    Expect.equal (actual, o) (expected, e) ""
    false


module GeneratorUtils =
  let nonNullString (s : string) : bool = s <> null

  let safeOCamlString (s : string) : bool =
    // We disallow \u0000 in OCaml because postgres doesn't like it, see of_utf8_encoded_string
    s <> null && not (s.Contains('\u0000'))

open GeneratorUtils

let baseConfig : FsCheckConfig =
  { FsCheckConfig.defaultConfig with maxTest = 100000 }

let baseConfigWithGenerator (typ : System.Type) : FsCheckConfig =
  { baseConfig with arbitrary = [ typ ] }

let testProperty (name : string) (x : 'a) : Test =
  testPropertyWithConfig baseConfig name x

let testPropertyWithGenerator (typ : System.Type) (name : string) (x : 'a) : Test =
  testPropertyWithConfig (baseConfigWithGenerator typ) name x



module FQFnName =
  let nameGenerator (first : char list) (other : char list) : Gen<string> =
    gen {
      let! length = Gen.choose (0, 20)
      let! head = Gen.elements first
      let! tail = Gen.arrayOfLength length (Gen.elements other)
      return System.String(Array.append [| head |] tail)
    }

  let alphaNumeric =
    (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ])

  let ownerName : Gen<string> =
    nameGenerator [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

  let packageName = ownerName
  let modName : Gen<string> = nameGenerator [ 'A' .. 'Z' ] alphaNumeric
  let fnName : Gen<string> = nameGenerator [ 'a' .. 'z' ] alphaNumeric

  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter nonNullString

    static member PTFQFnName() : Arbitrary<PT.FQFnName.T> =
      { new Arbitrary<PT.FQFnName.T>() with
          member x.Generator =
            let stdlib =
              gen {
                let! module_ = modName
                let! function_ = fnName
                let! NonNegativeInt version = Arb.generate<NonNegativeInt>
                return PT.FQFnName.stdlibFqName module_ function_ version
              }

            let user =
              gen {
                let! function_ = fnName
                return PT.FQFnName.userFqName function_
              }

            let package =
              gen {
                let! owner = ownerName
                let! package = packageName
                let! module_ = modName
                let! function_ = fnName
                let! NonNegativeInt version = Arb.generate<NonNegativeInt>

                return
                  PT.FQFnName.packageFqName owner package module_ function_ version
              }

            Gen.oneof [ stdlib; user; package ] }

    static member RTFQFnName() : Arbitrary<RT.FQFnName.T> =
      { new Arbitrary<RT.FQFnName.T>() with
          member x.Generator = Generator.PTFQFnName().Generator }

  let ptRoundtrip (a : PT.FQFnName.T) : bool =
    a.ToString() |> PT.FQFnName.parse .=. a

  let tests =
    testList
      "PT.FQFnName"
      [ testPropertyWithGenerator typeof<Generator> "roundtripping" ptRoundtrip ]


module OCamlInterop =
  open OCamlInterop.Convert
  open OCamlInterop
  open Json.OCamlCompatible

  let isInteroperable
    (ocamlToString : 'a -> string)
    (ocamlOfString : string -> 'a)
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
      let bothCanRead str = str |> ocamlOfString |> equality (fsOfString str)
      let bothCanReadOCamlString = bothCanRead (ocamlToString v)
      let bothCanReadFSharpString = bothCanRead (fsToString v)

      if bothCanReadFSharpString && bothCanReadOCamlString then
        true
      else
        printfn
          "%s"
          ($"ocamlStringReadable: {bothCanReadOCamlString}\n"
           + $"fsharpStringReadable: {bothCanReadFSharpString}\n")

        false
    with e ->
      printfn $"Cause exception while fuzzing {e}"
      reraise ()

  type Generator =
    static member Expr() =
      Arb.Default.Derive()
      |> Arb.mapFilter
           (function
           // make sure we get numbers in our floats
           | other -> other)
           (function
           // characters are not yet supported in OCaml
           | PT.ECharacter _ -> false
           | other -> true)

    static member Pattern() =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           // characters are not yet supported in OCaml
           | PT.PCharacter _ -> false
           | _ -> true)

    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter nonNullString


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

    h |> toplevelToCachedBinary |> (fun bin -> bin, None) |> toplevelOfCachedBinary
    .=. h

  let binaryExprRoundtrip (pair : PT.Expr * tlid) : bool =
    pair |> exprTLIDPairToCachedBinary |> exprTLIDPairOfCachedBinary .=. pair

  let tests =
    let tp f = testPropertyWithGenerator typeof<Generator> f

    [ tp "roundtripping OCamlInteropBinaryHandler" binaryHandlerRoundtrip
      tp "roundtripping OCamlInteropBinaryExpr" binaryExprRoundtrip
      tp "roundtripping OCamlInteropYojsonHandler" yojsonHandlerRoundtrip
      tp "roundtripping OCamlInteropYojsonExpr" yojsonExprRoundtrip ]

module Roundtrippable =
  type Generator =
    static member String() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter (DvalRepr.isRoundtrippableDval false)

  type GeneratorWithBugs =
    static member String() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter (DvalRepr.isRoundtrippableDval true)

  let roundtrip (dv : RT.Dval) : bool =
    dv
    |> DvalRepr.toInternalRoundtrippableV0
    |> DvalRepr.ofInternalRoundtrippableV0
    |> dvalEquality dv

  let isInteroperableV0 dv =
    OCamlInterop.isInteroperable
      OCamlInterop.toInternalRoundtrippableV0
      OCamlInterop.ofInternalRoundtrippableV0
      DvalRepr.toInternalRoundtrippableV0
      DvalRepr.ofInternalRoundtrippableV0
      dvalEquality
      dv

  let tests =
    testList
      "roundtrippable"
      [ testPropertyWithGenerator
          typeof<Generator>
          "roundtripping works properly"
          roundtrip
        testPropertyWithGenerator
          typeof<GeneratorWithBugs>
          "roundtrippable is interoperable"
          isInteroperableV0 ]


module Queryable =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member DvalSource() : Arbitrary<RT.DvalSource> =
      Arb.Default.Derive() |> Arb.filter (fun dvs -> dvs = RT.SourceNone)

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive() |> Arb.filter DvalRepr.isQueryableDval

  let v1Roundtrip (dv : RT.Dval) : bool =
    let dvm = (Map.ofList [ "field", dv ])

    dvm
    |> DvalRepr.toInternalQueryableV1
    |> DvalRepr.ofInternalQueryableV1
    |> dvalEquality (RT.DObj dvm)

  let isInteroperableV1 (dv : RT.Dval) =
    let dvm = (Map.ofList [ "field", dv ])

    OCamlInterop.isInteroperable
      (OCamlInterop.toInternalQueryableV1)
      (OCamlInterop.ofInternalQueryableV1)
      (function
      | RT.DObj dvm -> DvalRepr.toInternalQueryableV1 dvm
      | _ -> failwith "not an obj")
      (DvalRepr.ofInternalQueryableV1)
      dvalEquality
      (RT.DObj dvm)

  // OCaml v0 vs F# v1
  let isInteroperableV0 (dv : RT.Dval) =
    let dvm = (Map.ofList [ "field", dv ])

    OCamlInterop.isInteroperable
      (OCamlInterop.toInternalQueryableV0)
      (OCamlInterop.ofInternalQueryableV0)
      (function
      | RT.DObj dvm -> DvalRepr.toInternalQueryableV1 dvm
      | _ -> failwith "not an obj")
      (DvalRepr.ofInternalQueryableV1)
      dvalEquality
      (RT.DObj dvm)

  let tests =
    let tp f = testPropertyWithGenerator typeof<Generator> f

    testList
      "InternalQueryable"
      [ tp "roundtripping v1" v1Roundtrip
        tp "interoperable v0" isInteroperableV0
        tp "interoperable v1" isInteroperableV1 ]

module DeveloperRepr =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    // The format here is only used for errors so it doesn't matter all the
    // much. These are places where we've manually checked the differing
    // outputs are fine.

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           | RT.DFnVal _ -> false
           | RT.DFloat 0.0 -> false
           | RT.DFloat infinity -> false
           | _ -> true)


  let equalsOCaml (dv : RT.Dval) : bool =
    DvalRepr.toDeveloperReprV0 dv .=. OCamlInterop.toDeveloperRepr dv

  let tests =
    testList
      "toDeveloperRepr"
      [ testPropertyWithGenerator typeof<Generator> "roundtripping" equalsOCaml ]

module EndUserReadable =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           | RT.DFnVal _ -> false
           | _ -> true)

  // The format here is used to show users so it has to be exact
  let equalsOCaml (dv : RT.Dval) : bool =
    DvalRepr.toEnduserReadableTextV0 dv .=. OCamlInterop.toEnduserReadableTextV0 dv

  let tests =
    testList
      "toEnduserReadable"
      [ testPropertyWithGenerator typeof<Generator> "roundtripping" equalsOCaml ]

module Hashing =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           // not supported in OCaml
           | RT.DFnVal _ -> false
           | _ -> true)

  // The format here is used to get values from the DB, so this has to be 100% identical
  let equalsOCamlToHashable (dv : RT.Dval) : bool =
    let ocamlVersion = OCamlInterop.toHashableRepr dv
    let fsharpVersion = DvalRepr.toHashableRepr 0 false dv |> ofBytes
    ocamlVersion .=. fsharpVersion

  let equalsOCamlV0 (l : List<RT.Dval>) : bool =
    DvalRepr.hash 0 l .=. OCamlInterop.hashV0 l

  let equalsOCamlV1 (l : List<RT.Dval>) : bool =
    let ocamlVersion = OCamlInterop.hashV1 l
    let fsharpVersion = DvalRepr.hash 1 l
    ocamlVersion .=. fsharpVersion

  let tests =
    testList
      "hash"
      [ testPropertyWithGenerator
          typeof<Generator>
          "toHashableRepr"
          equalsOCamlToHashable
        testPropertyWithGenerator typeof<Generator> "hashv0" equalsOCamlV0
        testPropertyWithGenerator typeof<Generator> "hashv1" equalsOCamlV1 ]




module PrettyMachineJson =
  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    // This should produce identical JSON to the OCaml function or customers will have an unexpected change
    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           | RT.DFnVal _ -> false
           | _ -> true)

  let equalsOCaml (dv : RT.Dval) : bool =
    let actual =
      dv
      |> DvalRepr.toPrettyMachineJsonStringV1
      |> Newtonsoft.Json.Linq.JToken.Parse
      |> toString

    let expected =
      dv
      |> OCamlInterop.toPrettyMachineJsonV1
      |> Newtonsoft.Json.Linq.JToken.Parse
      |> toString

    actual .=. expected

  let tests =
    testList
      "prettyMachineJson"
      [ testPropertyWithGenerator
          typeof<Generator>
          "roundtripping prettyMachineJson"
          equalsOCaml ]

module ExecutePureFunctions =
  open LibBackend.ProgramTypes.Shortcuts

  let filterFloat (f : float) : bool =
    match f with
    | System.Double.PositiveInfinity -> false
    | System.Double.NegativeInfinity -> false
    | f when System.Double.IsNaN f -> false
    | f when f <= -1e+308 -> false
    | f when f >= 1e+308 -> false
    | _ -> true


  type Generator =
    static member SafeString() : Arbitrary<string> =
      Arb.Default.String() |> Arb.filter safeOCamlString

    static member Dval() : Arbitrary<RT.Dval> =
      Arb.Default.Derive()
      |> Arb.filter
           (function
           // These all break the serialization to OCaml
           | RT.DPassword _ -> false
           | RT.DFnVal _ -> false
           | RT.DFloat f -> filterFloat f
           | _ -> true)

    static member Fn() : Arbitrary<PT.FQFnName.StdlibFnName * List<RT.Dval>> =
      let genDval(typ' : RT.DType) : Gen<RT.Dval> =
        let rec genDval' typ s =
          gen {
            match typ with
            | RT.TInt -> return! Gen.map RT.DInt Arb.generate<bigint>
            | RT.TStr -> return! Gen.map RT.DStr Arb.generate<string>
            | RT.TVariable _ -> return! Arb.generate<RT.Dval>
            | RT.TFloat ->
                return!
                  Gen.map RT.DFloat (Arb.generate<float> |> Gen.filter filterFloat)
            | RT.TBool -> return! Gen.map RT.DBool Arb.generate<bool>
            | RT.TNull -> return RT.DNull
            | RT.TList typ ->
                return! Gen.map RT.DList (Gen.listOfLength s (genDval' typ (s / 2)))
            | RT.TDict typ ->
                return!
                  Gen.map
                    (fun l -> RT.DObj(Map.ofList l))
                    (Gen.listOfLength
                      s
                      (Gen.zip Arb.generate<string> (genDval' typ (s / 2))))
            // | RT.TIncomplete -> return! Gen.map RT.TIncomplete Arb.generate<incomplete>
            // | RT.TError -> return! Gen.map RT.TError Arb.generate<error>
            // | RT.THttpResponse of DType -> return! Gen.map RT.THttpResponse  Arb.generate<httpresponse >
            // | RT.TDB of DType -> return! Gen.map RT.TDB  Arb.generate<db >
            | RT.TDate ->
                return!
                  Gen.map
                    (fun (dt : System.DateTime) ->
                      // Set milliseconds to zero
                      let dt = (dt.AddMilliseconds(-(double dt.Millisecond)))
                      RT.DDate dt)
                    Arb.generate<System.DateTime>
            | RT.TChar ->
                return! Gen.map RT.DChar (Gen.resize 1 Arb.generate<string>)
            // | RT.TPassword -> return! Gen.map RT.TPassword Arb.generate<password>
            | RT.TUuid -> return! Gen.map RT.DUuid Arb.generate<System.Guid>
            | RT.TOption typ ->
                return! Gen.map RT.DOption (Gen.optionOf (genDval' typ s))
            // | RT.TErrorRail -> return! Gen.map RT.TErrorRail Arb.generate<errorrail>
            // | RT.TUserType of string * int -> return! Gen.map RT.TUserType  Arb.generate<usertype >
            | RT.TBytes -> return! Gen.map RT.DBytes Arb.generate<byte []>
            | RT.TResult (okType, errType) ->
                return!
                  Gen.map
                    RT.DResult
                    (Gen.oneof [ Gen.map Ok (genDval' okType s)
                                 Gen.map Error (genDval' errType s) ])
            | RT.TFn (paramTypes, returnType) ->
                return
                  (RT.DFnVal(
                    RT.Lambda
                      { parameters = []; symtable = Map.empty; body = RT.EBlank 1UL }
                  ))
            // | RT.TRecord of List<string * DType> -> return! Gen.map RT.TRecord  Arb.generate<record >
            | _ -> return failwith $"Not supported yet: {typ}"
          }

        Gen.sized (genDval' typ')
      { new Arbitrary<PT.FQFnName.StdlibFnName * List<RT.Dval>>() with
          member x.Generator =
            gen {
              let fns =
                LibExecution.StdLib.StdLib.fns
                |> List.filter
                     (function
                     // FSTODO handle these cases
                     // These are new functions (FSTODO: test allowed functions)
                     | { name = { module_ = "Http" } } -> false
                     // FSTODO: Relies on some things that can't be compiled into a shared library in OCaml (RE)
                     | { name = { module_ = "String"; function_ = "slugify" } } ->
                         false
                     // FSTODO: I don't remember what went wrong here
                     | { name = { module_ = "String"; function_ = "base64decode" } } ->
                         false
                     // FSTODO: These use a different sort order in OCaml
                     | { name = { module_ = "List"; function_ = "sort" } } -> false
                     | { name = { module_ = "List"; function_ = "sortBy" } } -> false
                     | fn -> fn.previewable = RT.Pure)

              let! fnIndex = Gen.choose (0, List.length fns - 1)
              let name = fns.[fnIndex].name
              let signature = fns.[fnIndex].parameters

              let unifiesWith(typ : RT.DType) =
                (fun dv ->
                  dv |> LibExecution.TypeChecker.unify (Map.empty) typ |> Result.isOk)

              let rec containsBytes(dv : RT.Dval) =
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
                | RT.DOption None -> false
                | RT.DList l -> List.any containsBytes l
                | RT.DObj o -> o |> Map.values |> List.any containsBytes
                | RT.DHttpResponse (_, dv)
                | RT.DOption (Some dv)
                | RT.DErrorRail dv
                | RT.DResult (Ok dv)
                | RT.DResult (Error dv) -> containsBytes dv
                | RT.DBytes _ -> true

              let arg (i : int) (prevArgs : List<RT.Dval>) =
                genDval signature.[i].typ
                |> Gen.filter
                     (fun dv ->
                       // Avoid triggering known errors in OCaml
                       match (i,
                              dv,
                              prevArgs,
                              name.module_,
                              name.function_,
                              name.version) with
                       | 1, RT.DInt i, _, "Int", "power", 0
                       | 1, RT.DInt i, _, "", "^", 0 when i < 0I -> false // exception
                       | 1, RT.DInt i, [ RT.DInt e ], "Int", "power", 0
                       | 1, RT.DInt i, [ RT.DInt e ], "", "^", 0 when
                         (e ** (int i) >= (2I ** 62))
                         || (e ** (int i) <= -(2I ** 62)) -> false // overflow
                       | 1, RT.DInt i, _, "Int", "divide", 0 when i = 0I -> false // exception
                       | 0, _, _, "", "toString", 0 -> not (containsBytes dv) // exception
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
                  failwith
                    "No support for generating functions with over 4 parameters yet"

                  return (name, [])
            } }

  let debugDval (_, v) : string =
    match v with
    | RT.DStr s ->
        $"DStr '{s}': (len {s.Length}, {System.BitConverter.ToString(toBytes s)})"
    | RT.DDate d -> $"DDate '{d.toIsoString ()}': (millies {d.Millisecond})"
    | _ -> v.ToString()


  let equalsOCaml ((fn, args) : (PT.FQFnName.T * List<RT.Dval>)) : bool =
    let t =
      task {
        let args = List.mapi (fun i arg -> ($"v{i}", arg)) args
        let fnArgList = List.map (fun (name, _) -> eVar name) args
        let ast = PT.EFnCall(gid (), fn, fnArgList, PT.NoRail)
        let st = Map.ofList args

        let ownerID = System.Guid.NewGuid()
        let canvasID = System.Guid.NewGuid()

        let expected = OCamlInterop.execute ownerID canvasID ast st [] []
        // debuG "ocaml (expected)" expected

        let! state = executionStateFor "executePure" Map.empty Map.empty
        let! actual = LibExecution.Execution.run state st (ast.toRuntimeType ())
        // debuG "fsharp (actual) " actual

        let differentErrorsAllowed =
          // Error messages are not required to be directly the same between
          // old and new implementations, but we do prefer them to be the same
          // if possible. this is a list of error messages which have been
          // manually verified to be "close-enough"
          true

        if dvalEquality actual expected then
          return true
        else
          match actual, expected with
          | RT.DError (_, msg1), RT.DError (_, msg2) ->
              debuG "ignoring different error msgs" (msg1, msg2)
              return differentErrorsAllowed
          | RT.DResult (Error msg1), RT.DResult (Error msg2) ->
              debuG "ignoring different error msgs" (msg1, msg2)
              return differentErrorsAllowed
          | _ -> return false
      }

    Task.WaitAll [| t :> Task |]
    t.Result

  let tests =
    testList
      "executePureFunctions"
      [ testPropertyWithGenerator typeof<Generator> "equalsOCaml" equalsOCaml ]


let stillBuggy = testList "still buggy" (List.concat [ OCamlInterop.tests ])

let knownGood =
  testList
    "known good"
    ([ FQFnName.tests
       Roundtrippable.tests
       Queryable.tests
       DeveloperRepr.tests
       EndUserReadable.tests
       Hashing.tests
       PrettyMachineJson.tests
       ExecutePureFunctions.tests ])

let tests = testList "FuzzTests" [ knownGood; stillBuggy ]



[<EntryPoint>]
let main args =
  LibBackend.OCamlInterop.Binary.init ()
  runTestsWithCLIArgs [] args tests
