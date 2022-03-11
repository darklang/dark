module FuzzTests.ExecutePureFunctions

open System.Threading.Tasks
open FSharp.Control.Tasks
open System.Text.RegularExpressions

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

open FuzzTests.Utils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module G = Generators

let tpwg = testPropertyWithGenerator

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
                (fun i (v : RT.DType) -> (id i, $"{v.toOldString().ToLower()}_{i}"))
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
            let! d = Arb.generate<NodaTime.Instant>
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
            return Exception.raiseInternal $"Unsupported type (yet!)" [ "typ", typ ]
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
                (fun (dt : RT.DDateTime.T) ->
                  // Set milliseconds to zero
                  RT.DDate(dt.PlusMilliseconds(-dt.Millisecond)))

                Arb.generate<RT.DDateTime.T>
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
      let fnArgList = List.map (fun (name, _) -> PT.EVariable(gid (), name)) args

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
              let sameGroups = actualMatch.Groups.Count = expectedMatch.Groups.Count

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
      elif Expect.dvalEquality actual expected then
        return true
      else
        match actual, expected with
        | RT.DError (_, aMsg), RT.DError (_, eMsg) ->
          let allowed = errorAllowed false aMsg eMsg
          // For easier debugging. Check once then step through
          let allowed2 = if not allowed then errorAllowed true aMsg eMsg else allowed

          if not allowed2 then
            debugFn ()

            print $"Got different error msgs:\n\"{aMsg}\"\n\nvs\n\"{eMsg}\"\n\n"

          return allowed
        | RT.DResult (Error (RT.DStr aMsg)), RT.DResult (Error (RT.DStr eMsg)) ->
          let allowed = errorAllowed false aMsg eMsg
          // For easier debugging. Check once then step through
          let allowed2 = if not allowed then errorAllowed true aMsg eMsg else allowed

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
