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
module PTParser = LibExecution.ProgramTypesParser
module RT = LibExecution.RuntimeTypes
module OCamlInterop = LibBackend.OCamlInterop
module G = Generators

let allowedErrors = AllowedFuzzerErrors.allowedErrors

module Generators =
  // Used to ensure we generate values of a consistent type
  // for collection types such as lists and dicts.
  let rec private generateTypeToMatchCollection (typ : RT.DType) : Gen<RT.DType> =
    gen {
      match typ with
      | RT.TVariable _name ->
        // Generally return a homogenous list. We'll sometimes get a
        // TVariable which will give us a heterogenous list. It's fine to
        // do that occasionally
        return! Arb.generate<RT.DType>
      | typ -> return typ
    }

  /// Generates an expression that should evaluate to the given type
  let exprFromType (typ' : RT.DType) : Gen<RT.Expr> =
    let rec genExpr' typ size =
      let callFn mod_ fn version args =
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
          let! v = G.ocamlSafeString
          return RT.EString(gid (), v)
        | RT.TChar ->
          // We don't have a construct for characters, so create code to generate the character
          let! v = G.char
          return callFn "String" "toChar" 0 [ RT.EString(gid (), v) ]
        // Don't generate a random value as some random values are invalid
        // (e.g. constructor outside certain names). Ints should be fine for
        // whatever purpose there is here
        | RT.TVariable _ -> return! genExpr' RT.TInt size
        | RT.TFloat ->
          let! v = Arb.generate<float>
          return RT.EFloat(gid (), v)
        | RT.TBool ->
          let! v = Arb.generate<bool>
          return RT.EBool(gid (), v)
        | RT.TNull -> return RT.ENull(gid ())
        | RT.TList typ ->
          let! typ = generateTypeToMatchCollection typ
          let! v = Gen.listOfLength size (genExpr' typ (size / 2))
          return RT.EList(gid (), v)
        | RT.TDict typ ->
          let! typ = generateTypeToMatchCollection typ

          return!
            Gen.map
              (fun l -> RT.ERecord(gid (), l))
              (Gen.listOfLength
                size
                (Gen.zip G.ocamlSafeString (genExpr' typ (size / 2))))
        | RT.TUserType (_name, _version) ->
          let! typ = Arb.generate<RT.DType>

          return!
            Gen.map
              (fun l -> RT.ERecord(gid (), l))
              (Gen.listOfLength
                size
                (Gen.zip G.ocamlSafeString (genExpr' typ (size / 2))))

        | RT.TRecord pairs ->
          let! entries =
            List.fold
              (Gen.constant [])
              (fun (l : Gen<List<string * RT.Expr>>) ((k, t) : string * RT.DType) ->
                gen {
                  let! l = l
                  let! v = genExpr' t size
                  return (k, v) :: l
                })
              pairs
            |> Gen.map List.reverse

          return RT.ERecord(gid (), entries)
        | RT.TOption typ ->
          match! Gen.optionOf (genExpr' typ size) with
          | Some v -> return RT.EConstructor(gid (), "Just", [ v ])
          | None -> return RT.EConstructor(gid (), "Nothing", [])
        | RT.TResult (okType, errType) ->
          let! v =
            Gen.oneof [ Gen.map Ok (genExpr' okType size)
                        Gen.map Error (genExpr' errType size) ]

          match v with
          | Ok v -> return RT.EConstructor(gid (), "Ok", [ v ])
          | Error v -> return RT.EConstructor(gid (), "Error", [ v ])

        | RT.TFn (paramTypes, returnType) ->
          let parameters =
            List.mapi
              (fun i (v : RT.DType) -> (id i, $"{v.toOldString().ToLower()}_{i}"))
              paramTypes

          let! returnType =
            Gen.frequency [ (98, Gen.constant returnType)
                            (2, G.RuntimeTypes.dType) ]

          // FSTODO: can we use the argument to get this type?
          let! body = genExpr' returnType size
          return RT.ELambda(gid (), parameters, body)
        | RT.TBytes ->
          // FSTODO: this doesn't really do anything useful
          let! bytes = Arb.generate<byte []>
          let v = RT.EString(gid (), Base64.defaultEncodeToString bytes)
          return callFn "String" "toBytes" 0 [ v ]
        | RT.TDB _ ->
          let! name = G.ocamlSafeString
          let ti = System.Globalization.CultureInfo.InvariantCulture.TextInfo
          let name = ti.ToTitleCase name
          return RT.EVariable(gid (), name)
        | RT.TDate ->
          let! d = Arb.generate<NodaTime.Instant>
          return callFn "Date" "parse" 0 [ RT.EString(gid (), d.toIsoString ()) ]
        | RT.TUuid ->
          let! u = Arb.generate<System.Guid>
          return callFn "String" "toUUID" 0 [ RT.EString(gid (), string u) ]
        | RT.THttpResponse typ ->
          let! code = genExpr' RT.TInt size
          let! body = genExpr' typ size
          return callFn "Http" "response" 0 [ body; code ]
        | RT.TError ->
          let! msg = genExpr' RT.TStr size
          return callFn "Test" "typeError" 0 [ msg ]

        // FSTODO support all types
        | RT.TIncomplete
        | RT.TPassword
        | RT.TErrorRail ->
          return Exception.raiseInternal $"Unsupported type (yet!)" [ "typ", typ ]
      }

    Gen.sized (genExpr' typ')

  let StdLibFn : Gen<RT.BuiltInFn> =
    LibRealExecution.RealExecution.stdlibFns
    |> Map.values
    |> List.filter (fun fn ->
      let name = RT.FQFnName.StdlibFnName.toString fn.name

      // FSTODO: reduce/resolve these
      let isKnownDifference =
        let has set = Set.contains name set
        has allowedErrors.knownDifferingFunctions

      if isKnownDifference then
        false
      elif allowedErrors.functionToTest = None then
        // FSTODO: Add JWT and X509 functions here
        fn.previewable = RT.Pure || fn.previewable = RT.ImpurePreviewable
      elif Some name = allowedErrors.functionToTest then
        true
      else
        false)
    |> Gen.elements

  /// <summary>
  /// Generates a valid Dval for a given type.
  /// </summary>
  /// <remarks>
  /// This handles 'correct' mappings - something surrounding this can/should/does
  /// ensure we try _bad_ data sometimes.
  ///
  /// Respects incoming `size` value.
  /// </remarks>
  let dvalFromType (typ' : RT.DType) : Gen<RT.Dval> =

    let rec genDval' typ s : Gen<RT.Dval> =
      gen {
        match typ with
        | RT.TInt ->
          let! v = Arb.generate<int64>
          return RT.DInt v
        | RT.TStr ->
          let! v = G.ocamlSafeString
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
          let! typ = generateTypeToMatchCollection typ
          return! Gen.map RT.DList (Gen.listOfLength s (genDval' typ (s / 2)))
        | RT.TDict typ ->
          let! typ = generateTypeToMatchCollection typ

          return!
            Gen.map
              (fun l -> RT.DObj(Map.ofList l))
              (Gen.listOfLength
                s
                (Gen.zip (G.ocamlSafeString) (genDval' typ (s / 2))))
        | RT.TDB _ -> return! Gen.map RT.DDB (G.ocamlSafeString)
        | RT.TDate ->
          return!
            Gen.map
              (fun (dt : RT.DDateTime.T) ->
                // Set milliseconds to zero
                RT.DDate(dt.PlusMilliseconds(-dt.Millisecond)))

              Arb.generate<RT.DDateTime.T>
        | RT.TChar ->
          let! v = G.char
          return RT.DChar v
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

          let! body = exprFromType returnType

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
            Gen.listOfLength s (Gen.zip (G.ocamlSafeString) (genDval' typ (s / 2)))

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
            Gen.elements [ RT.Response(code, headers, body); RT.Redirect url ]
            |> Gen.map RT.DHttpResponse
        | RT.TErrorRail ->
          let! typ = Arb.generate<RT.DType>
          return! Gen.map RT.DErrorRail (genDval' typ s)

        // FSTODO: support all types
        | RT.TPassword
        | RT.TIncomplete ->
          return Exception.raiseInternal "Type not supported yet" [ "type", typ ]
      }

    Gen.sized (genDval' typ')

type FnAndArgs = RT.FQFnName.StdlibFnName * List<RT.Dval>

type Generator =
  static member LocalDateTime() : Arbitrary<NodaTime.LocalDateTime> =
    G.NodaTime.LocalDateTime
  static member Instant() : Arbitrary<NodaTime.Instant> = G.NodaTime.Instant
  static member String() : Arbitrary<string> = G.OCamlSafeString
  static member Float() : Arbitrary<float> = G.OCamlSafeFloat
  static member Int64() : Arbitrary<int64> = G.OCamlSafeInt64
  static member Dval() : Arbitrary<RT.Dval> = G.RuntimeTypes.Dval
  static member DType() : Arbitrary<RT.DType> = G.RuntimeTypes.DType

  // this is the type expected/generated for the below property
  // generates a function, and a list of valid params to be applied to it
  static member FnAndArgs() : Arbitrary<FnAndArgs> =
    gen {
      let! fn = Generators.StdLibFn
      let name = fn.name
      let signature = fn.parameters

      /// `argIndex` is the index of the argument in the fn definition
      let arg (argIndex : int) (prevArgs : List<RT.Dval>) : Gen<RT.Dval> =
        // If the parameters need to be in a particular format to get
        // meaningful testing, generate them here.
        let specific =
          gen {
            match RT.FQFnName.StdlibFnName.toString name, argIndex with
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
              let! v = G.char
              return RT.DStr v
            | "JWT::signAndEncode", 0
            | "JWT::signAndEncode_v1", 0 ->
              return! G.Certificates.rsaPrivateKey |> Gen.map RT.DStr
            | _ -> return! Generators.dvalFromType signature[argIndex].typ
          }

        // Still throw in random data occasionally test errors, edge-cases, etc.
        let randomValue =
          gen {
            let! typ = Arb.generate<RT.DType>
            return! Generators.dvalFromType typ
          }

        // todo: filter within the 'specific' thing?
        Gen.frequency [ (1, randomValue); (99, specific) ]
        |> Gen.filter (fun dv ->
          // Avoid triggering known errors in OCaml
          match (argIndex, dv, prevArgs, name.module_, name.function_, name.version)
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
            && G.isValidOCamlInt i
            && i <= 2000L
            && G.isValidOCamlInt (int64 (bigint e ** (int i)))
          | 1, RT.DInt i, [ RT.DInt e ], "", "*", 0
          | 1, RT.DInt i, [ RT.DInt e ], "Int", "multiply", 0 ->
            G.isValidOCamlInt (e * i)
          | 1, RT.DInt i, [ RT.DInt e ], "", "+", 0
          | 1, RT.DInt i, [ RT.DInt e ], "Int", "add", 0 -> G.isValidOCamlInt (e + i)
          | 1, RT.DInt i, [ RT.DInt e ], "", "-", 0
          | 1, RT.DInt i, [ RT.DInt e ], "Int", "subtract", 0 ->
            G.isValidOCamlInt (e - i)
          | 0, RT.DList l, _, "Int", "sum", 0 ->
            l
            |> List.map (function
              | RT.DInt i -> i
              | _ -> 0L)
            |> List.fold 0L (+)
            |> G.isValidOCamlInt
          // Int overflow converting from Floats
          | 0, RT.DFloat f, _, "Float", "floor", 0
          | 0, RT.DFloat f, _, "Float", "roundDown", 0
          | 0, RT.DFloat f, _, "Float", "roundTowardsZero", 0
          | 0, RT.DFloat f, _, "Float", "round", 0
          | 0, RT.DFloat f, _, "Float", "ceiling", 0
          | 0, RT.DFloat f, _, "Float", "roundUp", 0
          | 0, RT.DFloat f, _, "Float", "truncate", 0 ->
            f |> int64 |> G.isValidOCamlInt
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

          // Exception - don't try to stringify
          | 0, _, _, "", "toString", 0 -> not (G.RuntimeTypes.containsBytes dv)
          | _ -> true)

      // When generating arguments, we sometimes make use of the previous params
      // which requires us to generate the arguments in order, as below
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
    }
    |> Arb.fromGen


/// Checks if a fn and some arguments result in the same Dval
/// against both OCaml and F# backends.
///
/// Some differences are OK, managed by `AllowedFuzzerErrors` module
let equalsOCaml ((fn, args) : FnAndArgs) : bool =
  let isErrorAllowed = AllowedFuzzerErrors.errorIsAllowed fn

  task {
    // evaluate the fn call against both backends
    let! meta = initializeTestCanvas "ExecutePureFunction"
    let args = List.mapi (fun i arg -> ($"v{i}", arg)) args

    let ast =
      let callFn mod_ fn version args =
        let call =
          RT.EFQFnValue(
            gid (),
            RT.FQFnName.Stdlib(RT.FQFnName.stdlibFnName mod_ fn version)
          )

        RT.EApply(gid (), call, args, RT.NotInPipe, RT.NoRail)

      let fnArgList = List.map (fun (name, _) -> RT.EVariable(gid (), name)) args
      callFn fn.module_ fn.function_ fn.version fnArgList

    let symtable = Map.ofList args

    let! expected = OCamlInterop.executeExpr meta.owner meta.id ast symtable

    let! state = executionStateFor meta Map.empty Map.empty
    let! actual = LibExecution.Execution.executeExpr state symtable ast

    // check if Dvals are (roughly) the same
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
        let allowed = isErrorAllowed false aMsg eMsg
        // For easier debugging. Check once then step through
        let allowed2 = if not allowed then isErrorAllowed true aMsg eMsg else allowed

        if not allowed2 then
          debugFn ()

          print $"Got different error msgs:\n\"{aMsg}\"\n\n(F#) vs (OCaml)\n\"{eMsg}\"\n\n"

        return allowed
      | RT.DResult (Error (RT.DStr aMsg)), RT.DResult (Error (RT.DStr eMsg)) ->
        let allowed = isErrorAllowed false aMsg eMsg
        // For easier debugging. Check once then step through
        let allowed2 = if not allowed then isErrorAllowed true aMsg eMsg else allowed

        if not allowed2 then
          debugFn ()

          print $"Got different DError msgs:\n\"{aMsg}\"\n\n(F#) vs (OCaml)\n\"{eMsg}\"\n\n"

        return allowed
      | _ ->
        debugFn ()
        debuG "ocaml (expected)" (debugDval expected)
        debuG "fsharp (actual) " (debugDval actual)
        return false
  }
  |> result

let tests config =
  testList
    "executePureFunctions"
    [ testProperty config typeof<Generator> "equalsOCaml" equalsOCaml ]
