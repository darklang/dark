/// This used to test whether pure standard library functions (such as
/// addition) resultsed in the same value against both the old OCaml backend
/// and the current F# backend. The OCaml backend is no longer maintained, so
/// the remains of this file are generally a placeholder for any similar test
/// in the future.
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

module RT = LibExecution.RuntimeTypes
module G = Generators

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

        RT.EApply(gid (), call, args, RT.NotInPipe)

      gen {
        match typ with
        | RT.TInt ->
          let! v = Arb.generate<int64>
          return RT.EInteger(gid (), v)
        | RT.TStr ->
          let! v = G.safeUnicodeString
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
        | RT.TUnit -> return RT.EUnit(gid ())
        | RT.TList typ ->
          let! typ = generateTypeToMatchCollection typ
          let! v = Gen.listOfLength size (genExpr' typ (size / 2))
          return RT.EList(gid (), v)
        | RT.TTuple (firstType, secondType, otherTypes) ->
          let! first = genExpr' firstType (size / 2)
          let! second = genExpr' secondType (size / 2)
          let! theRest = Gen.collect (fun t -> genExpr' t (size / 2)) otherTypes
          return RT.ETuple(gid (), first, second, theRest)
        | RT.TDict typ ->
          let! typ = generateTypeToMatchCollection typ

          return!
            Gen.map
              (fun l -> RT.ERecord(gid (), l))
              (Gen.listOfLength
                size
                (Gen.zip G.safeUnicodeString (genExpr' typ (size / 2))))
        | RT.TUserType (_name, _version) ->
          let! typ = Arb.generate<RT.DType>

          return!
            Gen.map
              (fun l -> RT.ERecord(gid (), l))
              (Gen.listOfLength
                size
                (Gen.zip G.safeUnicodeString (genExpr' typ (size / 2))))

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
          // FSTODO: expand our byte level testing to include non-stringable bytes
          // (to test edges of conversions to strings, and just be more robust in general).
          // If we're able to remove `containsBytes` or reduce its scope, all the better.
          let! bytes = Arb.generate<byte []>
          let v = RT.EString(gid (), Base64.defaultEncodeToString bytes)
          return callFn "String" "toBytes" 0 [ v ]
        | RT.TDB _ ->
          let! name = G.safeUnicodeString
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
        | RT.TPassword ->
          return Exception.raiseInternal $"Unsupported type (yet!)" [ "typ", typ ]
      }

    Gen.sized (genExpr' typ')

  let StdLibFn : Gen<RT.BuiltInFn> =
    LibRealExecution.RealExecution.stdlibFns |> Map.values |> Gen.elements

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
          let! v = G.safeUnicodeString
          return RT.DStr v
        | RT.TVariable _ ->
          let! newtyp = Arb.generate<RT.DType>
          return! genDval' newtyp s
        | RT.TFloat ->
          let! v = Arb.generate<float>
          return RT.DFloat v
        | RT.TBool -> return! Gen.map RT.DBool Arb.generate<bool>
        | RT.TUnit -> return RT.DUnit
        | RT.TList typ ->
          let! typ = generateTypeToMatchCollection typ
          return! Gen.map RT.DList (Gen.listOfLength s (genDval' typ (s / 2)))
        | RT.TTuple (firstType, secondType, otherTypes) ->
          let! first = genDval' firstType (s / 2)
          let! second = genDval' secondType (s / 2)
          let! theRest = Gen.collect (fun z -> genDval' z (s / 2)) otherTypes
          return RT.DTuple(first, second, theRest)
        | RT.TDict typ ->
          let! typ = generateTypeToMatchCollection typ

          return!
            Gen.map
              (fun l -> RT.DObj(Map.ofList l))
              (Gen.listOfLength
                s
                (Gen.zip (G.safeUnicodeString) (genDval' typ (s / 2))))
        | RT.TDB _ -> return! Gen.map RT.DDB (G.safeUnicodeString)
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
            Gen.listOfLength s (Gen.zip (G.safeUnicodeString) (genDval' typ (s / 2)))

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
  static member String() : Arbitrary<string> = G.SafeUnicodeString
  static member Float() : Arbitrary<float> = G.SafeFloat
  static member Int64() : Arbitrary<int64> = G.SafeInt64
  static member Dval() : Arbitrary<RT.Dval> = G.RuntimeTypes.Dval |> Arb.fromGen
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
              return
                RT.DStr
                  "-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ\nd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN\nc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2\nuUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B\nCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW\nDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa\noNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq\np0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+\n1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ\nXUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv\nvz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua\n6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509\nrWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz\nNBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj\namEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr\nr48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5\nPJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J\nhpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g\nMcgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn\n4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV\nctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf\nn8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9\n5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb\nT/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I\nFW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=\n-----END RSA PRIVATE KEY-----"
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
          // Int Overflow
          | 1, RT.DInt i, [ RT.DInt _ ], "Int", "power", 0
          | 1, RT.DInt i, [ RT.DInt _ ], "", "^", 0 ->
            i <> 1L && i <> (-1L) && i <= 2000L
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


/// Checks if a fn and some arguments may be evaluated successfully.
///
/// This used to ensure that the old OCaml backend and the current F#
/// backend returned the same responses - this this basically just simply
/// tests that execution doesn't fail outright.
let isOk ((fn, args) : FnAndArgs) : bool =
  (task {
    // evaluate the fn call against both backends
    let! meta = initializeTestCanvas (Randomized "ExecutePureFunction")
    let args = List.mapi (fun i arg -> ($"v{i}", arg)) args

    let ast =
      let callFn mod_ fn version args =
        let call =
          RT.EFQFnValue(
            gid (),
            RT.FQFnName.Stdlib(RT.FQFnName.stdlibFnName mod_ fn version)
          )

        RT.EApply(gid (), call, args, RT.NotInPipe)

      let fnArgList = List.map (fun (name, _) -> RT.EVariable(gid (), name)) args
      callFn fn.module_ fn.function_ fn.version fnArgList

    let symtable = Map.ofList args

    let! state = executionStateFor meta Map.empty Map.empty
    let! _actual = LibExecution.Execution.executeExpr state symtable ast

    return true
  })
    .Result

let tests config =
  testList
    "executePureFunctions"
    [ testProperty config typeof<Generator> "isOk" isOk ]
