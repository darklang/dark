/// Generators
module FuzzTests.Generators

open NodaTime
open FsCheck

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

/// List of all a..z, A..Z, 0..9, and _ characters
let alphaNumericCharacters =
  List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ]; [ 'A' .. 'Z' ]; [ '_' ] ]

/// Generates a string that 'normalizes' successfully
let safeUnicodeString =
  /// We disallow `\u0000` because Postgres doesn't like it
  let isSafeString (s : string) : bool = s <> null && not (s.Contains('\u0000'))

  let normalizesSuccessfully (s : string) : bool =
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
  |> Gen.filter normalizesSuccessfully
  // Now that we know it can be normalized, actually normalize it
  |> Gen.map String.normalize
  |> Gen.filter isSafeString

let SafeUnicodeString = safeUnicodeString |> Arb.fromGen

let char : Gen<string> =
  safeUnicodeString
  |> Gen.map String.toEgcSeq
  |> Gen.map Seq.toList
  |> Gen.map List.head
  |> Gen.filter Option.isSome
  |> Gen.map (Option.defaultValue "")
  |> Gen.filter ((<>) "")

/// Generates an `int` >= 0
let nonNegativeInt =
  gen {
    let! (NonNegativeInt i) = Arb.generate<NonNegativeInt>
    return i
  }

let safeFloat =
  gen {
    let specials = interestingFloats |> List.map Tuple2.second |> Gen.elements

    return! Gen.frequency [ (5, specials); (5, Arb.generate<float>) ]
  }

let SafeFloat = Arb.fromGen safeFloat

let safeInt64 =
  gen {
    let specials = interestingInts |> List.map Tuple2.second |> Gen.elements

    return! Gen.frequency [ (5, specials); (5, Arb.generate<int64>) ]
  }

let SafeInt64 = Arb.fromGen safeInt64

/// Helper function to generate allowed function name parts, bindings, etc.
let simpleName (first : char list) (other : char list) : Gen<string> =
  gen {
    let! tailLength = Gen.choose (0, 20)
    let! head = Gen.elements first
    let! tail = Gen.arrayOfLength tailLength (Gen.elements other)
    return System.String(Array.append [| head |] tail)
  }

module NodaTime =
  let instant =
    Arb.generate<System.DateTime>
    |> Gen.map (fun dt -> dt.ToUniversalTime())
    |> Gen.map (fun dt -> Instant.FromDateTimeUtc dt)

  let localDateTime : Gen<NodaTime.LocalDateTime> =
    Arb.generate<System.DateTime> |> Gen.map NodaTime.LocalDateTime.FromDateTime

  let Instant = instant |> Arb.fromGen
  let LocalDateTime = localDateTime |> Arb.fromGen

module FQFnName =
  let ownerName =
    simpleName [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

  let packageName =
    simpleName [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

  let modName = simpleName [ 'A' .. 'Z' ] alphaNumericCharacters

  let fnName = simpleName [ 'a' .. 'z' ] alphaNumericCharacters

module RuntimeTypes =
  let Dval = Arb.generate<RT.Dval>

  let DType =
    let rec isSupportedType dtype =
      match dtype with
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
      | RT.TTuple (first, second, theRest) ->
        List.all isSupportedType ([ first; second ] @ theRest)
      | RT.TResult (t1, t2) -> isSupportedType t1 && isSupportedType t2
      | RT.TFn (ts, rt) -> isSupportedType rt && List.all isSupportedType ts
      | RT.TRecord (pairs) ->
        pairs |> List.map Tuple2.second |> List.all isSupportedType

      // FSTODO: support all types
      | RT.TDB _
      | RT.TIncomplete
      | RT.TPassword
      | RT.TErrorRail -> false

    Arb.Default.Derive() |> Arb.filter isSupportedType

  let dType = DType.Generator

  // TODO: use this less over time
  let simpleString =
    simpleName [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

  module MatchPattern =
    let genInt = Arb.generate<int64> |> Gen.map (fun i -> RT.MPInteger(gid (), i))
    let genBool = Arb.generate<bool> |> Gen.map (fun b -> RT.MPBool(gid (), b))
    let genBlank = gen { return RT.MPBlank(gid ()) }
    let genNull = gen { return RT.MPNull(gid ()) }
    let genChar = char |> Gen.map (fun c -> RT.MPCharacter(gid (), c))
    let genStr = simpleString |> Gen.map (fun s -> RT.MPString(gid (), s))
    let genFloat = Arb.generate<float> |> Gen.map (fun f -> RT.MPFloat(gid (), f))

    let genVar = simpleString |> Gen.map (fun s -> RT.MPVariable(gid (), s))

    let constructor (s, genArg) : Gen<RT.MatchPattern> =
      let withMostlyFixedArgLen (name, expectedParamCount) =
        gen {
          let! argCount =
            Gen.frequency [ (95, Gen.constant expectedParamCount)
                            (5, Gen.elements [ 1..20 ]) ]

          let! args = Gen.listOfLength argCount (genArg (s / 2))

          return RT.MPConstructor(gid (), name, args)
        }

      let ok = withMostlyFixedArgLen ("Ok", 1)
      let error = withMostlyFixedArgLen ("Error", 1)
      let just = withMostlyFixedArgLen ("Just", 1)
      let nothing = withMostlyFixedArgLen ("Nothing", 0)

      Gen.frequency [ (24, ok) // OK [p] (usually 1 arg; rarely, more)
                      (24, error) // Error [p] (usually 1 arg; rarely, more)

                      (24, just) // Just [p] (usually 1 arg; rarely, more)
                      (24, nothing) // Nothing [] (usually 0 args; rarely, more)

                      //(4, ok) // TODO: random string, with 0-5 args
                       ]

  open MatchPattern

  let matchPattern =
    // TODO: consider adding 'weight' such that certain patterns are generated
    // more often than others
    let rec gen' s : Gen<RT.MatchPattern> =
      let finitePatterns =
        [ genInt; genBool; genBlank; genNull; genChar; genStr; genVar ]

      let allPatterns = constructor (s, gen') :: finitePatterns

      match s with
      | 0 -> Gen.oneof finitePatterns
      | n when n > 0 -> Gen.oneof allPatterns
      | _ -> invalidArg "s" "Only positive arguments are allowed"

    Gen.sized gen' // todo: depth of 20 seems kinda reasonable


  let patternsForMatch : Gen<List<RT.MatchPattern>> =
    gen {
      let! len = Gen.choose (1, 20)
      return! Gen.listOfLength len matchPattern
    }

  module Expr =
    // Non-recursive exprs
    let genInt = Arb.generate<int64> |> Gen.map (fun i -> RT.EInteger(gid (), i))
    let genBool = Arb.generate<bool> |> Gen.map (fun b -> RT.EBool(gid (), b))
    let genBlank = gen { return RT.EBlank(gid ()) }
    let genNull = gen { return RT.ENull(gid ()) }
    let genChar = char |> Gen.map (fun c -> RT.ECharacter(gid (), c))
    let genStr = simpleString |> Gen.map (fun s -> RT.EString(gid (), s))
    let genVar = simpleString |> Gen.map (fun s -> RT.EVariable(gid (), s))
    let genFloat = Arb.generate<float> |> Gen.map (fun f -> RT.EFloat(gid (), f))

    // Recursive exprs
    let genLet genSubExpr s =
      gen {
        let! varName = simpleString
        let! rhsExpr = genSubExpr (s / 2)
        let! nextExpr = genSubExpr (s / 2)

        return RT.ELet(gid (), varName, rhsExpr, nextExpr)
      }

    let genIf genSubExpr s =
      gen {
        let! condExpr = Gen.frequency [ (90, genBool); (10, genSubExpr (s / 2)) ]
        let! thenExpr = genSubExpr (s / 2)
        let! elseExpr = genSubExpr (s / 2)

        return RT.EIf(gid (), condExpr, thenExpr, elseExpr)
      }

    let genFF genSubExpr s =
      gen {
        let! condExpr = Gen.frequency [ (90, genBool); (10, genSubExpr (s / 2)) ]
        let! thenExpr = genSubExpr (s / 2)
        let! elseExpr = genSubExpr (s / 2)

        return RT.EFeatureFlag(gid (), condExpr, thenExpr, elseExpr)
      }

    let genConstructor genSubExpr s : Gen<RT.Expr> =
      let withMostlyFixedArgLen (name, expectedParamCount) =
        gen {
          let! argCount =
            Gen.frequency [ (95, Gen.constant expectedParamCount)
                            (5, Gen.elements [ 1..20 ]) ]

          let! args = Gen.listOfLength argCount (genSubExpr (s / 2))

          return RT.EConstructor(gid (), name, args)
        }

      let ok = withMostlyFixedArgLen ("Ok", 1)
      let error = withMostlyFixedArgLen ("Error", 1)
      let just = withMostlyFixedArgLen ("Just", 1)
      let nothing = withMostlyFixedArgLen ("Nothing", 0)

      Gen.frequency [ (24, ok) // OK [p] (usually 1 arg; rarely, more)
                      (24, error) // Error [p] (usually 1 arg; rarely, more)

                      (24, just) // Just [p] (usually 1 arg; rarely, more)
                      (24, nothing) // Nothing [] (usually 0 args; rarely, more)

                      //(4, ok) // TODO: random string, with 0-5 args
                       ]

    let genTuple genSubExpr s =
      gen {
        let! first = genSubExpr (s / 2)
        let! second = genSubExpr (s / 2)

        // 7-element tuples seem sufficient
        let! tailLength = Gen.elements [ 0..5 ]
        let! theRest = Gen.listOfLength tailLength (genSubExpr (s / 2))

        return RT.ETuple(gid (), first, second, theRest)
      }

    let genList genSubExpr s =
      gen {
        let! els = Gen.listOf (genSubExpr (s / 2))
        return RT.EList(gid (), els)
      }

    let genRecord genSubExpr s =
      gen {
        let! pairs =
          gen {
            let! name = simpleString
            let! v = genSubExpr (s / 2)
            return (name, v)
          }
          |> Gen.listOf

        return RT.ERecord(gid (), pairs)
      }

    let genMatch genPattern genSubExpr s =
      gen {
        // TODO: consider limiting the # of cases - something between 1 and 10?
        let! cases =
          gen {
            let! p = genPattern
            let! v = genSubExpr (s / 2)
            return (p, v)
          }
          |> Gen.listOf

        let! matchExpr = genSubExpr (s / 2)

        return RT.EMatch(gid (), matchExpr, cases)
      }


  // We haven't yet created generators for these
  // They eventually belong above in the Expr sub-module
  // TODO: ELambda
  // TODO: EFieldAccess
  // TODO: EApply
  // TODO: EFQFnValue

  open Expr

  // TODO: consider adding 'weight' such that certain patterns are generated more often than others
  let rec expr' s : Gen<RT.Expr> =
    let finiteExprs =
      [ genNull; genBlank; genBool; genInt; genFloat; genChar; genStr; genVar ]

    let recursiveExprs =
      [ genConstructor
        genLet
        genIf
        genFF
        genTuple
        genRecord
        genList
        genMatch matchPattern ]
      |> List.map (fun g -> g expr' s)

    let allExprs = recursiveExprs @ finiteExprs

    match s with
    | 0 -> Gen.oneof finiteExprs
    | n when n > 0 -> Gen.oneof allExprs
    | _ -> invalidArg "s" "Only positive arguments are allowed"

  let expr = Gen.sized expr'


// TODO: figure out a way to ensure that these bottom-up generators exhaust all
// cases (as opposed to the generate-then-filter ones). With this style, it's
// easy to add a new DU case and then not add a corresponding generator here.

// TODO: for things like strings, mostly generate a small-ish pool of random
// values. That way, a generated PString pattern is more likely to match some
// generated EString value.

// TODO: clone all of this, and _really_ only generate a few values of each
// type. For example, for ints we really only need a handful of different
// options. That way, patterns are more likely to _actually_ match.

module ProgramTypes =
  // TODO: use this less over time
  let simpleString =
    simpleName [ 'a' .. 'z' ] (List.concat [ [ 'a' .. 'z' ]; [ '0' .. '9' ] ])

  module MatchPattern =
    let genInt = Arb.generate<int64> |> Gen.map (fun i -> PT.MPInteger(gid (), i))
    let genBool = Arb.generate<bool> |> Gen.map (fun b -> PT.MPBool(gid (), b))
    let genBlank = gen { return PT.MPBlank(gid ()) }
    let genNull = gen { return PT.MPNull(gid ()) }
    let genChar = char |> Gen.map (fun c -> PT.MPCharacter(gid (), c))
    let genStr = simpleString |> Gen.map (fun s -> PT.MPString(gid (), s))

    // TODO: genFloat
    // let genFloat = gen {return PT.PBlank (gid()) }

    let genVar = simpleString |> Gen.map (fun s -> PT.MPVariable(gid (), s))

    let constructor (s, genArg) : Gen<PT.MatchPattern> =
      let withMostlyFixedArgLen (name, expectedParamCount) =
        gen {
          let! argCount =
            Gen.frequency [ (95, Gen.constant expectedParamCount)
                            (5, Gen.elements [ 1..20 ]) ]

          let! args = Gen.listOfLength argCount (genArg (s / 2))

          return PT.MPConstructor(gid (), name, args)
        }

      let ok = withMostlyFixedArgLen ("Ok", 1)
      let error = withMostlyFixedArgLen ("Error", 1)
      let just = withMostlyFixedArgLen ("Just", 1)
      let nothing = withMostlyFixedArgLen ("Nothing", 0)

      Gen.frequency [ (24, ok) // OK [p] (usually 1 arg; rarely, more)
                      (24, error) // Error [p] (usually 1 arg; rarely, more)

                      (24, just) // Just [p] (usually 1 arg; rarely, more)
                      (24, nothing) // Nothing [] (usually 0 args; rarely, more)

                      //(4, ok) // TODO: random string, with 0-5 args
                       ]

  open MatchPattern

  let matchPattern =
    // TODO: consider adding 'weight' such that certain patterns are generated more often than others
    let rec gen' s : Gen<PT.MatchPattern> =
      let finitePatterns =
        [ genInt; genBool; genBlank; genNull; genChar; genStr; genVar ]

      let allPatterns = constructor (s, gen') :: finitePatterns

      match s with
      | 0 -> Gen.oneof finitePatterns
      | n when n > 0 -> Gen.oneof allPatterns
      | _ -> invalidArg "s" "Only positive arguments are allowed"

    Gen.sized gen' // todo: depth of 20 seems kinda reasonable


  let patternsForMatch : Gen<List<PT.MatchPattern>> =
    gen {
      let! len = Gen.choose (1, 20)
      return! Gen.listOfLength len matchPattern
    }

  module Expr =
    // Non-recursive exprs
    let genInt = Arb.generate<int64> |> Gen.map (fun i -> PT.EInteger(gid (), i))

    let genBool = Arb.generate<bool> |> Gen.map (fun b -> PT.EBool(gid (), b))

    let genBlank = gen { return PT.EBlank(gid ()) }

    let genNull = gen { return PT.ENull(gid ()) }

    let genChar = char |> Gen.map (fun c -> PT.ECharacter(gid (), c))

    let genStr = simpleString |> Gen.map (fun s -> PT.EString(gid (), s))

    let genVar = simpleString |> Gen.map (fun s -> PT.EVariable(gid (), s))

    // TODO: genFloat

    // Recursive exprs
    let genLet genSubExpr s =
      gen {
        let! varName = simpleString
        let! rhsExpr = genSubExpr (s / 2)
        let! nextExpr = genSubExpr (s / 2)

        return PT.ELet(gid (), varName, rhsExpr, nextExpr)
      }

    let genIf genSubExpr s =
      gen {
        let! condExpr = Gen.frequency [ (90, genBool); (10, genSubExpr (s / 2)) ]
        let! thenExpr = genSubExpr (s / 2)
        let! elseExpr = genSubExpr (s / 2)

        return PT.EIf(gid (), condExpr, thenExpr, elseExpr)
      }

    let genConstructor genSubExpr s : Gen<PT.Expr> =
      let withMostlyFixedArgLen (name, expectedParamCount) =
        gen {
          let! argCount =
            Gen.frequency [ (95, Gen.constant expectedParamCount)
                            (5, Gen.elements [ 1..20 ]) ]

          let! args = Gen.listOfLength argCount (genSubExpr (s / 2))

          return PT.EConstructor(gid (), name, args)
        }

      let ok = withMostlyFixedArgLen ("Ok", 1)
      let error = withMostlyFixedArgLen ("Error", 1)
      let just = withMostlyFixedArgLen ("Just", 1)
      let nothing = withMostlyFixedArgLen ("Nothing", 0)

      Gen.frequency [ (24, ok) // OK [p] (usually 1 arg; rarely, more)
                      (24, error) // Error [p] (usually 1 arg; rarely, more)

                      (24, just) // Just [p] (usually 1 arg; rarely, more)
                      (24, nothing) // Nothing [] (usually 0 args; rarely, more)

                      //(4, ok) // TODO: random string, with 0-5 args
                       ]

    let genTuple genSubExpr s =
      gen {
        let! first = genSubExpr (s / 2)
        let! second = genSubExpr (s / 2)

        // 7-element tuples seem sufficient
        let! tailLength = Gen.elements [ 0..5 ]
        let! theRest = Gen.listOfLength tailLength (genSubExpr (s / 2))

        return PT.ETuple(gid (), first, second, theRest)
      }

    let genList genSubExpr s =
      gen {
        let! els = Gen.listOf (genSubExpr (s / 2))
        return PT.EList(gid (), els)
      }

    let genRecord genSubExpr s =
      gen {
        let! pairs =
          gen {
            let! name = simpleString
            let! v = genSubExpr (s / 2)
            return (name, v)
          }
          |> Gen.listOf

        return PT.ERecord(gid (), pairs)
      }

    let genMatch genPattern genSubExpr s =
      gen {
        // TODO: consider limiting the # of cases - something between 1 and 10?
        let! cases =
          gen {
            let! p = genPattern
            let! v = genSubExpr (s / 2)
            return (p, v)
          }
          |> Gen.listOf

        let! matchExpr = genSubExpr (s / 2)

        return PT.EMatch(gid (), matchExpr, cases)
      }


  // We haven't yet created generators for these
  // They eventually belong above in the Expr sub-module
  // TODO: EInfix
  // TODO: ELambda
  // TODO: EFieldAccess
  // TODO: EFnCall
  // TODO: EPipe
  // TODO: EPipeTarget
  // TODO: EFeatureFlag

  open Expr

  // TODO: consider adding 'weight' such that certain patterns are generated more often than others
  let rec expr' s : Gen<PT.Expr> =
    let finiteExprs = [ genInt; genBool; genBlank; genNull; genChar; genStr; genVar ]

    let recursiveExprs =
      [ genConstructor
        genLet
        genIf
        genTuple
        genRecord
        genList
        genMatch matchPattern ]
      |> List.map (fun g -> g expr' s)

    let allExprs = recursiveExprs @ finiteExprs

    match s with
    | 0 -> Gen.oneof finiteExprs
    | n when n > 0 -> Gen.oneof allExprs
    | _ -> invalidArg "s" "Only positive arguments are allowed"

  let expr = Gen.sized expr'
