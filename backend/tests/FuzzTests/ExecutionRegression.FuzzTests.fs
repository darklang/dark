/// This fuzztest generates randomized PT.Exprs paired with and RT.Dvals, and
/// A/B tests changes in the interpreter for regressions. Note: there is
/// currently no A/B testing mechanism committed, so this will need review the
/// next time such a need comes up.
module FuzzTests.ExecutionRegression

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

module G = Generators

module RT = LibExecution.RuntimeTypes
module Tracing = LibBackend.Tracing
module Exe = LibExecution.Execution

type TestCase = RT.Expr

let errorOnTraceDifferences = false


/// Without any context of the Expr we're trying to generate a pattern for, the
/// generated Exprs are bound to almost never match. This attempts to ensure we
/// generate matches at a reasonable frequency. That said, not all Exprs have
/// Patterns that will match them.
let rec patternFromExpr (expr : RT.Expr) : Gen<RT.MatchPattern> =
  match expr with
  | RT.EBlank (id) ->
    Gen.frequency [ (1, Gen.constant <| RT.MPBlank(id))
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  | RT.ENull (id) ->
    Gen.frequency [ (1, Gen.constant <| RT.MPNull(id))
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  | RT.EBool (id, b) ->
    Gen.frequency [ (1, Gen.constant <| RT.MPBool(id, b))
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  | RT.EInteger (id, f) ->
    Gen.frequency [ (1, Gen.constant <| RT.MPInteger(id, f))
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  | RT.EFloat (id, f) ->
    Gen.frequency [ (1, Gen.constant <| RT.MPFloat(id, f))
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  | RT.ECharacter (id, c) ->
    Gen.frequency [ (1, Gen.constant <| RT.MPCharacter(id, c))
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  | RT.EString (id, s) ->
    Gen.frequency [ (1, Gen.constant <| RT.MPString(id, s))
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  | RT.EConstructor (id, name, args) ->
    Gen.frequency [ (1,
                     gen {
                       let! args = args |> List.map patternFromExpr |> Gen.sequence
                       return RT.MPConstructor(id, name, args)
                     })
                    (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

  // TODO: Some of these exprs _could_ be simplified such that we recommend a
  // pattern based on the simplified expr
  | RT.EIf _
  | RT.EMatch _
  | RT.EVariable _
  | RT.EFieldAccess _
  | RT.EFeatureFlag _
  | RT.ERecord _
  | RT.EList _
  | RT.ETuple _
  | RT.ELambda _
  | RT.EApply _
  | RT.EFQFnValue _
  | RT.EAnd _
  | RT.EOr _

  // TODO: we could populate a Symtable to use with EVariable and EFieldAccess
  // usages in the RHS expr
  | RT.ELet _ ->
    Gen.frequency [ (1, G.RuntimeTypes.MatchPattern.genVar)
                    (1, G.RuntimeTypes.matchPattern) ]

module Generators =
  /// This generates EMatch exprs where we attempt to ensure Patterns have a
  /// reasonable chance of matching the exprs.
  ///
  /// Written to ensure safety in an interpreter change in logic to
  /// pattern-matching.
  let matchExpr : Gen<RT.Expr> =
    gen {
      let! argExpr = G.RuntimeTypes.expr' 10 // without setting the size, we tend to get absurd exprs

      let! caseCount = Gen.choose (1, 20)

      let! patterns = patternFromExpr argExpr |> Gen.listOfLength caseCount

      let! rhsExprs = Gen.listOfLength caseCount (G.RuntimeTypes.expr' 10)

      let cases = List.zip patterns rhsExprs

      return RT.EMatch(gid (), argExpr, cases)
    }


let eval expr =
  task {
    let! meta = createTestCanvas (Randomized "ExecutionRegression")

    let! state = executionStateFor meta Map.empty Map.empty

    // Note - `traces` are mutated by Exe.executeExpr
    let traces, traceFn = Exe.traceDvals ()

    let state =
      { state with
          tracing =
            { state.tracing with traceDval = traceFn; realOrPreview = RT.Preview } }

    let! result =
      let inputVars = Map.empty
      Exe.executeExpr state inputVars expr

    return (result, traces)
  }

let isOK (expr : RT.Expr) : bool =
  (task {
    let! (_resultA, _tracesA) = eval expr

    // Currently, this only tests that eval does not crash. If/when this test
    // is useful again, some new A/B testing mechamism is likely to be needed.
    return true
  })
    .Result

type Generator =
  static member Expr() : Arbitrary<RT.Expr> = Arb.fromGen Generators.matchExpr

let tests config =
  testList
    "executionRegression"
    [ testProperty config typeof<Generator> "isOK" isOK ]
