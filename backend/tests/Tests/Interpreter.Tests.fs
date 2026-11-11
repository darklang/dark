module Tests.Interpreter

open Expecto
open Prelude
open TestUtils.TestUtils
open TestUtils.PTShortcuts

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module VT = LibExecution.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RTE = RT.RuntimeError
module Dval = LibExecution.Dval

let private allowAllAccess =
  Some(
    LibExecution.Permissions.Access.start LibExecution.Permissions.Policy.allowAll
  )

module E = TestValues.Expressions
module PM = TestValues.PM

let tCheckVM
  name
  ptExpr
  expectedInsts
  (extraAssertions : RT.ExecutionState -> RT.VMState -> unit)
  =
  testTask name {
    let vmState =
      ptExpr |> PT2RT.Expr.toRT Map.empty 0 None |> RT.VMState.createWithoutTLID

    let! exeState = executionStateFor TestValues.pm false

    let! actual = LibExecution.Interpreter.execute exeState vmState |> Ply.toTask
    Expect.equal actual expectedInsts ""

    extraAssertions exeState vmState
  }

let t name ptExpr expectedInsts = tCheckVM name ptExpr expectedInsts (fun _ _ -> ())


let tFail name ptExpr expectedRte =
  testTask name {
    let instructions = ptExpr |> PT2RT.Expr.toRT Map.empty 0 None

    let! exeState = executionStateFor TestValues.pm false

    let! actual = LibExecution.Execution.executeExpr exeState instructions

    match actual with
    | Ok _ -> return Expect.equal 1 2 "Expected an RTE, but got a successful result"
    | Error(actualRte, _) -> return Expect.equal actualRte expectedRte ""
  }


// The evaluation basics (literals, let, match, pipes, records, enums, infix, calls,
// statements) are package tests in packages/darklang/tests/language/. What stays here
// is what a Dark program can't observe (the shape of the applicable an unapplied
// lambda or function evaluates to, the shared lambda cache, the F# eval harness), and
// builtin calls that tests/builtin keeps out of packages/.

module Values =
  /// A store value whose body is a call. The harness used to read it as `()`.
  let storeValueThatIsACall =
    testTask "a store value that is a call reads as its value" {
      let! expected =
        evalDarkExpr
          "Stdlib.String.displayWidth(Darklang.Cli.CompletionPicker.minimumHelpText)"
      let! actual = evalDarkExpr "Darklang.Cli.CompletionPicker.minimumWidth"
      match expected, actual with
      | Ok(RT.DInt64 _ as e), Ok a ->
        Expect.equal a e "minimumWidth is the display width of minimumHelpText"
      | _ -> return failtest $"unexpected results: {expected} and {actual}"
    }

  let tests = testList "Values" [ storeValueThatIsACall ]


module Lambdas =
  let private fullAccess =
    LibExecution.Permissions.Access.start LibExecution.Permissions.Policy.allowAll

  /// An unapplied lambda evaluates to an `AppLambda` and registers its body in the
  /// shared lambda cache.
  let private unapplied name ptExpr exprId closedRegisters =
    tCheckVM
      name
      ptExpr
      (RT.DApplicable(
        RT.AppLambda
          { exprId = exprId
            closedRegisters = closedRegisters
            access = fullAccess
            typeSymbolTable = RT.TST.empty
            expectedReturn = ValueNone
            writtenIn = ValueSome RT.Source }
      ))
      (fun exeState _vm ->
        Expect.isFalse exeState.lambdaInstrCache.IsEmpty "no lambdas registered")

  let identity =
    unapplied "fn x -> x" E.Lambdas.Identity.unapplied E.Lambdas.Identity.id [||]

  let add = unapplied "fn x y -> x + y" E.Lambdas.Add.unapplied E.Lambdas.Add.id [||]

  let addTuple =
    unapplied
      "fn (x, y) -> x + y"
      E.Lambdas.AddTuple.unapplied
      E.Lambdas.AddTuple.id
      [||]

  let addToClosedVars =
    unapplied
      "let x = 5\nlet y=10\nfun a -> a + x + y"
      E.Lambdas.AddToClosedVars.unapplied
      E.Lambdas.AddToClosedVars.id
      (RT.Captures.ofList [ (1, RT.DInt64 5); (2, RT.DInt64 10) ])

  /// Two VMs, one `ExecutionState`. VM-A (built here) runs `fn x -> x` and
  /// registers the lambda in `exeState.lambdaInstrCache`. `executeApplicable`
  /// internally spins up VM-B to invoke the `DApplicable`; VM-B must find
  /// the lambda in the shared exeState cache, not its own empty one.
  /// Regression guard for moving `lambdaInstrCache` off VMState.
  let crossVM =
    testTask "cross-VM lambda invocation" {
      let! exeState = executionStateFor TestValues.pm false

      // VM-A: evaluate `fn x -> x`, producing a DApplicable whose instructions
      // live in exeState.lambdaInstrCache after this call.
      let vmA =
        E.Lambdas.Identity.unapplied
        |> PT2RT.Expr.toRT Map.empty 0 None
        |> RT.VMState.createWithoutTLID
      let! lambdaDval = LibExecution.Interpreter.execute exeState vmA |> Ply.toTask

      let applicable =
        match lambdaDval with
        | RT.DApplicable app -> app
        | other -> failtestf "expected DApplicable from VM-A, got %A" other

      // VM-B lives inside executeApplicable — a fresh VMState is created there
      // to run the Apply. If lambdaInstrCache were still per-VM, VM-B's cache
      // would be empty and this call would fail with `LambdaNotAvailable`.
      let! result =
        LibExecution.Execution.executeApplicable
          exeState
          // Host-initiated, as `execute` is: the state's own access.
          exeState.access
          applicable
          (NEList.singleton (RT.DInt64 42L))
        |> Ply.toTask

      match result with
      | Ok dval -> Expect.equal dval (RT.DInt64 42L) "identity lambda across VMs"
      | Error(rte, _) -> failtestf "executeApplicable errored: %A" rte
    }

  /// A lambda whose instructions this execution never registered: what a lambda
  /// stored by an earlier run looks like when it is read back. Applying it is a
  /// runtime error the person can act on, not an internal "lambda not found"
  /// exception carrying only an id.
  let staleLambda =
    testTask "applying a lambda from another execution is a runtime error" {
      // The real packages: the message comes from the Dark printer.
      let! (exeState : RT.ExecutionState) = executionStateFor pmPT false

      let applicable =
        RT.AppLambda
          { exprId = 987654321UL
            closedRegisters = [||]
            typeSymbolTable = RT.TST.empty
            access = exeState.access
            expectedReturn = ValueNone
            writtenIn = ValueNone }

      let! result =
        LibExecution.Execution.executeApplicable
          exeState
          exeState.access
          applicable
          (NEList.singleton (RT.DInt64 42L))
        |> Ply.toTask

      match result with
      | Error((RTE.Apply(RTE.Applications.LambdaNotAvailable exprId) as rte), _) ->
        Expect.equal exprId 987654321UL "names the lambda it could not find"
        // The printer is where the hint lives: it says what to write instead.
        match! LibExecution.Execution.runtimeErrorToString exeState rte with
        | Ok(RT.DString message) ->
          Expect.stringContains message "earlier run" "says what happened"
          Expect.stringContains message "let f() =" "says what to write instead"
        | other -> failtestf "expected a rendered message, got %A" other
      | Error(rte, _) -> failtestf "expected LambdaNotAvailable, got %A" rte
      | Ok dval -> failtestf "expected an error, got %A" dval
    }

  let tests =
    testList
      "Lambdas"
      [ identity; add; addTuple; addToClosedVars; crossVM; staleLambda ]


module Fns =
  /// A named fn with no arguments evaluates to an `AppNamedFn` carrying the caller's access.
  let private unapplied name ptExpr fnName =
    t
      name
      ptExpr
      (RT.DApplicable(
        RT.AppNamedFn
          { name = fnName
            typeSymbolTable = RT.TST.empty
            typeArgs = []
            approvalName = None
            access = allowAllAccess
            argsSoFar = []
            boundImpls = [] }
      ))

  let builtin =
    unapplied "Builtin.add" E.Fns.Builtin.unapplied (RT.FQFnName.fqBuiltin "add" 0)

  let myAdd =
    unapplied
      "Test.myAdd"
      E.Fns.Package.MyAdd.unapplied
      (RT.FQFnName.fqPackage E.Fns.Package.MyAdd.hash)

  let fact =
    unapplied
      "Test.fact"
      E.Fns.Package.Fact.unapplied
      (RT.FQFnName.fqPackage E.Fns.Package.Fact.hash)

  // A builtin called with too few arguments raises, and a call on the short call's result
  // never runs.
  let private tooFew =
    RTE.Applications.TooFewArgsForFn(RT.FQFnName.fqBuiltin "add" 0, 2, 1)
    |> RTE.Apply

  let builtinPartiallyApplied =
    tFail "Builtin.add(1)" E.Fns.Builtin.partiallyApplied tooFew

  let builtinTwoStepApplied =
    tFail "(Builtin.add(1))(2)" E.Fns.Builtin.twoStepApplication tooFew

  let tests =
    testList
      "Fns"
      [ builtin; myAdd; fact; builtinPartiallyApplied; builtinTwoStepApplied ]


/// The synchronous unifier must never disagree with the computation-expression one.
///
/// `TypeChecker.tryUnifySync` exists purely to answer the ordinary cases without allocating a
/// continuation, so the only acceptable behaviours are "same answer as the slow path" or "declined, go
/// ask the slow path". A `ValueSome` that disagrees would silently change what type-checks, which is the
/// one way this optimisation could be dangerous rather than merely ineffective.
module SyncUnify =
  /// Every shape worth crossing: scalars against themselves and each other, containers at one and two
  /// levels, tuples of matching and differing arity, `Unknown`, and type variables both unbound and
  /// already bound.
  let private expectations : List<RT.TypeReference> =
    [ RT.TUnit
      RT.TBool
      RT.TInt64
      RT.TBigInt
      RT.TFloat
      RT.TChar
      RT.TString
      RT.TUuid
      RT.TVariable "a"
      RT.TVariable "b"
      RT.TList RT.TInt64
      RT.TList RT.TString
      RT.TList(RT.TVariable "a")
      RT.TList(RT.TList RT.TInt64)
      RT.TDict(RT.TString, RT.TInt64)
      RT.TDict(RT.TString, RT.TVariable "a")
      RT.TDict(RT.TInt64, RT.TString)
      RT.TStream RT.TInt64
      RT.TTuple(RT.TInt64, RT.TString, [])
      RT.TTuple(RT.TInt64, RT.TString, [ RT.TBool ])
      RT.TTuple(RT.TVariable "a", RT.TVariable "b", []) ]

  let private actuals : List<RT.Dval> =
    [ RT.DUnit
      RT.DBool true
      RT.DInt64 1L
      LibExecution.Dval.bigInt 1I
      RT.DFloat 1.0
      RT.DChar "c"
      RT.DString "s"
      RT.DUuid(System.Guid.NewGuid())
      RT.DList(RT.ValueType.Known RT.KTInt64, RT.Items([ RT.DInt64 1L ]))
      RT.DList(RT.ValueType.Known RT.KTString, RT.Items([ RT.DString "s" ]))
      RT.DList(RT.ValueType.Unknown, RT.Items([]))
      RT.DList(
        RT.ValueType.Known(RT.KTList(RT.ValueType.Known RT.KTInt64)),
        RT.Items([])
      )
      RT.DDict(
        RT.ValueType.Known RT.KTString,
        RT.ValueType.Known RT.KTInt64,
        RT.DictMap.empty
      )
      RT.DTuple(RT.DInt64 1L, RT.DString "s", [])
      RT.DTuple(RT.DInt64 1L, RT.DString "s", [ RT.DBool true ]) ]

  let private symbolTables : List<RT.TypeSymbolTable> =
    [ RT.TST.empty
      RT.TST.ofList [ "a", RT.ValueType.Known RT.KTInt64 ]
      RT.TST.ofList [ "a", RT.ValueType.Unknown ]
      RT.TST.ofList
        [ "a", RT.ValueType.Known RT.KTString; "b", RT.ValueType.Known RT.KTInt64 ] ]

  let agreesWithAsyncPath =
    testTask "sync unifier agrees with the async one, or declines" {
      let! (exeState : RT.ExecutionState) = executionStateFor pmPT false
      let mutable asyncAccepted = 0
      let mutable answeredWhenAccepted = 0

      for tst in symbolTables do
        for expected in expectations do
          for actual in actuals do
            let sync = LibExecution.TypeChecker.tryUnifySync tst expected actual
            let! async' =
              LibExecution.TypeChecker.checkFnParam
                exeState.types
                (RT.FQFnName.fqBuiltin "test" 0)
                tst
                0
                "p"
                expected
                actual
              |> Ply.toTask

            match async' with
            | Ok asyncTst ->
              asyncAccepted <- asyncAccepted + 1
              match sync with
              | ValueNone -> () // declining is always allowed; the caller falls back
              | ValueSome syncTst ->
                answeredWhenAccepted <- answeredWhenAccepted + 1
                Expect.equal
                  syncTst
                  asyncTst
                  $"same symbol table for {expected} vs {actual} under {tst}"
            | Error _ ->
              match sync with
              | ValueNone -> () // correct: failures go to the slow path for error rendering
              | ValueSome _ ->
                Exception.raiseInternal
                  "sync unifier accepted what the async path rejected"
                  [ "expected", expected; "actual", actual; "tst", tst ]

      // Most of the matrix is deliberately mismatched pairs, which decline correctly. The ratio that
      // matters is how much of what the slow path *accepts* the fast path also answers: if that fell to
      // nothing the optimisation would be silently inert and this test would still pass on agreement.
      Expect.isGreaterThan
        (answeredWhenAccepted * 10)
        (asyncAccepted * 8)
        $"fast path should answer most accepted unifications ({answeredWhenAccepted} of {asyncAccepted})"
    }

  let tests = testList "SyncUnify" [ agreesWithAsyncPath ]


module AliasChecks =
  module TC = LibExecution.TypeChecker

  let private aliasName = RT.FQTypeName.fqPackage "alias-cache-test"
  let private targetName = RT.FQTypeName.fqPackage "alias-cache-target"

  let private reference name args =
    RT.TCustomType({ originalName = [ "Alias" ]; resolved = Ok name }, args)

  let private alias = reference aliasName []

  let private declaration name parameters target : RT.PackageType.PackageType =
    let (RT.FQTypeName.Package hash) = name
    { hash = hash
      declaration =
        { typeParams = parameters; definition = RT.TypeDeclaration.Alias target } }

  let private typesFor target : RT.Types =
    { package = fun _ -> Ply.Ply(Some(declaration aliasName [] target)) }

  let private check types expected actual =
    TC.checkFnParam
      types
      (RT.FQFnName.fqBuiltin "test" 0)
      RT.TST.empty
      0
      "value"
      expected
      actual
    |> Ply.toTask

  let tests =
    testList
      "AliasChecks"
      [ testTask "warm aliases use the sync path and retain their error name" {
          let types = typesFor RT.TInt64
          let! accepted = check types alias (RT.DInt64 1L)
          Expect.equal accepted (Ok RT.TST.empty) "first call resolves the alias"
          Expect.equal
            (TC.tryUnifyWithAliasesSync types RT.TST.empty alias (RT.DInt64 1L))
            (ValueSome RT.TST.empty)
            "subsequent calls use the synchronous checker"
          let! rejected = check types alias (RT.DString "wrong")
          match rejected with
          | Error(RTE.Apply(RTE.Applications.FnParameterNotExpectedType(_,
                                                                        _,
                                                                        _,
                                                                        Some expected,
                                                                        _,
                                                                        _,
                                                                        _))) ->
            Expect.equal expected alias "error retains the declared alias"
          | _ -> failtest "expected an argument type error"
          let! result =
            TC.checkFnResult
              types
              (RT.FQFnName.fqBuiltin "test" 0)
              RT.TST.empty
              alias
              (RT.DString "wrong")
            |> Ply.toTask
          match result with
          | Error(RTE.Apply(RTE.Applications.FnResultNotExpectedType(_,
                                                                     Some expected,
                                                                     _,
                                                                     _,
                                                                     _))) ->
            Expect.equal expected alias "return error retains the declared alias"
          | _ -> failtest "expected a return type error"
        }
        testTask "aliases of records and enums use the nominal fast path" {
          let target = reference targetName []
          let cases =
            [ RT.TypeDeclaration.Record(
                NEList.singleton { name = "field"; typ = RT.TInt64 }
              ),
              RT.DRecord(
                targetName,
                targetName,
                [],
                RT.RecordFields.ofList [ "field", RT.DInt64 1L ]
              )
              RT.TypeDeclaration.Enum(
                NEList.singleton { name = "Case"; fields = [] }
              ),
              RT.DEnum(targetName, targetName, [], "Case", []) ]
          for definition, actual in cases do
            let types : RT.Types =
              { package =
                  fun hash ->
                    if RT.FQTypeName.Package hash = aliasName then
                      Ply.Ply(Some(declaration aliasName [] target))
                    else
                      Ply.Ply(
                        Some
                          { hash = hash
                            declaration =
                              { typeParams = []; definition = definition } }
                      ) }
            let! accepted = check types alias actual
            Expect.equal
              accepted
              (Ok RT.TST.empty)
              "the alias accepts the nominal value"
            Expect.equal
              (TC.tryUnifyWithAliasesSync types RT.TST.empty alias actual)
              (ValueSome RT.TST.empty)
              "the warm nominal alias is synchronous"
        }
        testTask "resolved aliases are scoped to their type provider" {
          let integers = typesFor RT.TInt64
          let strings = typesFor RT.TString
          let! _ = check integers alias (RT.DInt64 1L)
          Expect.equal
            (TC.tryUnifyWithAliasesSync strings RT.TST.empty alias (RT.DInt64 1L))
            ValueNone
            "another provider cannot reuse the resolution"
          let! accepted = check strings alias (RT.DString "right")
          Expect.equal accepted (Ok RT.TST.empty) "the other declaration wins"
          Expect.equal
            (TC.tryUnifyWithAliasesSync strings RT.TST.empty alias (RT.DInt64 1L))
            ValueNone
            "a warm cache cannot accept the other provider's type"
        }
        testTask "generic alias chains do not cache call-specific bindings" {
          let expected = reference aliasName [ RT.TVariable "call" ]
          let types : RT.Types =
            { package =
                fun hash ->
                  let decl =
                    if RT.FQTypeName.Package hash = aliasName then
                      declaration
                        aliasName
                        [ "a" ]
                        (reference targetName [ RT.TVariable "a" ])
                    else
                      declaration targetName [ "b" ] (RT.TList(RT.TVariable "b"))
                  Ply.Ply(Some decl) }
          for actual in
            [ RT.DList(VT.known RT.KTInt64, RT.Items([ RT.DInt64 1L ]))
              RT.DList(VT.known RT.KTString, RT.Items([ RT.DString "text" ])) ] do
            let! accepted = check types expected actual
            match accepted with
            | Ok tst ->
              Expect.equal
                (TC.tryUnifyWithAliasesSync types RT.TST.empty expected actual)
                (ValueSome tst)
                "each call binds its own type variable"
            | Error _ -> failtest "expected the generic alias to accept both calls"
        }
        testTask "a missing alias-chain endpoint is retried" {
          let mutable available = false
          let types : RT.Types =
            { package =
                fun hash ->
                  if RT.FQTypeName.Package hash = aliasName then
                    Ply.Ply(Some(declaration aliasName [] (reference targetName [])))
                  elif available then
                    Ply.Ply(Some(declaration targetName [] RT.TInt64))
                  else
                    Ply.Ply None }
          let! _ = RT.TypeReference.unwrapAlias types alias |> Ply.toTask
          Expect.equal
            (RT.TypeReference.tryResolvedAlias types alias)
            ValueNone
            "an incomplete chain is not cached"
          available <- true
          let! accepted = check types alias (RT.DInt64 1L)
          Expect.equal accepted (Ok RT.TST.empty) "a newly available alias resolves"
          Expect.equal
            (TC.tryUnifyWithAliasesSync types RT.TST.empty alias (RT.DInt64 1L))
            (ValueSome RT.TST.empty)
            "the complete chain can now be cached"
        } ]


module DictKeyOrdering =
  let private oneOfEachCase : List<string * RT.Dval> =
    [ "DUnit", RT.DUnit
      "DBool", RT.DBool true
      "DInt8", RT.DInt8 1y
      "DUInt8", RT.DUInt8 1uy
      "DInt16", RT.DInt16 1s
      "DUInt16", RT.DUInt16 1us
      "DInt32", RT.DInt32 1
      "DUInt32", RT.DUInt32 1u
      "DInt64", RT.DInt64 1L
      "DUInt64", RT.DUInt64 1UL
      "DInt128", RT.DInt128(System.Int128.op_Implicit 1)
      "DUInt128", RT.DUInt128(System.UInt128.op_Implicit 1u)
      "DBigInt", LibExecution.Dval.bigInt 1I
      "DFloat", RT.DFloat 1.0
      "DDecimal", RT.DDecimal 1.5m
      "DChar", RT.DChar "c"
      "DString", RT.DString "s"
      "DDateTime", RT.DDateTime(LibExecution.DarkDateTime.T(2026, 8, 31, 0, 0, 0))
      "DUuid", RT.DUuid(System.Guid.NewGuid())
      "DList", RT.DList(VT.unknown, RT.Items([]))
      "DTuple", RT.DTuple(RT.DUnit, RT.DUnit, [])
      "DDict", RT.DDict(VT.unknown, VT.unknown, RT.DictMap.empty)
      "DRecord",
      RT.DRecord(
        RT.FQTypeName.Package(RT.Hash "t"),
        RT.FQTypeName.Package(RT.Hash "t"),
        [],
        RT.RecordFields.empty
      )
      "DEnum",
      RT.DEnum(
        RT.FQTypeName.Package(RT.Hash "t"),
        RT.FQTypeName.Package(RT.Hash "t"),
        [],
        "Case",
        []
      )
      "DBytes", RT.DBytes(RT.Persistent("hash", 0L)) ]

  let allCasesAreDistinctKeys =
    testCase "every Dval case is a distinct dict key" (fun _ ->
      let asDict =
        oneOfEachCase
        |> List.map (fun (name, dv) -> dv, RT.DString name)
        |> RT.DictMap.ofList
      Expect.equal
        (RT.DictMap.count asDict)
        (List.length oneOfEachCase)
        "two Dval cases collapsed into one key — check DictKey.caseTag for a collision")

  let everyCaseFindsItself =
    testCase "every Dval case can be looked up again" (fun _ ->
      let asDict =
        oneOfEachCase
        |> List.map (fun (name, dv) -> dv, RT.DString name)
        |> RT.DictMap.ofList
      for (name, dv) in oneOfEachCase do
        Expect.equal
          (RT.DictMap.tryFind dv asDict)
          (Some(RT.DString name))
          $"{name} must find itself")

  let admissibilityMatchesComparability =
    testCase "admissible keys are comparable; inadmissible ones are named" (fun _ ->
      for (name, dv) in oneOfEachCase do
        if LibExecution.RuntimeTypes.Dval.isUsableDictKey dv then
          let asDict = RT.DictMap.ofList [ dv, RT.DString name ]
          Expect.equal
            (RT.DictMap.tryFind dv asDict)
            (Some(RT.DString name))
            $"{name} is admissible, so it must compare without raising"
        else
          failtest $"unexpected inadmissible case: {name}")

  let inadmissibleValuesPoisonTheirContainers =
    testCase "a container holding an inadmissible value is inadmissible" (fun _ ->
      let bad = LibExecution.Stream.newFromIO VT.int64 (fun () -> Ply None) None
      let isValid = LibExecution.RuntimeTypes.Dval.isUsableDictKey
      let dictOfBad =
        RT.DDict(VT.unknown, VT.unknown, RT.DictMap.ofList [ RT.DUnit, bad ])
      Expect.isFalse (isValid bad) "a stream is not a key"
      Expect.isFalse
        (isValid (RT.DList(VT.unknown, RT.Items([ bad ]))))
        "nor a list of one"
      Expect.isFalse
        (isValid (RT.DTuple(RT.DUnit, bad, [])))
        "nor a tuple holding one"
      Expect.isFalse (isValid dictOfBad) "nor a dict holding one as a value"
      Expect.isFalse (isValid (RT.DFloat nan)) "NaN is not a key"
      Expect.isFalse (isValid (RT.DFloat32 nanf)) "nor a Float32 NaN"
      Expect.isFalse
        (isValid (RT.DList(VT.unknown, RT.Items([ RT.DFloat 1.0; RT.DFloat nan ]))))
        "nor a list holding one"
      Expect.isFalse
        (isValid (
          RT.DDict(
            VT.unknown,
            VT.unknown,
            RT.DictMap.ofList [ RT.DUnit, RT.DFloat nan ]
          )
        ))
        "nor a dict holding one as a value"
      Expect.isTrue (isValid (RT.DFloat infinity)) "infinity is still a key"
      Expect.isTrue
        (isValid (RT.DList(VT.unknown, RT.Items([ RT.DInt64 1L ]))))
        "a list of ordinary values is still a key")

  let private seamPairs : List<string * RT.Dval * RT.Dval> =
    let runtimeName = RT.FQTypeName.Package(RT.Hash "shared-runtime-type")
    let sourceA = RT.FQTypeName.Package(RT.Hash "written-as-alias")
    let sourceB = RT.FQTypeName.Package(RT.Hash "written-directly")
    let fields = RT.RecordFields.ofList [ "x", RT.DInt64 1L ]
    [ "record via alias vs directly",
      RT.DRecord(sourceA, runtimeName, [], fields),
      RT.DRecord(sourceB, runtimeName, [], fields)

      "enum via alias vs directly",
      RT.DEnum(sourceA, runtimeName, [], "Case", [ RT.DInt64 1L ]),
      RT.DEnum(sourceB, runtimeName, [], "Case", [ RT.DInt64 1L ])

      "signed zero", RT.DFloat 0.0, RT.DFloat -0.0

      // `==` and decimal's own Equals ignore scale; a dict key has to as well
      "decimal scale", RT.DDecimal 1.10m, RT.DDecimal 1.1m

      "persistent blobs differing only in length",
      RT.DBytes(RT.Persistent("samehash", 1L)),
      RT.DBytes(RT.Persistent("samehash", 2L)) ]

  let private sameKey (a : RT.Dval) (b : RT.Dval) : bool =
    RT.DictMap.containsKey a (RT.DictMap.ofList [ b, RT.DUnit ])

  let identityAgreesWithEquality =
    testCase "key identity agrees with == on every pair but NaN" (fun _ ->
      let values =
        (oneOfEachCase |> List.map (fun (n, dv) -> n, dv))
        @ [ "DFloat 0.0", RT.DFloat 0.0
            "DFloat -0.0", RT.DFloat -0.0
            "DFloat 1.0", RT.DFloat 1.0 ]
      for (leftName, left) in values do
        for (rightName, right) in values do
          Expect.equal
            (sameKey left right)
            (LibExecution.Dval.equals left right)
            $"{leftName} vs {rightName}: same key and == must agree")

  let identityAgreesWithEqualityOnSeams =
    testCase "...including on the pairs most likely to drift" (fun _ ->
      for (name, left, right) in seamPairs do
        Expect.equal
          (sameKey left right)
          (LibExecution.Dval.equals left right)
          $"{name}: same key and == must agree")

  let nestedIncompatibilityIsReported =
    testCase "nested incompatible values are reported" (fun _ ->
      let left = RT.DList(VT.unknown, RT.Items([ RT.DInt64 1L ]))
      let right = RT.DList(VT.unknown, RT.Items([ RT.DString "one" ]))
      try
        RT.DvalOrdering.compareForSort left right |> ignore<int>
        failtest "expected the incompatible nested values"
      with :? RT.DvalComparisonException as e ->
        match e.Left, e.Right with
        | RT.DInt64 1L, RT.DString "one" -> ()
        | _ -> failtest "expected the incompatible nested values")

  let equalityIsNotTransitive =
    testCase "Dval.equals is not an equivalence relation" (fun _ ->
      let tn = RT.FQTypeName.Package(RT.Hash "some-enum")
      let withArg vt = RT.DEnum(tn, tn, [ vt ], "None", [])
      let unknown = withArg RT.ValueType.Unknown
      let ints = withArg VT.int64
      let strings = withArg VT.string
      Expect.isTrue
        (LibExecution.Dval.equals unknown ints)
        "Unknown == Int64 (merge succeeds)"
      Expect.isTrue
        (LibExecution.Dval.equals unknown strings)
        "Unknown == String (merge succeeds)"
      Expect.isFalse
        (LibExecution.Dval.equals ints strings)
        "but Int64 <> String — so equals is not transitive")

  let divergencesAreDeliberate =
    testCase "the two divergences from == are deliberate" (fun _ ->
      let nan = RT.DFloat(0.0 / 0.0)
      Expect.isFalse (LibExecution.Dval.equals nan nan) "== says NaN <> NaN"
      Expect.isTrue (sameKey nan nan) "the key order still needs NaN to find itself"
      Expect.isFalse
        (LibExecution.RuntimeTypes.Dval.isUsableDictKey nan)
        "so a NaN is refused as a key rather than stored"

      let tn = RT.FQTypeName.Package(RT.Hash "some-enum")
      let withArg vt = RT.DEnum(tn, tn, [ vt ], "None", [])
      Expect.isFalse
        (LibExecution.Dval.equals (withArg VT.int64) (withArg VT.string))
        "== distinguishes differing type args"
      Expect.isTrue
        (sameKey (withArg VT.int64) (withArg VT.string))
        "key identity ignores them, because merge-based equality isn't transitive")

  let tests =
    testList
      "DictKeyOrdering"
      [ allCasesAreDistinctKeys
        everyCaseFindsItself
        admissibilityMatchesComparability
        inadmissibleValuesPoisonTheirContainers
        identityAgreesWithEquality
        identityAgreesWithEqualityOnSeams
        nestedIncompatibilityIsReported
        divergencesAreDeliberate
        equalityIsNotTransitive ]


module ListUnique =
  let private comparer = Dval.equalityComparer

  let private lambda (id : id) : RT.Dval =
    RT.DApplicable(
      RT.AppLambda
        { exprId = id
          closedRegisters = [||]
          typeSymbolTable = RT.TST.empty
          access =
            LibExecution.Permissions.Access.start
              LibExecution.Permissions.Policy.denyAll
          expectedReturn = ValueNone
          writtenIn = ValueNone }
    )

  let private named (name : string) (args : RT.Dval list) : RT.Dval =
    RT.DApplicable(
      RT.AppNamedFn
        { name = RT.FQFnName.fqBuiltin name 0
          typeSymbolTable = RT.TST.empty
          typeArgs = []
          approvalName = None
          access = None
          argsSoFar = args
          boundImpls = [] }
    )

  let private equalHashes =
    test "equal values have equal hashes, including compatible metadata" {
      let name = RT.FQTypeName.fqPackage "resolved"
      let alias = RT.FQTypeName.fqPackage "alias"
      let fields = RT.RecordFields.ofList [ "value", RT.DFloat 0.0 ]
      let record source args = RT.DRecord(source, name, args, fields)
      let enum source args = RT.DEnum(source, name, args, "None", [])
      let dict key value =
        RT.DDict(VT.float, VT.float, RT.DictMap.ofList [ RT.DFloat key, value ])
      let stream = LibExecution.Stream.newFromIO VT.unit (fun () -> Ply None) None
      let promise =
        RT.Promise(
          System.Threading.Tasks.Task.FromResult RT.DUnit,
          RT.FQFnName.fqBuiltin "testRead" 0,
          RT.ExecutionPoint.Source
        )
      let blob = LibExecution.Bytes.newEphemeral [| 1uy |]
      let dictWithKey key =
        RT.DDict(VT.unknown, VT.unit, RT.DictMap.ofList [ key, RT.DUnit ])
      let pairs =
        [ RT.DFloat 0.0, RT.DFloat -0.0
          RT.DList(VT.unknown, RT.Items([])), RT.DList(VT.int64, RT.Items([]))
          record alias [ VT.unknown ], record name [ VT.int64 ]
          enum alias [ VT.unknown ], enum name [ VT.int64 ]
          // Dict keys ignore type arguments even when `equals` would distinguish
          // the keys themselves. Their enclosing dicts must still hash alike.
          dictWithKey (record alias [ VT.int64 ]),
          dictWithKey (record name [ VT.string ])
          dictWithKey (enum alias [ VT.int64 ]),
          dictWithKey (enum name [ VT.string ])
          dict 0.0 (RT.DFloat 1.0), dict -0.0 (RT.DFloat 1.0)
          dict nan (RT.DFloat 1.0), dict -nan (RT.DFloat 1.0)
          lambda 1UL, lambda 1UL
          named "bigIntAdd" [], named "bigIntAdd" []
          named "bigIntAdd" [ RT.DInt64 1L ], named "bigIntAdd" [ RT.DInt64 1L ]
          RT.DBytes(RT.Persistent("same-hash", 5L)),
          RT.DBytes(RT.Persistent("same-hash", 5L))
          blob,
          (match blob with
           | RT.DBytes bytes ->
             RT.DBytes(RT.Persistent(bytes.ContentHash, bytes.Length))
           | _ -> failtest "expected Bytes")
          blob, blob
          stream, stream
          RT.DPromise promise, RT.DPromise promise ]
      for a, b in pairs do
        let wrappers =
          [ a, b
            RT.DList(VT.unknown, RT.Items([ a ])),
            RT.DList(VT.unknown, RT.Items([ b ]))
            RT.DTuple(a, RT.DUnit, []), RT.DTuple(b, RT.DUnit, [])
            RT.DRecord(name, name, [], RT.RecordFields.ofList [ "value", a ]),
            RT.DRecord(alias, name, [], RT.RecordFields.ofList [ "value", b ])
            RT.DEnum(name, name, [], "Some", [ a ]),
            RT.DEnum(alias, name, [], "Some", [ b ])
            RT.DDict(VT.string, VT.unknown, RT.DictMap.ofList [ RT.DString "k", a ]),
            RT.DDict(VT.string, VT.unknown, RT.DictMap.ofList [ RT.DString "k", b ]) ]
        for left, right in wrappers do
          Expect.isTrue (comparer.Equals(left, right)) "the pair compares equal"
          Expect.equal
            (comparer.GetHashCode left)
            (comparer.GetHashCode right)
            "equal values must land in the same hash bucket"
    }

  let private longPrefixes =
    test "long shared prefixes do not require quadratic equality calls" {
      let prefix = List.replicate 64 (RT.DInt64 0L)
      let name = RT.FQTypeName.fqPackage "record"
      let list xs = RT.DList(VT.int64, RT.Items(xs))
      let tuple xs = RT.DTuple(RT.DUnit, RT.DUnit, xs)
      let enum xs = RT.DEnum(name, name, [], "Values", xs)
      let dict xs =
        RT.DDict(
          VT.int64,
          VT.int64,
          xs |> List.mapi (fun i x -> RT.DInt64(int64 i), x) |> RT.DictMap.ofList
        )
      let record xs =
        RT.DRecord(
          name,
          name,
          [],
          xs |> List.mapi (fun i x -> $"field{i:D3}", x) |> RT.RecordFields.ofList
        )
      let nested xs =
        [ 1..64 ]
        |> List.fold
          (fun value _ -> RT.DList(VT.unknown, RT.Items([ value ])))
          (list xs)
      for make in [ list; tuple; enum; dict; record; nested; named "partial" ] do
        let mutable comparisons = 0
        let counted =
          HashIdentity.FromFunctions comparer.GetHashCode (fun a b ->
            comparisons <- comparisons + 1
            comparer.Equals(a, b))
        let seen = System.Collections.Generic.HashSet<RT.Dval>(counted)
        let rows =
          [ 1..3000 ]
          |> List.map (fun i -> make (prefix @ [ RT.DInt64(int64 (i % 1500)) ]))
        let kept = rows |> List.filter seen.Add
        Expect.equal kept.Length 1500 "one copy of each row survives"
        Expect.isLessThan
          comparisons
          10_000
          "differences beyond any fixed prefix must contribute to the hash"
    }

  let private collisionCounts =
    test "same-shaped rows do not require quadratic equality calls" {
      let mutable comparisons = 0
      let counted =
        HashIdentity.FromFunctions comparer.GetHashCode (fun a b ->
          comparisons <- comparisons + 1
          comparer.Equals(a, b))
      let seen = System.Collections.Generic.HashSet<RT.Dval>(counted)
      let rows =
        [ 1..3000 ]
        |> List.map (fun i ->
          RT.DList(
            VT.string,
            RT.Items(
              [ RT.DString "owner"
                RT.DString "modules"
                RT.DString(string (i % 1500)) ]
            )
          ))
      let kept = rows |> List.filter seen.Add
      Expect.equal kept.Length 1500 "one copy of each row survives"
      Expect.isLessThan comparisons 10_000 "a constant or shallow hash must fail"
    }

  let private identityDistribution =
    test "identity-bearing values do not all share a hash" {
      let streams =
        [ 1..256 ]
        |> List.map (fun _ ->
          LibExecution.Stream.newFromIO VT.unit (fun () -> Ply None) None)
      let promises =
        [ 1..256 ]
        |> List.map (fun _ ->
          RT.DPromise(
            RT.Promise(
              System.Threading.Tasks.Task.FromResult RT.DUnit,
              RT.FQFnName.fqBuiltin "testRead" 0,
              RT.ExecutionPoint.Source
            )
          ))
      let families =
        [ streams
          promises
          [ 1..256 ]
          |> List.map (fun i -> LibExecution.Bytes.newEphemeral [| byte i |])
          [ 1..256 ] |> List.map (fun i -> RT.DBytes(RT.Persistent(string i, 0L)))
          [ 1..256 ] |> List.map (fun i -> lambda (uint64 i))
          [ 1..256 ] |> List.map (fun i -> named $"fn{i}" []) ]
      for values in families do
        let hashes = values |> List.map comparer.GetHashCode |> Set.ofList
        Expect.isGreaterThan
          hashes.Count
          128
          "use the identity or content, not just the Dval tag"
    }

  let private collisionsStillCompare =
    test "collisions preserve distinct values and first occurrences" {
      let make last =
        RT.DList(
          VT.int64,
          RT.Items(List.replicate 40 (RT.DInt64 0L) @ [ RT.DInt64 last ])
        )
      let first = make 1L
      let second = make 2L
      let colliding =
        HashIdentity.FromFunctions (fun _ -> 0) (fun a b -> comparer.Equals(a, b))
      let seen = System.Collections.Generic.HashSet<RT.Dval>(colliding)
      let kept = [ second; first; second; first ] |> List.filter seen.Add
      Expect.equal kept [ second; first ] "equality resolves the hash collision"
    }

  let private nonReflexiveAndCompatible =
    test "deduplication follows Dark equality for NaN and compatible types" {
      let name = RT.FQTypeName.fqPackage "enum"
      let value typ = RT.DEnum(name, name, [ typ ], "None", [])
      let unknown = value VT.unknown
      let integers = value VT.int64
      let strings = value VT.string
      let keep values =
        let seen = System.Collections.Generic.HashSet<RT.Dval>(comparer)
        List.filter seen.Add values
      Expect.equal
        (keep [ unknown; integers; strings ]).Length
        1
        "both concrete types match the first unknown representative"
      Expect.equal
        (keep [ integers; strings; unknown ]).Length
        2
        "the incompatible concrete representatives are both kept"
      let nan = RT.DFloat nan
      Expect.equal (keep [ nan; nan ]).Length 2 "NaN is unequal to itself"
    }

  let private forkMakers : List<RT.Dval -> RT.Dval -> RT.Dval> =
    let name = RT.FQTypeName.fqPackage "shared"
    [ (fun a b -> RT.DList(VT.unknown, RT.Items([ a; b ])))
      (fun a b -> RT.DTuple(a, b, []))
      (fun a b ->
        RT.DDict(
          VT.string,
          VT.unknown,
          RT.DictMap.ofList [ RT.DString "a", a; RT.DString "b", b ]
        ))
      (fun a b ->
        RT.DRecord(name, name, [], RT.RecordFields.ofList [ "a", a; "b", b ]))
      (fun a b -> RT.DEnum(name, name, [], "Node", [ a; b ]))
      (fun a b -> named "partial" [ a; b ]) ]

  let private sharedSubtrees =
    test "hashing shared subtrees does not allocate the expanded tree" {
      for make in forkMakers do
        let shared depth =
          [ 1..depth ] |> List.fold (fun child _ -> make child child) (RT.DInt64 1L)
        // Warm generic hashing and map-enumerator paths before measuring.
        comparer.GetHashCode(shared 2) |> ignore<int>
        let value = shared 14
        let before = System.GC.GetAllocatedBytesForCurrentThread()
        comparer.GetHashCode value |> ignore<int>
        let allocated = System.GC.GetAllocatedBytesForCurrentThread() - before
        // A small graph represents thousands of repeated subtrees. This generous
        // bound catches expansion without a timing assertion or a runaway test.
        Expect.isLessThan
          allocated
          262_144L
          "memoize completed subtrees instead of expanding shared references"
    }

  let private sharingDoesNotChangeHashes =
    test "shared and separately allocated equal subtrees have equal hashes" {
      for make in forkMakers do
        let rec build depth share =
          if depth = 0 then
            RT.DInt64 1L
          else
            let left = build (depth - 1) share
            let right = if share then left else build (depth - 1) share
            make left right
        let shared = build 6 true
        let copied = build 6 false
        Expect.isTrue
          (comparer.Equals(shared, copied))
          "the values compare equal regardless of sharing"
        Expect.equal
          (comparer.GetHashCode shared)
          (comparer.GetHashCode copied)
          "reference memoization must not affect the resulting hash"
        let seen = System.Collections.Generic.HashSet<RT.Dval>(comparer)
        Expect.isTrue (seen.Add shared) "the first value is kept"
        Expect.isFalse (seen.Add copied) "the equal copy is removed"
    }

  let tests =
    testList
      "ListUnique"
      [ equalHashes
        longPrefixes
        collisionCounts
        identityDistribution
        collisionsStillCompare
        nonReflexiveAndCompatible
        sharedSubtrees
        sharingDoesNotChangeHashes ]


/// A dict's entries are a sorted array up to `DictMap.smallLimit` and a tree past it.
/// Everything here checks that the reader cannot tell which: the same entries in the same
/// order, equal, hashed alike and written as the same bytes.
module DictShapes =
  let private limit = RT.DictMap.smallLimit

  let private isSmall (d : RT.DictMap) =
    match d with
    | RT.DictMap.Small _ -> true
    | RT.DictMap.Large _ -> false

  /// The same entries as a tree, however few: grown past the limit with fillers, then
  /// the fillers removed.
  let private asLarge (entries : List<RT.Dval * RT.Dval>) : RT.DictMap =
    let fillers = List.init (limit + 1) (fun i -> RT.DString $"~filler{i}")
    let grown =
      entries @ List.map (fun k -> k, RT.DUnit) fillers
      |> List.fold (fun acc (k, v) -> RT.DictMap.add k v acc) RT.DictMap.empty
    let d = List.fold (fun acc k -> RT.DictMap.remove k acc) grown fillers
    Expect.isFalse (isSmall d) "asLarge makes a large one"
    d

  /// Each node's stored height is right, and no node's sides differ by more than two,
  /// the bound F#'s `Map` keeps. Returns the height.
  let rec private checkBalanced (t : RT.DictTree) : int =
    match t with
    | null -> 0
    | :? RT.DictTreeNode as n ->
      let hl = checkBalanced n.Left
      let hr = checkBalanced n.Right
      Expect.isLessThanOrEqual (abs (hl - hr)) 2 "the sides differ by at most two"
      Expect.equal n.Height (max hl hr + 1) "an inner node's stored height"
      n.Height
    | leaf ->
      Expect.equal leaf.Height 1 "a leaf's stored height"
      1

  let private expectSame (reference : Map<int64, RT.Dval>) (d : RT.DictMap) msg =
    match d with
    | RT.DictMap.Large root -> checkBalanced root |> ignore<int>
    | RT.DictMap.Small _ -> ()
    Expect.equal
      (RT.DictMap.toList d)
      (reference |> Map.toList |> List.map (fun (k, v) -> RT.DInt64 k, v))
      $"{msg}: entries, in key order"
    Expect.equal (RT.DictMap.count d) reference.Count $"{msg}: count"

  /// Inserts, overwrites and removes in a scrambled order, across the limit and back.
  let agreesWithMapAcrossTheLimit =
    testCase
      "agrees with Map at every step, growing past the limit and shrinking"
      (fun _ ->
        let n = 301
        // 37 is coprime with n, so this visits every key once, out of order.
        let order = List.init n (fun i -> int64 ((i * 37) % n))
        let mutable reference = Map.empty
        let mutable d = RT.DictMap.empty
        for k in order do
          reference <- Map.add k (RT.DInt64(k * 10L)) reference
          d <- RT.DictMap.add (RT.DInt64 k) (RT.DInt64(k * 10L)) d
          expectSame reference d $"after adding {k}"
          Expect.equal
            (isSmall d)
            (reference.Count <= limit)
            "the shape follows the size"
        for k in order do
          reference <- Map.add k (RT.DString "again") reference
          d <- RT.DictMap.add (RT.DInt64 k) (RT.DString "again") d
        expectSame reference d "after overwriting every key"
        for k in List.rev order do
          Expect.equal
            (RT.DictMap.tryFind (RT.DInt64 k) d)
            (Some(RT.DString "again"))
            $"{k} finds itself"
          reference <- Map.remove k reference
          d <- RT.DictMap.remove (RT.DInt64 k) d
          expectSame reference d $"after removing {k}"
          Expect.isFalse (RT.DictMap.containsKey (RT.DInt64 k) d) $"{k} is gone"
        Expect.equal (RT.DictMap.count d) 0 "empty at the end")

  let sameEntriesEitherShape =
    testCase
      "the same entries in either shape are equal, hash alike and save alike"
      (fun _ ->
        for n in [ 0; 1; 7; limit ] do
          let entries =
            List.init n (fun i -> RT.DString $"k{i}", RT.DInt64(int64 i))
          let small = RT.DictMap.ofList entries
          let large = asLarge entries
          Expect.isTrue (isSmall small) "ofList makes a small one"
          Expect.equal small large $"{n} entries: equal"
          Expect.equal
            (small.GetHashCode())
            (large.GetHashCode())
            $"{n} entries: same hash"
          let dict d = RT.DDict(VT.string, VT.int64, d)
          Expect.isTrue
            (Dval.equals (dict small) (dict large))
            $"{n} entries: Dval.equals"
          Expect.equal
            (LibSerialization.Binary.Serialization.RT.Dval.serialize
              "dval"
              (dict small))
            (LibSerialization.Binary.Serialization.RT.Dval.serialize
              "dval"
              (dict large))
            $"{n} entries: same bytes")

  let ofListKeepsTheLastDuplicate =
    testCase "ofList keeps the last value given for a key, small or large" (fun _ ->
      // 9 gives 18 entries, three keys given three times each: small, with repeats.
      for n in [ 9; limit + 3 ] do
        let entries =
          List.init n (fun i -> RT.DInt64(int64 (i % 3)), RT.DInt64(int64 i))
          @ List.init n (fun i -> RT.DInt64(int64 (100 + i)), RT.DUnit)
        let d = RT.DictMap.ofList entries
        Expect.equal (isSmall d) (List.length entries <= limit) $"{n}: the shape"
        let expected =
          entries
          |> List.fold (fun acc (k, v) -> RT.DictMap.add k v acc) RT.DictMap.empty
        Expect.equal
          (RT.DictMap.toList d)
          (RT.DictMap.toList expected)
          $"{n}: as adds"
        Expect.equal
          (RT.DictMap.tryFind (RT.DInt64 0L) d)
          (Some(RT.DInt64(int64 (n - 1 - ((n - 1) % 3)))))
          $"{n}: the last 0 wins")

  let unionAgreesWithAdds =
    testCase "union keeps the right side's value, whichever way it merges" (fun _ ->
      let dict (keys : List<int>) (tag : string) =
        keys
        |> List.map (fun k -> RT.DInt64(int64 k), RT.DString $"{tag}{k}")
        |> RT.DictMap.ofList
      let range a b = List.init (b - a) (fun i -> a + i)
      // small and small, small and small past the limit, a few keys into a big
      // tree, two big trees overlapping, and either side empty
      for left, right in
        [ range 0 5, range 3 9
          range 0 20, range 10 30
          range 0 300, [ 7; 301; 150 ]
          range 0 100, range 50 200
          [], range 0 30
          range 0 30, [] ] do
        let l = dict left "l"
        let r = dict right "r"
        let expected = RT.DictMap.fold (fun acc k v -> RT.DictMap.add k v acc) l r
        let actual = RT.DictMap.union l r
        Expect.equal
          (RT.DictMap.toList actual)
          (RT.DictMap.toList expected)
          $"{List.length left} into {List.length right}"
        match actual with
        | RT.DictMap.Large root -> checkBalanced root |> ignore<int>
        | RT.DictMap.Small entries ->
          Expect.isLessThanOrEqual entries.Length limit "a small result stays small")

  let tests =
    testList
      "DictShapes"
      [ agreesWithMapAcrossTheLimit
        sameEntriesEitherShape
        ofListKeepsTheLastDuplicate
        unionAgreesWithAdds ]


/// `RecordFields` keeps a record's fields sorted by name, ordinally, with no name twice.
module RecordFieldsShape =
  let private entry (name : string) (i : int64) = struct (name, RT.DInt64 i)
  let private names (entries : (struct (string * RT.Dval))[]) : List<string> =
    entries |> Array.map (fun (struct (n, _)) -> n) |> List.ofArray

  let sortedOrdinally =
    testCase "fields are sorted by code unit, not by culture" (fun _ ->
      let fields =
        [ "b"; "B"; "_x"; "a"; "\u00e9" ]
        |> List.mapi (fun i n -> n, RT.DInt64(int64 i))
        |> RT.RecordFields.ofList
      Expect.equal
        (names fields.Entries)
        [ "B"; "_x"; "a"; "b"; "\u00e9" ]
        "ordinal order"
      Expect.equal
        (RT.RecordFields.tryFind "\u00e9" fields)
        (Some(RT.DInt64 4L))
        "found by search")

  let ofArrayKeepsTheLastDuplicate =
    testCase "a name given several times keeps its last value" (fun _ ->
      let fields =
        RT.RecordFields.ofArray
          [| entry "x" 1L; entry "y" 2L; entry "x" 3L; entry "x" 4L; entry "a" 5L |]
      Expect.equal (names fields.Entries) [ "a"; "x"; "y" ] "one of each name"
      Expect.equal
        (RT.RecordFields.tryFind "x" fields)
        (Some(RT.DInt64 4L))
        "the last x")

  let insertSortedPlacesOrRefuses =
    testCase
      "insertSorted moves a new name into place and refuses a repeat"
      (fun _ ->
        for name in [ "0"; "b"; "d"; "f" ] do
          let entries =
            [| entry "a" 0L; entry "c" 0L; entry "e" 0L; entry name 1L |]
          Expect.isTrue (RT.RecordFields.insertSorted entries 3) $"{name} is new"
          Expect.equal
            (names entries)
            (List.sort [ "a"; "c"; "e"; name ])
            $"{name} in place"
        for name in [ "a"; "c"; "e" ] do
          let entries =
            [| entry "a" 0L; entry "c" 0L; entry "e" 0L; entry name 1L |]
          Expect.isFalse (RT.RecordFields.insertSorted entries 3) $"{name} repeats"
          Expect.equal
            (names entries)
            [ "a"; "c"; "e"; name ]
            $"{name} moved nothing")

  let setWritesOrInserts =
    testCase "set writes a present name in place and inserts a new one" (fun _ ->
      let start () = [| entry "b" 0L; entry "d" 0L |]
      let present = start ()
      let after = RT.RecordFields.set "d" (RT.DInt64 9L) present
      Expect.isTrue (obj.ReferenceEquals(after, present)) "same array"
      Expect.equal
        (RT.RecordFields.tryFind "d" (RT.RecordFields.ofArray after))
        (Some(RT.DInt64 9L))
        "written"
      for name, expected in
        [ "a", [ "a"; "b"; "d" ]; "c", [ "b"; "c"; "d" ]; "e", [ "b"; "d"; "e" ] ] do
        Expect.equal
          (names (RT.RecordFields.set name (RT.DInt64 1L) (start ())))
          expected
          $"{name} inserted")

  let tests =
    testList
      "RecordFieldsShape"
      [ sortedOrdinally
        ofArrayKeepsTheLastDuplicate
        insertSortedPlacesOrRefuses
        setWritesOrInserts ]


/// Deep equality, ordering, type merging, and substitution must fail the stack check.
/// A small thread stack triggers the guard without allocating values large enough
/// to stall the parallel suite. A missing check terminates the test process.
module DeepValues =
  let private depth = 20_000

  /// A list and its element type, both nested `depth` levels deep.
  let private deepList () : RT.Dval =
    let rec build (n : int) (value : RT.Dval) (typ : RT.ValueType) : RT.Dval =
      if n = 0 then
        value
      else
        build (n - 1) (RT.DList(typ, RT.Items([ value ]))) (VT.list typ)

    build depth (RT.DInt64 1L) (VT.known RT.KTInt64)

  let private onSmallStack (f : unit -> unit) : Option<exn> =
    let mutable thrown = None
    let thread =
      System.Threading.Thread(
        (fun () ->
          try
            f ()
          with ex ->
            thrown <- Some ex),
        256 * 1024
      )
    thread.Start()
    thread.Join()
    thrown

  let private refusesToOverflow (name : string) (f : RT.Dval -> RT.Dval -> unit) =
    test name {
      let a = deepList ()
      let b = deepList ()

      match onSmallStack (fun () -> f a b) with
      | Some(:? System.InsufficientExecutionStackException) -> ()
      | Some ex ->
        failtest $"expected the stack check to fail, got {ex.GetType().Name}"
      | None ->
        failtest "expected the stack check to fail, but the comparison finished"
    }

  let private substitutionRefusesToOverflow =
    test "Types.substitute" {
      let typ =
        [ 1..depth ] |> List.fold (fun inner _ -> RT.TList inner) (RT.TVariable "a")

      match
        onSmallStack (fun () ->
          RT.Types.substitute [ "a" ] [ RT.TBigInt ] typ |> ignore<RT.TypeReference>)
      with
      | Some(:? System.InsufficientExecutionStackException) -> ()
      | Some ex ->
        failtest $"expected the stack check to fail, got {ex.GetType().Name}"
      | None ->
        failtest "expected the stack check to fail, but substitution finished"
    }

  let private hashesMixedContainers =
    test "hashing deep maps and mixed containers finishes on a small stack" {
      let name = RT.FQTypeName.fqPackage "deep"
      let fn : RT.ApplicableNamedFn =
        { name = RT.FQFnName.fqBuiltin "partial" 0
          typeSymbolTable = RT.TST.empty
          typeArgs = []
          approvalName = None
          access = None
          argsSoFar = []
          boundImpls = [] }
      let value =
        [ 1..depth ]
        |> List.fold
          (fun inner i ->
            match i % 6 with
            | 0 -> RT.DList(VT.unknown, RT.Items([ inner ]))
            | 1 -> RT.DTuple(RT.DUnit, inner, [])
            | 2 ->
              RT.DDict(VT.unit, VT.unknown, RT.DictMap.ofList [ RT.DUnit, inner ])
            | 3 ->
              RT.DRecord(name, name, [], RT.RecordFields.ofList [ "value", inner ])
            | 4 -> RT.DEnum(name, name, [], "Value", [ inner ])
            | _ -> RT.DApplicable(RT.AppNamedFn { fn with argsSoFar = [ inner ] }))
          (RT.DInt64 1L)
      match onSmallStack (fun () -> Dval.hash value |> ignore<int>) with
      | None -> ()
      | Some ex -> failtest $"iterative hashing raised {ex.GetType().Name}"
    }

  let tests =
    testList
      "DeepValues"
      [ test "complete hashing finishes on a small stack" {
          let value = deepList ()
          match onSmallStack (fun () -> Dval.hash value |> ignore<int>) with
          | None -> ()
          | Some ex -> failtest $"iterative hashing raised {ex.GetType().Name}"
        }
        hashesMixedContainers
        refusesToOverflow "equals" (fun a b -> Dval.equals a b |> ignore<bool>)
        refusesToOverflow "compareForSort" (fun a b ->
          RT.DvalOrdering.compareForSort a b |> ignore<int>)
        refusesToOverflow "ValueType.merge" (fun a b ->
          match a, b with
          | RT.DList(ta, _), RT.DList(tb, _) ->
            VT.merge ta tb |> ignore<Result<RT.ValueType, unit>>
          | _ -> failtest "expected lists")
        substitutionRefusesToOverflow ]

/// Reload is name resolution, not code replacement (live programming's H2). The caches that key on
/// a HASH keep answering after an edit, and they are right to: a hash's body never changes. The
/// caches that key on a NAME are dropped (`LibDB.Caching.invalidateAll`), and the next lookup sees
/// the new binding. So a fn reference held as a value keeps running the version it was made from,
/// and a fresh lookup of the same name runs the new one.
module Reload =
  let private m = "InterpReload"
  let private loc (name : string) : PT.PackageLocation =
    { owner = "Darklang"; modules = [ m ]; name = name }

  let private applicableFor (hash : PT.Hash) =
    let (PT.Hash h) = hash
    RT.AppNamedFn
      { name = RT.FQFnName.fqPackage h
        typeSymbolTable = RT.TST.empty
        typeArgs = []
        approvalName = None
        access = None
        argsSoFar = []
        boundImpls = [] }

  let private call (state : RT.ExecutionState) (app : RT.Applicable) =
    task {
      match!
        LibExecution.Execution.executeApplicable
          state
          state.access
          app
          (NEList.singleton RT.DUnit)
        |> Ply.toTask
      with
      | Ok dv -> return dv
      | Error(rte, _) -> return failtest $"the call raised: {rte}"
    }

  let heldReferencesKeepTheirHash =
    testTask
      "a held fn reference keeps its version across an edit; a fresh lookup gets the new one" {
      // Fresh bodies per run: the log is content-addressed, so a body it already holds is a
      // re-binding of an old hash rather than the edit this test is about.
      let one = int64 (System.Random.Shared.Next(1_000, 1_000_000_000))
      let two = one + 1L
      let! v1 = authorIntoMain $"module Darklang.{m}\n\nlet answer(): Int64 = {one}"
      let h1 = hashBoundTo v1 "answer"
      let! state = executionStateFor pmPT false
      let held = applicableFor h1
      let! before = call state held
      Expect.equal before (RT.DInt64 one) "the first version answers"

      let! v2 = authorIntoMain $"module Darklang.{m}\n\nlet answer(): Int64 = {two}"
      let h2 = hashBoundTo v2 "answer"
      Expect.notEqual h1 h2 "the edit is a new hash"

      // The write path dropped the name caches; a fresh lookup sees the new binding...
      let! bound = pmPT.findFn (loc "answer") |> Ply.toTask
      Expect.equal bound (Some h2) "the name now resolves to the new hash"
      let! fresh = call state (applicableFor h2)
      Expect.equal fresh (RT.DInt64 two) "and runs the new body"

      // ...while the reference made before the edit, and the hash-keyed caches behind it, still
      // run the old body. Nothing overwrote it.
      // Checked BEFORE calling the held reference again. Calling it re-populates this entry,
      // so asserting it afterwards held whether or not the edit had evicted it; the property
      // is that the edit left it alone.
      let (PT.Hash h1s) = h1
      Expect.isTrue
        (state.packageFnCallCache.ContainsKey(RT.Hash h1s))
        "the hash-keyed call cache kept the old version's entry"

      let! after = call state held
      Expect.equal after (RT.DInt64 one) "the held reference still answers as before"
    }

  /// A lookup that started before an edit and finished after it must not leave what it read in
  /// the cache the edit cleared, or the old answer comes back until something else drops it.
  let lookupStillRunningAtAnEditIsNotKept =
    let check
      (name : string)
      (makeCache : (string -> Ply<Option<int>>) -> (string -> Ply<Option<int>>))
      (before : Option<int>)
      =
      testTask name {
        let gate = System.Threading.Tasks.TaskCompletionSource<unit>()
        let store = ref before
        let lookup =
          makeCache (fun _ ->
            uply {
              let seen = store.Value
              do! gate.Task // the query is still running...
              return seen
            })
        let running = lookup "k" |> Ply.toTask
        store.Value <- Some 2 // ...when an edit changes the store and drops the caches
        LibDB.Caching.invalidateAll ()
        gate.SetResult()
        let! _ = running
        let! after = lookup "k" |> Ply.toTask
        Expect.equal after (Some 2) "the next lookup sees the edit"
      }
    testList
      "a lookup still running when the caches are dropped does not keep what it read"
      [ check "a found answer" LibDB.Caching.withCache (Some 1)
        check "a remembered miss" LibDB.Caching.withNegativeCache None ]

  let tests =
    testList
      "Reload"
      [ heldReferencesKeepTheirHash; lookupStillRunningAtAnEditIsNotKept ]


/// A bounded fn remembers that its bounds held, so it does not prove them again on every call.
/// What it remembered has to be forgotten when the implementations change.
module BoundMemo =
  let private m = "InterpBoundMemo"

  /// One state for every call, over the store's own package manager: the memo lives there, and
  /// only it has a generation that moves when the store changes. A state per call, or one over
  /// `PT2RT.PackageManager.toRT`, would start empty each time and could not go stale.
  let private sharedState () : RT.ExecutionState =
    LibExecution.Execution.createState
      (localBuiltIns pmPT)
      pmRT
      LibExecution.Execution.noTracing
      (fun _ _ _ _ -> uply { return () })
      (fun _ _ _ _ -> uply { return () })
    |> LibExecution.Execution.setInstancePolicy
      LibExecution.Permissions.Policy.allowAll

  let private call (state : RT.ExecutionState) (hash : PT.Hash) =
    task {
      let (PT.Hash h) = hash
      let app =
        RT.AppNamedFn
          { name = RT.FQFnName.fqPackage h
            typeSymbolTable = RT.TST.empty
            typeArgs = []
            approvalName = None
            access = None
            argsSoFar = []
            boundImpls = [] }
      return!
        LibExecution.Execution.executeApplicable
          state
          state.access
          app
          (NEList.singleton RT.DUnit)
        |> Ply.toTask
    }

  let private entered (state : RT.ExecutionState) (hash : PT.Hash) (why : string) =
    task {
      match! call state hash with
      | Ok dv -> Expect.equal dv (RT.DString "entered") why
      | Error(rte, _) -> return failtest $"{why}: the call raised {rte}"
    }

  let private refusedForNoImpl
    (state : RT.ExecutionState)
    (hash : PT.Hash)
    (why : string)
    =
    task {
      match! call state hash with
      | Error(RT.RuntimeError.Trait(RT.RuntimeError.Traits.MissingImpl _), _) ->
        return ()
      | other -> return failtest $"{why}: expected MissingImpl, got %A{other}"
    }

  /// `authorIntoMain`, but leaving a name the parser cannot resolve yet for the refresh, as
  /// the authoring builtin does. `ThrowError` refuses an implementation's own methods.
  let private authorStep (label : string) (source : string) =
    task {
      let! parsed =
        LibParser.Package.parse
          (localBuiltIns pmPT)
          pmPT
          LibParser.NameResolver.OnMissing.Allow
          source
        |> Ply.toTask
      match parsed with
      | Error errs -> return failtest $"authoring {label} failed to parse: {errs}"
      | Ok ops ->
        let stabilized = LibDB.HashStabilization.computeRealHashes ops
        let! _ = LibDB.Inserts.insertAndApplyOps stabilized
        let! _ = LibDB.WipRefresh.refresh pmPT
        return stabilized
    }

  let boundCheckFollowsImplementations =
    testTask
      "a bounded fn's remembered bound follows implementations added and deprecated" {
      // Fresh field names per run: a type is content-addressed, so a shape left in the store by
      // an earlier run would already carry this run's implementations.
      let r = System.Random.Shared.Next(1_000, 1_000_000_000)
      let q = $"Darklang.{m}"
      // One item kind per authoring, each resolving against what the step before stored.
      let! _ =
        authorStep
          "types"
          $"module {q}\n\ntype Box = {{ boxWidth{r}: Int64 }}\n\ntype Crate = {{ crateDepth{r}: Int64 }}"
      let! _ =
        authorStep "trait" $"module {q}\n\ntrait Lbl<'a> =\n  let lbl(v: 'a): String"
      let! boxOps =
        authorStep
          "box impl"
          $"module {q}\n\nimpl {q}.Lbl for {q}.Box =\n  let lbl(b: {q}.Box): String = \"box\""
      let! _ =
        authorStep
          "fns"
          $"module {q}

let guarded<'a: {q}.Lbl>(v: 'a): String = \"entered\"

let callBox(): String = {q}.guarded({q}.Box {{ boxWidth{r} = 1 }})

let callCrate(): String = {q}.guarded({q}.Crate {{ crateDepth{r} = 1 }})"
      // Read back from the store: the refresh may have re-hashed what the parse returned.
      let current
        (find : PT.PackageLocation -> Ply<Option<PT.Hash>>)
        (l : PT.PackageLocation)
        =
        task {
          match! find l |> Ply.toTask with
          | Some h -> return h
          | None -> return failtest $"nothing bound at {l}"
        }
      let at (name : string) : PT.PackageLocation =
        { owner = "Darklang"; modules = [ m ]; name = name }
      let! callBox = current pmPT.findFn (at "callBox")
      let! callCrate = current pmPT.findFn (at "callCrate")
      let boxImplAt =
        boxOps
        |> List.pick (fun op ->
          match op with
          | PT.PackageOp.SetName(l, PT.Reference.PackageTraitImpl _, _) -> Some l
          | _ -> None)
      let! boxImpl = current pmPT.findTraitImpl boxImplAt

      // `guarded` never calls the trait's method, so only the check at entry can refuse it.
      let state = sharedState ()
      do! entered state callBox "the bound holds, and is remembered"
      do! entered state callBox "and holds again from what was remembered"
      do! refusedForNoImpl state callCrate "a type with no implementation is refused"

      let! _ =
        authorStep
          "crate impl"
          $"module {q}\n\nimpl {q}.Lbl for {q}.Crate =\n  let lbl(c: {q}.Crate): String = \"crate\""
      do! entered state callCrate "a new implementation is seen at once"

      let! _ =
        LibDB.Inserts.insertAndApplyOps
          [ PT.PackageOp.Deprecate(
              PT.Reference.PackageTraitImpl boxImpl,
              PT.DeprecationKind.Obsolete,
              "bound memo test",
              None
            ) ]
      do!
        refusedForNoImpl
          state
          callBox
          "a deprecated implementation no longer satisfies the remembered bound"
    }

  let tests = testList "BoundMemo" [ boundCheckFollowsImplementations ]


/// `List.filter` only calls a builtin, so the interpreter usually skips it and calls
/// the builtin directly. It doesn't skip it while collecting expression values. A bad
/// callback must give the same error either way.
module LambdaResultChecks =
  let private run (collectExprValues : bool) (code : string) =
    task {
      let! ptExpr = parsePTExpr code
      let! (state : RT.ExecutionState) = executionStateFor pmPT false
      let state =
        { state with
            tracing = { state.tracing with collectExprValues = collectExprValues } }
      return!
        LibExecution.Execution.executeExpr
          state
          (PT2RT.Expr.toRT Map.empty 0 None ptExpr)
    }

  let private failedParam (result : RT.ExecutionResult) =
    match result with
    | Error(RTE.Apply(RTE.Applications.LambdaResultNotExpectedType(fnName,
                                                                   index,
                                                                   name,
                                                                   _,
                                                                   _,
                                                                   _,
                                                                   _)),
            _) -> Some(fnName, index, name)
    | _ -> None

  let sameWhetherOrNotWrapperIsSkipped =
    testTask "List.filter checks its callback whether or not the wrapper is skipped" {
      let code = "Stdlib.List.filter([ 1, 2 ], fun x -> 2)"
      let! skipped = run false code
      let! viaFrame = run true code
      let a = failedParam skipped
      let b = failedParam viaFrame
      Expect.isSome a $"skipped: expected a callback error, got {skipped}"
      Expect.equal a b "the same error either way"
      Expect.equal
        (a |> Option.map (fun (_, i, n) -> (i, n)))
        (Some(1, "f"))
        "the param"
    }

  let tests = testList "LambdaResultChecks" [ sameWhetherOrNotWrapperIsSkipped ]


/// The text error paths fall back to when the Dark printer cannot run. Release
/// builds cannot run `%A`, so these must not lean on `string dv`.
module DebugText =
  let valuesKeepTheirContents =
    test "a value's fallback text keeps its contents" {
      let tn = RT.FQTypeName.fqPackage "00000000000000000000000000000000"
      let dv =
        RT.DTuple(
          RT.DInt64 5L,
          RT.DString "hi",
          [ RT.DList(RT.ValueType.Unknown, RT.Items([ RT.DBool true; RT.DUnit ]))
            RT.DEnum(tn, tn, [], "Some", [ RT.DFloat 1.5 ])
            RT.DRecord(tn, tn, [], RT.RecordFields.ofList [ "a", RT.DChar "x" ]) ]
        )
      Expect.equal
        (LibExecution.Dval.toDebugString dv)
        "(5, \"hi\", [true, ()], Some(1.5), { a = 'x' })"
        "contents"
      let rte = RT.RuntimeError.UncaughtException("boom", [ "k", RT.DInt64 1L ])
      let shown = LibExecution.Execution.runtimeErrorDebugString rte
      Expect.stringContains shown "UncaughtException" "the case"
      Expect.stringContains shown "boom" "the message"
      Expect.equal
        (LibExecution.Execution.metadataValueString (box (RT.DInt64 3L)))
        "3"
        "a value in metadata"
    }

  let tests = testList "DebugText" [ valuesKeepTheirContents ]


/// The integer operators (`LibExecution.NumOps`), at each fixed-width type's edges: the table
/// against a reference written in `BigInteger`, and then each way of running an operator against
/// the table. Those are `a op b` answered from the registers, the same with values being collected
/// (so not quickened), and the polymorphic builtin the stdlib's impls call. Each runs inside a
/// lambda, since that is where operator calls are quickened.
module IntOperators =
  module NumOps = LibExecution.NumOps
  module BigInts = RT.RuntimeError.BigInts

  type private IntType =
    { name : string
      signed : bool
      bits : int
      min : bigint
      max : bigint
      expr : bigint -> PT.Expr
      dval : bigint -> RT.Dval }

  let private parse128 (v : bigint) = System.Int128.Parse(string v)
  let private parseU128 (v : bigint) = System.UInt128.Parse(string v)

  let private types : List<IntType> =
    let signed name bits expr dval =
      { name = name
        signed = true
        bits = bits
        min = -(bigint.Pow(2I, bits - 1))
        max = bigint.Pow(2I, bits - 1) - 1I
        expr = expr
        dval = dval }
    let unsigned name bits expr dval =
      { name = name
        signed = false
        bits = bits
        min = 0I
        max = bigint.Pow(2I, bits) - 1I
        expr = expr
        dval = dval }
    [ signed "Int8" 8 (fun v -> eInt8 (int8 v)) (fun v -> RT.DInt8(int8 v))
      unsigned "UInt8" 8 (fun v -> euInt8 (uint8 v)) (fun v -> RT.DUInt8(uint8 v))
      signed "Int16" 16 (fun v -> eInt16 (int16 v)) (fun v -> RT.DInt16(int16 v))
      unsigned "UInt16" 16 (fun v -> euInt16 (uint16 v)) (fun v ->
        RT.DUInt16(uint16 v))
      signed "Int32" 32 (fun v -> eInt32 (int32 v)) (fun v -> RT.DInt32(int32 v))
      unsigned "UInt32" 32 (fun v -> euInt32 (uint32 v)) (fun v ->
        RT.DUInt32(uint32 v))
      signed "Int64" 64 (fun v -> eInt64 (int64 v)) (fun v -> RT.DInt64(int64 v))
      unsigned "UInt64" 64 (fun v -> euInt64 (uint64 v)) (fun v ->
        RT.DUInt64(uint64 v))
      signed "Int128" 128 (parse128 >> eInt128) (parse128 >> RT.DInt128)
      unsigned "UInt128" 128 (parseU128 >> euInt128) (parseU128 >> RT.DUInt128) ]

  /// The extremes, the small values, and the shift counts either side of the width.
  let private edges (t : IntType) : List<bigint> =
    let b = bigint t.bits
    let candidates =
      [ 0I; 1I; 2I; 3I; 7I; b - 1I; b; b + 1I; 2I * b; t.max - 1I; t.max ]
      @ (if t.signed then [ -1I; -2I; -3I; -7I; -b; t.min; t.min + 1I ] else [])
    candidates |> List.filter (fun v -> v >= t.min && v <= t.max) |> List.distinct

  /// `v` wrapped into the type, as two's complement does.
  let private wrap (t : IntType) (v : bigint) : bigint =
    let m = bigint.Pow(2I, t.bits)
    let r = ((v % m) + m) % m
    if t.signed && r > t.max then r - m else r

  /// What each operator means, written without `NumOps`: the value, or the error.
  let private reference
    (t : IntType)
    (op : int)
    (a : bigint)
    (b : bigint)
    : Result<RT.Dval, BigInts.Error> =
    let value v = Ok(t.dval (wrap t v))
    let truth v = Ok(RT.DBool v)
    // the count modulo the width, as the low bits of a two's complement count
    let count () = int (((b % bigint t.bits) + bigint t.bits) % bigint t.bits)
    if op = NumOps.add then
      value (a + b)
    elif op = NumOps.subtract then
      value (a - b)
    elif op = NumOps.multiply then
      value (a * b)
    elif op = NumOps.divide then
      if b = 0I then Error BigInts.DivideByZeroError else value (bigint.Divide(a, b))
    elif op = NumOps.modulo then
      if b = 0I then
        Error BigInts.ZeroModulus
      elif b < 0I then
        Error BigInts.NegativeModulus
      else
        let r = a % b
        value (if r < 0I then r + b else r)
    elif op = NumOps.power then
      if b < 0I then
        Error BigInts.NegativeExponent
      else
        value (bigint.ModPow(a, b, bigint.Pow(2I, t.bits)))
    elif op = NumOps.bitwiseAnd then
      value (a &&& b)
    elif op = NumOps.bitwiseOr then
      value (a ||| b)
    elif op = NumOps.bitwiseXor then
      value (a ^^^ b)
    elif op = NumOps.shiftLeft then
      value (a <<< count ())
    elif op = NumOps.shiftRight then
      value (a >>> count ())
    elif op = NumOps.lessThan then
      truth (a < b)
    elif op = NumOps.lessThanOrEqualTo then
      truth (a <= b)
    elif op = NumOps.greaterThan then
      truth (a > b)
    else
      truth (a >= b)

  /// The binary operators: the table's number, the infix operator, and the builtin.
  let ops : List<int * PT.InfixFnName * string> =
    [ NumOps.add, PT.ArithmeticPlus, "add"
      NumOps.subtract, PT.ArithmeticMinus, "subtract"
      NumOps.multiply, PT.ArithmeticMultiply, "multiply"
      NumOps.divide, PT.ArithmeticDivide, "divide"
      NumOps.modulo, PT.ArithmeticModulo, "modulo"
      NumOps.power, PT.ArithmeticPower, "power"
      NumOps.bitwiseAnd, PT.BitwiseAnd, "bitwiseAnd"
      NumOps.bitwiseOr, PT.BitwiseOr, "bitwiseOr"
      NumOps.bitwiseXor, PT.BitwiseXor, "bitwiseXor"
      NumOps.shiftLeft, PT.ShiftLeft, "shiftLeft"
      NumOps.shiftRight, PT.ShiftRight, "shiftRight"
      NumOps.lessThan, PT.ComparisonLessThan, "lessThan"
      NumOps.lessThanOrEqualTo, PT.ComparisonLessThanOrEqual, "lessThanOrEqualTo"
      NumOps.greaterThan, PT.ComparisonGreaterThan, "greaterThan"
      NumOps.greaterThanOrEqualTo,
      PT.ComparisonGreaterThanOrEqual,
      "greaterThanOrEqualTo" ]

  let private outcome (o : NumOps.Outcome) : Result<RT.Dval, BigInts.Error> =
    match o with
    | NumOps.Value v -> Ok v
    | NumOps.Raises(RT.RuntimeError.BigInt e) -> Error e
    | NumOps.Raises other -> failtestf "an integer operator raised %A" other
    | NumOps.Unsupported ->
      failtest "the table did not answer two integers of one type"

  let run (collectExprValues : bool) (expr : PT.Expr) =
    task {
      let! (state : RT.ExecutionState) = executionStateFor pmPT false
      let state =
        { state with
            tracing = { state.tracing with collectExprValues = collectExprValues } }
      return!
        LibExecution.Execution.executeExpr
          state
          (PT2RT.Expr.toRT Map.empty 0 None expr)
    }

  /// `let f = fun a b -> <body> in [ f a1 b1; f a2 b2; ... ]`.
  let applyEach
    (parameters : List<string>)
    (body : PT.Expr)
    (args : List<List<PT.Expr>>)
    : PT.Expr =
    eLet
      (lpVar "f")
      (eLambda (gid ()) (List.map lpVar parameters) body)
      (eList (args |> List.map (fun xs -> eApply (eVar "f") [] xs)))

  /// What a run gives, as the table says it: the values, or the integer error it raised.
  let private asOutcomes
    (what : string)
    (result : RT.ExecutionResult)
    : List<Result<RT.Dval, BigInts.Error>> =
    match result with
    | Ok(RT.DList(_, items)) -> RT.DvalList.toList items |> List.map Ok
    | Error(RT.RuntimeError.BigInt e, _) -> [ Error e ]
    | other -> failtestf "%s: got %A" what other

  let private tableMatchesReference (t : IntType) =
    test $"{t.name}: the table gives what the reference does" {
      let values = edges t
      for (op, infix, _) in ops do
        for a in values do
          for b in values do
            Expect.equal
              (outcome (NumOps.binary op (t.dval a) (t.dval b)))
              (reference t op a b)
              $"{t.name} {a} {infix} {b}"
      for a in values do
        Expect.equal
          (outcome (NumOps.unary NumOps.bitwiseNot (t.dval a)))
          (Ok(t.dval (wrap t (-a - 1I))))
          $"{t.name} ~{a}"
        if t.signed then
          Expect.equal
            (outcome (NumOps.unary NumOps.negate (t.dval a)))
            (Ok(t.dval (wrap t (-a))))
            $"{t.name} -{a}"
        else
          Expect.equal
            (NumOps.unary NumOps.negate (t.dval a))
            NumOps.Unsupported
            $"{t.name} has no -{a}"
    }

  /// Runs <param body> over <param args> three ways, and expects <param expected> of each:
  /// quickened, collecting values (so not quickened), and <param viaBuiltin> where given.
  let private threeWays
    (name : string)
    (parameters : List<string>)
    (body : PT.Expr)
    (viaBuiltin : Option<PT.Expr>)
    (args : List<List<PT.Expr>>)
    (expected : List<Result<RT.Dval, BigInts.Error>>)
    =
    task {
      let! quick = run false (applyEach parameters body args)
      Expect.equal (asOutcomes name quick) expected $"{name}, quickened"
      let! traced = run true (applyEach parameters body args)
      Expect.equal (asOutcomes name traced) expected $"{name}, collecting values"
      match viaBuiltin with
      | Some builtinBody ->
        let! direct = run false (applyEach parameters builtinBody args)
        Expect.equal (asOutcomes name direct) expected $"{name}, through the builtin"
      | None -> ()
    }

  /// The pairs the table answers, all in one run; then the ones it raises on, one run each with a
  /// few left operands, since the first error ends a run.
  let private everyPathMatchesTheTable (t : IntType) =
    testTask $"{t.name}: every way of running an operator gives what the table does" {
      let values = edges t
      for (op, infix, builtin) in ops do
        let infixBody = eInfix (PT.Infix.InfixFnCall infix) (eVar "a") (eVar "b")
        let builtinBody = eApply (eBuiltinFn builtin 0) [] [ eVar "a"; eVar "b" ]
        let answer a b = outcome (NumOps.binary op (t.dval a) (t.dval b))
        let answered =
          [ for a in values do
              for b in values do
                match answer a b with
                | Ok _ -> (a, b)
                | Error _ -> () ]
        do!
          threeWays
            $"{t.name} {infix}"
            [ "a"; "b" ]
            infixBody
            (Some builtinBody)
            (answered |> List.map (fun (a, b) -> [ t.expr a; t.expr b ]))
            (answered |> List.map (fun (a, b) -> answer a b))
        for b in values do
          for a in [ t.min; 0I; 1I; t.max ] do
            match answer a b with
            | Error _ as raised ->
              do!
                threeWays
                  $"{t.name} {a} {infix} {b}"
                  [ "a"; "b" ]
                  infixBody
                  (Some builtinBody)
                  [ [ t.expr a; t.expr b ] ]
                  [ raised ]
            | Ok _ -> ()
      // The builtins the `Negate` and `BitwiseNot` implementations call.
      for (op, builtin) in
        [ NumOps.bitwiseNot, "bitwiseNot" ]
        @ (if t.signed then [ NumOps.negate, "negate" ] else []) do
        do!
          threeWays
            $"{t.name} {builtin}"
            [ "a" ]
            (eApply (eBuiltinFn builtin 0) [] [ eVar "a" ])
            None
            (values |> List.map (fun a -> [ t.expr a ]))
            (values |> List.map (fun a -> outcome (NumOps.unary op (t.dval a))))
    }

  /// A mixed pair declines, and reports what it reports with nothing quickened.
  let mixedPairs =
    testTask "a mixed pair raises what it raises unquickened" {
      for op in
        [ PT.ArithmeticDivide; PT.ArithmeticPower; PT.BitwiseAnd; PT.ShiftLeft ] do
        let expr =
          applyEach
            [ "a"; "b" ]
            (eInfix (PT.Infix.InfixFnCall op) (eVar "a") (eVar "b"))
            [ [ eInt64 1L; eInt8 2y ] ]
        let! quick = run false expr
        let! traced = run true expr
        match quick, traced with
        | Error(e1, _), Error(e2, _) -> Expect.equal e1 e2 $"{op}"
        | _ -> failtestf "%A: expected two errors, got %A and %A" op quick traced
    }

  let tests =
    testList
      "IntOperators"
      ([ for t in types do
           tableMatchesReference t
           everyPathMatchesTheTable t ]
       @ [ mixedPairs ])



/// The other numeric operators (`LibExecution.NumOps`): `BigInt`, `Float`, `Float32` and
/// `Decimal`, as `IntOperators` does the integers. The table against a reference at each type's
/// edges, and then each way of running an operator against the table.
module NumOperators =
  module NumOps = LibExecution.NumOps
  module BigInts = RT.RuntimeError.BigInts

  /// A value or an error as text, so two answers compare exactly: floats by their bits, so
  /// `-0.0` is not `0.0` (every NaN is one NaN), and decimals by theirs, so the scale counts.
  let private show (dv : RT.Dval) : string =
    match dv with
    | RT.DFloat f when System.Double.IsNaN f -> "Float NaN"
    | RT.DFloat f -> $"Float {System.BitConverter.DoubleToInt64Bits f}"
    | RT.DFloat32 f when System.Single.IsNaN f -> "Float32 NaN"
    | RT.DFloat32 f -> $"Float32 {System.BitConverter.SingleToInt32Bits f}"
    | RT.DDecimal d -> $"Decimal %A{System.Decimal.GetBits d}"
    | RT.DBigInt b -> $"BigInt {RT.DarkBigInt.toBigInt b}"
    | RT.DBool b -> $"Bool {b}"
    | other -> failtestf "not a number: %A" other

  let private raises (e : RT.RuntimeError.Error) : string = $"raises %A{e}"
  let private noSuchOperator = "no such operator"

  let private answer (o : NumOps.Outcome) : string =
    match o with
    | NumOps.Value v -> show v
    | NumOps.Raises e -> raises e
    | NumOps.Unsupported -> noSuchOperator

  type private NumType =
    {
      name : string
      values : List<RT.Dval>
      /// The right operands of `**`, `<<` and `>>`, where they differ from <field values>.
      counts : List<RT.Dval>
      expr : RT.Dval -> PT.Expr
      reference : int -> RT.Dval -> RT.Dval -> string
      negate : RT.Dval -> string
      bitwiseNot : RT.Dval -> string
    }

  // `BigInt`, in `BigInteger`

  let private big (dv : RT.Dval) : bigint =
    match dv with
    | RT.DBigInt b -> RT.DarkBigInt.toBigInt b
    | other -> failtestf "not a BigInt: %A" other

  let private bigReference (op : int) (a : RT.Dval) (b : RT.Dval) : string =
    let a = big a
    let b = big b
    let value (v : bigint) = $"BigInt {v}"
    let truth (t : bool) = $"Bool {t}"
    let fails e = raises (RT.RuntimeError.BigInt e)
    let int32Max = bigint System.Int32.MaxValue
    if op = NumOps.add then
      value (a + b)
    elif op = NumOps.subtract then
      value (a - b)
    elif op = NumOps.multiply then
      value (a * b)
    elif op = NumOps.divide then
      if b = 0I then fails BigInts.DivideByZeroError else value (bigint.Divide(a, b))
    elif op = NumOps.modulo then
      if b = 0I then
        fails BigInts.ZeroModulus
      elif b < 0I then
        fails BigInts.NegativeModulus
      else
        let r = bigint.Remainder(a, b)
        value (if r < 0I then r + b else r)
    elif op = NumOps.power then
      if b < 0I then fails BigInts.NegativeExponent
      elif b <= int32Max then value (bigint.Pow(a, int b))
      elif a = 0I || a = 1I then value a
      elif a = -1I then value (if b.IsEven then 1I else -1I)
      else fails BigInts.OutOfRange
    elif op = NumOps.bitwiseAnd then
      value (a &&& b)
    elif op = NumOps.bitwiseOr then
      value (a ||| b)
    elif op = NumOps.bitwiseXor then
      value (a ^^^ b)
    elif op = NumOps.shiftLeft then
      if b < 0I || b > int32Max then
        fails BigInts.OutOfRange
      else
        value (a * bigint.Pow(2I, int b))
    elif op = NumOps.shiftRight then
      if b < 0I || b > int32Max then
        fails BigInts.OutOfRange
      else
        // flooring, as an arithmetic shift does
        let d = bigint.Pow(2I, int b)
        let q = bigint.Divide(a, d)
        value (if a < 0I && q * d <> a then q - 1I else q)
    elif op = NumOps.lessThan then
      truth (a < b)
    elif op = NumOps.lessThanOrEqualTo then
      truth (a <= b)
    elif op = NumOps.greaterThan then
      truth (a > b)
    else
      truth (a >= b)

  let private bigInts : NumType =
    let int64Max = bigint System.Int64.MaxValue
    let int64Min = bigint System.Int64.MinValue
    let d (v : bigint) = RT.Dval.bigInt v
    { name = "BigInt"
      values =
        [ 0I
          1I
          -1I
          2I
          -2I
          7I
          -7I
          13I
          int64Max
          int64Max + 1I
          int64Min
          int64Min - 1I
          bigint.Pow(2I, 64)
          (1I - bigint.Pow(2I, 100)) ]
        |> List.map d
      // Large enough to leave the int64 range, small enough to compute; and the counts past an
      // `Int32`, which raise.
      counts =
        [ -2I
          -1I
          0I
          1I
          2I
          7I
          63I
          64I
          65I
          bigint System.Int32.MaxValue + 1I
          int64Max ]
        |> List.map d
      expr = fun v -> PT.EBigInt(gid (), big v)
      reference = bigReference
      negate = fun a -> $"BigInt {-(big a)}"
      bitwiseNot = fun a -> $"BigInt {-(big a) - 1I}" }

  // `Float` and `Float32`, in the hardware's IEEE arithmetic

  /// A finite float's exact decimal digits, as a literal's whole part and fraction.
  let private exactDigits (v : float) : string * string =
    let bits = System.BitConverter.DoubleToInt64Bits(abs v)
    let rawExp = int ((bits >>> 52) &&& 0x7FFL)
    let fraction = bits &&& 0xFFFFFFFFFFFFFL
    let (m, e) =
      if rawExp = 0 then
        (fraction, -1074)
      else
        (fraction ||| (1L <<< 52), rawExp - 1075)
    if e >= 0 then
      (string (bigint m * bigint.Pow(2I, e)), "0")
    else
      // v * 10^-e is a whole number
      let digits = (string (bigint m * bigint.Pow(5I, -e))).PadLeft(-e + 1, '0')
      let split = digits.Length + e
      let fractionDigits = digits.Substring(split).TrimEnd('0')
      (digits.Substring(0, split),
       (if fractionDigits = "" then "0" else fractionDigits))

  let private signOf (v : float) : Sign =
    if v < 0.0 || (v = 0.0 && System.Double.IsNegative v) then
      Negative
    else
      Positive

  /// A literal for a finite value; for an infinity or NaN, the division that makes it.
  let private floatLiteral
    (literal : Sign -> string -> string -> PT.Expr)
    (v : float)
    : PT.Expr =
    let finite (v : float) =
      let (whole, fraction) = exactDigits v
      literal (signOf v) whole fraction
    if System.Double.IsNaN v then
      eInfix (PT.Infix.InfixFnCall PT.ArithmeticDivide) (finite 0.0) (finite 0.0)
    elif System.Double.IsInfinity v then
      eInfix
        (PT.Infix.InfixFnCall PT.ArithmeticDivide)
        (finite (if v > 0.0 then 1.0 else -1.0))
        (finite 0.0)
    else
      finite v

  /// The operator on two floats in double precision, rounded by <param back>.
  let private ieeeReference
    (get : RT.Dval -> float)
    (back : float -> RT.Dval)
    (op : int)
    (a : RT.Dval)
    (b : RT.Dval)
    : string =
    let x = get a
    let y = get b
    let truth (t : bool) = $"Bool {t}"
    if op = NumOps.add then show (back (x + y))
    elif op = NumOps.subtract then show (back (x - y))
    elif op = NumOps.multiply then show (back (x * y))
    elif op = NumOps.divide then show (back (x / y))
    elif op = NumOps.power then show (back (System.Math.Pow(x, y)))
    // IEEE: a NaN compares false
    elif op = NumOps.lessThan then truth (x < y)
    elif op = NumOps.lessThanOrEqualTo then truth (x <= y)
    elif op = NumOps.greaterThan then truth (x > y)
    elif op = NumOps.greaterThanOrEqualTo then truth (x >= y)
    else noSuchOperator

  let private floats : NumType =
    let get (dv : RT.Dval) =
      match dv with
      | RT.DFloat f -> f
      | other -> failtestf "not a Float: %A" other
    { name = "Float"
      values =
        [ 0.0
          -0.0
          1.0
          -1.0
          0.5
          3.0
          -7.25
          9007199254740993.0
          System.Double.MaxValue
          System.Double.MinValue
          System.Double.Epsilon
          System.Double.PositiveInfinity
          System.Double.NegativeInfinity
          System.Double.NaN ]
        |> List.map RT.DFloat
      counts = []
      expr = fun v -> floatLiteral eFloat (get v)
      reference = ieeeReference get RT.DFloat
      negate = fun a -> show (RT.DFloat(-(get a)))
      bitwiseNot = fun _ -> noSuchOperator }

  let private float32s : NumType =
    let get (dv : RT.Dval) =
      match dv with
      | RT.DFloat32 f -> float f
      | other -> failtestf "not a Float32: %A" other
    { name = "Float32"
      values =
        [ 0.0f
          -0.0f
          1.0f
          -1.0f
          0.5f
          3.0f
          -7.25f
          16777217.0f
          System.Single.MaxValue
          System.Single.MinValue
          System.Single.Epsilon
          System.Single.PositiveInfinity
          System.Single.NegativeInfinity
          System.Single.NaN ]
        |> List.map RT.DFloat32
      counts = []
      expr =
        fun v -> floatLiteral (fun s w f -> PT.EFloat32(gid (), s, w, f)) (get v)
      // In double precision and rounded once, which for `+ - * /` is the single-precision answer
      reference = ieeeReference get (float32 >> RT.DFloat32)
      negate = fun a -> show (RT.DFloat32(float32 (-(get a))))
      bitwiseNot = fun _ -> noSuchOperator }

  // `Decimal`, through `System.Decimal`'s named methods

  let private decimalReference (op : int) (a : RT.Dval) (b : RT.Dval) : string =
    let (x, y) =
      match a, b with
      | RT.DDecimal x, RT.DDecimal y -> (x, y)
      | _ -> failtestf "not two Decimals: %A %A" a b
    let checkedly (f : unit -> decimal) =
      try
        show (RT.DDecimal(f ()))
      with :? System.OverflowException ->
        raises RT.RuntimeError.DecimalOutOfRange
    let fails e = raises (RT.RuntimeError.BigInt e)
    let truth (t : bool) = $"Bool {t}"
    if op = NumOps.add then
      checkedly (fun () -> System.Decimal.Add(x, y))
    elif op = NumOps.subtract then
      checkedly (fun () -> System.Decimal.Subtract(x, y))
    elif op = NumOps.multiply then
      checkedly (fun () -> System.Decimal.Multiply(x, y))
    elif op = NumOps.divide then
      if y = 0m then
        fails BigInts.DivideByZeroError
      else
        checkedly (fun () -> System.Decimal.Divide(x, y))
    elif op = NumOps.modulo then
      if y = 0m then
        fails BigInts.ZeroModulus
      elif y < 0m then
        fails BigInts.NegativeModulus
      else
        let r = System.Decimal.Remainder(x, y)
        show (RT.DDecimal(if r < 0m then System.Decimal.Add(r, y) else r))
    elif op = NumOps.lessThan then
      truth (System.Decimal.Compare(x, y) < 0)
    elif op = NumOps.lessThanOrEqualTo then
      truth (System.Decimal.Compare(x, y) <= 0)
    elif op = NumOps.greaterThan then
      truth (System.Decimal.Compare(x, y) > 0)
    elif op = NumOps.greaterThanOrEqualTo then
      truth (System.Decimal.Compare(x, y) >= 0)
    else
      noSuchOperator

  let private decimals : NumType =
    let get (dv : RT.Dval) =
      match dv with
      | RT.DDecimal d -> d
      | other -> failtestf "not a Decimal: %A" other
    { name = "Decimal"
      values =
        [ 0m
          1m
          -1m
          1.00m
          0.5m
          -0.5m
          3m
          7m
          -2.25m
          0.0000000000000000000000000001m
          System.Decimal.MaxValue
          System.Decimal.MinValue ]
        |> List.map RT.DDecimal
      counts = []
      expr = fun v -> PT.EDecimal(gid (), get v)
      reference = decimalReference
      negate = fun a -> show (RT.DDecimal(System.Decimal.Negate(get a)))
      bitwiseNot = fun _ -> noSuchOperator }

  let private types = [ bigInts; floats; float32s; decimals ]

  /// The right operands for <param op>.
  let private rightsFor (t : NumType) (op : int) : List<RT.Dval> =
    if
      (op = NumOps.power || op = NumOps.shiftLeft || op = NumOps.shiftRight)
      && not (List.isEmpty t.counts)
    then
      t.counts
    else
      t.values

  let private tableMatchesReference (t : NumType) =
    test $"{t.name}: the table gives what the reference does" {
      for (op, infix, _) in IntOperators.ops do
        for a in t.values do
          for b in rightsFor t op do
            Expect.equal
              (answer (NumOps.binary op a b))
              (t.reference op a b)
              $"{t.name} {show a} {infix} {show b}"
      for a in t.values do
        Expect.equal
          (answer (NumOps.unary NumOps.negate a))
          (t.negate a)
          $"{t.name} -{show a}"
        Expect.equal
          (answer (NumOps.unary NumOps.bitwiseNot a))
          (t.bitwiseNot a)
          $"{t.name} ~{show a}"
    }

  /// What a run gives: the values, or the error it raised.
  let private asAnswers
    (what : string)
    (result : RT.ExecutionResult)
    : List<string> =
    match result with
    | Ok(RT.DList(_, items)) -> RT.DvalList.toList items |> List.map show
    | Error(e, _) -> [ raises e ]
    | other -> failtestf "%s: got %A" what other

  /// Runs <param body> over <param args> quickened, collecting values (so not quickened), and
  /// <param viaBuiltin> where given, and expects <param expected> of each.
  let private threeWays
    (name : string)
    (parameters : List<string>)
    (body : PT.Expr)
    (viaBuiltin : Option<PT.Expr>)
    (args : List<List<PT.Expr>>)
    (expected : List<string>)
    =
    task {
      let! quick =
        IntOperators.run false (IntOperators.applyEach parameters body args)
      Expect.equal (asAnswers name quick) expected $"{name}, quickened"
      let! traced =
        IntOperators.run true (IntOperators.applyEach parameters body args)
      Expect.equal (asAnswers name traced) expected $"{name}, collecting values"
      match viaBuiltin with
      | Some builtinBody ->
        let! direct =
          IntOperators.run false (IntOperators.applyEach parameters builtinBody args)
        Expect.equal (asAnswers name direct) expected $"{name}, through the builtin"
      | None -> ()
    }

  /// The pairs the table answers, all in one run; then the ones it raises on, one run each.
  let private everyPathMatchesTheTable (t : NumType) =
    testTask $"{t.name}: every way of running an operator gives what the table does" {
      for (op, infix, builtin) in IntOperators.ops do
        let infixBody = eInfix (PT.Infix.InfixFnCall infix) (eVar "a") (eVar "b")
        let builtinBody = eApply (eBuiltinFn builtin 0) [] [ eVar "a"; eVar "b" ]
        let outcomes =
          [ for a in t.values do
              for b in rightsFor t op do
                (a, b, NumOps.binary op a b) ]
        let answered =
          outcomes
          |> List.choose (fun (a, b, o) ->
            match o with
            | NumOps.Value _ -> Some(a, b, answer o)
            | _ -> None)
        if not (List.isEmpty answered) then
          do!
            threeWays
              $"{t.name} {infix}"
              [ "a"; "b" ]
              infixBody
              (Some builtinBody)
              (answered |> List.map (fun (a, b, _) -> [ t.expr a; t.expr b ]))
              (answered |> List.map (fun (_, _, expected) -> expected))
        // One run per right operand and error: the first error ends a run.
        let raising =
          outcomes
          |> List.filter (fun (_, _, o) ->
            match o with
            | NumOps.Raises _ -> true
            | _ -> false)
          |> List.distinctBy (fun (_, b, o) -> (show b, answer o))
        for (a, b, o) in raising do
          do!
            threeWays
              $"{t.name} {show a} {infix} {show b}"
              [ "a"; "b" ]
              infixBody
              (Some builtinBody)
              [ [ t.expr a; t.expr b ] ]
              [ answer o ]
      // `-a` and `~a` are stored as `Builtin.negate a` and `Builtin.bitwiseNot a`.
      for (op, builtin) in
        [ NumOps.negate, "negate"; NumOps.bitwiseNot, "bitwiseNot" ] do
        let answered =
          t.values
          |> List.choose (fun a ->
            match NumOps.unary op a with
            | NumOps.Value _ as o -> Some(a, answer o)
            | _ -> None)
        if not (List.isEmpty answered) then
          do!
            threeWays
              $"{t.name} {builtin}"
              [ "a" ]
              (eApply (eBuiltinFn builtin 0) [] [ eVar "a" ])
              None
              (answered |> List.map (fun (a, _) -> [ t.expr a ]))
              (answered |> List.map snd)
    }

  let tests =
    testList
      "NumOperators"
      [ for t in types do
          tableMatchesReference t
          everyPathMatchesTheTable t ]

let tests =
  testList
    "Interpreter"
    [ Tests.PermissionsGate.tests
      Values.tests
      Lambdas.tests
      Fns.tests
      SyncUnify.tests
      AliasChecks.tests
      DictKeyOrdering.tests
      ListUnique.tests
      DictShapes.tests
      RecordFieldsShape.tests
      DeepValues.tests
      Reload.tests
      BoundMemo.tests
      LambdaResultChecks.tests
      DebugText.tests
      IntOperators.tests
      NumOperators.tests ]
