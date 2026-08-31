module Tests.Interpreter

open Expecto
open Prelude
open TestUtils.TestUtils
open TestUtils.PTShortcuts

module RT = LibExecution.RuntimeTypes
module Cap = LibExecution.Capabilities
module VT = LibExecution.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RTE = RT.RuntimeError
module Dval = LibExecution.Dval

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

    let! exeState = executionStateFor TestValues.pm false Map.empty

    let! actual = LibExecution.Interpreter.execute exeState vmState |> Ply.toTask
    Expect.equal actual expectedInsts ""

    extraAssertions exeState vmState
  }

let t name ptExpr expectedInsts = tCheckVM name ptExpr expectedInsts (fun _ _ -> ())



let tFail name ptExpr expectedRte =
  testTask name {
    let instructions = ptExpr |> PT2RT.Expr.toRT Map.empty 0 None

    let! exeState = executionStateFor TestValues.pm false Map.empty

    let! actual = LibExecution.Execution.executeExpr exeState instructions

    match actual with
    | Ok _ -> return Expect.equal 1 2 "Expected an RTE, but got a successful result"
    | Error(actualRte, _) -> return Expect.equal actualRte expectedRte ""
  }


module Basic =
  // CLEANUP back fill with more simple stuff

  let one = t "1" E.Basic.one (RT.DInt64 1L)

  let tests = testList "Basic" [ one ]


module List =
  let simple =
    t
      "[true, false, true]"
      E.List.simple
      (RT.DList(VT.bool, [ RT.DBool true; RT.DBool false; RT.DBool true ]))

  let nested =
    t
      "[[true, false], [false, true]]"
      E.List.nested
      (RT.DList(
        VT.list VT.bool,
        [ RT.DList(VT.bool, [ RT.DBool true; RT.DBool false ])
          RT.DList(VT.bool, [ RT.DBool false; RT.DBool true ]) ]
      ))

  let mixed =
    tFail
      "[1, true]"
      E.List.mixed
      (RTE.Lists.TriedToAddMismatchedData(1, VT.int64, VT.bool, RT.DBool true)
       |> RTE.List)

  let tests = testList "Lists" [ simple; nested; mixed ]


module Let =
  let simple = t "let x = true\nx" E.Let.simple (RT.DBool true)

  let tuple = t "let (x, y) = (1, 2)\nx" E.Let.tuple (RT.DInt64 1L)

  let tupleNotTuple =
    tFail
      "let (a, b) = 1 in a"
      E.Let.tupleNotTuple
      (RTE.Error.Let(
        RTE.Lets.Error.PatternDoesNotMatch(
          RT.DInt64 1,
          RT.LPTuple(RT.LPVariable 1, RT.LPVariable 2, [])
        )
      ))

  let tupleIncorrectLen =
    tFail
      "let (a, b) = (1, 2, 3) in a"
      E.Let.tupleIncorrectLen
      (RTE.Error.Let(
        RTE.Lets.Error.PatternDoesNotMatch(
          RT.DTuple(RT.DInt64 1, RT.DInt64 2, [ RT.DInt64 3 ]),
          RT.LPTuple(RT.LPVariable 4, RT.LPVariable 5, [])
        )
      ))

  let tupleNested =
    t "let (a, (b, c)) = (1, (2, 3))\nb" E.Let.tupleNested (RT.DInt64 2L)

  let undefinedVar = tFail "a" E.Let.undefinedVar (RTE.VariableNotFound "a")

  let tests =
    testList
      "Let"
      [ simple; tuple; tupleNotTuple; tupleIncorrectLen; tupleNested; undefinedVar ]


module String =
  let simple = t "[\"hello\"]" E.String.simple (RT.DString "hello")

  let withInterpolation =
    t
      "[let x = \"world\" in $\"hello {x}\"]"
      E.String.withInterpolation
      (RT.DString "hello, world")

  let tests = testList "Strings" [ simple; withInterpolation ]


module Dict =
  let empty = t "Dict {}" E.Dict.empty (RT.DDict(VT.unknown, VT.unknown, Map.empty))

  let simple =
    t
      "Dict { \"key\": true }"
      E.Dict.simple
      (Dval.stringDict RT.KTBool [ "key", RT.DBool true ])

  let multEntries =
    t
      "Dict { \"t\": true; \"f\": false }"
      E.Dict.multEntries
      (Dval.stringDict RT.KTBool [ "t", RT.DBool true; "f", RT.DBool false ])

  let dupeKey =
    tFail
      "Dict { \"t\": true; \"f\": false; \"t\": false }"
      E.Dict.dupeKey
      (RTE.Dict(RTE.Dicts.TriedToAddKeyAfterAlreadyPresent(RT.DString "t")))

  let tests = testList "Dict" [ empty; simple; multEntries; dupeKey ]


module If =
  let gotoThenBranch = t "if true then 1 else 2" E.If.gotoThenBranch (RT.DInt64 1L)
  let gotoElseBranch = t "if false then 1 else 2" E.If.gotoElseBranch (RT.DInt64 2L)
  let elseMissing = t "if false then 1" E.If.elseMissing RT.DUnit

  let tests = testList "If" [ gotoThenBranch; gotoElseBranch; elseMissing ]


module Tuples =
  let two =
    t "(false, true)" E.Tuples.two (RT.DTuple(RT.DBool false, RT.DBool true, []))

  let three =
    t
      "(false, true, false)"
      E.Tuples.three
      (RT.DTuple(RT.DBool false, RT.DBool true, [ RT.DBool false ]))

  let nested =
    t
      "((false, true), true, (true, false)))"
      E.Tuples.nested
      (RT.DTuple(
        RT.DTuple(RT.DBool false, RT.DBool true, []),
        RT.DBool true,
        [ RT.DTuple(RT.DBool true, RT.DBool false, []) ]
      ))

  let tests = testList "Tuples" [ two; three; nested ]


module Match =
  let simple =
    t
      "match true with\n| false -> \"first branch\"\n| true -> \"second branch\""
      E.Match.simple
      (RT.DString "second branch")

  let notMatched =
    tFail
      "match true with\n| false -> \"first branch\""
      E.Match.notMatched
      (RTE.Match(RTE.Matches.MatchUnmatched(RT.DBool true)))

  let withVar = t "match true with\n| x -> x" E.Match.withVar (RT.DBool true)

  let withVarAndWhenCondition =
    t
      "match 4 with\n| 1 -> \"first branch\"\n| x when x % 2 == 0 -> \"second branch\""
      E.Match.withVarAndWhenCondition
      (RT.DString "second branch")

  let list =
    t
      "match [1, 2] with\n| [1, 2] -> \"first branch\""
      E.Match.list
      (RT.DString "first branch")

  let listCons =
    t
      "match [1, 2] with\n| 1 :: tail -> tail"
      E.Match.listCons
      (RT.DList(VT.int64, [ RT.DInt64 2L ]))

  let tuple =
    t
      "match (1, 2) with\n| (1, 2) -> \"first branch\""
      E.Match.tuple
      (RT.DString "first branch")

  let combinedPatternsFirstPatMatches =
    t
      "match (1, 2) with\n| (1, 2) | (2, 1) -> \"first branch\"\n| _ -> \"second branch\""
      E.Match.combinedPatternsFirstPatMatches
      (RT.DString "first branch")

  let combinedPatternsSecondPatMatches =
    t
      "match (2, 1) with\n| (1, 2) | (2, 1) -> \"first branch\"\n| _ -> \"second branch\""
      E.Match.combinedPatternsSecondPatMatches
      (RT.DString "first branch")

  let combinedPatternsWithWhenCond =
    t
      "match (2, 1) with\n| (1, 2) | (2, 1) when false -> \"first branch\"\n| _ -> \"second branch\""
      E.Match.combinedPatternsWithWhenCond
      (RT.DString "second branch")

  let combinedPatternsWithVarAndWhenCond =
    t
      "match (1L,2L) with\n| (x,2L) | (2L,x) when x == 1L -> \"first branch\"\n _ -> \"second branch\""
      E.Match.combinedPatternsWithVarAndWhenCond
      (RT.DString "first branch")



  let tests =
    testList
      "Match"
      [ simple
        notMatched
        withVar
        withVarAndWhenCondition
        list
        listCons
        tuple
        combinedPatternsFirstPatMatches
        combinedPatternsSecondPatMatches
        combinedPatternsWithWhenCond
        combinedPatternsWithVarAndWhenCond ]

module Pipes =
  let lambda = t "1 |> fun x -> x" E.Pipes.lambda (RT.DInt64 1L)
  let infix = t "1 |> (+) 2" E.Pipes.infix (RT.DInt64 3L)
  let fnCall = t "1 |> Builtin.int64Add 2" E.Pipes.fnCall (RT.DInt64 3L)
  let variable =
    t "let myLambda = fun x -> x + 1\n1 |> myLambda" E.Pipes.variable (RT.DInt64 2L)
  let multiple =
    t
      "let incr = fun x -> x + 1\n2 |> incr |> fun x -> x * 2 |> Builtin.int64Add 3 |> (+) 4"
      E.Pipes.multiple
      (RT.DInt64 13L)
  let tests = testList "Pipes" [ lambda; infix; fnCall; variable; multiple ]

module Records =
  let simple =
    let typeName = RT.FQTypeName.fqPackage PM.Types.Records.singleField
    t
      "Test.Test { key = true }"
      E.Records.simple
      (RT.DRecord(typeName, typeName, [], Map [ "key", RT.DBool true ]))

  let nested =
    let outerTypeName = RT.FQTypeName.fqPackage PM.Types.Records.nested
    let innerTypeName = RT.FQTypeName.fqPackage PM.Types.Records.singleField
    t
      "Test.Test2 { outer = (Test.Test { key = true }) }"
      E.Records.nested
      (RT.DRecord(
        outerTypeName,
        outerTypeName,
        [],
        Map
          [ "outer",
            RT.DRecord(
              innerTypeName,
              innerTypeName,
              [],
              Map [ "key", RT.DBool true ]
            ) ]
      ))


  let tests = testList "Records" [ simple; nested ]


module RecordFieldAccess =
  let simple =
    t "(Test.Test { key = true }).key" E.RecordFieldAccess.simple (RT.DBool true)
  let notRecord =
    tFail
      "1.key"
      E.RecordFieldAccess.notRecord
      (RTE.Record(RTE.Records.FieldAccessNotRecord VT.int64))

  let missingField =
    tFail
      "(Test.Test { key = true }).missing"
      E.RecordFieldAccess.missingField
      (RTE.Record(RTE.Records.FieldAccessFieldNotFound "missing"))

  let nested =
    t
      "(Test.Test2 { outer = (Test.Test { key = true }) }).outer.key"
      E.RecordFieldAccess.nested
      (RT.DBool true)

  let tests =
    testList "RecordFieldAccess" [ simple; notRecord; missingField; nested ]

module RecordUpdate =
  let simple =
    t
      "let r = Test.Test { key = true }\nlet r2 = { r | key = false }"
      E.RecordUpdate.simple
      (RT.DRecord(
        RT.FQTypeName.fqPackage PM.Types.Records.singleField,
        RT.FQTypeName.fqPackage PM.Types.Records.singleField,
        [],
        Map [ "key", RT.DBool false ]
      ))

  let notRecord =
    tFail
      "let r = 1\nlet r2 = { r | key = false }"
      E.RecordUpdate.notRecord
      (RTE.Record(RTE.Records.UpdateNotRecord VT.int64))

  let fieldThatShouldNotExist =
    tFail
      "let r = Test.Test { key = true }\nlet r2 = { r | bonus = false }\nr2.key"
      E.RecordUpdate.fieldThatShouldNotExist
      (RTE.Record(RTE.Records.UpdateFieldNotExpected "bonus"))

  let fieldWithWrongType =
    tFail
      "let r = Test.Test { key = true }\nlet r2 = { r | key = 1 }\nr2.key"
      E.RecordUpdate.fieldWithWrongType
      (RTE.Record(
        RTE.Records.UpdateFieldOfWrongType(
          "key",
          Some RT.TBool,
          VT.bool,
          VT.int64,
          RT.DInt64 1L
        )
      ))

  let tests =
    testList
      "RecordUpdate"
      [ simple; notRecord; fieldThatShouldNotExist; fieldWithWrongType ]

// TODO: add more tests
module Enum =
  let simple =
    let typeName = RT.FQTypeName.fqPackage PM.Types.Enums.withoutFields
    t
      "Test.ColorEnum.Blue"
      E.Enums.simple
      (RT.DEnum(typeName, typeName, [], "Blue", []))

  let withFields =
    let typeName = RT.FQTypeName.fqPackage PM.Types.Enums.withFields
    t
      "Test.MyOption.Some 1"
      E.Enums.withFields
      (RT.DEnum(typeName, typeName, [], "Some", [ RT.DInt64 1L ]))

  let tests = testList "Enum" [ simple; withFields ]


module Values =
  module Package =
    let mySpecialNumber =
      t "Test.mySpecialNumber" E.Values.Package.MySpecialNumber.usage (RT.DInt64 17L)
    let tests = testList "Package" [ mySpecialNumber ]
  let tests = testList "Values" [ Package.tests ]


module Infix =
  module And =
    let mixed = t "true && false" E.Infix.And.mixed (RT.DBool false)
    let nested = t "true && (true && false)" E.Infix.And.nested (RT.DBool false)
    let bothTrue = t "true && true" E.Infix.And.bothTrue (RT.DBool true)
    let bothFalse = t "false && false" E.Infix.And.bothFalse (RT.DBool false)
    let tests = testList "And" [ mixed; nested; bothTrue; bothFalse ]

  module Or =
    let mixed = t "true || false" E.Infix.Or.mixed (RT.DBool true)
    let nested = t "true || (true || false)" E.Infix.Or.nested (RT.DBool true)
    let bothTrue = t "true || true" E.Infix.Or.bothTrue (RT.DBool true)
    let bothFalse = t "false || false" E.Infix.Or.bothFalse (RT.DBool false)
    let tests = testList "Or" [ mixed; nested; bothTrue; bothFalse ]

  module Add =
    let simple = t "1 + 2" E.Infix.Add.simple (RT.DInt64 3L)
    let tests = testList "Add" [ simple ]

  module Subtract =
    let simple = t "1 - 2" E.Infix.Subtract.simple (RT.DInt64(-1L))
    let tests = testList "Subtract" [ simple ]

  let tests = testList "Infix" [ And.tests; Or.tests; Add.tests; Subtract.tests ]


module Lambdas =
  module Identity =
    let unapplied =
      tCheckVM
        "fn x -> x"
        E.Lambdas.Identity.unapplied
        (RT.DApplicable(
          RT.AppLambda
            { exprId = E.Lambdas.Identity.id
              closedRegisters = []
              argsSoFar = []
              typeSymbolTable = RT.TST.empty }
        ))
        (fun exeState _vm ->
          Expect.isFalse exeState.lambdaInstrCache.IsEmpty "no lambdas registered")

    let applied = t "(fn x -> x) 1" E.Lambdas.Identity.applied (RT.DInt64 1L)

    let tests = testList "Identity" [ unapplied; applied ]

  module Add =
    let unapplied =
      tCheckVM
        "fn x y -> x + y"
        E.Lambdas.Add.unapplied
        (RT.DApplicable(
          RT.AppLambda
            { exprId = E.Lambdas.Add.id
              closedRegisters = []
              argsSoFar = []
              typeSymbolTable = RT.TST.empty }
        ))
        (fun exeState _vm ->
          Expect.isFalse exeState.lambdaInstrCache.IsEmpty "no lambdas registered")

    let partiallyApplied =
      t
        "(fn x y -> x + y) 1"
        E.Lambdas.Add.partiallyApplied
        (RT.DApplicable(
          RT.AppLambda
            { exprId = E.Lambdas.Add.id
              closedRegisters = []
              argsSoFar = [ RT.DInt64 1L ]
              typeSymbolTable = RT.TST.empty }
        ))

    let fullyApplied =
      t "(fn x y -> x + y) 1 2" E.Lambdas.Add.fullyApplied (RT.DInt64 3L)

    let tests = testList "Add" [ unapplied; partiallyApplied; fullyApplied ]


  module AddTuple =
    let unapplied =
      tCheckVM
        "fn (x, y) -> x + y"
        E.Lambdas.AddTuple.unapplied
        (RT.DApplicable(
          RT.AppLambda
            { exprId = E.Lambdas.AddTuple.id
              closedRegisters = []
              argsSoFar = []
              typeSymbolTable = RT.TST.empty }
        ))
        (fun exeState _vm ->
          Expect.isFalse exeState.lambdaInstrCache.IsEmpty "no lambdas registered")

    let applied =
      t "(fn (x, y) -> x + y) (1, 2)" E.Lambdas.AddTuple.applied (RT.DInt64 3L)

    let tests = testList "AddTuple" [ unapplied; applied ]



  module AddToClosedVars =
    let unapplied =
      tCheckVM
        "let x = 5\nlet y=10\nfun a -> a + x + y"
        E.Lambdas.AddToClosedVars.unapplied
        (RT.DApplicable(
          RT.AppLambda
            { exprId = E.Lambdas.AddToClosedVars.id
              closedRegisters = [ (1, RT.DInt64 5); (2, RT.DInt64 10) ]
              argsSoFar = []
              typeSymbolTable = RT.TST.empty }
        ))
        (fun exeState _vm ->
          Expect.isFalse exeState.lambdaInstrCache.IsEmpty "no lambdas registered")

    let applied =
      t
        "let x = 5\nlet y=10\nlet addFifteen = fun a -> a + x + y\naddFifteen 25"
        E.Lambdas.AddToClosedVars.applied
        (RT.DInt64 40L)

    let tests = testList "AddToClosedVars" [ unapplied; applied ]

  module Nested =
    let fullyApplied =
      t "(fn x -> fn y -> x + y) 3 4" E.Lambdas.Nested.fullyApplied (RT.DInt64 7L)

    let tests = testList "Nested" [ fullyApplied ]

  /// Two VMs, one `ExecutionState`. VM-A (built here) runs `fn x -> x` and
  /// registers the lambda in `exeState.lambdaInstrCache`. `executeApplicable`
  /// internally spins up VM-B to invoke the `DApplicable`; VM-B must find
  /// the lambda in the shared exeState cache, not its own empty one.
  /// Regression guard for moving `lambdaInstrCache` off VMState.
  let crossVM =
    testTask "cross-VM lambda invocation" {
      let! exeState = executionStateFor TestValues.pm false Map.empty

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
      // would be empty and this call would raise "lambda not found".
      let! result =
        LibExecution.Execution.executeApplicable
          exeState
          applicable
          (NEList.singleton (RT.DInt64 42L))
        |> Ply.toTask

      match result with
      | Ok dval -> Expect.equal dval (RT.DInt64 42L) "identity lambda across VMs"
      | Error(rte, _) -> failtestf "executeApplicable errored: %A" rte
    }

  let tests =
    testList
      "Lambdas"
      [ Identity.tests
        Add.tests
        AddTuple.tests
        AddToClosedVars.tests
        Nested.tests
        crossVM ]


module Fns =
  module Builtin =
    let unapplied =
      t
        "Builtin.int64Add"
        E.Fns.Builtin.unapplied
        (RT.DApplicable(
          RT.AppNamedFn
            { name = RT.FQFnName.fqBuiltin "int64Add" 0
              typeSymbolTable = RT.TST.empty
              typeArgs = []
              argsSoFar = [] }
        ))

    let partiallyApplied =
      t
        "Builtin.int64Add 1"
        E.Fns.Builtin.partiallyApplied
        (RT.DApplicable(
          RT.AppNamedFn
            { name = RT.FQFnName.fqBuiltin "int64Add" 0
              typeSymbolTable = RT.TST.empty
              typeArgs = []
              argsSoFar = [ RT.DInt64 1 ] }
        ))

    let fullyApplied =
      t "Builtin.int64Add 1 2" E.Fns.Builtin.fullyApplied (RT.DInt64 3L)

    let twoStepApplied =
      t "(Builtin.int64Add 1) 2" E.Fns.Builtin.twoStepApplication (RT.DInt64 3L)

    let tests =
      testList
        "Builtin"
        [ unapplied; partiallyApplied; fullyApplied; twoStepApplied ]


  module Package =
    module MyAdd =

      let unapplied =
        t
          "Test.myAdd"
          E.Fns.Package.MyAdd.unapplied
          (RT.DApplicable(
            RT.AppNamedFn
              { name = RT.FQFnName.fqPackage E.Fns.Package.MyAdd.hash
                typeSymbolTable = RT.TST.empty
                typeArgs = []
                argsSoFar = [] }
          ))

      let partiallyApplied =
        t
          "Test.myAdd 1"
          E.Fns.Package.MyAdd.partiallyApplied
          (RT.DApplicable(
            RT.AppNamedFn
              { name = RT.FQFnName.fqPackage E.Fns.Package.MyAdd.hash
                typeSymbolTable = RT.TST.empty
                typeArgs = []
                argsSoFar = [ RT.DInt64 1 ] }
          ))

      let fullyApplied =
        t "Test.myAdd 1 2" E.Fns.Package.MyAdd.fullyApplied (RT.DInt64 3L)


      let tests = testList "Myadd" [ unapplied; partiallyApplied; fullyApplied ]


    module Fact =
      let unapplied =
        t
          "Test.fact"
          E.Fns.Package.Fact.unapplied
          (RT.DApplicable(
            RT.AppNamedFn
              { name = RT.FQFnName.fqPackage E.Fns.Package.Fact.hash
                typeSymbolTable = RT.TST.empty
                typeArgs = []
                argsSoFar = [] }
          ))

      let appliedWith2 =
        t "Test.fact 2" E.Fns.Package.Fact.appliedWith2 (RT.DInt64 2L)

      let appliedWith20 =
        t
          "Test.fact 20"
          E.Fns.Package.Fact.appliedWith20
          (RT.DInt64 2432902008176640000L)

      let tests = testList "Fact" [ unapplied; appliedWith2; appliedWith20 ]

    module Recusrsion =
      let addUpTo =
        t "Test.addUpTo 30000" E.Fns.Package.Recursion.applied (RT.DInt64 30000L)

      let tests = testList "Recursion" [ addUpTo ]


    module MyFnThatTakesALambda =
      let fullyApplied =
        t
          "Test.myFnThatTakesALambda 4L (fun x -> x + 11L)"
          E.Fns.Package.MyFnThatTakesALambda.fullyApplied2
          (RT.DInt64 15L)

      let tests = testList "MyFnThatTakesALambda" [ fullyApplied ]

    module Outer =
      let applied =
        t
          "Test.outer<Bool, String> true \"ignored\""
          E.Fns.Package.Outer.applied
          (RT.DBool true)
      let tests = testList "Outer" [ applied ]

    let tests =
      testList
        "Package"
        [ MyAdd.tests
          Fact.tests
          Recusrsion.tests
          MyFnThatTakesALambda.tests
          Outer.tests ]

  let tests = testList "Fns" [ Builtin.tests; Package.tests ]

module Statement =
  let simple = t "()\n true" E.Statements.simple (RT.DInt64(1L))

  let nested =
    t
      "myFnThatReturnsUnit()\nmyFnThatReturnsUnit()\nmyFnThatReturnsUnit()\n0L"
      E.Statements.nested
      (RT.DInt64(0L))

  let shouldError =
    tFail
      "1L\n true"
      E.Statements.shouldError
      (RTE.Error.Statement(
        RTE.Statements.FirstExpressionMustBeUnit(VT.unit, VT.int64, RT.DInt64 1L)
      ))
  let tests = testList "Statement" [ simple; nested; shouldError ]


module CapsGate =
  // the call-site gate ENFORCES — a builtin whose capability isn't granted raises at runtime.
  // `timeNowMs` needs `clock`; under noCaps it's denied, under allCaps (the default) it runs. Proves
  // "blockers work" end-to-end through the interpreter.
  let clockCall () =
    eApply (eBuiltinFn "timeNowMs" 0) [] [ eUnit () ]
    |> PT2RT.Expr.toRT Map.empty 0 None

  let denied =
    testTask "an effectful builtin is DENIED when its capability isn't granted" {
      let! exeState = executionStateFor TestValues.pm false Map.empty
      let restricted : RT.ExecutionState = { exeState with grantedCaps = Cap.noCaps }
      let! actual = LibExecution.Execution.executeExpr restricted (clockCall ())
      match actual with
      | Ok _ -> Expect.equal 1 2 "expected a capability-denied RTE, got success"
      | Error(RTE.Error.UncaughtException(msg, _), _) ->
        Expect.stringContains
          msg
          "capability denied"
          "the gate denied the ungranted builtin"
        Expect.stringContains msg "clock" "the error names the missing domain"
      | Error(other, _) ->
        Expect.equal
          1
          2
          $"expected UncaughtException(capability denied), got {other}"
    }

  let allowed =
    testTask
      "the gate does NOT deny when the capability IS granted (allCaps default)" {
      let! exeState = executionStateFor TestValues.pm false Map.empty // allCaps
      let! actual = LibExecution.Execution.executeExpr exeState (clockCall ())
      // We only assert the GATE was transparent — the builtin reaches `fn.fn`. (Its result may not
      // typecheck headless without the OS package type; that's downstream of the gate, not a denial.)
      match actual with
      | Ok _ -> ()
      | Error(RTE.Error.UncaughtException(msg, _), _) ->
        Expect.isFalse
          (msg.Contains "capability denied")
          "under allCaps the gate must NOT deny — it got past the gate"
      | Error _ -> () // any other RTE means it ran past the gate — fine
    }

  // NOTE: the path-scoped file/env/db enforcement (gate refines the need to the concrete target) is
  // covered at the unit level by `scopedGrantsEnforcedByRefine` in Capabilities.Tests — the Cli file
  // builtins aren't registered in this test execution state, so it can't be exercised end-to-end here.
  let tests = testList "CapsGate" [ denied; allowed ]


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
      RT.TInt
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
      LibExecution.Dval.int 1I
      RT.DFloat 1.0
      RT.DChar "c"
      RT.DString "s"
      RT.DUuid(System.Guid.NewGuid())
      RT.DList(RT.ValueType.Known RT.KTInt64, [ RT.DInt64 1L ])
      RT.DList(RT.ValueType.Known RT.KTString, [ RT.DString "s" ])
      RT.DList(RT.ValueType.Unknown, [])
      RT.DList(RT.ValueType.Known(RT.KTList(RT.ValueType.Known RT.KTInt64)), [])
      RT.DDict(
        RT.ValueType.Known RT.KTString,
        RT.ValueType.Known RT.KTInt64,
        Map.empty
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
      let! (exeState : RT.ExecutionState) =
        executionStateFor TestValues.pm false Map.empty
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
      "DInt", LibExecution.Dval.int 1I
      "DFloat", RT.DFloat 1.0
      "DChar", RT.DChar "c"
      "DString", RT.DString "s"
      "DDateTime", RT.DDateTime(LibExecution.DarkDateTime.T(2026, 8, 31, 0, 0, 0))
      "DUuid", RT.DUuid(System.Guid.NewGuid())
      "DList", RT.DList(VT.unknown, [])
      "DTuple", RT.DTuple(RT.DUnit, RT.DUnit, [])
      "DDict", RT.DDict(VT.unknown, VT.unknown, Map.empty)
      "DRecord",
      RT.DRecord(
        RT.FQTypeName.Package(RT.Hash "t"),
        RT.FQTypeName.Package(RT.Hash "t"),
        [],
        Map.empty
      )
      "DEnum",
      RT.DEnum(
        RT.FQTypeName.Package(RT.Hash "t"),
        RT.FQTypeName.Package(RT.Hash "t"),
        [],
        "Case",
        []
      )
      "DDB", RT.DDB "db"
      "DBlob", RT.DBlob(RT.Persistent("hash", 0L)) ]

  let allCasesAreDistinctKeys =
    testCase "every Dval case is a distinct dict key" (fun _ ->
      let asMap =
        oneOfEachCase |> List.map (fun (name, dv) -> RT.DictKey dv, name) |> Map
      Expect.equal
        (Map.count asMap)
        (List.length oneOfEachCase)
        "two Dval cases collapsed into one key — check DictKey.caseTag for a collision")

  let everyCaseFindsItself =
    testCase "every Dval case can be looked up again" (fun _ ->
      let asMap =
        oneOfEachCase |> List.map (fun (name, dv) -> RT.DictKey dv, name) |> Map
      for (name, dv) in oneOfEachCase do
        Expect.equal
          (Map.tryFind (RT.DictKey dv) asMap)
          (Some name)
          $"{name} must find itself")

  let admissibilityMatchesComparability =
    testCase "admissible keys are comparable; inadmissible ones are named" (fun _ ->
      for (name, dv) in oneOfEachCase do
        if LibExecution.RuntimeTypes.Dval.isUsableDictKey dv then
          let asMap = Map [ RT.DictKey dv, name ]
          Expect.equal
            (Map.tryFind (RT.DictKey dv) asMap)
            (Some name)
            $"{name} is admissible, so it must compare without raising"
        else
          Expect.equal name "DDB" $"unexpected inadmissible case: {name}")

  let inadmissibleValuesPoisonTheirContainers =
    testCase "a container holding an inadmissible value is inadmissible" (fun _ ->
      let db = RT.DDB "somedb"
      let isValid = LibExecution.RuntimeTypes.Dval.isUsableDictKey
      let dictOfDb =
        RT.DDict(VT.unknown, VT.unknown, Map [ RT.DictKey RT.DUnit, db ])
      Expect.isFalse (isValid db) "a DB ref is not a key"
      Expect.isFalse (isValid (RT.DList(VT.unknown, [ db ]))) "nor a list of one"
      Expect.isFalse
        (isValid (RT.DTuple(RT.DUnit, db, [])))
        "nor a tuple holding one"
      Expect.isFalse (isValid dictOfDb) "nor a dict holding one as a value"
      Expect.isTrue
        (isValid (RT.DList(VT.unknown, [ RT.DInt64 1L ])))
        "a list of ordinary values is still a key")

  let private seamPairs : List<string * RT.Dval * RT.Dval> =
    let runtimeName = RT.FQTypeName.Package(RT.Hash "shared-runtime-type")
    let sourceA = RT.FQTypeName.Package(RT.Hash "written-as-alias")
    let sourceB = RT.FQTypeName.Package(RT.Hash "written-directly")
    let fields = Map [ "x", RT.DInt64 1L ]
    [ "record via alias vs directly",
      RT.DRecord(sourceA, runtimeName, [], fields),
      RT.DRecord(sourceB, runtimeName, [], fields)

      "enum via alias vs directly",
      RT.DEnum(sourceA, runtimeName, [], "Case", [ RT.DInt64 1L ]),
      RT.DEnum(sourceB, runtimeName, [], "Case", [ RT.DInt64 1L ])

      "signed zero", RT.DFloat 0.0, RT.DFloat -0.0

      "persistent blobs differing only in length",
      RT.DBlob(RT.Persistent("samehash", 1L)),
      RT.DBlob(RT.Persistent("samehash", 2L)) ]

  let private sameKey (a : RT.Dval) (b : RT.Dval) : bool =
    Map.containsKey (RT.DictKey a) (Map [ RT.DictKey b, () ])

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
      let left = RT.DList(VT.unknown, [ RT.DInt64 1L ])
      let right = RT.DList(VT.unknown, [ RT.DString "one" ])
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
      Expect.isTrue (sameKey nan nan) "as a key, NaN must find itself"

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


let tests =
  testList
    "Interpreter"
    [ Basic.tests
      CapsGate.tests
      List.tests
      Let.tests
      String.tests
      Dict.tests
      If.tests
      Tuples.tests
      Match.tests
      Pipes.tests
      Records.tests
      RecordFieldAccess.tests
      RecordUpdate.tests
      Enum.tests
      Values.tests
      Infix.tests
      Lambdas.tests
      Fns.tests
      Statement.tests
      SyncUnify.tests
      DictKeyOrdering.tests ]
