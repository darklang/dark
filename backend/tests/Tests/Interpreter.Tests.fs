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
      "Dict { \"t\": true, \"f\": false }"
      E.Dict.multEntries
      (Dval.stringDict RT.KTBool [ "t", RT.DBool true; "f", RT.DBool false ])

  // "t" appears twice; when the dict is built, the second "t" replaces the first.
  let dupeKey =
    t
      "Dict { \"t\": true, \"f\": false, \"t\": false }"
      E.Dict.dupeKey
      (Dval.stringDict RT.KTBool [ "t", RT.DBool false; "f", RT.DBool false ])

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
  let infix = t "1 |> (fun v -> v + 2)" E.Pipes.infix (RT.DInt64 3L)
  let fnCall = t "1 |> Builtin.int64Add(2)" E.Pipes.fnCall (RT.DInt64 3L)
  let variable =
    t "let myLambda = fun x -> x + 1\n1 |> myLambda" E.Pipes.variable (RT.DInt64 2L)
  let multiple =
    t
      "let incr = fun x -> x + 1\n2 |> incr |> fun x -> x * 2 |> Builtin.int64Add(3) |> (fun v -> v + 4)"
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
  let private fullAccess =
    LibExecution.Permissions.Access.start LibExecution.Permissions.Policy.allowAll

  module Identity =
    let unapplied =
      tCheckVM
        "fn x -> x"
        E.Lambdas.Identity.unapplied
        (RT.DApplicable(
          RT.AppLambda
            { exprId = E.Lambdas.Identity.id
              closedRegisters = [||]
              access = fullAccess
              typeSymbolTable = RT.TST.empty
              expectedReturn = ValueNone
              writtenIn = ValueSome RT.Source }
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
              closedRegisters = [||]
              access = fullAccess
              typeSymbolTable = RT.TST.empty
              expectedReturn = ValueNone
              writtenIn = ValueSome RT.Source }
        ))
        (fun exeState _vm ->
          Expect.isFalse exeState.lambdaInstrCache.IsEmpty "no lambdas registered")

    let partiallyApplied =
      tFail
        "(fn x y -> x + y) 1"
        E.Lambdas.Add.partiallyApplied
        (RTE.Applications.TooFewArgsForLambda(E.Lambdas.Add.id, 2, 1) |> RTE.Apply)

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
              closedRegisters = [||]
              access = fullAccess
              typeSymbolTable = RT.TST.empty
              expectedReturn = ValueNone
              writtenIn = ValueSome RT.Source }
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
              closedRegisters =
                RT.Captures.ofList [ (1, RT.DInt64 5); (2, RT.DInt64 10) ]
              access = fullAccess
              typeSymbolTable = RT.TST.empty
              expectedReturn = ValueNone
              writtenIn = ValueSome RT.Source }
        ))
        (fun exeState _vm ->
          Expect.isFalse exeState.lambdaInstrCache.IsEmpty "no lambdas registered")

    let applied =
      t
        "let x = 5\nlet y=10\nlet addFifteen = fun a -> a + x + y\naddFifteen(25)"
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
      // would be empty and this call would raise "lambda not found".
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
              access = allowAllAccess
              argsSoFar = []
              boundImpls = [] }
        ))

    let tooFew =
      RTE.Applications.TooFewArgsForFn(RT.FQFnName.fqBuiltin "int64Add" 0, 2, 1)
      |> RTE.Apply

    let partiallyApplied =
      tFail "Builtin.int64Add(1)" E.Fns.Builtin.partiallyApplied tooFew

    let fullyApplied =
      t "Builtin.int64Add(1, 2)" E.Fns.Builtin.fullyApplied (RT.DInt64 3L)

    // The inner call stops short, so it raises before the outer one runs.
    let twoStepApplied =
      tFail "(Builtin.int64Add(1))(2)" E.Fns.Builtin.twoStepApplication tooFew

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
                access = allowAllAccess
                argsSoFar = []
                boundImpls = [] }
          ))

      let partiallyApplied =
        tFail
          "Test.myAdd 1"
          E.Fns.Package.MyAdd.partiallyApplied
          (RTE.Applications.TooFewArgsForFn(
            RT.FQFnName.fqPackage E.Fns.Package.MyAdd.hash,
            2,
            1
           )
           |> RTE.Apply)

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
                access = allowAllAccess
                argsSoFar = []
                boundImpls = [] }
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
          "Test.myFnThatTakesALambda(4L, fun x -> x + 11L)"
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
      let! (exeState : RT.ExecutionState) = executionStateFor TestValues.pm false
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
                Map.ofList [ "field", RT.DInt64 1L ]
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
            [ RT.DList(VT.known RT.KTInt64, [ RT.DInt64 1L ])
              RT.DList(VT.known RT.KTString, [ RT.DString "text" ]) ] do
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
      "DBytes", RT.DBytes(RT.Persistent("hash", 0L)) ]

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
          failtest $"unexpected inadmissible case: {name}")

  let inadmissibleValuesPoisonTheirContainers =
    testCase "a container holding an inadmissible value is inadmissible" (fun _ ->
      let bad = LibExecution.Stream.newFromIO VT.int64 (fun () -> Ply None) None
      let isValid = LibExecution.RuntimeTypes.Dval.isUsableDictKey
      let dictOfBad =
        RT.DDict(VT.unknown, VT.unknown, Map [ RT.DictKey RT.DUnit, bad ])
      Expect.isFalse (isValid bad) "a stream is not a key"
      Expect.isFalse (isValid (RT.DList(VT.unknown, [ bad ]))) "nor a list of one"
      Expect.isFalse
        (isValid (RT.DTuple(RT.DUnit, bad, [])))
        "nor a tuple holding one"
      Expect.isFalse (isValid dictOfBad) "nor a dict holding one as a value"
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
      RT.DBytes(RT.Persistent("samehash", 1L)),
      RT.DBytes(RT.Persistent("samehash", 2L)) ]

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
          access = None
          argsSoFar = args
          boundImpls = [] }
    )

  let private equalHashes =
    test "equal values have equal hashes, including compatible metadata" {
      let name = RT.FQTypeName.fqPackage "resolved"
      let alias = RT.FQTypeName.fqPackage "alias"
      let fields = Map [ "value", RT.DFloat 0.0 ]
      let record source args = RT.DRecord(source, name, args, fields)
      let enum source args = RT.DEnum(source, name, args, "None", [])
      let dict key value =
        RT.DDict(VT.float, VT.float, Map [ RT.DictKey(RT.DFloat key), value ])
      let stream = LibExecution.Stream.newFromIO VT.unit (fun () -> Ply None) None
      let promise =
        RT.Promise(
          System.Threading.Tasks.Task.FromResult RT.DUnit,
          RT.FQFnName.fqBuiltin "testRead" 0,
          RT.ExecutionPoint.Source
        )
      let blob = LibExecution.Bytes.newEphemeral [| 1uy |]
      let dictWithKey key =
        RT.DDict(VT.unknown, VT.unit, Map [ RT.DictKey key, RT.DUnit ])
      let pairs =
        [ RT.DFloat 0.0, RT.DFloat -0.0
          RT.DList(VT.unknown, []), RT.DList(VT.int64, [])
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
          named "intAdd" [], named "intAdd" []
          named "intAdd" [ RT.DInt64 1L ], named "intAdd" [ RT.DInt64 1L ]
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
            RT.DList(VT.unknown, [ a ]), RT.DList(VT.unknown, [ b ])
            RT.DTuple(a, RT.DUnit, []), RT.DTuple(b, RT.DUnit, [])
            RT.DRecord(name, name, [], Map [ "value", a ]),
            RT.DRecord(alias, name, [], Map [ "value", b ])
            RT.DEnum(name, name, [], "Some", [ a ]),
            RT.DEnum(alias, name, [], "Some", [ b ])
            RT.DDict(VT.string, VT.unknown, Map [ RT.DictKey(RT.DString "k"), a ]),
            RT.DDict(VT.string, VT.unknown, Map [ RT.DictKey(RT.DString "k"), b ]) ]
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
      let list xs = RT.DList(VT.int64, xs)
      let tuple xs = RT.DTuple(RT.DUnit, RT.DUnit, xs)
      let enum xs = RT.DEnum(name, name, [], "Values", xs)
      let dict xs =
        RT.DDict(
          VT.int64,
          VT.int64,
          xs |> List.mapi (fun i x -> RT.DictKey(RT.DInt64(int64 i)), x) |> Map
        )
      let record xs =
        RT.DRecord(
          name,
          name,
          [],
          xs |> List.mapi (fun i x -> $"field{i:D3}", x) |> Map
        )
      let nested xs =
        [ 1..64 ]
        |> List.fold (fun value _ -> RT.DList(VT.unknown, [ value ])) (list xs)
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
            [ RT.DString "owner"
              RT.DString "modules"
              RT.DString(string (i % 1500)) ]
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
        RT.DList(VT.int64, List.replicate 40 (RT.DInt64 0L) @ [ RT.DInt64 last ])
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
    [ (fun a b -> RT.DList(VT.unknown, [ a; b ]))
      (fun a b -> RT.DTuple(a, b, []))
      (fun a b ->
        RT.DDict(
          VT.string,
          VT.unknown,
          Map [ RT.DictKey(RT.DString "a"), a; RT.DictKey(RT.DString "b"), b ]
        ))
      (fun a b -> RT.DRecord(name, name, [], Map [ "a", a; "b", b ]))
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


/// Deep equality, ordering, type merging, and substitution must fail the stack check.
/// A small thread stack triggers the guard without allocating values large enough
/// to stall the parallel suite. A missing check terminates the test process.
module DeepValues =
  let private depth = 20_000

  /// A list and its element type, both nested `depth` levels deep.
  let private deepList () : RT.Dval =
    let rec build (n : int) (value : RT.Dval) (typ : RT.ValueType) : RT.Dval =
      if n = 0 then value else build (n - 1) (RT.DList(typ, [ value ])) (VT.list typ)

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
          RT.Types.substitute [ "a" ] [ RT.TInt ] typ |> ignore<RT.TypeReference>)
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
          access = None
          argsSoFar = []
          boundImpls = [] }
      let value =
        [ 1..depth ]
        |> List.fold
          (fun inner i ->
            match i % 6 with
            | 0 -> RT.DList(VT.unknown, [ inner ])
            | 1 -> RT.DTuple(RT.DUnit, inner, [])
            | 2 -> RT.DDict(VT.unit, VT.unknown, Map [ RT.DictKey RT.DUnit, inner ])
            | 3 -> RT.DRecord(name, name, [], Map [ "value", inner ])
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
      let! v1 = authorIntoMain $"module Darklang.{m}\n\nlet answer(): Int64 = {one}L"
      let h1 = hashBoundTo v1 "answer"
      let! state = executionStateFor pmPT false
      let held = applicableFor h1
      let! before = call state held
      Expect.equal before (RT.DInt64 one) "the first version answers"

      let! v2 = authorIntoMain $"module Darklang.{m}\n\nlet answer(): Int64 = {two}L"
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
        let! _ = LibDB.Inserts.insertAndApplyOpsAsWip stabilized
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

let callBox(): String = {q}.guarded({q}.Box {{ boxWidth{r} = 1L }})

let callCrate(): String = {q}.guarded({q}.Crate {{ crateDepth{r} = 1L }})"
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
        LibDB.Inserts.insertAndApplyOpsAsWip
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
      let code = "Stdlib.List.filter([ 1L, 2L ], fun x -> 2L)"
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


let tests =
  testList
    "Interpreter"
    [ Basic.tests
      Tests.PermissionsGate.tests
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
      AliasChecks.tests
      DictKeyOrdering.tests
      ListUnique.tests
      DeepValues.tests
      Reload.tests
      BoundMemo.tests
      LambdaResultChecks.tests ]
