module Tests.Interpreter

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module VT = LibExecution.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RTE = RT.RuntimeError

module E = TestValues.Expressions
module PM = TestValues.PM

let tCheckVM
  name
  ptExpr
  expectedInsts
  (extraVmStateAssertions : RT.VMState -> unit)
  =
  testTask name {
    let vmState =
      ptExpr |> PT2RT.Expr.toRT Map.empty 0 |> RT.VMState.createWithoutTLID

    let! exeState =
      executionStateFor TestValues.pm (System.Guid.NewGuid()) false false Map.empty

    let! actual = LibExecution.Interpreter.execute exeState vmState |> Ply.toTask
    Expect.equal actual expectedInsts ""

    extraVmStateAssertions vmState
  }

let t name ptExpr expectedInsts =
  tCheckVM name ptExpr expectedInsts (ignore<RT.VMState>)



let tFail name ptExpr expectedRte =
  testTask name {
    let instructions = ptExpr |> PT2RT.Expr.toRT Map.empty 0

    let! exeState =
      executionStateFor TestValues.pm (System.Guid.NewGuid()) false false Map.empty

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
      "[true; false; true]"
      E.List.simple
      (RT.DList(VT.bool, [ RT.DBool true; RT.DBool false; RT.DBool true ]))

  let nested =
    t
      "[[true; false]; [false; true]]"
      E.List.nested
      (RT.DList(
        VT.list VT.bool,
        [ RT.DList(VT.bool, [ RT.DBool true; RT.DBool false ])
          RT.DList(VT.bool, [ RT.DBool false; RT.DBool true ]) ]
      ))

  let mixed =
    tFail
      "[1; true]"
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
  let empty = t "Dict {}" E.Dict.empty (RT.DDict(VT.unknown, Map.empty))

  let simple =
    t
      "Dict { t: true}"
      E.Dict.simple
      (RT.DDict(VT.bool, Map [ "key", RT.DBool true ]))

  let multEntries =
    t
      "Dict {t: true; f: false}"
      E.Dict.multEntries
      (RT.DDict(VT.bool, Map [ "t", RT.DBool true; "f", RT.DBool false ]))

  let dupeKey =
    tFail
      "Dict {t: true; f: false; t: false}"
      E.Dict.dupeKey
      (RTE.Dict(RTE.Dicts.TriedToAddKeyAfterAlreadyPresent "t"))

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
    let typeName = RT.FQTypeName.fqPackage PM.Types.Records.singleFieldHash
    t
      "Test.Test { key = true }"
      E.Records.simple
      (RT.DRecord(typeName, typeName, [], Map [ "key", RT.DBool true ]))

  let nested =
    let outerTypeName = RT.FQTypeName.fqPackage PM.Types.Records.nestedHash
    let innerTypeName = RT.FQTypeName.fqPackage PM.Types.Records.singleFieldHash
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
        RT.FQTypeName.fqPackage PM.Types.Records.singleFieldHash,
        RT.FQTypeName.fqPackage PM.Types.Records.singleFieldHash,
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
        RTE.Records.UpdateFieldOfWrongType("key", VT.bool, VT.int64, RT.DInt64 1L)
      ))

  let tests =
    testList
      "RecordUpdate"
      [ simple; notRecord; fieldThatShouldNotExist; fieldWithWrongType ]

// TODO: add more tests
module Enum =
  let simple =
    let typeName = RT.FQTypeName.fqPackage PM.Types.Enums.withoutFieldsHash
    t
      "Test.ColorEnum.Blue"
      E.Enums.simple
      (RT.DEnum(typeName, typeName, [], "Blue", []))

  let withFields =
    let typeName = RT.FQTypeName.fqPackage PM.Types.Enums.withFieldsHash
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
              typeSymbolTable = Map.empty }
        ))
        (fun vm ->
          Expect.isFalse (Map.isEmpty vm.lambdaInstrCache) "no lambdas in VMState")

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
              typeSymbolTable = Map.empty }
        ))
        (fun vm ->
          Expect.isFalse (Map.isEmpty vm.lambdaInstrCache) "no lambdas in VMState")

    let partiallyApplied =
      t
        "(fn x y -> x + y) 1"
        E.Lambdas.Add.partiallyApplied
        (RT.DApplicable(
          RT.AppLambda
            { exprId = E.Lambdas.Add.id
              closedRegisters = []
              argsSoFar = [ RT.DInt64 1L ]
              typeSymbolTable = Map.empty }
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
              typeSymbolTable = Map.empty }
        ))
        (fun vm ->
          Expect.isFalse (Map.isEmpty vm.lambdaInstrCache) "no lambdas in VMState")

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
              typeSymbolTable = Map.empty }
        ))
        (fun vm ->
          Expect.isFalse (Map.isEmpty vm.lambdaInstrCache) "no lambdas in VMState")

    let applied =
      t
        "let x = 5\nlet y=10\nlet addFifteen = fun a -> a + x + y\naddFifteen 25"
        E.Lambdas.AddToClosedVars.applied
        (RT.DInt64 40L)

    let tests = testList "AddToClosedVars" [ unapplied; applied ]

  let tests =
    testList
      "Lambdas"
      [ Identity.tests; Add.tests; AddTuple.tests; AddToClosedVars.tests ]


module Fns =
  module Builtin =
    let unapplied =
      t
        "Builtin.int64Add"
        E.Fns.Builtin.unapplied
        (RT.DApplicable(
          RT.AppNamedFn
            { name = RT.FQFnName.fqBuiltin "int64Add" 0
              typeSymbolTable = Map.empty
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
              typeSymbolTable = Map.empty
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
                typeSymbolTable = Map.empty
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
                typeSymbolTable = Map.empty
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
                typeSymbolTable = Map.empty
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


let tests =
  testList
    "Interpreter"
    [ Basic.tests
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
      Statement.tests ]
