module Tests.TestValues

open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module PackageIDs = LibExecution.PackageIDs
module RT = LibExecution.RuntimeTypes

open TestUtils.PTShortcuts

// TODO: consider adding an Expect.equalInstructions,
// which better points out the diffs in the lists

module PM =
  module Types =
    let make hash name definition : PT.PackageType.PackageType =
      { hash = hash
        name = name
        declaration = { typeParams = []; definition = definition }
        description = "TODO"
        deprecated = PT.NotDeprecated }

    module Records =
      let make (hash : Hash) name fields =
        make hash name (PT.TypeDeclaration.Record(NEList.ofListUnsafe "" [] fields))

      let singleFieldId = System.Guid.NewGuid()
      let singleFieldHash = Hash "single-field"
      let nestedHash = Hash "nested"

      let all : List<PT.PackageType.PackageType> =
        [ make
            singleFieldHash
            (PT.PackageType.name "Test" [] "Test")
            [ { name = "key"; typ = PT.TBool; description = "TODO" } ]

          make
            nestedHash
            (PT.PackageType.name "Test" [] "Test2")
            [ { name = "outer"
                typ = PT.TCustomType(Ok(PT.FQTypeName.fqPackage singleFieldHash), [])
                description = "TODO" } ] ]

    module Enums =
      let withoutFieldsHash = Hash "without-fields"
      let withFieldsHash = Hash "with-fields"
      let resultIdHash = Hash "result-id"
      let make hash name cases =
        make hash name (PT.TypeDeclaration.Enum(NEList.ofListUnsafe "" [] cases))

      let colorEnum =
        make
          withoutFieldsHash
          (PT.PackageType.name "Test" [] "ColorEnum")
          [ { name = "Red"; fields = []; description = "TODO" }
            { name = "Green"; fields = []; description = "TODO" }
            { name = "Blue"; fields = []; description = "TODO" } ]

      let MyOption =
        make
          withFieldsHash
          (PT.PackageType.name "Test" [] "MyOption")
          [ { name = "None"; fields = []; description = "TODO" }
            { name = "Some"
              fields = [ { typ = PT.TInt64; label = None; description = "TODO" } ]
              description = "TODO" } ]

      let MyResult =
        make
          resultIdHash
          (PT.PackageType.name "Test" [] "MyResult")
          [ { name = "Ok"
              fields = [ { typ = PT.TInt64; label = None; description = "TODO" } ]
              description = "TODO" }
            { name = "Error"
              fields = [ { typ = PT.TString; label = None; description = "TODO" } ]
              description = "TODO" } ]

      let all : List<PT.PackageType.PackageType> = [ colorEnum; MyOption; MyResult ]


    let all = Records.all @ Enums.all

  module Values =
    let all = []

  module Functions =
    let all = []



module Expressions =
  module Basic =
    let one = eInt64 1

  // let onePlusTwo =
  //   eApply
  //     (PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "int64Add" 0)))
  //     []
  //     [ eInt64 1; eInt64 2 ]


  module Let =
    // TODO: try to use undefined variable
    // TODO: lpunit
    let simple = eLet (lpVar "x") (eBool true) (eVar "x")

    let tuple =
      eLet
        (lpTuple (lpVar "x") (lpVar "y") [])
        (eTuple (eInt64 1) (eInt64 2) [])
        (eVar "x")

    /// `let (a, b) = 1 in a`
    let tupleNotTuple =
      eLet (lpTuple (lpVar "a") (lpVar "b") []) (eInt64 1) (eVar "a")

    /// `let (a, b) = (1, 2, 3) in a`
    let tupleIncorrectLen =
      eLet
        (lpTuple (lpVar "a") (lpVar "b") [])
        (eTuple (eInt64 1) (eInt64 2) [ eInt64 3 ])
        (eVar "a")


    /// `let (a, (b, c)) = (1, (2, 3)) in b`
    let tupleNested =
      eLet
        (lpTuple (lpVar "a") (lpTuple (lpVar "b") (lpVar "c") []) [])
        (eTuple (eInt64 1) (eTuple (eInt64 2) (eInt64 3) []) [])
        (eVar "b")

    let undefinedVar = eVar "a"


  module List =
    let simple = eList [ eBool true; eBool false; eBool true ]

    let nested =
      eList [ eList [ eBool true; eBool false ]; eList [ eBool false; eBool true ] ]

    let mixed = eList [ eInt64 1; eBool true ]


  module String =
    let simple = eStr [ strText "hello" ]

    let withInterpolation =
      eLet
        (lpVar "x")
        (eStr [ strText ", world" ])
        (eStr [ strText "hello"; strInterp (eVar "x") ])


  module Dict =
    let empty = eDict []
    let simple = eDict [ "key", eBool true ]
    let multEntries = eDict [ "t", eBool true; "f", eBool false ]
    let dupeKey = eDict [ "t", eBool true; "f", eBool false; "t", eBool false ]

  module If =
    let gotoThenBranch = eIf (eBool true) (eInt64 1) (Some(eInt64 2))
    let gotoElseBranch = eIf (eBool false) (eInt64 1) (Some(eInt64 2))
    let elseMissing = eIf (eBool false) (eInt64 1) None


  module Tuples =
    /// `(false, true)`
    let two = eTuple (eBool false) (eBool true) []

    /// `(false, true, false)`
    let three = eTuple (eBool false) (eBool true) [ eBool false ]

    /// `((false, true), true, (true, false))`
    let nested =
      eTuple
        (eTuple (eBool false) (eBool true) [])
        (eBool true)
        [ eTuple (eBool true) (eBool false) [] ]

  // TODO: test MPEnum
  module Match =
    /// match true with
    /// | false -> "first branch"
    /// | true -> "second branch"
    let simple =
      eMatch
        (eBool true)
        [ { pat = PT.MPBool(gid (), false)
            whenCondition = None
            rhs = eStr [ strText "first branch" ] }
          { pat = PT.MPBool(gid (), true)
            whenCondition = None
            rhs = eStr [ strText "second branch" ] } ]

    /// match true with
    /// | false -> "first branch"
    let notMatched =
      eMatch
        (eBool true)
        [ { pat = PT.MPBool(gid (), false)
            whenCondition = None
            rhs = eStr [ strText "first branch" ] } ]

    /// match true with
    /// | x -> x
    let withVar =
      eMatch
        (eBool true)
        [ { pat = PT.MPVariable(gid (), "x"); whenCondition = None; rhs = eVar "x" } ]

    /// match 4 with
    /// | 1 -> "first branch"
    /// | x when x % 2 == 0 -> "second branch"
    let withVarAndWhenCondition =
      eMatch
        (eInt64 4)
        [ { pat = PT.MPInt64(gid (), 1)
            whenCondition = None
            rhs = eStr [ strText "first branch" ] }
          { pat = PT.MPVariable(gid (), "x")
            // "is even"
            whenCondition =
              Some(
                eApply
                  (PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "equals" 0)))
                  []
                  [ eApply
                      (PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "int64Mod" 0)))
                      []
                      [ eVar "x"; eInt64 2 ]
                    eInt64 0 ]
              )
            rhs = eStr [ strText "second branch" ] } ]

    let list =
      eMatch
        (eList [ eInt64 1; eInt64 2 ])
        [ { pat = PT.MPList(gid (), [ PT.MPInt64(gid (), 1); PT.MPInt64(gid (), 2) ])
            whenCondition = None
            rhs = eStr [ strText "first branch" ] } ]

    let listCons =
      eMatch
        (eList [ eInt64 1; eInt64 2 ])
        [ { pat =
              PT.MPListCons(
                gid (),
                PT.MPInt64(gid (), 1),
                PT.MPVariable(gid (), "tail")
              )
            whenCondition = None
            rhs = eVar "tail" } ]

    let tuple =
      eMatch
        (eTuple (eInt64 1) (eInt64 2) [])
        [ { pat =
              PT.MPTuple(gid (), PT.MPInt64(gid (), 1), PT.MPInt64(gid (), 2), [])
            whenCondition = None
            rhs = eStr [ strText "first branch" ] } ]

    //match (1, 2) with\n| (1, 2) | (2, 1) -> \"first branch\"\n| _ -> \"second branch\"
    let combinedPatternsFirstPatMatches =
      eMatch
        (eTuple (eInt64 1) (eInt64 2) [])
        [ { pat =
              PT.MPOr(
                gid (),
                NEList.ofList
                  (PT.MPTuple(
                    gid (),
                    PT.MPInt64(gid (), 1),
                    PT.MPInt64(gid (), 2),
                    []
                  ))
                  [ PT.MPTuple(
                      gid (),
                      PT.MPInt64(gid (), 2),
                      PT.MPInt64(gid (), 1),
                      []
                    ) ]
              )
            whenCondition = None
            rhs = eStr [ strText "first branch" ] }
          { pat = PT.MPVariable(gid (), "_")
            whenCondition = None
            rhs = eStr [ strText "second branch" ] } ]

    //match (2, 1) with\n| (1, 2) | (2, 1) -> \"first branch\"\n| _ -> \"second branch\"
    let combinedPatternsSecondPatMatches =
      eMatch
        (eTuple (eInt64 2) (eInt64 1) [])
        [ { pat =
              PT.MPOr(
                gid (),
                NEList.ofList
                  (PT.MPTuple(
                    gid (),
                    PT.MPInt64(gid (), 1),
                    PT.MPInt64(gid (), 2),
                    []
                  ))
                  [ PT.MPTuple(
                      gid (),
                      PT.MPInt64(gid (), 2),
                      PT.MPInt64(gid (), 1),
                      []
                    ) ]
              )
            whenCondition = None
            rhs = eStr [ strText "first branch" ] }
          { pat = PT.MPVariable(gid (), "_")
            whenCondition = None
            rhs = eStr [ strText "second branch" ] } ]

    // match (1, 2) with\n| (1, 2) | (2, 1) when false -> \"first branch\"\n| _ -> \"second branch\"
    let combinedPatternsWithWhenCond =
      eMatch
        (eTuple (eInt64 1) (eInt64 2) [])
        [ { pat =
              PT.MPOr(
                gid (),
                NEList.ofList
                  (PT.MPTuple(
                    gid (),
                    PT.MPInt64(gid (), 1),
                    PT.MPInt64(gid (), 2),
                    []
                  ))
                  [ PT.MPTuple(
                      gid (),
                      PT.MPInt64(gid (), 2),
                      PT.MPInt64(gid (), 1),
                      []
                    ) ]
              )
            whenCondition = Some(eBool false)
            rhs = eStr [ strText "first branch" ] }
          { pat = PT.MPVariable(gid (), "_")
            whenCondition = None
            rhs = eStr [ strText "second branch" ] } ]

    // match (1L,2L) with\n| (x,2L) | (2L,x) when x == 1L -> "first branch"\n _ -> "second branch"
    let combinedPatternsWithVarAndWhenCond =
      eMatch
        (eTuple (eInt64 1) (eInt64 2) [])
        [ { pat =
              PT.MPOr(
                gid (),
                NEList.ofList
                  (PT.MPTuple(
                    gid (),
                    PT.MPVariable(gid (), "x"),
                    PT.MPInt64(gid (), 2),
                    []
                  ))
                  [ PT.MPTuple(
                      gid (),
                      PT.MPInt64(gid (), 2),
                      PT.MPVariable(gid (), "x"),
                      []
                    ) ]
              )
            whenCondition =
              Some(
                eApply
                  (PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "equals" 0)))
                  []
                  [ eVar "x"; eInt64 1 ]
              )
            rhs = eStr [ strText "first branch" ] }
          { pat = PT.MPVariable(gid (), "_")
            whenCondition = None
            rhs = eStr [ strText "second branch" ] } ]


    // match (Stdlib.Result.Result.Ok 1L, Stdlib.Result.Result.Error "error") with
    // | (Error err, Ok _) | (Ok _, Error err) -> err
    let combinedPatSameVarDifferentPos =
      eMatch
        (eTuple
          (eEnum (typeNamePkg PM.Types.Enums.resultIdHash) [] "Ok" [ eInt64 1 ])
          (eEnum
            (typeNamePkg PM.Types.Enums.resultIdHash)
            []
            "Error"
            [ eStr [ strText "error" ] ])
          [])
        [ { pat =
              PT.MPOr(
                gid (),
                NEList.ofList
                  (PT.MPTuple(
                    gid (),
                    PT.MPEnum(gid (), "Error", [ PT.MPVariable(gid (), "err") ]),
                    PT.MPEnum(gid (), "Ok", [ PT.MPVariable(gid (), "_") ]),
                    []
                  ))
                  [ PT.MPTuple(
                      gid (),
                      PT.MPEnum(gid (), "Ok", [ PT.MPVariable(gid (), "_") ]),
                      PT.MPEnum(gid (), "Error", [ PT.MPVariable(gid (), "err") ]),
                      []
                    ) ]
              )
            whenCondition = None
            rhs = eVar "err" } ]


  module Pipes =
    let lambdaID = gid ()
    let pipeID = gid ()
    /// `1 |> fun x -> x`
    let lambda = ePipe (eInt64 1) [ pLambda pipeID [ lpVar "x" ] (eVar "x") ]

    /// `1 |> (+) 2`
    let infix =
      ePipe
        (eInt64 1)
        [ pInfix pipeID (PT.Infix.InfixFnCall PT.ArithmeticPlus) (eInt64 2) ]

    /// `1 |> Builtin.int64Add 2`
    let fnCall =
      ePipe
        (eInt64 1)
        [ pFnCall pipeID (PT.FQFnName.fqBuiltIn "int64Add" 0) [] [ eInt64 2 ] ]

    //let enum = ePipe (eInt64 1) [ pEnum (PT.FQEnumName.fqPackage (System.Guid.NewGuid())) "variant" [] ]

    /// let myLambda = fun x -> x + 1
    /// 1 |> myLambda
    let variable =
      eLet
        (lpVar "myLambda")
        (eLambda
          lambdaID
          [ lpVar "x" ]
          (eInfix (PT.Infix.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1)))
        (ePipe (eInt64 1) [ pVariable pipeID "myLambda" [] ])

    /// ```fsharp
    /// let incr = fun x -> x + 1
    /// 2 |> incr |> fun x -> x * 2 |> Builtin.int64Add 3 |> (+) 4
    /// ```
    let multiple =
      eLet
        (lpVar "incr")
        (eLambda
          lambdaID
          [ lpVar "x" ]
          (eInfix (PT.Infix.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1)))
        (ePipe
          (eInt64 2)
          [ pVariable pipeID "incr" []
            pLambda
              pipeID
              [ lpVar "x" ]
              (eInfix
                (PT.Infix.InfixFnCall PT.ArithmeticMultiply)
                (eVar "x")
                (eInt64 2))
            pFnCall pipeID (PT.FQFnName.fqBuiltIn "int64Add" 0) [] [ eInt64 3 ]
            pInfix pipeID (PT.Infix.InfixFnCall PT.ArithmeticPlus) (eInt64 4) ])


  module Records =
    let simple =
      eRecord (typeNamePkg PM.Types.Records.singleFieldHash) [] [ "key", eBool true ]

    let nested =
      eRecord (typeNamePkg PM.Types.Records.nestedHash) [] [ "outer", simple ]

  module RecordFieldAccess =
    let simple = eFieldAccess Records.simple "key"
    let notRecord = eFieldAccess (eInt64 1) "key"
    let missingField = eFieldAccess Records.simple "missing"
    let nested = eFieldAccess (eFieldAccess Records.nested "outer") "key"


  module RecordUpdate =
    let simple = eRecordUpdate Records.simple [ "key", eBool false ]
    let notRecord = eRecordUpdate (eInt64 1) [ "key", eBool false ]
    let fieldThatShouldNotExist =
      eRecordUpdate Records.simple [ "bonus", eBool false ]
    let fieldWithWrongType = eRecordUpdate Records.simple [ "key", eInt64 1 ]


  module Enums =
    let simple = eEnum (typeNamePkg PM.Types.Enums.withoutFieldsHash) [] "Blue" []
    let withFields =
      eEnum (typeNamePkg PM.Types.Enums.withFieldsHash) [] "Some" [ eInt64 1 ]

  module Values =
    // CLEANUP we don't really have builtin values, so not bothering to test for now
    // module Builtin =
    //   let infinity = eBuiltinValue "infinity" 0

    module Package =
      module MySpecialNumber =
        // 17
        let id = System.Guid.Parse "1823ae7e-cc59-4843-a884-18591398abb0"
        let hash = Hash "test-hash" // TODO: generate real hash
        let usage = ePackageValue hash


  module Infix =
    module And =
      let mixed = eInfix (PT.Infix.BinOp PT.BinOpAnd) (eBool true) (eBool false)
      let nested = eInfix (PT.Infix.BinOp PT.BinOpAnd) mixed (eBool true)
      let bothTrue = eInfix (PT.Infix.BinOp PT.BinOpAnd) (eBool true) (eBool true)
      let bothFalse = eInfix (PT.Infix.BinOp PT.BinOpAnd) (eBool false) (eBool false)

    module Or =
      let mixed = eInfix (PT.Infix.BinOp PT.BinOpOr) (eBool true) (eBool false)
      let nested = eInfix (PT.Infix.BinOp PT.BinOpOr) mixed (eBool true)
      let bothTrue = eInfix (PT.Infix.BinOp PT.BinOpOr) (eBool true) (eBool true)
      let bothFalse = eInfix (PT.Infix.BinOp PT.BinOpOr) (eBool false) (eBool false)

    module Add =
      let simple =
        eInfix (PT.Infix.InfixFnCall PT.ArithmeticPlus) (eInt64 1) (eInt64 2)

    module Subtract =
      let simple =
        eInfix (PT.Infix.InfixFnCall PT.ArithmeticMinus) (eInt64 1) (eInt64 2)



  // TODO: test nested lambdas
  module Lambdas =
    module Identity =
      let id = gid ()

      let unapplied = eLambda id [ lpVar "x" ] (eVar "x")

      let applied = eApply unapplied [] [ eInt64 1 ]

    module Add =
      let id = gid ()
      let unapplied =
        eLambda
          id
          [ lpVar "a"; lpVar "b" ]
          (eApply (eBuiltinFn "int64Add" 0) [] [ eVar "a"; eVar "b" ])
      let partiallyApplied = eApply unapplied [] [ eInt64 1 ]
      let fullyApplied = eApply unapplied [] [ eInt64 1; eInt64 2 ]

    module AddTuple =
      let id = gid ()
      let unapplied =
        eLambda
          id
          [ lpTuple (lpVar "a") (lpVar "b") [] ]
          (eApply (eBuiltinFn "int64Add" 0) [] [ eVar "a"; eVar "b" ])
      let applied = eApply unapplied [] [ eTuple (eInt64 1) (eInt64 2) [] ]

    ///```fsharp
    /// let x = 5
    /// let y = 10
    /// let addFifteen = fun a -> a + x + y
    /// addFifteen 25
    /// ```
    module AddToClosedVars =
      let id = gid ()
      let unapplied =
        eLet
          (lpVar "x")
          (eInt64 5)
          (eLet
            (lpVar "y")
            (eInt64 10)
            (eLambda
              id
              [ lpVar "a" ]
              (eApply
                (eBuiltinFn "int64Add" 0)
                []
                [ (eVar "a")
                  (eApply (eBuiltinFn "int64Add" 0) [] [ eVar "x"; eVar "y" ]) ])))

      let applied =
        eLet
          (lpVar "addFifteen")
          unapplied
          (eApply (eVar "addFifteen") [] [ eInt64 25 ])


  module Fns =
    module Builtin =
      let unapplied = eBuiltinFn "int64Add" 0
      let partiallyApplied = eApply unapplied [] [ eInt64 1 ]
      let fullyApplied = eApply unapplied [] [ eInt64 1; eInt64 2 ]
      let twoStepApplication = eApply partiallyApplied [] [ eInt64 2 ]

    module Package =
      module MyAdd =
        let hash = Hash "myadd-hash" // TODO: generate real hash

        let unapplied = ePackageFn hash
        let partiallyApplied = eApply unapplied [] [ eInt64 1 ]
        let fullyApplied = eApply unapplied [] [ eInt64 1; eInt64 2 ]

      module Inner =
        let hash = Hash "inner-hash" // TODO: generate real hash

        let unapplied = ePackageFn hash
      //let applied = eApply unapplied [] [ eInt64 1 ]

      module Outer =
        let hash = Hash "outer-hash" // TODO: generate real hash
        let unapplied = ePackageFn hash
        let applied =
          eApply
            unapplied
            [ PT.TBool; PT.TString ]
            [ eBool true; eStr [ strText "ignored" ] ]


      module Fact =
        let hash = Hash "fact-hash" // TODO: generate real hash
        let unapplied = ePackageFn hash
        let appliedWith2 = eApply unapplied [] [ eInt64 2 ]
        let appliedWith20 = eApply unapplied [] [ eInt64 20 ]

      module Recursion =
        let hash = Hash "recursion-hash" // TODO: generate real hash
        let unapplied = ePackageFn hash
        let applied = eApply unapplied [] [ eInt64 30000 ]


      module MyFnThatTakesALambda =
        let lambdaID = gid ()
        let hash = Hash "myfn-hash" // TODO: generate real hash
        let unapplied = ePackageFn hash

        let fullyApplied =
          let list = eList [ eInt64 1L; eInt64 2L ]
          let lambda =
            eLambda
              lambdaID
              [ lpVar "x" ]
              (eInfix (PT.Infix.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 1))
          eApply unapplied [] [ list; lambda ]


        let fullyApplied2 =
          let lambda =
            eLambda
              lambdaID
              [ lpVar "x" ]
              (eInfix (PT.Infix.InfixFnCall PT.ArithmeticPlus) (eVar "x") (eInt64 11))

          eApply unapplied [] [ eInt64 4L; lambda ]

      module MyFnThatReturnsUnit =
        let hash = Hash "myfnthatreturnsunit-hash" // TODO: generate real hash
        let applied = eApply (ePackageFn hash) [] [ (eUnit ()) ]

  module Statements =
    let simple = eStatement (eUnit ()) (eInt64 1)
    let statement =
      eStatement
        (eApply (eBuiltinFn "printLine" 0) [] [ eStr [ strText "hello" ] ])
        (eInt64 3)

    let nested =
      eStatement
        (eStatement
          Fns.Package.MyFnThatReturnsUnit.applied
          Fns.Package.MyFnThatReturnsUnit.applied)
        (eStatement Fns.Package.MyFnThatReturnsUnit.applied (eInt64 0))

    let shouldError = eStatement (eInt64 1) (eBool true)


//CLEANUP: Migrate this to the top
let pm : PT.PackageManager =
  PT.PackageManager.empty
  |> PT.PackageManager.withExtras
    // Types
    PM.Types.all

    // values
    [ { hash = Expressions.Values.Package.MySpecialNumber.hash
        name = PT.PackageValue.name "Test" [] "seventeen"
        description = "TODO"
        deprecated = PT.NotDeprecated
        body = PT.EInt64(gid (), 17L) } ]

    // fns
    [ { hash = Expressions.Fns.Package.Inner.hash
        name = PT.PackageFn.name "Test" [] "inner"
        typeParams = [ "x"; "y" ]
        parameters =
          NEList.ofList
            { name = "x"; typ = PT.TVariable "x"; description = "TODO" }
            [ { name = "_y"; typ = PT.TVariable "y"; description = "TODO" } ]
        returnType = PT.TVariable "x"
        body = eVar "x"
        description = "TODO"
        deprecated = PT.NotDeprecated }

      { hash = Expressions.Fns.Package.Outer.hash
        name = PT.PackageFn.name "Test" [] "outer"
        typeParams = [ "x"; "y" ]
        parameters =
          NEList.ofList
            { name = "x"; typ = PT.TVariable "x"; description = "TODO" }
            [ { name = "_y"; typ = PT.TVariable "y"; description = "TODO" } ]
        returnType = PT.TVariable "x"
        body =
          eLet
            (lpVar "ignored")
            (eApply
              (ePackageFn Expressions.Fns.Package.Inner.hash)
              [ PT.TString; PT.TBool ]
              [ eStr [ strText "hi" ]; eBool true ])
            (eVar "x")
        description = "TODO"
        deprecated = PT.NotDeprecated }

      { hash = Expressions.Fns.Package.MyAdd.hash
        name = PT.PackageFn.name "Test" [] "add"
        typeParams = []
        parameters =
          NEList.ofList
            { name = "a"; typ = PT.TInt64; description = "TODO" }
            [ { name = "b"; typ = PT.TInt64; description = "TODO" } ]
        returnType = PT.TInt64
        body = eApply (eBuiltinFn "int64Add" 0) [] [ eVar "a"; eVar "b" ]
        description = "TODO"
        deprecated = PT.NotDeprecated }

      { hash = Expressions.Fns.Package.Fact.hash
        name = PT.PackageFn.name "Test" [] "fact"
        typeParams = []
        parameters =
          NEList.ofList { name = "a"; typ = PT.TInt64; description = "TODO" } []
        returnType = PT.TInt64
        body =
          eIf
            (eApply (eBuiltinFn "equals" 0) [] [ eVar "a"; eInt64 1 ])
            (eInt64 1)
            (Some(
              eApply
                (eBuiltinFn "int64Multiply" 0)
                []
                [ eVar "a"
                  (eApply
                    (ePackageFn Expressions.Fns.Package.Fact.hash)
                    []
                    [ eApply (eBuiltinFn "int64Subtract" 0) [] [ eVar "a"; eInt64 1 ] ]) ]
            ))

        description = "TODO"
        deprecated = PT.NotDeprecated }

      // let addUpTO (n : Int64) : Int64 =
      //   if n <= 0 then 0
      //   else 1 + addUpTo (n - 1)
      { hash = Expressions.Fns.Package.Recursion.hash
        name = PT.PackageFn.name "Test" [] "addUpTo"
        typeParams = []
        parameters =
          NEList.ofList { name = "n"; typ = PT.TInt64; description = "TODO" } []
        returnType = PT.TInt64
        body =
          eIf
            (eApply
              (eBuiltinFn "int64LessThanOrEqualTo" 0)
              []
              [ eVar "n"; eInt64 0L ])
            (eInt64 0L)
            (Some(
              eApply
                (eBuiltinFn "int64Add" 0)
                []
                [ eInt64 1L
                  (eApply
                    (ePackageFn Expressions.Fns.Package.Recursion.hash)
                    []
                    [ eApply
                        (eBuiltinFn "int64Subtract" 0)
                        []
                        [ eVar "n"; eInt64 1L ] ]) ]
            ))
        description = "TODO"
        deprecated = PT.NotDeprecated }

      { hash = Expressions.Fns.Package.MyFnThatTakesALambda.hash
        name = PT.PackageFn.name "Test" [] "myFnThatTakesALambda"
        typeParams = []
        parameters =
          NEList.ofList
            { name = "x"; typ = PT.TInt64; description = "TODO" }
            [ { name = "fn"
                typ =
                  PT.TFn({ head = PT.TVariable "a"; tail = [] }, PT.TVariable "b")
                description = "TODO" } ]
        returnType = PT.TInt64
        body = eApply (eVar "fn") [] [ eVar "x" ]
        description = "TODO"
        deprecated = PT.NotDeprecated }

      { hash = Expressions.Fns.Package.Fact.hash
        name = PT.PackageFn.name "Test" [] "fact"
        typeParams = []
        parameters =
          NEList.ofList { name = "a"; typ = PT.TInt64; description = "TODO" } []
        returnType = PT.TInt64
        body =
          eIf
            (eApply (eBuiltinFn "equals" 0) [] [ eVar "a"; eInt64 1 ])
            (eInt64 1)
            (Some(
              eApply
                (eBuiltinFn "int64Multiply" 0)
                []
                [ eVar "a"
                  (eApply
                    (ePackageFn Expressions.Fns.Package.Fact.hash)
                    []
                    [ eApply (eBuiltinFn "int64Subtract" 0) [] [ eVar "a"; eInt64 1 ] ]) ]
            ))

        description = "TODO"
        deprecated = PT.NotDeprecated }

      { hash = Expressions.Fns.Package.MyFnThatReturnsUnit.hash
        name = PT.PackageFn.name "Test" [] "myFnThatReturnsUnit"
        typeParams = []
        parameters =
          NEList.ofList { name = "unit"; typ = PT.TUnit; description = "TODO" } []
        returnType = PT.TUnit
        body = eUnit ()
        description = "TODO"
        deprecated = PT.NotDeprecated } ]
