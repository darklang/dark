module Tests.SqlCompiler

open Expecto
open Prelude
open TestUtils.TestUtils

open System.Threading.Tasks
open FSharp.Control.Tasks

module PT = LibExecution.ProgramTypes
open LibExecution.RuntimeTypes

module C = LibCloud.SqlCompiler
module S = TestUtils.RTShortcuts
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

let p (code : string) : Task<Expr> =
  LibParser.Parser.parseRTExpr nameResolver "sqlcompiler.tests.fs" code
  |> Ply.toTask

let parse
  (nameResolver : LibParser.NameResolver.NameResolver)
  (code : string)
  : Task<Expr> =
  LibParser.Parser.parseRTExpr nameResolver "sqlcompiler.tests.fs" code
  |> Ply.toTask

let compile
  (tlid : tlid)
  (symtable : DvalMap)
  (paramName : string)
  ((rowName, rowType) : string * TypeReference)
  (expr : Expr)
  : Task<string * Map<string, SqlValue>> =
  task {
    let canvasID = System.Guid.NewGuid()

    let typeName : TypeName.UserProgram =
      { modules = []; name = TypeName.TypeName "MyType"; version = 0 }
    let field : TypeDeclaration.RecordField = { name = rowName; typ = rowType }
    let userType : UserType.T =
      { tlid = gid ()
        name = typeName
        declaration =
          { typeParams = []
            definition = TypeDeclaration.Record(NEList.singleton field) } }
    let userTypes = Map [ typeName, userType ]
    let typeReference = TCustomType(Ok(FQName.UserProgram typeName), [])

    let! state =
      executionStateFor canvasID false false Map.empty userTypes Map.empty Map.empty

    try
      let! compiled =
        C.compileLambda state tlid Map.empty symtable paramName typeReference expr

      match compiled with
      | Ok compiled ->
        let vars = Map.ofList compiled.vars
        return compiled.sql, vars
      | Error err ->
        return
          Exception.raiseInternal "could not compile lambda to sql" [ "err", err ]

    with e ->
      return
        Exception.raiseInternal e.Message [ "paramName", paramName; "expr", expr ]
  }

let matchSql
  (sql : string)
  (pattern : string)
  (args : Map<string, SqlValue>)
  (expected : List<SqlValue>)
  =
  Expect.isMatchGroups
    sql
    pattern
    (fun g ->
      match g.Count with // implicit full match counts for 1
      | 1 -> true
      | 2 -> Map.findUnsafe g[1].Value args = expected[0]
      | _ -> Exception.raiseInternal "not supported yet" [ "count", g.Count ])
    "compare sql"

let compileTests =
  let tlid = 777777234983UL

  testList
    "compile tests"
    [ testTask "compile true" {
        let! e = p "true"
        let! sql, args = compile tlid Map.empty "value" ("myfield", TBool) e
        matchSql sql @"\(@([A-Z]+)\)" args [ Sql.bool true ]
      }
      testTask "compile field" {
        let! e = p "value.myfield"
        let! sql, args = compile tlid Map.empty "value" ("myfield", TBool) e

        matchSql sql @"\(CAST\(data::jsonb->>'myfield' as bool\)\)" args []
      }
      testTask "check escaped fields" {
        let injection = "'; select * from user_data_v0 ;'field"

        let expr =
          S.eFn
            []
            "equals"
            0
            []
            [ S.eFieldAccess (S.eVar "value") injection; (S.eStr "x") ]

        let! sql, args = compile tlid Map.empty "value" (injection, TString) expr

        matchSql
          sql
          @"\(CAST\(data::jsonb->>'''; select * from user_data_v0;''field' as text\)\) = \('x'\)\)"
          args
          []
      }
      testTask "symtable values escaped" {
        let! expr = p "var == value.name"
        let symtable = Map.ofList [ "var", DString "';select * from user_data_v0;'" ]

        let! sql, args = compile tlid symtable "value" ("name", TString) expr

        matchSql
          sql
          @"\(\(''';select * from user_data_v0;'''\) = \(CAST\(data::jsonb->>'name' as text\)\)\)"
          args
          []
      }
      testTask "pipes expand correctly into nested functions" {
        let! expr = p "value.age |> (-) 2 |> (+) value.age |> (<) 3"
        let! sql, args = compile tlid Map.empty "value" ("age", TInt) expr

        matchSql
          sql
          @"\(\(\(\(CAST\(data::jsonb->>'age' as integer\)\) - \(2\)\) + \(CAST\(data::jsonb->>'age' as integer\)\)\) < \(3\)\)"
          args
          []
      } ]


let inlineWorksAtRoot =
  testTask "inlineWorksAtRoot" {
    let! state =
      executionStateFor
        (System.Guid.NewGuid())
        false
        false
        Map.empty
        Map.empty
        Map.empty
        Map.empty
    let fns = ExecutionState.availableFunctions state
    let! expr =
      LibParser.Parser.parseRTExpr
        nameResolver
        "test.fs"
        "let y = 5 in let x = 6 in (3 + (let x = 7 in y))"
      |> Ply.toTask

    let! expected = p "3 + 5"
    let result =
      uply {
        let! result = C.inline' fns "value" Map.empty expr
        return Expect.equalExprIgnoringIDs result expected
      }
    return! result |> Ply.toTask
  }

let inlineWorksWithNested =
  testTask "inlineWorksWithNested" {
    let! state =
      executionStateFor
        (System.Guid.NewGuid())
        false
        false
        Map.empty
        Map.empty
        Map.empty
        Map.empty
    let fns = ExecutionState.availableFunctions state
    let! expr = p "let x = 5 in (let x = 6 in (3 + (let x = 7 in x)))"

    let! expected = p "3 + 7"
    let result =
      uply {
        let! result = C.inline' fns "value" Map.empty expr
        return Expect.equalExprIgnoringIDs result expected
      }
    return! result |> Ply.toTask
  }

let inlineWorksWithPackageFunctionsSqlBinOp =
  testTask "inlineWorksWithPackageFunctionsSqlBinOp" {
    let! state =
      executionStateFor
        (System.Guid.NewGuid())
        false
        false
        Map.empty
        Map.empty
        Map.empty
        Map.empty
    let fns = ExecutionState.availableFunctions state
    let! expr =
      p
        "let x = true in (let y = false in (PACKAGE.Darklang.Stdlib.Bool.and_v0 x y))"

    let! expected = p "true && false"
    let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
    return Expect.equalExprIgnoringIDs result expected
  }

let inlineWorksWithPackageFunctionsSqlFunction =
  testTask "inlineWorksWithPackageFunctionsSqlFunction" {
    let! state =
      executionStateFor
        (System.Guid.NewGuid())
        false
        false
        Map.empty
        Map.empty
        Map.empty
        Map.empty
    let fns = ExecutionState.availableFunctions state
    let! expr =
      p
        """let x = "package" in (let y = "e" in (PACKAGE.Darklang.Stdlib.String.replaceAll_v0 x y "es"))"""

    let! expected = p """Builtin.String.replaceAll_v0 "package" "e" "es" """
    let result =
      uply {
        let! result = C.inline' fns "value" Map.empty expr
        return Expect.equalExprIgnoringIDs result expected
      }
    return! result |> Ply.toTask
  }


let inlineWorksWithUserFunctions =
  testTask "inlineWorksWithUserFunctions" {
    let! state =
      executionStateFor
        (System.Guid.NewGuid())
        false
        false
        Map.empty
        Map.empty
        Map.empty
        Map.empty

    let userAddBody =
      PT.EInfix(
        0UL,
        PT.InfixFnCall(PT.ArithmeticPlus),
        PT.EVariable(0UL, "a"),
        PT.EVariable(0UL, "b")
      )

    let (userAdd : UserFunction.T) =
      testUserFn "userAdd" [] (NEList.doubleton "a" "b") PT.TInt userAddBody
      |> PT2RT.UserFunction.toRT

    let existingFunctions = ExecutionState.availableFunctions state
    let updatedUserProgram =
      Map.add userAdd.name userAdd existingFunctions.userProgram
    let fns = { existingFunctions with userProgram = updatedUserProgram }

    let nr =
      { nameResolver with
          userFns = Set.singleton (PT.FnName.userProgram [] "userAdd" 0) }

    let! expr =
      parse
        nr
        "let a = 1 in let b = 9 in let c = userAdd 6 4 in (p.height == c) && (p.age == b)"

    let! expected = p "p.height == (6 + 4) && p.age == 9"
    let result =
      uply {
        let! result = C.inline' fns "value" Map.empty expr
        return Expect.equalExprIgnoringIDs result expected
      }
    return! result |> Ply.toTask
  }

let inlineWorksWithPackageAndUserFunctions =
  testTask "inlineWorksWithPackageAndUserFunctions" {
    let! state =
      executionStateFor
        (System.Guid.NewGuid())
        false
        false
        Map.empty
        Map.empty
        Map.empty
        Map.empty

    let (userAnd : UserFunction.T) =
      testUserFn
        "userAnd"
        []
        (NEList.doubleton "a" "b")
        PT.TBool
        (PT.EInfix(
          0UL,
          PT.BinOp(PT.BinOpAnd),
          PT.EVariable(0UL, "a"),
          PT.EVariable(0UL, "b")
        ))
      |> PT2RT.UserFunction.toRT

    let existingFunctions = ExecutionState.availableFunctions state
    let updatedUserProgram =
      Map.add userAnd.name userAnd existingFunctions.userProgram
    let fns = { existingFunctions with userProgram = updatedUserProgram }

    let nr =
      { nameResolver with
          userFns = Set.singleton (PT.FnName.userProgram [] "userAnd" 0) }

    let! expr =
      parse
        nr
        "userAnd user.human (PACKAGE.Darklang.Stdlib.Int.lessThan_v0 user.height (PACKAGE.Darklang.Stdlib.Int.add height 1))"

    let! expected = p "user.human && (user.height < (height + 1))"

    let result =
      uply {
        let! result = C.inline' fns "value" Map.empty expr
        return Expect.equalExprIgnoringIDs result expected
      }
    return! result |> Ply.toTask
  }
let inlineFunctionArguments =
  testTask "inlineWorksArguments" {
    let! state =
      executionStateFor
        (System.Guid.NewGuid())
        false
        false
        Map.empty
        Map.empty
        Map.empty
        Map.empty

    let userAddBody =
      PT.EInfix(
        0UL,
        PT.InfixFnCall(PT.ArithmeticPlus),
        PT.EVariable(0UL, "a"),
        PT.EVariable(0UL, "b")
      )

    let (userAdd : UserFunction.T) =
      testUserFn "userAdd" [] (NEList.doubleton "a" "b") PT.TInt userAddBody
      |> PT2RT.UserFunction.toRT

    let existingFunctions = ExecutionState.availableFunctions state
    let updatedUserProgram =
      Map.add userAdd.name userAdd existingFunctions.userProgram
    let fns = { existingFunctions with userProgram = updatedUserProgram }

    let nr =
      { nameResolver with
          userFns = Set.singleton (PT.FnName.userProgram [] "userAdd" 0) }

    let! expr = parse nr "let a = 1 in let b = 9 in (p.height == userAdd 6 (b - 4))"

    let! expected = parse nr "p.height == 6 + (9 - 4)"
    let result =
      uply {
        let! result = C.inline' fns "value" Map.empty expr
        return Expect.equalExprIgnoringIDs result expected
      }
    return! result |> Ply.toTask
  }


let partialEvaluation =
  testManyTask
    "partialEvaluate"
    (fun (expr, vars) ->
      task {
        let canvasID = System.Guid.NewGuid()
        let tlid = 777777982742UL
        let! state =
          executionStateFor
            canvasID
            false
            false
            Map.empty
            Map.empty
            Map.empty
            Map.empty
        let! expr = p expr
        let result = C.partiallyEvaluate state tlid Map.empty (Map vars) "x" expr
        let! (dvals, result) = Ply.TplPrimitives.runPlyAsTask result
        match result with
        | EVariable(_, name) -> return (Map.findUnsafe name dvals)
        | _ ->
          Expect.isTrue false "didn't match"
          return DUnit
      })
    [ (("false && false", []), DBool false)
      (("false && true", []), DBool false)
      (("true && false", []), DBool false)
      (("true && true", []), DBool true)
      (("false && myVar", [ "myVar", DBool false ]), DBool false)
      (("false && myVar", [ "myVar", DBool true ]), DBool false)
      (("true && myVar", [ "myVar", DBool false ]), DBool false)
      (("true && myVar", [ "myVar", DBool true ]), DBool true)
      (("myVar && false", [ "myVar", DBool false ]), DBool false)
      (("myVar && true", [ "myVar", DBool false ]), DBool false)
      (("myVar && true", [ "myVar", DBool true ]), DBool true)
      (("myVar && true", [ "myVar", DBool true ]), DBool true)
      (("myVar && myVar2", [ "myVar", DBool false; "myVar2", DBool false ]),
       DBool false)
      (("myVar && myVar2", [ "myVar", DBool false; "myVar2", DBool true ]),
       DBool false)
      (("myVar && myVar2", [ "myVar", DBool true; "myVar2", DBool false ]),
       DBool false)
      (("myVar && myVar2", [ "myVar", DBool true; "myVar2", DBool true ]), DBool true)

      // same as above but for ||
      (("false || false", []), DBool false)
      (("false || true", []), DBool true)
      (("true || false", []), DBool true)
      (("true || true", []), DBool true)
      (("false || myVar", [ "myVar", DBool false ]), DBool false)
      (("false || myVar", [ "myVar", DBool true ]), DBool true)
      (("true || myVar", [ "myVar", DBool false ]), DBool true)
      (("true || myVar", [ "myVar", DBool true ]), DBool true)
      (("myVar || false", [ "myVar", DBool false ]), DBool false)
      (("myVar || true", [ "myVar", DBool false ]), DBool true)
      (("myVar || true", [ "myVar", DBool true ]), DBool true)
      (("myVar || true", [ "myVar", DBool true ]), DBool true)
      (("myVar || myVar2", [ "myVar", DBool false; "myVar2", DBool false ]),
       DBool false)
      (("myVar || myVar2", [ "myVar", DBool false; "myVar2", DBool true ]),
       DBool true)
      (("myVar || myVar2", [ "myVar", DBool true; "myVar2", DBool false ]),
       DBool true)
      (("myVar || myVar2", [ "myVar", DBool true; "myVar2", DBool true ]), DBool true)


      ]


let tests =
  testList
    "SqlCompiler"
    [ inlineWorksAtRoot
      inlineWorksWithNested
      inlineWorksWithPackageFunctionsSqlBinOp
      inlineWorksWithPackageFunctionsSqlFunction
      inlineWorksWithUserFunctions
      inlineWorksWithPackageAndUserFunctions
      inlineFunctionArguments
      partialEvaluation
      compileTests ]
