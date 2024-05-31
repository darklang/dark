module Tests.SqlCompiler

open Expecto
open Prelude
open TestUtils.TestUtils

open System.Threading.Tasks
open FSharp.Control.Tasks

module PT = LibExecution.ProgramTypes
open LibExecution.RuntimeTypes
module NR = LibParser.NameResolver

module C = LibCloud.SqlCompiler
module S = TestUtils.RTShortcuts
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

let p (code : string) : Task<Expr> =
  LibParser.Parser.parseRTExpr
    localBuiltIns
    packageManager
    NR.OnMissing.ThrowError
    "sqlcompiler.tests.fs"
    code
  |> Ply.toTask

let parse (fns : List<PackageFn.T>) (code : string) : Task<Expr> =
  LibParser.Parser.parseRTExpr
    localBuiltIns
    (PackageManager.withExtras packageManager [] [] fns)
    NR.OnMissing.ThrowError
    "sqlcompiler.tests.fs"
    code
  |> Ply.toTask

let compile
  (symtable : DvalMap)
  (paramName : string)
  ((rowName, rowType) : string * TypeReference)
  (expr : Expr)
  : Task<string * Map<string, SqlValue>> =
  task {
    let canvasID = System.Guid.NewGuid()

    let typeName : FQTypeName.Package =
      { owner = "Tests"; modules = []; name = "MyType" }
    let field : TypeDeclaration.RecordField = { name = rowName; typ = rowType }
    let typ : PackageType.T =
      { id = System.Guid.NewGuid()
        name = typeName
        declaration =
          { typeParams = []
            definition = TypeDeclaration.Record(NEList.singleton field) } }
    let typeReference = TCustomType(Ok(FQTypeName.Package typeName), [])

    let! state =
      executionStateFor
        (PackageManager.withExtras packageManager [ typ ] [] [])
        canvasID
        false
        false
        Map.empty

    let state = { state with symbolTable = symtable }

    try
      let! compiled = C.compileLambda state paramName typeReference expr

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
  testList
    "compile tests"
    [ testTask "compile true" {
        let! e = p "true"
        let! sql, args = compile Map.empty "value" ("myfield", TBool) e
        matchSql sql @"\(@([A-Z]+)\)" args [ Sql.bool true ]
      }
      testTask "compile field" {
        let! e = p "value.myfield"
        let! sql, args = compile Map.empty "value" ("myfield", TBool) e

        matchSql sql @"\(CAST\(data::jsonb->>'myfield' as bool\)\)" args []
      }
      testTask "check escaped fields" {
        let injection = "'; select * from user_data_v0 ;'field"

        let expr =
          S.eFn
            "equals"
            0
            []
            [ S.eFieldAccess (S.eVar "value") injection; (S.eStr "x") ]

        let! sql, args = compile Map.empty "value" (injection, TString) expr

        matchSql
          sql
          @"\(CAST\(data::jsonb->>'''; select * from user_data_v0;''field' as text\)\) = \('x'\)\)"
          args
          []
      }
      testTask "symtable values escaped" {
        let! expr = p "var == value.name"
        let symtable = Map.ofList [ "var", DString "';select * from user_data_v0;'" ]

        let! sql, args = compile symtable "value" ("name", TString) expr

        matchSql
          sql
          @"\(\(''';select * from user_data_v0;'''\) = \(CAST\(data::jsonb->>'name' as text\)\)\)"
          args
          []
      }
      testTask "pipes expand correctly into nested functions" {
        let! expr = p "value.age |> (-) 2L |> (+) value.age |> (<) 3L"
        let! sql, args = compile Map.empty "value" ("age", TInt64) expr

        matchSql
          sql
          @"\(\(\(\(CAST\(data::jsonb->>'age' as integer\)\) - \(2\)\) + \(CAST\(data::jsonb->>'age' as integer\)\)\) < \(3\)\)"
          args
          []
      } ]


let inlineWorksAtRoot =
  testTask "inlineWorksAtRoot" {
    let! state =
      executionStateFor packageManager (System.Guid.NewGuid()) false false Map.empty
    let fns = ExecutionState.availableFunctions state
    let! expr = p "let y = 5 in let x = 6 in (3 + (let x = 7 in y))"

    let! expected = p "3 + 5"
    let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
    return Expect.equalExprIgnoringIDs result expected
  }

let inlineWorksWithNested =
  testTask "inlineWorksWithNested" {
    let! state =
      executionStateFor packageManager (System.Guid.NewGuid()) false false Map.empty
    let fns = ExecutionState.availableFunctions state
    let! expr = p "let x = 5 in (let x = 6 in (3 + (let x = 7 in x)))"

    let! expected = p "3 + 7"
    let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
    return Expect.equalExprIgnoringIDs result expected
  }


let inlineWorksWithPackageFunctionsSqlBinOp =
  testTask "inlineWorksWithPackageFunctionsSqlBinOp" {
    let! state =
      executionStateFor packageManager (System.Guid.NewGuid()) false false Map.empty
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
      executionStateFor packageManager (System.Guid.NewGuid()) false false Map.empty
    let fns = ExecutionState.availableFunctions state
    let! expr =
      p
        """let x = "package" in (let y = "e" in (PACKAGE.Darklang.Stdlib.String.replaceAll_v0 x y "es"))"""

    let! expected = p """Builtin.stringReplaceAll_v0 "package" "e" "es" """
    let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
    return Expect.equalExprIgnoringIDs result expected
  }


let inlineFunctionArguments =
  testTask "inlineWorksArguments" {
    let fnBody =
      PT.EInfix(
        0UL,
        PT.InfixFnCall(PT.ArithmeticPlus),
        PT.EVariable(0UL, "a"),
        PT.EVariable(0UL, "b")
      )

    let (fn : PackageFn.T) =
      testPackageFn "Tests" "userAdd" [] (NEList.doubleton "a" "b") PT.TInt64 fnBody
      |> PT2RT.PackageFn.toRT

    let fns : Functions =
      { builtIn = localBuiltIns.fns; package = fun _name -> Ply(Some fn) }

    let! expr =
      parse [ fn ] "let a = 1 in let b = 9 in (p.height == Tests.userAdd 6 (b - 4))"

    let! expected = parse [ fn ] "p.height == 6 + (9 - 4)"
    let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
    return Expect.equalExprIgnoringIDs result expected
  }


let partialEvaluation =
  testManyTask
    "partialEvaluate"
    (fun (expr, vars) ->
      task {
        let canvasID = System.Guid.NewGuid()
        let! state = executionStateFor packageManager canvasID false false Map.empty
        let state = { state with symbolTable = Map vars }
        let! expr = p expr
        let result = C.partiallyEvaluate state "x" expr
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
      (("myVar || myVar2", [ "myVar", DBool true; "myVar2", DBool true ]), DBool true) ]


let tests =
  testList
    "SqlCompiler"
    [ inlineWorksAtRoot
      inlineWorksWithNested
      inlineWorksWithPackageFunctionsSqlBinOp
      inlineWorksWithPackageFunctionsSqlFunction
      inlineFunctionArguments
      partialEvaluation
      compileTests ]
