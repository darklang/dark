module Tests.SqlCompiler

open Expecto
open Prelude
open TestUtils.TestUtils

// open System.Threading.Tasks
// open FSharp.Control.Tasks

// module PT = LibExecution.ProgramTypes
// open LibExecution.RuntimeTypes

// module C = LibCloud.SqlCompiler
// module S = TestUtils.RTShortcuts
// module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
// module PT2DT = LibExecution.ProgramTypesToDarkTypes
// module C2DT = LibExecution.CommonToDarkTypes
// module PackageHashes = LibExecution.PackageHashes

// let pmPT = LibCloud.PackageManager.pt

// let parse (fns : List<PT.PackageFn.PackageFn>) (code : string) : Task<Expr> =
//   uply {
//     let pm = PT.PackageManager.withExtras pmPT [] [] fns
//     let! (state : ExecutionState) =
//       let canvasID = System.Guid.NewGuid()
//       executionStateFor pm canvasID false false Map.empty

//     let name =
//       FQFnName.FQFnName.Package PackageHashes.Fn.LanguageTools.Parser.parsePTExpr

//     let args = NEList.singleton (DString code)
//     let! execResult = LibExecution.Execution.executeFunction state name [] args

//     match execResult with
//     | Ok dval ->
//       match C2DT.Result.fromDT PT2DT.Expr.fromDT dval identity with
//       | Ok expr -> return expr |> PT2RT.Expr.toRT
//       | Error _ ->
//         return Exception.raiseInternal "Error converting Dval to PT.Expr" []
//     | _ -> return Exception.raiseInternal "Error executing parsePTExpr function" []
//   }
//   |> Ply.toTask

// let compile
//   (symtable : DvalMap)
//   (paramName : string)
//   ((rowName, rowType) : string * PT.TypeReference)
//   (expr : Expr)
//   : Task<string * Map<string, SqlValue>> =
//   task {
//     let canvasID = System.Guid.NewGuid()

//     let typeID = System.Guid.Parse "56d0445a-c273-4dfd-b9a6-3b7c63f92479"
//     let typeName = FQTypeName.Package typeID
//     let field : PT.TypeDeclaration.RecordField =
//       { name = rowName; typ = rowType; description = "" }
//     let typ : PT.PackageType.PackageType =
//       { id = typeID
//         name = { owner = "Tests"; modules = []; name = "MyType" }
//         description = ""
//         deprecated = PT.NotDeprecated
//         declaration =
//           { typeParams = []
//             definition = PT.TypeDeclaration.Record(NEList.singleton field) } }
//     let typeReference = TCustomType(Ok typeName, [])

//     let! state =
//       executionStateFor
//         (PT.PackageManager.withExtras pmPT [ typ ] [] [])
//         canvasID
//         false
//         false
//         Map.empty

//     let state = { state with symbolTable = symtable }

//     try
//       let! compiled = C.compileLambda state paramName typeReference expr

//       match compiled with
//       | Ok compiled ->
//         let vars = Map.ofList compiled.vars
//         return compiled.sql, vars
//       | Error err ->
//         return
//           Exception.raiseInternal "could not compile lambda to sql" [ "err", err ]

//     with e ->
//       return
//         Exception.raiseInternal e.Message [ "paramName", paramName; "expr", expr ]
//   }

// let matchSql
//   (sql : string)
//   (pattern : string)
//   (args : Map<string, SqlValue>)
//   (expected : List<SqlValue>)
//   =
//   Expect.isMatchGroups
//     sql
//     pattern
//     (fun g ->
//       match g.Count with // implicit full match counts for 1
//       | 1 -> true
//       | 2 -> Map.findUnsafe g[1].Value args = expected[0]
//       | _ -> Exception.raiseInternal "not supported yet" [ "count", g.Count ])
//     "compare sql"

// let compileTests =
//   testList
//     "compile tests"
//     [ testTask "compile true" {
//         let! e = parse [] "true"
//         let! sql, args = compile Map.empty "value" ("myfield", PT.TBool) e
//         matchSql sql @"\(@([A-Z]+)\)" args [ Sql.bool true ]
//       }
//       testTask "compile field" {
//         let! e = parse [] "value.myfield"
//         let! sql, args = compile Map.empty "value" ("myfield", PT.TBool) e

//         matchSql sql @"\(CAST\(data::jsonb->>'myfield' as bool\)\)" args []
//       }
//       testTask "check escaped fields" {
//         let injection = "'; select * from user_data_v0 ;'field"

//         let expr =
//           S.eFn
//             "equals"
//             0
//             []
//             [ S.eFieldAccess (S.eVar "value") injection; (S.eStr "x") ]

//         let! sql, args = compile Map.empty "value" (injection, PT.TString) expr

//         matchSql
//           sql
//           @"\(CAST\(data::jsonb->>'''; select * from user_data_v0;''field' as text\)\) = \('x'\)\)"
//           args
//           []
//       }
//       testTask "symtable values escaped" {
//         let! expr = parse [] "var == value.name"
//         let symtable = Map.ofList [ "var", DString "';select * from user_data_v0;'" ]

//         let! sql, args = compile symtable "value" ("name", PT.TString) expr

//         matchSql
//           sql
//           @"\(\(''';select * from user_data_v0;'''\) = \(CAST\(data::jsonb->>'name' as text\)\)\)"
//           args
//           []
//       }
//       testTask "pipes expand correctly into nested functions" {
//         let! expr = parse [] "value.age |> (-) 2L |> (+) value.age |> (<) 3L"
//         let! sql, args = compile Map.empty "value" ("age", PT.TInt64) expr

//         matchSql
//           sql
//           @"\(\(\(\(CAST\(data::jsonb->>'age' as integer\)\) - \(2\)\) + \(CAST\(data::jsonb->>'age' as integer\)\)\) < \(3\)\)"
//           args
//           []
//       } ]


// let inlineWorksAtRoot =
//   testTask "inlineWorksAtRoot" {
//     let! state = executionStateFor pmPT (System.Guid.NewGuid()) false false Map.empty
//     let fns = ExecutionState.availableFunctions state
//     let! expr = parse [] "let y = 5L\nlet x = 6L\n(3L + (let x = 7L\ny))"

//     let! expected = parse [] "3L + 5L"
//     let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
//     return Expect.equalExprIgnoringIDs result expected
//   }

// let inlineWorksWithNested =
//   testTask "inlineWorksWithNested" {
//     let! state = executionStateFor pmPT (System.Guid.NewGuid()) false false Map.empty
//     let fns = ExecutionState.availableFunctions state
//     let! expr = parse [] "let x = 5L\n(let x = 6L\n(3L + (let x = 7L\nx)))"

//     let! expected = parse [] "3L + 7L"
//     let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
//     return Expect.equalExprIgnoringIDs result expected
//   }


// let inlineWorksWithPackageFunctionsSqlBinOp =
//   testTask "inlineWorksWithPackageFunctionsSqlBinOp" {
//     let! state = executionStateFor pmPT (System.Guid.NewGuid()) false false Map.empty
//     let fns = ExecutionState.availableFunctions state
//     let! expr =
//       parse
//         []
//         "let x = true\n(let y = false\n(Darklang.Stdlib.Bool.and x y))"

//     let! expected = parse [] "true && false"
//     let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
//     return Expect.equalExprIgnoringIDs result expected
//   }


// let inlineWorksWithPackageFunctionsSqlFunction =
//   testTask "inlineWorksWithPackageFunctionsSqlFunction" {
//     let! state = executionStateFor pmPT (System.Guid.NewGuid()) false false Map.empty
//     let fns = ExecutionState.availableFunctions state
//     let! expr =
//       parse
//         []
//         """let x = "package"
// (let y = "e"
// (Darklang.Stdlib.String.replaceAll x y "es"))"""

//     let! expected = parse [] """Builtin.stringReplaceAll "package" "e" "es" """
//     let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
//     return Expect.equalExprIgnoringIDs result expected
//   }


// let inlineFunctionArguments =
//   testTask "inlineWorksArguments" {
//     let fnBody =
//       PT.EInfix(
//         0UL,
//         PT.InfixFnCall(PT.ArithmeticPlus),
//         PT.EVariable(0UL, "a"),
//         PT.EVariable(0UL, "b")
//       )

//     let (fn : PT.PackageFn.PackageFn) =
//       testPackageFn "Tests" "userAdd" [] (NEList.doubleton "a" "b") PT.TInt64 fnBody

//     let fns : Functions =
//       { builtIn = (localBuiltIns pmPT).fns
//         package = fun _name -> Ply(Some(PT2RT.PackageFn.toRT fn)) }

//     let! expr =
//       parse [ fn ] "let a = 1L\nlet b = 9L\n(p.height == (Tests.userAdd 6L (b - 4L)))"

//     let! expected = parse [ fn ] "p.height == 6L + (9L - 4L)"
//     let! result = (C.inline' fns "value" Map.empty expr) |> Ply.toTask
//     return Expect.equalExprIgnoringIDs result expected
//   }


// let partialEvaluation =
//   testManyTask
//     "partialEvaluate"
//     (fun (expr, vars) ->
//       task {
//         let canvasID = System.Guid.NewGuid()
//         let! state = executionStateFor pmPT canvasID false false Map.empty
//         let state = { state with symbolTable = Map vars }
//         let! expr = parse [] expr
//         let result = C.partiallyEvaluate state "x" expr
//         let! (dvals, result) = Ply.TplPrimitives.runPlyAsTask result
//         match result with
//         | EVariable(_, name) -> return (Map.findUnsafe name dvals)
//         | _ ->
//           Expect.isTrue false "didn't match"
//           return DUnit
//       })
//     [ (("false && false", []), DBool false)
//       (("false && true", []), DBool false)
//       (("true && false", []), DBool false)
//       (("true && true", []), DBool true)
//       (("false && myVar", [ "myVar", DBool false ]), DBool false)
//       (("false && myVar", [ "myVar", DBool true ]), DBool false)
//       (("true && myVar", [ "myVar", DBool false ]), DBool false)
//       (("true && myVar", [ "myVar", DBool true ]), DBool true)
//       (("myVar && false", [ "myVar", DBool false ]), DBool false)
//       (("myVar && true", [ "myVar", DBool false ]), DBool false)
//       (("myVar && true", [ "myVar", DBool true ]), DBool true)
//       (("myVar && true", [ "myVar", DBool true ]), DBool true)
//       (("myVar && myVar2", [ "myVar", DBool false; "myVar2", DBool false ]),
//        DBool false)
//       (("myVar && myVar2", [ "myVar", DBool false; "myVar2", DBool true ]),
//        DBool false)
//       (("myVar && myVar2", [ "myVar", DBool true; "myVar2", DBool false ]),
//        DBool false)
//       (("myVar && myVar2", [ "myVar", DBool true; "myVar2", DBool true ]), DBool true)

//       // same as above but for ||
//       (("false || false", []), DBool false)
//       (("false || true", []), DBool true)
//       (("true || false", []), DBool true)
//       (("true || true", []), DBool true)
//       (("false || myVar", [ "myVar", DBool false ]), DBool false)
//       (("false || myVar", [ "myVar", DBool true ]), DBool true)
//       (("true || myVar", [ "myVar", DBool false ]), DBool true)
//       (("true || myVar", [ "myVar", DBool true ]), DBool true)
//       (("myVar || false", [ "myVar", DBool false ]), DBool false)
//       (("myVar || true", [ "myVar", DBool false ]), DBool true)
//       (("myVar || true", [ "myVar", DBool true ]), DBool true)
//       (("myVar || true", [ "myVar", DBool true ]), DBool true)
//       (("myVar || myVar2", [ "myVar", DBool false; "myVar2", DBool false ]),
//        DBool false)
//       (("myVar || myVar2", [ "myVar", DBool false; "myVar2", DBool true ]),
//        DBool true)
//       (("myVar || myVar2", [ "myVar", DBool true; "myVar2", DBool false ]),
//        DBool true)
//       (("myVar || myVar2", [ "myVar", DBool true; "myVar2", DBool true ]), DBool true) ]


let tests =
  testList
    "SqlCompiler"
    [
    // inlineWorksAtRoot
    // inlineWorksWithNested
    // inlineWorksWithPackageFunctionsSqlBinOp
    // inlineWorksWithPackageFunctionsSqlFunction
    // inlineFunctionArguments
    // partialEvaluation
    // compileTests
    ]
