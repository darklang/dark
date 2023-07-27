module Tests.SqlCompiler

open Expecto
open Prelude
open TestUtils.TestUtils

open System.Threading.Tasks
open FSharp.Control.Tasks

module PT = LibExecution.ProgramTypes
open LibExecution.RuntimeTypes

module C = LibBackend.SqlCompiler
module S = TestUtils.RTShortcuts
module Errors = LibExecution.Errors

let p (code : string) : Expr =
  Parser.Parser.parseRTExpr builtinResolver "sqlcompiler.tests.fs" code

let compile
  (symtable : DvalMap)
  (paramName : string)
  ((rowName, rowType) : string * TypeReference)
  (expr : Expr)
  : Task<string * Map<string, SqlValue>> =
  task {
    let canvasID = System.Guid.NewGuid()

    let typeName : TypeName.UserProgram =
      { modules = []; name = TypeName.TypeName "MyType"; version = 0 }
    let field : TypeDeclaration.RecordField =
      { name = rowName; typ = rowType; description = "" }
    let userType : UserType.T =
      { tlid = gid ()
        name = typeName
        declaration =
          { typeParams = []; definition = TypeDeclaration.Record(field, []) } }
    let userTypes = Map [ typeName, userType ]
    let typeReference = TCustomType(FQName.UserProgram typeName, [])

    let! state = executionStateFor canvasID false false Map.empty userTypes Map.empty

    try
      let! sql, args = C.compileLambda state symtable paramName typeReference expr

      let args = Map.ofList args
      return sql, args
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
      | 2 -> Map.find g[1].Value args = expected[0]
      | _ -> Exception.raiseInternal "not supported yet" [ "count", g.Count ])
    "compare sql"

let compileTests =

  testList
    "compile tests"
    [ testTask "compile true" {
        let! sql, args = compile Map.empty "value" ("myfield", TBool) (p "true")
        matchSql sql @"\(@([A-Z]+)\)" args [ Sql.bool true ]
      }
      testTask "compile field" {
        let! sql, args =
          compile Map.empty "value" ("myfield", TBool) (p "value.myfield")

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

        let! sql, args = compile Map.empty "value" (injection, TString) expr

        matchSql
          sql
          @"\(CAST\(data::jsonb->>'''; select * from user_data_v0;''field' as text\)\) = \('x'\)\)"
          args
          []
      }
      testTask "symtable values escaped" {
        let expr = p "var == value.name"
        let symtable = Map.ofList [ "var", DString "';select * from user_data_v0;'" ]

        let! sql, args = compile symtable "value" ("name", TString) expr

        matchSql
          sql
          @"\(\(''';select * from user_data_v0;'''\) = \(CAST\(data::jsonb->>'name' as text\)\)\)"
          args
          []
      }
      testTask "pipes expand correctly into nested functions" {
        let expr = p "value.age |> (-) 2 |> (+) value.age |> (<) 3"
        let! sql, args = compile Map.empty "value" ("age", TInt) expr

        matchSql
          sql
          @"\(\(\(\(CAST\(data::jsonb->>'age' as integer\)\) - \(2\)\) + \(CAST\(data::jsonb->>'age' as integer\)\)\) < \(3\)\)"
          args
          []
      } ]


let inlineWorksAtRoot =
  test "inlineWorksAtRoot" {
    let expr =
      Parser.Parser.parseRTExpr
        builtinResolver
        "test.fs"
        "let y = 5 in let x = 6 in (3 + (let x = 7 in y))"

    let expected = p "3 + 5"
    let result = C.inline' "value" Map.empty expr
    Expect.equalExprIgnoringIDs result expected
  }

let inlineWorksWithNested =
  test "inlineWorksWithNested" {
    let expr = p "let x = 5 in (let x = 6 in (3 + (let x = 7 in x)))"

    let expected = p "3 + 7"
    let result = C.inline' "value" Map.empty expr
    Expect.equalExprIgnoringIDs result expected
  }

let partialEvaluation =
  testManyTask
    "partialEvaluate"
    (fun (expr, vars) ->
      task {
        let canvasID = System.Guid.NewGuid()
        let! state =
          executionStateFor canvasID false false Map.empty Map.empty Map.empty
        let expr = p expr
        let result = C.partiallyEvaluate state "x" (Map vars) expr
        let! (dvals, result) = Ply.TplPrimitives.runPlyAsTask result
        match result with
        | EVariable(_, name) -> return (Map.find name dvals)
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
    [ inlineWorksAtRoot; inlineWorksWithNested; partialEvaluation; compileTests ]
