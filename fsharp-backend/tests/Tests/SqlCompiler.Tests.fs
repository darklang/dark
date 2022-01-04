module Tests.SqlCompiler

open Expecto
open Prelude
open TestUtils.TestUtils

open System.Threading.Tasks
open FSharp.Control.Tasks

module PT = LibExecution.ProgramTypes
open LibExecution.RuntimeTypes

module C = LibBackend.SqlCompiler
module S = LibExecution.Shortcuts

let compile
  (symtable : DvalMap)
  (paramName : string)
  (dbFields : List<string * DType>)
  (expr : Expr)
  : Task<string * Map<string, SqlValue>> =
  task {
    let! owner = testOwner.Force()
    let! state = executionStateFor owner "test" Map.empty Map.empty

    try
      let! sql, args =
        C.compileLambda state symtable paramName (Map.ofList dbFields) expr

      let args = Map.ofList args
      return sql, args
    with
    | LibExecution.Errors.DBQueryException msg as e ->
      Exception.raiseInternal msg [ "paramName", paramName; "expr", expr ]
      return ("", Map.empty)
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

  let p code = FSharpToExpr.parseRTExpr code

  testList
    "compile tests"
    [ testTask "compile true" {
        let! sql, args = compile Map.empty "value" [] (p "true")
        matchSql sql @"\(@([A-Z]+)\)" args [ Sql.bool true ]
      }
      testTask "compile field" {
        let! sql, args =
          compile Map.empty "value" [ "myfield", TBool ] (p "value.myfield")

        matchSql sql @"\(CAST\(data::jsonb->>'myfield' as bool\)\)" args []
      }
      testTask "check escaped fields" {
        let injection = "'; select * from user_data ;'field"

        let expr =
          S.eApply
            (S.eStdFnVal "" "==" 0)
            [ S.eFieldAccess (S.eVar "value") injection; (S.eStr "x") ]

        let! sql, args = compile Map.empty "value" [ injection, TStr ] expr

        matchSql
          sql
          @"\(CAST\(data::jsonb->>'''; select * from user_data ;''field' as text\)\) = \('x'\)\)"
          args
          []
      }
      testTask "symtable values escaped" {
        let expr = p "var == value.name"
        let symtable = Map.ofList [ "var", DStr "';select * from user_data;'" ]

        let! sql, args = compile symtable "value" [ "name", TStr ] expr

        matchSql
          sql
          @"\(\(''';select * from user_data;'''\) = \(CAST\(data::jsonb->>'name' as text\)\)\)"
          args
          []
      }
      testTask "pipes expand correctly into nested functions" {
        let expr = p "value.age |> (-) 2 |> (+) value.age |> (<) 3"
        let! sql, args = compile Map.empty "value" [ "age", TInt ] expr

        matchSql
          sql
          @"\(\(\(\(CAST\(data::jsonb->>'age' as integer\)\) - \(2\)\) + \(CAST\(data::jsonb->>'age' as integer\)\)\) < \(3\)\)"
          args
          []
      } ]


let inlineWorksAtRoot =
  test "inlineWorksAtRoot" {
    let expr =
      FSharpToExpr.parseRTExpr "let y = 5 in let x = 6 in (3 + (let x = 7 in y))"

    let expected = FSharpToExpr.parseRTExpr "3 + 5"
    let result = C.inline' "value" Map.empty expr
    Expect.equalExprIgnoringIDs result expected
  }

let inlineWorksWithNested =
  test "inlineWorksWithNested" {
    let expr =
      FSharpToExpr.parseRTExpr "let x = 5 in (let x = 6 in (3 + (let x = 7 in x)))"

    let expected = FSharpToExpr.parseRTExpr "3 + 7"
    let result = C.inline' "value" Map.empty expr
    Expect.equalExprIgnoringIDs result expected
  }


let tests =
  testList "SqlCompiler" [ inlineWorksAtRoot; inlineWorksWithNested; compileTests ]
