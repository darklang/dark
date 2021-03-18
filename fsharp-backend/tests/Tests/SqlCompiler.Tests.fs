module Tests.SqlCompiler

open Expecto
open Prelude
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes

module C = LibBackend.SqlCompiler


// let t_sql_compiler_works () =
//   let open Types in
//   let open Prelude in
//   let _, state, _ = Utils.test_execution_data [] in
//   let check
//       (msg : string)
//       ?(paramName = "value")
//       ?(dbFields = [])
//       ?(symtable = [])
//       (body : fluid_expr)
//       (generated : string) : unit =
//     let dbFields = StrDict.from_list dbFields in
//     let symtable = StrDict.from_list symtable in
//     let result =
//       Sql_compiler.compile_lambda ~state symtable paramName dbFields body
//     in
//     AT.check AT.string msg result generated
//   in
//   let checkError
//       (msg : string)
//       ?(paramName = "value")
//       ?(dbFields = [])
//       ?(symtable = [])
//       (body : fluid_expr)
//       (expectedError : string) : unit =
//     try check msg ~paramName ~dbFields ~symtable body "<error expected>"
//     with Db.DBQueryException e -> AT.check AT.string msg e expectedError
//   in
//   let true' = bool true in
//   check "true is true" true' "(true)" ;
//   let field = fieldAccess (var "value") "myfield" in
//   check
//     "correct SQL for field access"
//     ~dbFields:[("myfield", TBool)]
//     field
//     "(CAST(data::jsonb->>'myfield' as bool))" ;
//   checkError
//     "no field gives error"
//     field
//     "The datastore does not have a field named: myfield" ;
//   let injection = "'; select * from user_data ;'field" in
//   let field = binop "==" (fieldAccess (var "value") injection) (str "x") in
//   check
//     "field accesses are escaped"
//     ~dbFields:[(injection, TStr)]
//     field
//     "((CAST(data::jsonb->>'''; select * from user_data ;''field' as text)) = ('x'))" ;
//   let variable = binop "==" (var "var") (fieldAccess (var "value") "name") in
//   check
//     "symtable escapes correctly"
//     ~dbFields:[("name", TStr)]
//     ~symtable:[("var", Dval.dstr_of_string_exn "';select * from user_data;'")]
//     variable
//     "((''';select * from user_data;''') = (CAST(data::jsonb->>'name' as text)))" ;
//   let thread =
//     pipe
//       (fieldAccess (var "value") "age")
//       [ binop "-" pipeTarget (int 2)
//       ; binop "+" pipeTarget (fieldAccess (var "value") "age")
//       ; binop "<" pipeTarget (int 3) ]
//   in
//   check
//     ~dbFields:[("age", TInt)]
//     "pipes expand correctly into nested functions"
//     thread
//     "((((CAST(data::jsonb->>'age' as integer)) - (2)) + (CAST(data::jsonb->>'age' as integer))) < (3))" ;
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


let tests = testList "SqlCompiler" [ inlineWorksAtRoot; inlineWorksWithNested ]
