module Tests.SqlCompiler

open Expecto
open Prelude
open TestUtils

module PT = LibBackend.ProgramTypes
module RT = LibExecution.RuntimeTypes


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
//   check_fluid_expr
//     "canonicalize works on pipes with targets"
//     (Sql_compiler.canonicalize thread)
//     (binop
//        "<"
//        (binop
//           "+"
//           (binop "-" (fieldAccess (var "value") "age") (int 2))
//           (fieldAccess (var "value") "age"))
//        (int 3)) ;
//   check_fluid_expr
//     "inline works (with nested inlines)"
//     (let expr =
//        let'
//          "x"
//          (int 5)
//          (let' "x" (int 6) (binop "+" (int 3) (let' "x" (int 7) (var "x"))))
//      in
//      Sql_compiler.inline "value" StrDict.empty expr)
//     (binop "+" (int 3) (int 7)) ;
//   check_fluid_expr
//     "inline works (def at root)"
//     (let expr =
//        let'
//          "y"
//          (int 5)
//          (let' "x" (int 6) (binop "+" (int 3) (let' "x" (int 7) (var "y"))))
//      in
//      Sql_compiler.inline "value" StrDict.empty expr)
//     (binop "+" (int 3) (int 5)) ;
//   ()

let tests = testList "SqlCompiler" []