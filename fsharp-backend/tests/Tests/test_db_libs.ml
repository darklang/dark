let t_sql_compiler_works () =
  let open Types in
  let open Prelude in
  let _, state, _ = Utils.test_execution_data [] in
  let check
      (msg : string)
      ?(paramName = "value")
      ?(dbFields = [])
      ?(symtable = [])
      (body : fluid_expr)
      (generated : string) : unit =
    let dbFields = StrDict.from_list dbFields in
    let symtable = StrDict.from_list symtable in
    let result =
      Sql_compiler.compile_lambda ~state symtable paramName dbFields body
    in
    AT.check AT.string msg result generated
  in
  let checkError
      (msg : string)
      ?(paramName = "value")
      ?(dbFields = [])
      ?(symtable = [])
      (body : fluid_expr)
      (expectedError : string) : unit =
    try check msg ~paramName ~dbFields ~symtable body "<error expected>"
    with Db.DBQueryException e -> AT.check AT.string msg e expectedError
  in
  let true' = bool true in
  check "true is true" true' "(true)" ;
  let field = fieldAccess (var "value") "myfield" in
  check
    "correct SQL for field access"
    ~dbFields:[("myfield", TBool)]
    field
    "(CAST(data::jsonb->>'myfield' as bool))" ;
  checkError
    "no field gives error"
    field
    "The datastore does not have a field named: myfield" ;
  let injection = "'; select * from user_data ;'field" in
  let field = binop "==" (fieldAccess (var "value") injection) (str "x") in
  check
    "field accesses are escaped"
    ~dbFields:[(injection, TStr)]
    field
    "((CAST(data::jsonb->>'''; select * from user_data ;''field' as text)) = ('x'))" ;
  let variable = binop "==" (var "var") (fieldAccess (var "value") "name") in
  check
    "symtable escapes correctly"
    ~dbFields:[("name", TStr)]
    ~symtable:[("var", Dval.dstr_of_string_exn "';select * from user_data;'")]
    variable
    "((''';select * from user_data;''') = (CAST(data::jsonb->>'name' as text)))" ;
  let thread =
    pipe
      (fieldAccess (var "value") "age")
      [ binop "-" pipeTarget (int 2)
      ; binop "+" pipeTarget (fieldAccess (var "value") "age")
      ; binop "<" pipeTarget (int 3) ]
  in
  check
    ~dbFields:[("age", TInt)]
    "pipes expand correctly into nested functions"
    thread
    "((((CAST(data::jsonb->>'age' as integer)) - (2)) + (CAST(data::jsonb->>'age' as integer))) < (3))" ;
  check_fluid_expr
    "canonicalize works on pipes with targets"
    (Sql_compiler.canonicalize thread)
    (binop
       "<"
       (binop
          "+"
          (binop "-" (fieldAccess (var "value") "age") (int 2))
          (fieldAccess (var "value") "age"))
       (int 3)) ;
  check_fluid_expr
    "inline works (with nested inlines)"
    (let expr =
       let'
         "x"
         (int 5)
         (let' "x" (int 6) (binop "+" (int 3) (let' "x" (int 7) (var "x"))))
     in
     Sql_compiler.inline "value" StrDict.empty expr)
    (binop "+" (int 3) (int 7)) ;
  check_fluid_expr
    "inline works (def at root)"
    (let expr =
       let'
         "y"
         (int 5)
         (let' "x" (int 6) (binop "+" (int 3) (let' "x" (int 7) (var "y"))))
     in
     Sql_compiler.inline "value" StrDict.empty expr)
    (binop "+" (int 3) (int 5)) ;
  ()


let t_db_query_works () =
  check_dval
    "date::lessThanOrEquals"
    (DList [rachel; chandler; ross])
    ( queryv
        (binop
           "Date::<="
           (field "v" "dob")
           (fn "Date::parse" [str "2000-01-01T01:02:03Z"]))
    |> execs ) ;
  check_dval
    "date::lessThanOrEquals - equality"
    (DList [ross])
    ( queryv (binop "Date::<=" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "date::lessThan - equality"
    (DList [])
    ( queryv (binop "Date::<" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "date::greaterThanOrEquals"
    (DList [cat])
    ( queryv
        (binop
           "Date::>="
           (field "v" "dob")
           (fn "Date::parse" [str "2000-01-01T01:02:03Z"]))
    |> execs ) ;
  check_dval
    "date::greaterThanOrEquals - equality"
    (DList [cat; rachel; chandler; ross])
    ( queryv (binop "Date::>=" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "date::greaterThan - equality"
    (DList [cat; rachel; chandler])
    ( queryv (binop "Date::>" (field "v" "dob") (fn "Date::parse" [ross_dob]))
    |> execs ) ;
  check_dval
    "Date::add"
    (DList [cat; rachel; chandler; ross])
    ( queryv
        (binop
           "Date::<="
           (field "v" "dob")
           (fn "Date::add" [fn "Date::now" []; int 1]))
    |> execs ) ;
  check_dval
    "Date::subtract"
    (DList [cat; rachel; chandler; ross])
    ( queryv
        (binop
           "Date::<="
           (field "v" "dob")
           (fn "Date::subtract" [fn "Date::now" []; int 1]))
    |> execs ) ;
  check_dval
    "string::trim"
    (DList [chandler])
    ( queryv (binop "==" (fn "String::trim" [field "v" "name"]) (str "Chandler"))
    |> execs ) ;
  check_dval
    "string::trimStart"
    (DList [chandler])
    ( queryv
        (binop
           "=="
           (fn "String::trimStart" [field "v" "name"])
           (str "Chandler "))
    |> execs ) ;
  check_dval
    "string::trimEnd"
    (DList [chandler])
    ( queryv
        (binop "==" (fn "String::trimEnd" [field "v" "name"]) (str " Chandler"))
    |> execs ) ;
  check_dval
    "date::hour"
    (DList [chandler])
    ( queryv (binop "==" (fn "Date::hour_v1" [field "v" "dob"]) (int 10))
    |> execs ) ;
  check_dval
    "date::atStartOfDay"
    (DList [chandler])
    ( queryv
        (binop
           "=="
           (fn "Date::atStartOfDay" [field "v" "dob"])
           (fn "Date::parse" [str "1969-08-19T00:00:00Z"]))
    |> execs ) ;
  check_dval
    "date::day"
    (DList [chandler])
    (queryv (binop "==" (fn "Date::day" [field "v" "dob"]) (int 19)) |> execs) ;
  check_dval
    "date::minute"
    (DList [chandler])
    (queryv (binop "==" (fn "Date::minute" [field "v" "dob"]) (int 13)) |> execs) ;
  check_dval
    "date::month"
    (DList [chandler])
    (queryv (binop "==" (fn "Date::month" [field "v" "dob"]) (int 8)) |> execs) ;
  check_dval
    "date::second"
    (DList [chandler])
    (queryv (binop "==" (fn "Date::second" [field "v" "dob"]) (int 42)) |> execs) ;
  check_dval
    "date::year"
    (DList [rachel; chandler])
    (queryv (binop "==" (fn "Date::year" [field "v" "dob"]) (int 1969)) |> execs) ;
  (* -------------- *)
  (* Test partial evaluation *)
  (* -------------- *)
  check_dval
    "partial evaluation - fieldAccesses outside query"
    (DList [cat; rachel; chandler])
    ( let'
        "x"
        (record [("y", record [("z", record [("a", int 5)])])])
        (queryv
           (binop
              "<"
              (fieldAccess (fieldAccess (fieldAccess (var "x") "y") "z") "a")
              (fn "String::length" [field "v" "name"])))
    |> execs ) ;
  check_dval
    "partial evaluation - fieldAccesses"
    (DList [cat; rachel; chandler])
    ( queryv
        (let'
           "x"
           (record [("y", record [("z", record [("a", int 5)])])])
           (binop
              "<"
              (fieldAccess (fieldAccess (fieldAccess (var "x") "y") "z") "a")
              (fn "String::length" [field "v" "name"])))
    |> execs ) ;
  check_dval
    "partial execution - List::length"
    (DList [cat; rachel; chandler])
    ( queryv
        (let'
           "x"
           (record
              [ ( "y"
                , record
                    [ ( "z"
                      , record [("a", list [int 1; int 2; int 3; int 4; int 5])]
                      ) ] ) ])
           (binop
              "<"
              (pipe
                 (fieldAccess (fieldAccess (fieldAccess (var "x") "y") "z") "a")
                 [fn "List::length" [pipeTarget]])
              (fn "String::length" [field "v" "name"])))
    |> execs ) ;
  ()
