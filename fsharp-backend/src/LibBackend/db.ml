
(* ------------------------- *)
(* All commands go through here, does logging and such *)
(* ------------------------- *)
let execute
    ~name
    ~op
    ~params
    ~result
    ?subject
    ~(f :
          ?params:string array
       -> ?binary_params:bool array
       -> ?binary_result:bool
       -> string
       -> Postgresql.result)
    ~(r : Postgresql.result -> string * 'a)
    (sql : string) : 'a =
  let start = Unix.gettimeofday () in
  let time () =
    let finish = Unix.gettimeofday () in
    (finish -. start) *. 1000.0
  in
  let binary_result = result = BinaryResult in
  let binary_params = params |> List.map ~f:to_binary_bool |> Array.of_list in
  let string_params = params |> List.map ~f:to_sql |> Array.of_list in
  let subject_log =
    match subject with Some str -> [("subject", str)] | None -> []
  in
  try
    let res = f ~binary_params ~binary_result ~params:string_params sql in
    (* Transform and log the result *)
    let logr, result = r res in
    let result_log = if logr = "" then [] else [("result", logr)] in
    Log.succesS name ~params:([("op", op)] @ result_log @ subject_log) ;
    result
  with e ->
    let bt = Exception.get_backtrace () in
    let log_string = params |> List.map ~f:to_log |> String.concat ~sep:", " in
    Log.erroR name ~params:[("op", op); ("params", log_string); ("query", sql)] ;
    let msg =
      match e with
      | Postgresql.Error (Unexpected_status (_, msg, _)) ->
          msg
      | Postgresql.Error pge ->
          Postgresql.string_of_error pge
      | Exception.DarkException de ->
          Log.erroR
            ~bt
            "Caught DarkException in DB, reraising"
            ~params:
              [ ( "exn"
                , Exception.exception_data_to_yojson de |> Yojson.Safe.to_string
                )
              ; ("query_name", name)
              ; ("sql", sql) ] ;
          Caml.Printexc.raise_with_backtrace e bt
      | e ->
          Exception.exn_to_string e
    in
    Exception.storage
      msg
      ~bt
      ~info:
        [ ("time", time () |> string_of_float)
        ; ("query_name", name)
        ; ("sql", sql) ]


(* largely cribbed from
 * https://github.com/mmottl/postgresql-ocaml/blob/master/examples/cursor.ml *)
(* we could make this accumulate the results of the function, but ... for
       * now, iter is enough *)
let iter_with_cursor
    ~name
    ~params
    ?(result = TextResult)
    ~(f : string list -> unit)
    (sql : string) : unit =
  let start = Unix.gettimeofday () in
  let time () =
    let finish = Unix.gettimeofday () in
    (finish -. start) *. 1000.0
  in
  let subject = Some name in
  let binary_result = result = BinaryResult in
  ( try
      let cursor_name = "my_cursor" in
      let binary_params =
        params |> List.map ~f:to_binary_bool |> Array.of_list
      in
      let string_params = params |> List.map ~f:to_sql |> Array.of_list in
      ignore (conn#exec ~expect:[PG.Command_ok] "BEGIN") ;
      ignore
        (conn#exec
           ~expect:[PG.Command_ok]
           ~binary_params
           ~params:string_params
           ("DECLARE " ^ cursor_name ^ " CURSOR FOR " ^ sql)) ;
      let rec loop () =
        let res =
          conn#exec
            ~binary_result
            ~expect:[PG.Tuples_ok]
            ("FETCH IN " ^ cursor_name)
        in
        if res#ntuples <> 0
        then (
          let lst = res#get_tuple_lst 0 in
          f lst ;
          loop () )
      in
      loop () ;
      ignore (conn#exec ~expect:[PG.Command_ok] "CLOSE my_cursor") ;
      ignore (conn#exec ~expect:[PG.Command_ok] "END") ;
      let subject_log =
        match subject with Some str -> [("subject", str)] | None -> []
      in
      Log.succesS name ~params:([("op", "iter_with_cursor")] @ subject_log)
    with e ->
      let bt = Exception.get_backtrace () in
      let log_string =
        params |> List.map ~f:to_log |> String.concat ~sep:", "
      in
      Log.erroR
        name
        ~params:
          [("op", "iter_with_cursor"); ("params", log_string); ("query", sql)] ;
      let msg =
        match e with
        | Postgresql.Error (Unexpected_status (_, msg, _)) ->
            msg
        | Postgresql.Error pge ->
            Postgresql.string_of_error pge
        | Exception.DarkException de ->
            Log.erroR ~bt "Caught DarkException in DB, reraising" ;
            Caml.Printexc.raise_with_backtrace e bt
        | e ->
            Exception.exn_to_string e
      in
      Log.erroR
        msg
        ~bt
        ~params:
          [ ("msg", msg)
          ; ("time", time () |> string_of_float)
          ; ("query_name", name)
          ; ("sql", sql) ] ;
      Exception.storage
        "iter_with_cursor"
        ~bt
        ~info:
          [ ("msg", msg)
          ; ("time", time () |> string_of_float)
          ; ("query_name", name)
          ; ("sql", sql) ] ) ;
  ()


(** [tranasction] takes a function that (presumably) contains DB queries and wraps it in a
 * try and transaction - any exn raised will roll back the transaction, and
 * then the exn will be re-raised.
 *
 * Note: We can't just make this a wrapper around [run], because the reason you
 * want a transaction is to make _multiple_ DB queries atomic.
 * *)
let transaction ~(name : string) (f : unit -> unit) : unit =
  try
    run ~name:(name ^ " begin") "BEGIN" ~params:[] ;
    f () ;
    run ~name:(name ^ " commit") "COMMIT" ~params:[]
  with e ->
    run ~name:(name ^ " rollback") "ROLLBACK" ~params:[] ;
    raise e

type table_stats_row =
  { relation : string
  ; disk_bytes : int
  ; rows : int
  ; disk_human : string
  ; rows_human : string }

let table_stats () : table_stats_row list =
  (* Sizes from the pg_class table are fast (vs, say, running `SELECT count` on a
   * large table) but also are approximate, not precise. That's fine for purposes
   * of "how big are my tables growing to be?"
   *
   * Three steps in the query in table_stats:
   * 1) subquery "sizes" gets the data we want (size in bytes, number of rows)
   * 2) subquery "with_total_row" appends a row to the resultset that SUM()s the contents of each
   *    field
   * 3) the final query provides both raw- and humanized- formatted columns
   *)
  fetch
    ~name:"table_stats"
    ~params:[]
    "WITH sizes AS (
         SELECT
            relname as \"relation\",
            pg_total_relation_size (C .oid) as disk,
            reltuples::bigint AS \"rows\"
         FROM pg_class C
         LEFT JOIN pg_namespace N ON (N.oid = C .relnamespace)
         WHERE nspname NOT IN ('pg_catalog', 'information_schema')
         AND C .relkind <> 'i'
         AND nspname !~ '^pg_toast'
         ORDER BY pg_total_relation_size (C .oid) DESC
     ),

     -- with_total_row is a subquery that appends a SUM() row to the bottom of our result set
     with_total_row AS (
         SELECT relation, disk, \"rows\" FROM sizes
         UNION ALL
         SELECT
            'Total',
            SUM(disk),
            SUM(\"rows\")
         FROM sizes
     )

     -- now we actually do our output, including both raw and humanized-number
     -- columns for \"disk\" and \"rows\"
     SELECT relation,
         disk,
         \"rows\",
         pg_size_pretty(disk) as disk_human,
         -- NOTE: below uses pg_size_pretty to get us something human readable
         -- ('100M' is easier than '100000000', but it's _row count_, not bytes,
         -- hence trimming those parts off.)
         --
         -- Examples for trim(from substring(...)):
         -- 100 MB -> 100M
         -- 100 kb -> 100k
         -- 1 bytes -> 1
         trim(from
             substring(
                 pg_size_pretty ( \"rows\"::bigint)
                 from '[0-9]* [^b]?')
         ) as rows_human
     FROM with_total_row"
  |> List.map ~f:(function
         | [relation; disk_bytes; rows; disk_human; rows_human] ->
             { relation
             ; disk_bytes =
                 disk_bytes |> int_of_string_opt |> Option.value ~default:0
             ; rows = rows |> int_of_string_opt |> Option.value ~default:0
             ; disk_human
             ; rows_human }
         | _ ->
             Exception.internal "Wrong shape for table_stats query")
