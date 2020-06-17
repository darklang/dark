open Core_kernel
open Libcommon
open Libexecution
module PG = Postgresql

(* ------------------------- *)
(* Escaping and such *)
(* ------------------------- *)

let conn = Libservice.Dbconnection.conn

let escape_string s = conn#escape_string s

let single_quote v = "'" ^ v ^ "'"

let cast_to ~tipe v = v ^ "::" ^ tipe

let array_separator = ", "

let date_of_sqlstring (str : string) : Core_kernel.Time.t =
  Core.Time.parse str ~fmt:"%Y-%m-%d %H:%M:%S" ~zone:Core.Time.Zone.utc


let date_to_sqlstring (d : Core_kernel.Time.t) : string =
  Core.Time.format d "%Y-%m-%d %H:%M:%S" ~zone:Core.Time.Zone.utc


type 'expr_type param =
  | Int of int
  | Int63 of Int63.t
  | ID of Types.id
  | String of string
  | Uuid of Uuidm.t
  | Float of float
  | Binary of string
  (* only works for passed params *)
  | Secret of string
  | RoundtrippableDval of Types.RuntimeT.dval
  | RoundtrippableDvalmap of Types.RuntimeT.dval_map
  | QueryableDval of Types.RuntimeT.dval
  | QueryableDvalmap of Types.RuntimeT.dval_map
  | Time of Types.RuntimeT.time
  | Null
  | List of 'expr_type param list
  | Bool of bool
[@@deriving show]

(* only works for in-script params *)

type result =
  | TextResult
  | BinaryResult

let to_binary_bool param : bool =
  match param with Binary _ -> true | _ -> false


let rec escape (param : 'expr_type param) : string =
  match param with
  | Int i ->
      string_of_int i
  | Int63 i ->
      Int63.to_string i
  | ID id ->
      Types.string_of_id id
  | String str ->
      str |> escape_string |> single_quote
  | Float f ->
      string_of_float f
  | Uuid uuid ->
      uuid
      |> Uuidm.to_string
      |> escape_string
      |> single_quote
      |> cast_to ~tipe:"uuid"
  | Binary str ->
      Exception.internal "Prefer not to escape binary data"
  | Secret str ->
      str |> escape_string |> single_quote
  | RoundtrippableDval dv ->
      dv |> Dval.to_internal_roundtrippable_v0 |> escape_string |> single_quote
  | RoundtrippableDvalmap dvm ->
      DObj dvm
      |> Dval.to_internal_roundtrippable_v0
      |> escape_string
      |> single_quote
  | QueryableDval dv ->
      dv
      |> Dval.to_internal_queryable_field_v1
      |> escape_string
      |> single_quote
      |> cast_to ~tipe:"jsonb"
  | QueryableDvalmap dvm ->
      dvm
      |> Dval.to_internal_queryable_v1
      |> escape_string
      |> single_quote
      |> cast_to ~tipe:"jsonb"
  | Time t ->
      t |> date_to_sqlstring |> escape_string |> single_quote
  | Null ->
      "NULL"
  | List params ->
      params |> List.map ~f:escape |> String.concat ~sep:", "
  | Bool true ->
      "true"
  | Bool false ->
      "false"


(* This is used to create values that are passed by the Postgres
 * driver as parameterized arguments. They do not use SQL syntax. *)
let rec to_sql param : string =
  match param with
  | Int i ->
      string_of_int i
  | Int63 i ->
      Int63.to_string i
  | ID i ->
      Types.string_of_id i
  | String str ->
      str
  | Uuid uuid ->
      Uuidm.to_string uuid
  | Float f ->
      string_of_float f
  | Binary str ->
      str (* the to_binary_bool handled this *)
  | Secret str ->
      str
  | QueryableDval dv ->
      Dval.to_internal_queryable_field_v1 dv
  | QueryableDvalmap dvm ->
      Dval.to_internal_queryable_v1 dvm
  | RoundtrippableDval dv ->
      Dval.to_internal_roundtrippable_v0 dv
  | RoundtrippableDvalmap dvm ->
      Dval.to_internal_roundtrippable_v0 (DObj dvm)
  | Null ->
      Postgresql.null
  | Time t ->
      date_to_sqlstring t
  | List xs ->
      (* Whenever using Arrays you must use string_to_array *)
      xs |> List.map ~f:to_sql |> String.concat ~sep:array_separator
  | Bool true ->
      "true"
  | Bool false ->
      "false"


let rec to_log param : string =
  let max_length = 600 in
  let abbrev s =
    if String.length s > max_length
    then String.slice s 0 max_length ^ "..."
    else s
  in
  match param with
  | Int i ->
      string_of_int i
  | Int63 i ->
      Int63.to_string i
  | ID i ->
      Types.string_of_id i
  | String str ->
      abbrev ("'" ^ str ^ "'")
  | Uuid uuid ->
      Uuidm.to_string uuid
  | Float f ->
      string_of_float f
  | Binary str ->
      "<binary>"
  | Secret str ->
      "<secret>"
  | RoundtrippableDval dv ->
      dv |> Dval.to_internal_roundtrippable_v0 |> abbrev
  | RoundtrippableDvalmap dvm ->
      DObj dvm |> Dval.to_internal_roundtrippable_v0 |> abbrev
  | QueryableDval dv ->
      dv |> Dval.to_internal_queryable_field_v1 |> abbrev
  | QueryableDvalmap dvm ->
      dvm |> Dval.to_internal_queryable_v1 |> abbrev
  | Null ->
      "NULL"
  | Time t ->
      date_to_sqlstring t
  | List params ->
      params
      |> List.map ~f:to_log
      |> String.concat ~sep:","
      |> fun s -> "[" ^ s ^ "]"
  | Bool true ->
      "true"
  | Bool false ->
      "false"


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


(* ------------------------- *)
(* SQL Commands *)
(* ------------------------- *)
let run
    ~(params : 'expr_type param list)
    ?(result = TextResult)
    ~(name : string)
    ?subject
    (sql : string) : unit =
  execute
    ~op:"run"
    ~params
    ~result
    ~name
    ?subject
    sql
    ~f:(conn#exec ~expect:[PG.Command_ok])
    ~r:(fun r -> ("", ()))
  |> ignore


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


let delete
    ~(params : 'expr_type param list)
    ?(result = TextResult)
    ~(name : string)
    ?subject
    (sql : string) : int =
  execute
    ~op:"run"
    ~params
    ~result
    ~name
    ?subject
    sql
    ~f:(conn#exec ~expect:[PG.Command_ok])
    ~r:(fun res -> ("deleted_rows", res#cmd_tuples |> int_of_string))


let fetch
    ~(params : 'expr_type param list)
    ?(result = TextResult)
    ~(name : string)
    ?subject
    (sql : string) : string list list =
  execute
    ~op:"fetch"
    ~params
    ~result
    ~name
    ?subject
    sql
    ~f:(conn#exec ~expect:[PG.Tuples_ok])
    ~r:(fun res ->
      let result = res#get_all_lst in
      let length = List.length result |> string_of_int in
      (length ^ " cols", result))


let fetch_one
    ~(params : 'expr_type param list)
    ?(result = TextResult)
    ~(name : string)
    ?subject
    (sql : string) : string list =
  execute
    ~op:"fetch_one"
    ~params
    ~result
    ~name
    ?subject
    sql
    ~f:(conn#exec ~expect:[PG.Tuples_ok])
    ~r:(fun res ->
      match res#get_all_lst with
      | [single_result] ->
          let lengths = List.map ~f:String.length single_result in
          let sum = Util.int_sum lengths |> string_of_int in
          (sum ^ "bytes", single_result)
      | [] ->
          Exception.storage "Expected one result, got none"
      | _ ->
          Exception.storage "Expected exactly one result, got many")


let fetch_count
    ~(params : 'expr_type param list)
    ?(result = TextResult)
    ~(name : string)
    ?subject
    (sql : string) : int =
  fetch_one ~params ~result ~name ?subject sql |> List.hd_exn |> int_of_string


let fetch_one_option
    ~(params : 'expr_type param list)
    ?(result = TextResult)
    ~(name : string)
    ?subject
    (sql : string) : string list option =
  execute
    ~op:"fetch_one_option"
    ~params
    ~result
    ~name
    ?subject
    sql
    ~f:(conn#exec ~expect:[PG.Tuples_ok])
    ~r:(fun res ->
      match res#get_all_lst with
      | [single_result] ->
          let lengths = List.map ~f:String.length single_result in
          let sum = Util.int_sum lengths |> string_of_int in
          (sum ^ "bytes", Some single_result)
      | [] ->
          ("none", None)
      | _ ->
          Exception.storage "Expected exactly one result, got many")


let exists
    ~(params : 'expr_type param list) ~(name : string) ?subject (sql : string) :
    bool =
  execute
    ~op:"exists"
    ~params
    ~name
    ~result:TextResult
    ?subject
    sql
    ~f:(conn#exec ~expect:[PG.Tuples_ok])
    ~r:(fun res ->
      match res#get_all_lst with
      | [["1"]] ->
          ("true", true)
      | [] ->
          ("false", false)
      | r ->
          Exception.storage "Unexpected result" ~actual:(Log.dump r))


let delete_benchmarking_data () : unit =
  run
    ~name:"delete_benchmarking_data"
    "DELETE FROM oplists WHERE host like 'benchmarking\\_%%';
     DELETE FROM json_oplists WHERE host like 'benchmarking\\_%%';"
    ~params:[]


exception DBQueryException of string

let dbQueryExceptionToString = function
  | DBQueryException str ->
      "You're using our new experimental Datastore query compiler. It compiles your lambdas into optimized (and partially indexed) Datastore queries, which should be reasonably faster.\n\nUnfortunately, we hit a snag while compiling your lambda. We only support a subset of Dark's functionality, but will be expanding it in the future.\n\nSome Dark code is not supported in DB::query lambdas for now, and some of it won't be supported because it's an odd thing to do in a datastore query. If you think your operation should be supported, let us know in #general.\n\n  Error: "
      ^ str
  | _ ->
      ""


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
