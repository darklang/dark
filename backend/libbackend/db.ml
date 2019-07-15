open Core_kernel
open Libcommon
open Libexecution
module PG = Postgresql

(* ------------------------- *)
(* Escaping and such *)
(* ------------------------- *)

let conn = Libservice.Dbconnection.conn

let escape_single s = conn#escape_string s

let single_quote v = "'" ^ v ^ "'"

let cast_to ~tipe v = v ^ "::" ^ tipe

let array_separator = ", "

let date_of_sqlstring (str : string) : Core_kernel.Time.t =
  Core.Time.parse str ~fmt:"%Y-%m-%d %H:%M:%S" ~zone:Core.Time.Zone.utc


let date_to_sqlstring (d : Core_kernel.Time.t) : string =
  Core.Time.format d "%Y-%m-%d %H:%M:%S" ~zone:Core.Time.Zone.utc


type param =
  | Int of int
  | ID of Types.id
  | String of string
  | Uuid of Uuidm.t
  | Binary of string
  (* only works for passed params *)
  | Secret of string
  | RoundtrippableDval of Types.RuntimeT.dval
  | RoundtrippableDvalmap of Types.RuntimeT.dval_map
  | QueryableDval of Types.RuntimeT.dval
  | QueryableDvalmap of Types.RuntimeT.dval_map
  | Time of Types.RuntimeT.time
  | Null
  | List of param list
  | Bool of bool

(* only works for in-script params *)

type result =
  | TextResult
  | BinaryResult

let to_binary_bool param : bool =
  match param with Binary _ -> true | _ -> false


let rec escape (param : param) : string =
  match param with
  | Int i ->
      string_of_int i
  | ID id ->
      Types.string_of_id id
  | String str ->
      str |> escape_single |> single_quote
  | Uuid uuid ->
      uuid
      |> Uuidm.to_string
      |> escape_single
      |> single_quote
      |> cast_to ~tipe:"uuid"
  | Binary str ->
      Exception.internal "Prefer not to escape binary data"
  | Secret str ->
      str |> escape_single |> single_quote
  | RoundtrippableDval dv ->
      dv |> Dval.to_internal_roundtrippable_v0 |> escape_single |> single_quote
  | RoundtrippableDvalmap dvm ->
      DObj dvm
      |> Dval.to_internal_roundtrippable_v0
      |> escape_single
      |> single_quote
  | QueryableDval dv ->
      dv
      |> Dval.to_internal_queryable_v1
      |> escape_single
      |> single_quote
      |> cast_to ~tipe:"jsonb"
  | QueryableDvalmap dvm ->
      DObj dvm
      |> Dval.to_internal_queryable_v1
      |> escape_single
      |> single_quote
      |> cast_to ~tipe:"jsonb"
  | Time t ->
      t |> date_to_sqlstring |> escape_single |> single_quote
  | Null ->
      "NULL"
  | List params ->
      params |> List.map ~f:escape |> String.concat ~sep:", "
  | Bool true ->
      "true"
  | Bool false ->
      "false"


let rec to_sql param : string =
  match param with
  | Int i ->
      string_of_int i
  | ID i ->
      Types.string_of_id i
  | String str ->
      str
  | Uuid uuid ->
      Uuidm.to_string uuid
  | Binary str ->
      str (* the to_binary_bool handled this *)
  | Secret str ->
      str
  | QueryableDval dv ->
      Dval.to_internal_queryable_v1 dv
  | QueryableDvalmap dvm ->
      Dval.to_internal_queryable_v1 (DObj dvm)
  | RoundtrippableDval dv ->
      Dval.to_internal_roundtrippable_v0 dv
  | RoundtrippableDvalmap dvm ->
      Dval.to_internal_roundtrippable_v0 (DObj dvm)
  | Null ->
      Postgresql.null
  | Time t ->
      date_to_sqlstring t
  | List xs ->
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
  | ID i ->
      Types.string_of_id i
  | String str ->
      abbrev str
  | Uuid uuid ->
      Uuidm.to_string uuid
  | Binary str ->
      "<binary>"
  | Secret str ->
      "<secret>"
  | RoundtrippableDval dv ->
      dv |> Dval.to_internal_roundtrippable_v0 |> abbrev
  | RoundtrippableDvalmap dvm ->
      DObj dvm |> Dval.to_internal_roundtrippable_v0 |> abbrev
  | QueryableDval dv ->
      dv |> Dval.to_internal_queryable_v1 |> abbrev
  | QueryableDvalmap dvm ->
      DObj dvm |> Dval.to_internal_queryable_v1 |> abbrev
  | Null ->
      "NULL"
  | Time t ->
      date_to_sqlstring t
  | List params ->
      params
      |> List.map ~f:to_log
      |> String.concat ~sep:array_separator
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
                , Exception.exception_data_to_yojson de
                  |> Yojson.Safe.to_string ) ] ;
          Caml.Printexc.raise_with_backtrace e bt
      | e ->
          Exception.exn_to_string e
    in
    Exception.storage msg ~bt ~info:[("time", time () |> string_of_float)]


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
        ~params:[("msg", msg); ("time", time () |> string_of_float)] ;
      Exception.storage
        "iter_with_cursor"
        ~bt
        ~info:[("msg", msg); ("time", time () |> string_of_float)] ) ;
  ()


(* ------------------------- *)
(* SQL Commands *)
(* ------------------------- *)
let run
    ~(params : param list)
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


let delete
    ~(params : param list)
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
    ~(params : param list)
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
      (length ^ " cols", result) )


let fetch_one
    ~(params : param list)
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
          Exception.storage "Expected exactly one result, got many" )


let fetch_one_option
    ~(params : param list)
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
          Exception.storage "Expected exactly one result, got many" )


let exists ~(params : param list) ~(name : string) ?subject (sql : string) :
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
          Exception.storage "Unexpected result" ~actual:(Log.dump r) )


let delete_benchmarking_data () : unit =
  run
    ~name:"delete_benchmarking_data"
    "DELETE FROM oplists WHERE host like 'benchmarking\\_%%';
     DELETE FROM json_oplists WHERE host like 'benchmarking\\_%%';"
    ~params:[]
