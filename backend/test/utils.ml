open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Ast
open Lwt
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module Code = Cohttp.Code
module C = Canvas
module RT = Runtime
module TL = Toplevel
module Map = Map.Poly
module AT = Alcotest

(* ------------------- *)
(* Misc fns *)
(* ------------------- *)

(* Allows us to mock analysis *)
let test_fn_results : ((function_desc * dval list) * (dval * Time.t)) list ref
    =
  ref []


(* Not wired up yet *)
let test_fn_arguments : (dval_map * Time.t) list list ref = ref []

let clear_test_data () : unit =
  test_fn_results := [] ;
  test_fn_arguments := [] ;
  let owner = Account.for_host_exn "test" in
  let canvas_ids =
    Db.fetch
      ~params:[Uuid owner]
      ~name:"lol"
      "SELECT id
       FROM canvases
       WHERE account_id = $1"
    |> List.filter_map ~f:(fun cid -> cid |> List.hd_exn |> Uuidm.of_string)
    |> List.map ~f:(fun (cid : Uuidm.t) -> Db.Uuid cid)
  in
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_events_test_data"
    "DELETE FROM events where canvas_id = ANY (string_to_array ($1, $2)::uuid[])" ;
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_stored_events_test_data"
    "DELETE FROM stored_events_v2 where canvas_id = ANY (string_to_array ($1, $2)::uuid[])" ;
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_function_results_test_data"
    "DELETE FROM function_results_v2 where canvas_id = ANY (string_to_array ($1, $2)::uuid[])" ;
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_user_data_test_data"
    "DELETE FROM user_data where canvas_id = ANY (string_to_array ($1, $2)::uuid[])" ;
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_cron_records_test_data"
    "DELETE FROM cron_records where canvas_id = ANY (string_to_array ($1, $2)::uuid[])" ;
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_toplevel_oplists_test_data"
    "DELETE FROM toplevel_oplists WHERE canvas_id = ANY (string_to_array ($1, $2)::uuid[])" ;
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_function_arguments"
    "DELETE FROM function_arguments WHERE canvas_id = ANY (string_to_array ($1, $2)::uuid[])" ;
  Db.run
    ~params:[List canvas_ids; String Db.array_separator]
    ~name:"clear_canvases_test_data"
    "DELETE FROM canvases where id = ANY (string_to_array ($1, $2)::uuid[])" ;
  ()


(* ------------------- *)
(* Test fns *)
(* ------------------- *)

let at_dval =
  AT.testable
    (fun fmt dv -> Fmt.pf fmt "%s" (Dval.show dv))
    (fun a b -> compare_dval a b = 0)


let check_dval = AT.check at_dval

let check_dval_list = AT.check (AT.list at_dval)

let check_oplist = AT.check (AT.of_pp Op.pp_oplist)

let check_tlid_oplists = AT.check (AT.of_pp Op.pp_tlid_oplists)

let check_condition msg dval ~(f : dval -> bool) =
  AT.check AT.bool msg (f dval) true


let testable_handler = AT.testable HandlerT.pp_handler HandlerT.equal_handler

let testable_string_dval_pair =
  AT.testable pp_string_dval_pair equal_string_dval_pair


let check_exception ?(check = fun _ -> true) ~(f : unit -> dval) msg =
  let e =
    try
      let r = f () in
      Log.erroR "result" ~data:(Dval.to_developer_repr_v0 r) ;
      Some "no exception"
    with
    | Exception.DarkException ed ->
        if check ed
        then None
        else (
          Log.erroR "check failed" ~data:(Log.dump ed) ;
          Some "Check failed" )
    | e ->
        let bt = Backtrace.Exn.most_recent () in
        let msg = Exn.to_string e in
        print_endline (Backtrace.to_string bt) ;
        Log.erroR "different exception" ~data:msg ;
        Some "different exception"
  in
  AT.check (AT.option AT.string) msg None e


let check_error_contains (name : string) (result : dval) (substring : string) =
  let strresult = Dval.to_developer_repr_v0 result in
  AT.(check bool)
    (name ^ ": (\"" ^ strresult ^ "\" contains \"" ^ substring ^ "\"")
    true
    (String.is_substring ~substring strresult)


(* ------------------- *)
(* Set up test data *)
(* ------------------- *)

let fid = Util.create_id

let v str = Filled (fid (), Value str)

let b () = Blank (fid ())

let f a = Filled (fid (), a)

let fncall (a, b) = f (FnCall (a, b))

let tlid = Int63.of_int 7

let tlid2 = Int63.of_int 35

let tlid3 = Int63.of_int 70

let tipe_id = Int63.of_int 9

let dbid = Int63.of_int 89

let dbid2 = Int63.of_int 189

let colnameid = Int63.of_int 11

let coltypeid = Int63.of_int 12

let colnameid2 = Int63.of_int 13

let coltypeid2 = Int63.of_int 14

let colnameid3 = Int63.of_int 15

let coltypeid3 = Int63.of_int 16

let nameid = Int63.of_int 17

let nameid2 = Int63.of_int 217

let pos = {x = 0; y = 0}

let execution_id = Int63.of_int 6542

let ast_for = Expr_dsl.ast_for

let handler ?(tlid = tlid) ast : HandlerT.handler =
  { tlid
  ; ast
  ; spec =
      { module_ = b ()
      ; name = b ()
      ; modifier = b ()
      ; types = {input = b (); output = b ()} } }


let http_handler ?(tlid = tlid) ast : HandlerT.handler =
  { tlid
  ; ast
  ; spec =
      { module_ = f "HTTP"
      ; name = f "/test"
      ; modifier = f "GET"
      ; types = {input = b (); output = b ()} } }


let http_request_path = "/some/vars/and/such"

let http_route = "/some/:vars/:and/such"

let http_route_handler ?(tlid = tlid) ?(route = http_route) () :
    HandlerT.handler =
  { tlid
  ; ast = f (Value "5")
  ; spec =
      { module_ = f "HTTP"
      ; name = f route
      ; modifier = f "GET"
      ; types = {input = b (); output = b ()} } }


let daily_cron ast : HandlerT.handler =
  { tlid
  ; ast
  ; spec =
      { module_ = f "CRON"
      ; name = f "test"
      ; modifier = f "Daily"
      ; types = {input = b (); output = b ()} } }


let hop h = Op.SetHandler (tlid, pos, h)

let user_fn name params ast : user_fn =
  { tlid
  ; ast
  ; metadata =
      { name = f name
      ; parameters =
          List.map params ~f:(fun p ->
              { name = f p
              ; tipe = f TAny
              ; block_args = []
              ; optional = false
              ; description = "test" } )
      ; return_type = f TAny
      ; description = "test user fn"
      ; infix = false } }


let user_record name fields : user_tipe =
  {tlid = tipe_id; version = 0; name = f name; definition = UTRecord fields}


let t4_get1st (x, _, _, _) = x

let t4_get4th (_, _, _, x) = x

(* ------------------- *)
(* Execution *)
(* ------------------- *)
let ops2c (host : string) (ops : Op.op list) :
    (C.canvas ref, string list) Result.t =
  C.init host ops


let ops2c_exn (host : string) (ops : Op.op list) : C.canvas ref =
  C.init host ops
  |> Result.map_error ~f:(String.concat ~sep:", ")
  |> Prelude.Result.ok_or_internal_exception "Canvas load error"


let add_test_fn_result
    (desc : function_desc) (args : dval list) (result : dval * Time.t) : unit =
  test_fn_results := ((desc, args), result) :: !test_fn_results ;
  ()


let load_test_fn_results (desc : function_desc) (args : dval list) :
    (dval * Time.t) option =
  List.find !test_fn_results ~f:(fun ((desc', args'), result) ->
      (desc, args) = (desc', args') )
  |> Option.map ~f:Tuple2.get2


let test_execution_data ?(canvas_name = "test") ops :
    C.canvas ref * exec_state * input_vars =
  let c = ops2c_exn canvas_name ops in
  let vars = Execution.dbs_as_input_vars (TL.dbs !c.dbs) in
  let canvas_id = !c.id in
  let trace_id = Util.create_uuid () in
  let state =
    { tlid
    ; account_id = !c.owner
    ; canvas_id = !c.id
    ; user_fns = IDMap.data !c.user_functions
    ; user_tipes = IDMap.data !c.user_tipes
    ; fail_fn = None
    ; dbs = TL.dbs !c.dbs
    ; execution_id
    ; load_fn_result = load_test_fn_results
    ; store_fn_result =
        Stored_function_result.store ~canvas_id ~trace_id
        (* TODO: expose this too *)
    ; load_fn_arguments = Execution.load_no_arguments
    ; store_fn_arguments = Stored_function_arguments.store ~canvas_id ~trace_id
    }
  in
  (c, state, vars)


let execute_ops (ops : Op.op list) : dval =
  let ( c
      , { tlid
        ; load_fn_result
        ; load_fn_arguments
        ; execution_id
        ; dbs
        ; user_fns
        ; user_tipes
        ; account_id
        ; canvas_id }
      , input_vars ) =
    test_execution_data ops
  in
  let h = !c.handlers |> TL.handlers |> List.hd_exn in
  let result, _ =
    Execution.execute_handler
      h
      ~tlid
      ~execution_id
      ~dbs
      ~user_fns
      ~user_tipes
      ~account_id
      ~load_fn_result
      ~load_fn_arguments
      ~canvas_id
      ~input_vars:[]
  in
  result


(* already provided in execute_handler *)

let exec_handler ?(ops = []) (prog : string) : dval =
  prog
  |> ast_for
  (* |> Log.pp ~f:show_expr *)
  |> handler
  |> hop
  |> fun h -> execute_ops (ops @ [h])


let exec_ast ?(canvas_name = "test") (prog : string) : dval =
  let c, state, input_vars = test_execution_data ~canvas_name [] in
  let result, _ = Ast.execute_ast input_vars state (ast_for prog) in
  result


let exec_userfn (prog : string) : dval =
  let name = "test_function" in
  let ast = ast_for prog in
  let fn = user_fn name [] ast in
  let c, state, _ = test_execution_data [SetFunction fn] in
  let result, _ = Ast.execute_fn state name execution_id [] in
  result


(* Sample values *)
let sample_dvals =
  [ ("int", Dval.dint 5)
  ; ("int2", Dval.dint (-1))
  ; ("int_max_31_bits", Dval.dint 1073741824)
  ; ("int_above_31_bits", Dval.dint 1073741825)
  ; ("int_max_32_bits", Dval.dint 2147483647)
  ; ("int_above_32_bits", Dval.dint 2147483648)
  ; ("int_max_53_bits", Dval.dint 4503599627370496)
  ; ("int_above_53_bits", Dval.dint 4503599627370497)
  ; ("int_max_63_bits", Dval.dint 4611686018427387903)
  ; ("float", DFloat 7.2)
  ; ("float2", DFloat (-7.2))
  ; ("true", DBool true)
  ; ("false", DBool false)
  ; ("null", DNull)
  ; ("datastore", DDB "Visitors")
  ; ("string", Dval.dstr_of_string_exn "incredibly this was broken")
  ; ("list", DList [Dval.dint 4])
  ; ("obj", DObj (DvalMap.singleton "foo" (Dval.dint 5)))
  ; ( "obj2"
    , DObj
        (DvalMap.from_list
           [("type", Dval.dstr_of_string_exn "weird"); ("value", DNull)]) )
  ; ( "obj3"
    , DObj
        (DvalMap.from_list
           [ ("type", Dval.dstr_of_string_exn "weird")
           ; ("value", Dval.dstr_of_string_exn "x") ]) )
  ; ("incomplete", DIncomplete)
  ; ("error", DError "some error string")
  ; ("block", DBlock (fun _args -> DNull))
  ; ("errorrail", DErrorRail (Dval.dint 5))
  ; ("redirect", DResp (Redirect "/home", DNull))
  ; ( "httpresponse"
    , DResp (Response (200, []), Dval.dstr_of_string_exn "success") )
  ; ("db", DDB "Visitors")
  ; ("date", DDate (Time.of_string "2018-09-14T00:31:41Z"))
  ; ("password", DPassword (PasswordBytes.of_string "somebytes"))
  ; ("uuid", DUuid (Util.uuid_of_string "7d9e5495-b068-4364-a2cc-3633ab4d13e6"))
  ; ("option", DOption OptNothing)
  ; ("option2", DOption (OptJust (Dval.dint 15)))
  ; ("option3", DOption (OptJust (Dval.dstr_of_string_exn "a string")))
  ; ("character", DCharacter (Unicode_string.Character.unsafe_of_string "s"))
  ; ("result", DResult (ResOk (Dval.dint 15)))
  ; ( "result2"
    , DResult
        (ResError (DList [Dval.dstr_of_string_exn "dunno if really supported"]))
    )
  ; ("result3", DResult (ResOk (Dval.dstr_of_string_exn "a string")))
  ; ("bytes", DBytes ("JyIoXCg=" |> B64.decode |> RawBytes.of_string))
  ; ( "bytes2"
    , DBytes
        (* use image bytes here to test for any weird bytes forms *)
        (RawBytes.of_string
           (File.readfile ~root:Testdata "sample_image_bytes.png")) ) ]
