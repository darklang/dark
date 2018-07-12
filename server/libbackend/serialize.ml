open Core_kernel
open Libcommon
open Libexecution

let digest = Op.bin_shape_oplist
             |> Bin_prot.Shape.eval_to_digest_string

let write_shape_data () =
  if Config.should_write_shape_data
  then
    let shape_string = Op.bin_shape_oplist
                     |> Bin_prot.Shape.eval
                     |> Bin_prot.Shape.Canonical.to_string_hum
    in
    File.writefile ~root:Serialization digest shape_string
  else
    ()

let is_test (name: string) : bool =
  String.is_prefix ~prefix:"test_" name
  || String.is_prefix ~prefix:"test-" name

let root_of (name: string): Config.root =
  if is_test name
  then Completed_test
  else Appdata

let json_unversioned_filename name =
  name ^ "." ^ "json"

let json_file_hosts () : string list =
  File.lsdir ~root:Appdata ""
  |> List.filter
    ~f:(String.is_suffix ~suffix:".json")
  |> List.map
    ~f:(String.chop_suffix_exn ~suffix:".json")


(* ------------------------- *)
(* oplists *)
(* ------------------------- *)
let save_oplists ~(host: string) ~(digest: string) (data: string) : unit =
  Db.run
    ~name:"save_oplists"
    "INSERT INTO oplists
    (host, digest, data)
    VALUES ($1, $2, $3)
    ON CONFLICT (host, digest) DO UPDATE
    SET data = $3;"
    ~params:[String host; String digest; Binary data]



let load_oplists ~(host: string) ~(digest: string) : string option =
  Db.fetch_one_option
    ~name:"load_oplists"
    "SELECT data FROM oplists
     WHERE host = $1
     AND digest = $2;"
    ~params:[String host; String digest]
    ~result:BinaryResult
  |> Option.map ~f:List.hd_exn

let load_per_tlid_oplists ~(canvas_id: Uuidm.t) : (int * string) list =
  Db.fetch
    ~name:"load_per_tlid_oplists"
    "SELECT data, tlid FROM toplevel_oplists
     WHERE canvas_id = $1"
    ~params:[Uuid canvas_id]
    ~result:BinaryResult
  |> List.map
    ~f:(fun results ->
        match results with
        | [tlid; data] ->
          (int_of_string tlid, data)
        | _ -> Exception.internal "Shape of per_tlid oplists")

let save_toplevel_oplist ~tlid ~(ops:Op.oplist)
    ~(canvas_id: Uuidm.t) ~(account_id: Uuidm.t) ~(name:string)
    ~(module_: string) ~(modifier:string) (data:string)
  : unit =
  Db.run
    ~name:"save per tlid oplist"
    "INSERT INTO toplevel_oplists
    (canvas_id, account_id, tlid, digest, name, module, modifier, data)
    VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
    ON CONFLICT (canvas_id, tlid) DO UPDATE
    SET account_id = $2,
        digest = $4,
        name = $5,
        module = $6,
        modifier = $7,
        data = $8;"
    ~params:[ Uuid canvas_id
            ; Uuid account_id
            ; Int tlid
            ; String digest
            ; String name
            ; String module_
            ; String modifier
            ; Binary data]

let load_json_oplists ~(host: string) : string option =
  Db.fetch_one_option
    ~name:"load_json_oplists"
    "SELECT data FROM json_oplists
     WHERE host = $1"
    ~params:[String host]
  |> Option.map ~f:List.hd_exn

let save_json_oplists ~(host: string) ~(digest: string) (data: string) : unit =
  (* this is an upsert *)
  Db.run
    ~name:"save_json_oplists"
    "INSERT INTO json_oplists
    (host, digest, data)
    VALUES ($1, $2, $3)
    ON CONFLICT (host) DO UPDATE
    SET data = $3,
        digest = $2;"
    ~params:[String host; String digest; String data]

let all_oplists () : string list =
  Db.fetch
    ~name:"oplists"
    "SELECT DISTINCT host FROM oplists
     UNION
     SELECT DISTINCT host FROM json_oplists"
    ~params:[]
  |> List.map ~f:List.hd_exn
  |> List.filter ~f:(fun h ->
      not (String.is_prefix ~prefix:"test-" h))



(* ------------------------- *)
(* loading and saving *)
(* ------------------------- *)
let load_json_from_disk ~root ?(preprocess=ident) ~(host:string)
    ~(canvas_id: Uuidm.t) () : Op.oplist option =
  Log.infO "serialization" ~params:[ "load", "disk"
                                   ; "format", "json"
                                   ; "host", host];
  let filename = json_unversioned_filename host in
  File.maybereadjsonfile ~root filename
    ~conv:Op.oplist_of_yojson ~stringconv:preprocess

let load_json_from_db ?(preprocess=ident) ~(host: string)
    ~(canvas_id: Uuidm.t) () : Op.oplist option =
  Log.infO "serialization" ~params:[ "load_from", "db"
                                   ; "format", "json"
                                   ; "host", host];
  host
  |> load_json_oplists
  |> Option.map ~f:(fun ops_string ->
      ops_string
      |> preprocess
      |> Yojson.Safe.from_string
      |> Op.oplist_of_yojson
      |> Result.ok_or_failwith)

let save_json_to_disk ~root (filename: string) (ops: Op.oplist) : unit =
  Log.infO "serialization" ~params:[ "save_to", "disk"
                                   ; "format", "json"
                                   ; "filename", filename];
  ops
  |> Op.oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> File.writefile ~root filename

let save_json_to_db (host: string) (ops: Op.oplist) : unit =
  Log.infO "serialization" ~params:[ "save_to", "db"
                                   ; "format", "json"
                                   ; "digest", digest
                                   ; "host", host];
  ops
  |> Op.oplist_to_yojson
  |> Yojson.Safe.to_string
  |> save_json_oplists ~host ~digest

let save_binary_to_db (host: string) (ops: Op.oplist) : unit =
  Log.infO "serialization" ~params:[ "save_to", "db"
                                   ; "format", "binary"
                                   ; "digest", digest
                                   ; "host", host];
  ops
  |> Core_extended.Bin_io_utils.to_line Op.bin_oplist
  |> Bigstring.to_string
  |> save_oplists host digest

let save host ops : unit =
  save_binary_to_db host ops;
  ignore (File.convert_bin_to_json host)



let load_binary_from_db ~digest ~(host: string) ~(canvas_id: Uuidm.t) ()
  : Op.oplist option =
  Log.infO "serialization" ~params:[ "load_from", "db"
                                   ; "format", "binary"
                                   ; "digest", digest
                                   ; "host", host];
  load_oplists host digest
  |> Option.map
    ~f:(fun x ->
        (* Supposedly, we're supposed to remove an ending \n that
         * to_line helpfully adds, but that hasn't been a problem so
         * far. *)
        Core_extended.Bin_io_utils.of_line x Op.bin_oplist)

(* ------------------------- *)
(* preprocessing *)
(* ------------------------- *)
let preprocess_deprecated_undo str =
  (* this mutates all lambdas in the source to
   * the new format as of 53f3fb82
   *
    Safe because there's no other way [ "var" ] can
   * appear in the source (as of now).
   *)
  let transform_lambda s =
    let regex = Re2.create_exn "\\[ \"var\" \\]" in
    Re2.replace
      ~f:(fun _ ->
          Printf.sprintf
            "[ [\"Filled\", %i, \"var\"] ]"
            (Util.create_id ()))
      regex
      s
    |> Result.ok
    |> Option.value ~default:str
  in
  str
  |> Util.string_replace "\"Undo\"" "\"DeprecatedUndo\""
  |> Util.string_replace "\"Redo\"" "\"DeprecatedRedo\""
  |> Util.string_replace "\"Savepoint\"" "\"DeprecatedSavepoint\""
  |> transform_lambda

let preprocess_deprecated_savepoint2 str =
  Util.string_replace "\"Savepoint\"" "\"DeprecatedSavepoint2\"" str

let load_deprecated_undo_json_from_disk ~root =
  load_json_from_disk ~root ~preprocess:preprocess_deprecated_undo



(* ------------------------- *)
(* deserialing algorithm *)
(* ------------------------- *)
let deserialize_ordered
    (host : string)
    (canvas_id : Uuidm.t)
    (descs : (host:string -> canvas_id:Uuidm.t -> unit -> Op.oplist option) list)
    : Op.oplist =
  (* try each in turn. If the file exists, try it, and return if
    successful. If it fails, save the error and try the next one *)
  let (result, errors) =
    List.fold descs ~init:(None, [])
      ~f:(fun (prev, errors) fn ->
          match prev with
          | Some r -> (prev, errors)
          | None ->
            (try
               match fn ~host ~canvas_id () with
               | Some oplist -> (Some oplist, errors)
               | None -> (prev, errors)
             with
             | e ->
               Log.erroR "deserialization" ~data:(Exn.to_string e);
               (None, (errors @ [e]))))
  in
  match (result, errors) with
  | (Some r, _) -> r
  | (None, []) -> []
  | (None, es) ->
    let msgs =
      List.mapi ~f:(fun i ex -> (string_of_int i, Exn.to_string ex))
        es
    in
    Exception.internal ~info:msgs ("storage errors with " ^ host)

let alert_on_deprecated_ops host (ops: Op.oplist) : unit =
  List.iter ops ~f:(fun op ->
    match op with
    | Deprecated0
    | Deprecated1
    | Deprecated2
    | Deprecated3
    | Deprecated4 _ ->
      Exception.internal "bad op" ~info:["host", host] ~actual:(Op.show_op op)
    | _ -> ())



(* Use this to migrate to the per-tlid version *)
let load_migratory_from_db ~(host:string) ~(canvas_id: Uuidm.t) ()
  : Op.oplist option =
  let ops = load_binary_from_db ~digest ~host ~canvas_id () in
  Option.map ~f:(alert_on_deprecated_ops host) ops |> ignore;
  ops

let load_and_combine_from_per_tlid_oplists ~(host:string)
    ~(canvas_id: Uuidm.t) () : Op.oplist option =
  None

let save_from_per_tlid_oplists ~(host:string) ~(canvas_id: Uuidm.t)
  (ops: Op.oplist) : unit =
  ()


let search_and_load (host: string) (canvas_id: Uuidm.t) : Op.oplist =
  (* testfiles load and save from different directories *)
  if is_test host
  then
    (* when there are no oplists, read from disk. The test harnesses
     * clean up old oplists before running. *)
    deserialize_ordered host canvas_id
      [ load_binary_from_db ~digest
      ; load_json_from_disk ~root:Testdata ~preprocess:ident
      ]
  else
    let root = root_of host in
    deserialize_ordered host canvas_id
      [ load_migratory_from_db
      (* These are the only formats that exist in production, newest
       * first. *)
      ; load_binary_from_db ~digest:"58304561d23692e4e8559a6071de168d"
      ; load_binary_from_db ~digest:"50cfe9cc7ebe36ea830bd39f74b994da"
      ; load_binary_from_db ~digest:"89fd08b4f5adf0f816f2603b99397018"
      ; load_binary_from_db ~digest:"e7a6fac71750a911255315f6320970da"
      ; load_binary_from_db ~digest:"a0116b0508392a51289f61722c761d09"
      ; load_binary_from_db ~digest:"769671393dbb637ce12686d4f98c2d75"
      ; load_binary_from_db ~digest:"ec01527258fa0a757a4c5d98639c49c5"
      ; load_binary_from_db ~digest:"70031445271577a297fd1c8910e02117"
      ; load_binary_from_db ~digest:"cf19c8c21aec046d72a2107009682b24"
      ; load_binary_from_db ~digest:"b08b8c99492f79853719a559678d56cb"
      ; load_json_from_db ~preprocess:ident
      ; load_json_from_db ~preprocess:preprocess_deprecated_undo
      ; load_json_from_db ~preprocess:preprocess_deprecated_savepoint2
      ; load_json_from_disk ~root ~preprocess:ident
      ; load_deprecated_undo_json_from_disk ~root
      ]


(* ------------------------- *)
(* hosts *)
(* ------------------------- *)

(* https://stackoverflow.com/questions/15939902/is-select-or-insert-in-a-function-prone-to-race-conditions/15950324#15950324 *)
let fetch_canvas_id (owner:Uuidm.t) (host:string) : Uuidm.t =
  Db.fetch_one
    ~name:"fetch_canvas_id"
    "SELECT canvas_id($1, $2, $3)"
    ~params:[Uuid (Util.create_uuid ()); Uuid owner; String host]
  |> List.hd_exn
  |> Uuidm.of_string
  |> Option.value_exn

let current_hosts () : string list =
  (json_file_hosts () @ all_oplists ())
  |> List.dedup_and_sort ~compare

let check_all_oplists () : unit =
  current_hosts ()
  |> List.map ~f:(fun host ->
      let owner = Account.for_host host in
      let canvas_id = fetch_canvas_id owner host in
      match search_and_load host canvas_id with
      | [] -> Exception.internal "got nothing, expected something"
      | _ -> ())
  |> ignore
