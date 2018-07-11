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
  (* https://www.postgresql.org/docs/9.6/static/datatype-binary.html
   * Postgres advices us to parse the hex format. *)
  Db.fetch_one_option
    ~name:"load_oplists"
    "SELECT data FROM oplists
     WHERE host = $1
     AND digest = $2;"
    ~params:[String host; String digest]
    ~result:BinaryResult
  |> Option.map ~f:List.hd_exn

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
let load_json_from_disk ~root ?(preprocess=ident) (host:string) : Op.oplist option =
  Log.infO "serialization" ~params:[ "load", "disk"
                                   ; "format", "json"
                                   ; "host", host];
  let filename = json_unversioned_filename host in
  File.maybereadjsonfile ~root filename
    ~conv:Op.oplist_of_yojson ~stringconv:preprocess

let load_json_from_db ?(preprocess=ident) (host:string) : Op.oplist option =
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



let load_binary_from_db ~digest (host: string) : Op.oplist option =
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

let resave (f: string -> Op.oplist option) (host:string) : Op.oplist option =
  f host
  |> Option.map
    ~f:(fun ops -> save host ops; ops)

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
    let regex = Re2.Regex.create_exn "\\[ \"var\" \\]" in
    Re2.Regex.replace
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
    (descs : (string -> Op.oplist option) list)
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
               match fn host with
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

(* Use this to migrate to the per-tlid version *)
let load_migratory_from_db ~digest host : Op.oplist option =
  let ops = load_binary_from_db ~digest host in
  (match ops with
  | None -> ()
  | Some ops ->
    List.iter ops ~f:(fun op ->
        match op with
        | DeprecatedSavepoint
        | DeprecatedUndo
        | DeprecatedRedo
        | DeprecatedDeleteAll ->
          Exception.internal "bad op" ~actual:(Op.show_op op)
        | _ -> ());
    ());
  ops


let search_and_load (host: string) : Op.oplist =
  (* testfiles load and save from different directories *)
  if is_test host
  then
    (* when there are no oplists, read from disk. The test harnesses
     * clean up old oplists before running. *)
    deserialize_ordered host
      [ load_binary_from_db ~digest
      ; load_json_from_disk ~root:Testdata
      ]
  else
    let root = root_of host in
    deserialize_ordered host
      [ load_migratory_from_db ~digest (* do not resave, it's a fork-bomb *)
      (* These are the only formats that exist in production, newest
       * first. *)
      ; resave (load_binary_from_db ~digest:"89fd08b4f5adf0f816f2603b99397018")
      ; resave (load_binary_from_db ~digest:"e7a6fac71750a911255315f6320970da")
      ; resave (load_binary_from_db ~digest:"a0116b0508392a51289f61722c761d09")
      ; resave (load_binary_from_db ~digest:"769671393dbb637ce12686d4f98c2d75")
      ; resave (load_binary_from_db ~digest:"ec01527258fa0a757a4c5d98639c49c5")
      ; resave (load_binary_from_db ~digest:"70031445271577a297fd1c8910e02117")
      ; resave (load_binary_from_db ~digest:"cf19c8c21aec046d72a2107009682b24")
      ; resave (load_binary_from_db ~digest:"b08b8c99492f79853719a559678d56cb")
      ; resave (load_json_from_db)
      ; resave (load_json_from_db ~preprocess:preprocess_deprecated_undo)
      ; resave (load_json_from_db ~preprocess:preprocess_deprecated_savepoint2)
      ; resave (load_json_from_disk ~root)
      ; resave (load_deprecated_undo_json_from_disk ~root)
      ]


(* ------------------------- *)
(* hosts *)
(* ------------------------- *)

let current_hosts () : string list =
  (json_file_hosts () @ all_oplists ())
  |> List.dedup_and_sort

let check_all_oplists () : unit =
  current_hosts ()
  |> List.map ~f:(fun host ->
      match search_and_load host with
      | [] -> Exception.internal "got nothing, expected something"
      | _ -> ())
  |> ignore
