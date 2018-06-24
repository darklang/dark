open Core_kernel
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

let current_hosts () : string list =
  (json_file_hosts () @ Db.all_oplists ())
  |> List.dedup_and_sort

let load_json_from_disk ~root (host:string) : Op.oplist option =
  Log.infO "serialization" ~params:[ "load", "disk"
                                   ; "format", "json"
                                   ; "host", host];
  let filename = json_unversioned_filename host in
  File.maybereadjsonfile ~root ~conv:Op.oplist_of_yojson filename

let load_preprocessed_json_from_disk ~root
    ~(preprocess:(string -> string))
    (host:string) : Op.oplist option =
  Log.infO "serialization" ~params:[ "load_from", "disk"
                                   ; "format", "preprocessed_json"
                                   ; "host", host];
  let filename = json_unversioned_filename host in
  File.maybereadjsonfile ~root ~stringconv:preprocess ~conv:Op.oplist_of_yojson filename

let load_json_from_db (host:string) : Op.oplist option =
  Log.infO "serialization" ~params:[ "load_from", "db"
                                   ; "format", "json"
                                   ; "host", host];
  host
  |> Db.load_json_oplists
  |> Option.map ~f:(fun ops_string ->
      ops_string
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
                                   ; "host", host];
  ops
  |> Op.oplist_to_yojson
  |> Yojson.Safe.to_string
  |> Db.save_json_oplists ~host ~digest

let save_binary_to_db (host: string) (ops: Op.oplist) : unit =
  Log.infO "serialization" ~params:[ "save_to", "db"
                                   ; "format", "disk"
                                   ; "host", host];
  ops
  |> Core_extended.Bin_io_utils.to_line Op.bin_oplist
  |> Bigstring.to_string
  |> Db.save_oplists host digest

let load_binary_from_db (host: string) : Op.oplist option =
  Log.infO "serialization" ~params:[ "load_from", "db"
                                   ; "format", "binary"
                                   ; "host", host];
  Db.load_oplists host digest
  |> Option.map
    ~f:(fun x ->
        (* Supposedly, we're supposed to remove an ending \n that
         * to_line helpfully adds, but that hasn't been a problem so
         * far. *)
        Core_extended.Bin_io_utils.of_line x Op.bin_oplist)

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
             | e -> (None, (errors @ [e])))) in
  match (result, errors) with
  | (Some r, _) -> r
  | (None, []) -> []
  | (None, es) ->
    let msgs =
      List.mapi ~f:(fun i ex -> (string_of_int i, Exn.to_string ex))
        es
    in
    Log.erroR "deserialization" ~params:msgs;
    Exception.internal ~info:msgs ("storage errors with " ^ host)

let load_deprecated_undo_json_from_disk =
  load_preprocessed_json_from_disk
    ~preprocess:(fun str ->
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
      )

let search_and_load (host: string) : Op.oplist =
  (* testfiles load and save from different directories *)
  if is_test host
  then
    (* when there are no oplists, read from disk. The test harnesses
     * clean up old oplists before running. *)
    deserialize_ordered host
      [ load_binary_from_db
      ; load_json_from_disk ~root:Testdata
      ]
  else
    let root = root_of host in
    deserialize_ordered host
      [ load_binary_from_db
      ; load_json_from_db
      ; load_json_from_disk ~root
      ; load_deprecated_undo_json_from_disk ~root
      ]

