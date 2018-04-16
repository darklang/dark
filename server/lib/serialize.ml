open Core

let digest = Op.bin_shape_oplist
             |> Bin_prot.Shape.eval_to_digest_string

let write_shape_data () =
  if Config.should_write_shape_data
  then
    let shape_string = Op.bin_shape_oplist
                     |> Bin_prot.Shape.eval
                     |> Bin_prot.Shape.Canonical.to_string_hum
    in
    Util.writefile ~root:Serialization digest shape_string
  else
    ()

let is_test (name: string) : bool =
  String.is_prefix ~prefix:"test_" name

let root_of (name: string): Config.root =
  if is_test name
  then Completed_test
  else Appdata

let full name digest suffix =
  if digest = ""
  then
    name ^ "." ^ suffix
  else
    name ^ "_" ^ digest ^ "." ^ suffix

let binary_save_filename name = full name digest "dark"
let json_unversioned_filename name = full name "" "json"


let current_filenames () : string list =
  Util.lsdir ~root:Appdata ""
  |> List.filter
       ~f:(fun f ->
        String.is_substring ~substring:".dark" f
          && String.is_substring ~substring:digest f
          && (not (String.is_substring ~substring:"test_" f))
      )

let load_binary ~root (filename: string) : Op.oplist =
  (* We lost data here before!! We previously caught all exceptions and
   * used a default value of []. Which means we wiped out our old Dark
   * code on deserialization errors *facepalm*. So be careful here *)
  Util.readbinaryfile ~root ~conv:Op.bin_read_oplist filename

let save_binary ~root (filename: string) (ops: Op.oplist) : unit =
  Util.writebinaryfile ~root ~conv:Op.bin_writer_oplist ~value:ops filename

let load_json ~root (filename:string) : Op.oplist =
  Util.readjsonfile ~root ~conv:Op.oplist_of_yojson filename

let save_json ~root (filename: string) (ops: Op.oplist) : unit =
  ops
  |> Op.oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> Util.writefile ~root filename

let save_in_db (host: string) (ops: Op.oplist) : unit =
  ops
  |> Core_extended.Bin_io_utils.to_line Op.bin_oplist
  (* |> to_line Op.bin_oplist *)
  |> Bigstring.to_string
  |> Db.save_oplists host


let load_from_db (host: string) : Op.oplist option =
  host
  |> Db.load_oplists
  |> Option.map
    ~f:(fun x ->
        (* of_line x Op.bin_oplist) *)
        Core_extended.Bin_io_utils.of_line x Op.bin_oplist)

let deserialize_ordered
    (host : string)
    (descs : ((root:Config.root -> string -> Op.oplist) * Config.root * string) list)
    : Op.oplist =
  (* try each in turn. If the file exists, try it, and return if
    successful. If it fails, save the error and try the next one *)
  match load_from_db host with
  | Some ops -> ops
  | None ->
      let (result, errors) =
        List.fold descs ~init:(None, [])
          ~f:(fun (result, errors) (fn, root, filename) ->
              match result with
              | Some r -> (result, errors)
              | None ->
                if not (Util.file_exists ~root filename)
                then (None, errors)
                else
                  (try
                     (Some (fn ~root filename), errors)
                   with
                   | e -> (None, (errors @ [e])))) in
      match (result, errors) with
      | (Some r, _) -> r
      | (None, []) -> []
      | (None, es) ->
        Log.erroR "deserialization" es;
        Exception.internal ("storage error with " ^ host)

let search_and_load (host: string) : Op.oplist =
  (* testfiles load and save from different directories *)
  if is_test host
  then
    (* we allow loading from the Completed_test dir so that subsequent
     * steps in the test use the altered file. The test harness cleans
     * them first *)
    deserialize_ordered host
      [ (load_binary, Completed_test, full host digest "dark")
      ; (load_json, Testdata, full host "" "json")
      ]
  else
    let root = root_of host in
    let json = json_unversioned_filename host in
    let bin = binary_save_filename host in
    deserialize_ordered host
      [ (load_binary, root, bin)
      ; (load_json, root, json)
      ; (Deserialize_b68219ec99d4a17c9a1d6524129da928.load_json, root, json)
      ]

