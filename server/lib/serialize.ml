open Core

let digest = Op.bin_shape_oplist
             |> Bin_prot.Shape.eval_to_digest_string

let write_shape_data () =
  if Sys.getenv "DARK_CONFIG_SAVE_SERIALIZATION_DIGEST" <> None
  then
    let shape_string = Op.bin_shape_oplist
                     |> Bin_prot.Shape.eval
                     |> Bin_prot.Shape.Canonical.to_string_hum
    in
    Util.writefile (Config.serialization_dir ^ digest) shape_string
  else
    ()

let full dir name digest suffix =
  if digest = ""
  then
    dir ^ "/" ^ name ^ "." ^ suffix
  else
    dir ^ "/" ^ name ^ "_" ^ digest ^ "." ^ suffix

let binary_save_filename name = full Config.appdata_dir name digest "dark"
let json_save_filename name = full Config.appdata_dir name digest "json"
let json_unversioned_filename name = full Config.appdata_dir name "" "json"
let completed_test_filename name = full Config.completed_test_dir name digest "dark"
let testdata_filename name = full Config.testdata_dir name "" "dark"

let current_filenames () : string list =
  Sys.ls_dir Config.appdata_dir
  |> List.filter
       ~f:(fun f ->
        String.is_substring ~substring:".dark" f
          && String.is_substring ~substring:digest f
          && (not (String.is_substring ~substring:"test_" f))
      )



let load_binary (filename: string) : Op.oplist =
  (* We lost data here before!! We previously caught all exceptions and
   * used a default value of []. Which means we wiped out our old Dark
   * code on deserialization errors *facepalm*. So be careful here *)
  Core_extended.Bin_io_utils.load filename Op.bin_read_oplist

let save_binary (filename: string) (ops: Op.oplist) : unit =
  Core_extended.Bin_io_utils.save filename Op.bin_writer_oplist ops


let load_json (filename:string) : Op.oplist =
  filename
  |> Util.readfile
  |> Yojson.Safe.from_string
  |> Op.oplist_of_yojson
  |> Result.ok_or_failwith

let save_json (filename: string) (ops: Op.oplist) : unit =
  ops
  |> Op.oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> Util.writefile filename


let search_and_load (name: string) : Op.oplist =
  if String.is_prefix ~prefix:"test_" name
  then
    let f = completed_test_filename name in
    if Sys.file_exists f = `Yes
    then load_binary f
    else
      let f = testdata_filename name in
      if Sys.file_exists f = `Yes
      then load_json f
      else []
  else
    let f = binary_save_filename name in
    if Sys.file_exists f = `Yes
    then
      (try
        load_binary f
      with
      | e ->
          let f = json_save_filename name in
          if Sys.file_exists f = `Yes
          then load_json f
          else (raise e))

    else
      let f = json_save_filename name in
      if Sys.file_exists f = `Yes
      then load_json f

      (* No savefile - generate empty canvas *)
      else []


