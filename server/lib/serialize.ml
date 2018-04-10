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

let is_test (name: string) : bool =
  String.is_prefix ~prefix:"test_" name

let savedir (name: string): string =
  if is_test name
  then Config.completed_test_dir
  else Config.appdata_dir

let full dir name digest suffix =
  if digest = ""
  then
    dir ^ "/" ^ name ^ "." ^ suffix
  else
    dir ^ "/" ^ name ^ "_" ^ digest ^ "." ^ suffix

let binary_save_filename name = full (savedir name) name digest "dark"
let json_unversioned_filename name = full (savedir name) name "" "json"


let current_filenames () : string list =
  Util.lsdir Config.appdata_dir
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
  Util.readjsonfile ~conv:Op.oplist_of_yojson filename

let save_json (filename: string) (ops: Op.oplist) : unit =
  ops
  |> Op.oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> Util.writefile filename


let search_and_load (name: string) : Op.oplist =
  (* testfiles load and save from different directories *)
  if is_test name
  then
    (* we allow loading from the completed_test_dir so that subsequent
     * steps in the test use the altered file. The test harness cleans
     * them first *)
    let f = full Config.completed_test_dir name digest "dark" in
    if Sys.file_exists f = `Yes
    then load_binary f
    else
      let f = full Config.testdata_dir name "" "json" in
      if Sys.file_exists f = `Yes
      then load_json f
      else []
  else
    let bin = binary_save_filename name in
    let json = json_unversioned_filename name in

    (* binary file first, falling back to json if it won't load *)
    if Sys.file_exists bin = `Yes
    then
      (try
        load_binary bin
      with
      | e ->
          if Sys.file_exists json = `Yes
          then
            (try
              load_json json
             with
             | e ->
               Deserialize_b68219ec99d4a17c9a1d6524129da928.load_json json)
          else raise e)

    else
      if Sys.file_exists json  = `Yes
      then
        (try
           load_json json
         with
         | e ->
           Deserialize_b68219ec99d4a17c9a1d6524129da928.load_json json)

      else []


