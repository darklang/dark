open Core

let digest = Op.bin_shape_oplist
             |> Bin_prot.Shape.eval_to_digest_string

let write_shape_data () =
  let shape_string = Op.bin_shape_oplist
                   |> Bin_prot.Shape.eval
                   |> Bin_prot.Shape.Canonical.to_string_hum
  in
  Util.writefile (Config.serialization_dir ^ digest) shape_string

let no_digest_filename_for name = Config.appdata_dir ^ name ^ ".dark"
let current_filename_for name = Config.appdata_dir ^ name ^ "_" ^ digest ^ ".dark"
let json_filename_for name = Config.appdata_dir ^ name ^ "_" ^ digest ^ ".json"

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
  if Sys.file_exists filename = `Yes
  then
    Core_extended.Bin_io_utils.load filename Op.bin_read_oplist
  else
    []

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


let load_custom =
  (* load_binary *)
  load_json
  (* Deserialize_d2074bb17e2b1a88a49546687a5e8c2e.load_binary *)


