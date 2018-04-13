open Core
open Lwt.Infix

(* ------------------- *)
(* file stuff with checks *)
(* ------------------- *)
let check_filename ~(root:Config.root) ~mode f =
  let dir = Config.dir root in
  let f = dir ^ f in

  let debug str value =
    if false
    then
      Log.debug str value
    else
      value
  in

  if root <> No_check
  && (String.is_substring ~substring:".." f |> debug "dots"
      || String.contains f '~' |> debug "tilde"
      || String.is_suffix ~suffix:"." f |> debug "tilde"
      || (mode <> `Dir
          && String.is_suffix ~suffix:"/" f) |> debug "ends slash"
      || not (String.is_suffix ~suffix:"/" dir) |> debug "dir no slash"
      || String.is_substring ~substring:"etc/passwd" f |> debug "etc"
      (* being used wrong *)
      || String.is_substring ~substring:"//" f |> debug "double slash"
      (* check for irregular file *)
      || (mode = `Read
          && not (Sys.is_file f = `Yes))) |> debug "irreg"
  then
    (Log.erroR "SECURITY_VIOLATION" f;
    Exception.client "")
  else
    f

let file_exists ~root f : bool =
  let f = check_filename ~root ~mode:`Check f in
  Sys.file_exists f = `Yes

let mkdir ~root dir : unit =
  let dir = check_filename ~root ~mode:`Dir dir in
  Unix.mkdir_p dir

let lsdir ~root dir : string list =
  let dir = check_filename ~root ~mode:`Dir dir in
  Sys.ls_dir dir

let rmRF ~root dir : unit =
  let dir = check_filename ~root ~mode:`Dir dir in
  Core_extended.Shell.rm ~r:() ~f:() dir

let readfile ~root f : string =
  let f = check_filename ~root ~mode:`Read f in
  let ic = Caml.open_in f in
  (try
    let n = Caml.in_channel_length ic in
    let s = Bytes.create n in
    Caml.really_input ic s 0 n;
    Caml.close_in ic;
    Caml.Bytes.to_string s
  with e ->
    Caml.close_in_noerr ic;
    raise e)


let readfile_lwt ~root f : string Lwt.t =
  let f = check_filename ~root ~mode:`Read f in
  Lwt_io.with_file ~mode:Lwt_io.input f Lwt_io.read

let writefile ~root (f: string) (str: string) : unit =
  let f = check_filename ~root ~mode:`Write f in
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  Unix.with_file ~perm:0o600 ~mode:flags f
    ~f:(fun desc ->
        let _ = Unix.write desc ~buf:(Bytes.of_string str) in ())

(* ------------------- *)
(* json *)
(* ------------------- *)

let readjsonfile ~root ~(conv: (Yojson.Safe.json -> ('a, string) result)) (filename: string) : 'a =
  filename
  |> readfile ~root
  |> Yojson.Safe.from_string
  |> conv
  |> Result.ok_or_failwith

let writejsonfile ~root ~(conv: ('a -> Yojson.Safe.json)) ~(value:'a) filename
  : unit =
  value
  |> conv
  |> Yojson.Safe.to_string
  |> writefile ~root filename

(* ------------------- *)
(* binary *)
(* ------------------- *)
let writebinaryfile ~root ~(conv: 'a Bin_prot__Type_class.writer)
    ~(value: 'a) f : unit =
  let f = check_filename ~root ~mode:`Write f in
  Core_extended.Bin_io_utils.save f conv value

let readbinaryfile ~root ~(conv: 'a Core.Bin_prot.Read.reader) (f: string) : 'a =
  let f = check_filename ~root ~mode:`Read f in
  Core_extended.Bin_io_utils.load f conv

(* ------------------- *)
(* spawning *)
(* ------------------- *)
let convert_bin_to_json ~root (infile: string) (outfile: string) =
  let infile = check_filename ~root ~mode:`Read infile in
  let outfile = check_filename ~root ~mode:`Write outfile in


  (* Goes through execve, so never hits the shell *)
  Spawn.spawn
    ~prog:(Config.dir Config.Bin_root ^ "darkfile_bin_to_json.exe")
    ~argv:[""; infile; outfile]
    ()



(* ------------------- *)
(* random *)
(* ------------------- *)
let create_id (_ : unit) : int =
  Random.int (Int.pow 2 29)

let string_replace (search: string) (replace: string) (str: string) : string =
  String.Search_pattern.replace_all (String.Search_pattern.create search) ~in_:str ~with_:replace

let random_string length =
    let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let gen _ = String.make 1 (char_of_int(gen())) in
    String.concat ~sep:"" (Array.to_list (Array.init length gen))


let list_repeat times item =
  List.init times ~f:(fun _ -> item)

let merge_left = Map.merge ~f:(fun ~key v ->
    match v with
    | `Left v -> Some v
    | `Right v -> Some v
    | `Both (v1, v2) -> Some v1)

let list_any ~(f: 'a -> 'b) (l: 'a list) : bool =
  List.length (List.filter ~f l) > 0

let hash (input: string) : string =
  input
  |> Cstruct.of_string
  |> Nocrypto.Hash.SHA1.digest
  |> Cstruct.to_string
  |> B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false

