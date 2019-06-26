open Core
open Libcommon
open Lwt.Infix
open Libexecution

(* ------------------- *)
(* file stuff with checks *)
(* ------------------- *)
let check_filename ~(root : Config.root) ~mode f =
  let dir = Config.dir root in
  let f = dir ^ f in
  let debug name value =
    if false then Log.debuG name ~data:(string_of_bool value) ;
    value
  in
  if root <> No_check
     && ( String.is_substring ~substring:".." f |> debug "dots"
        || String.contains f '~' |> debug "tilde"
        || String.is_suffix ~suffix:"." f |> debug "tilde"
        || (mode <> `Dir && String.is_suffix ~suffix:"/" f)
           |> debug "ends slash"
        || (not (String.is_suffix ~suffix:"/" dir)) |> debug "dir no slash"
        || String.is_substring ~substring:"etc/passwd" f |> debug "etc"
        (* being used wrong *)
        || String.is_substring ~substring:"//" f |> debug "double slash"
        (* check for irregular file *)
        || (mode = `Read && not (Sys.is_file f = `Yes)) )
        |> debug "irreg"
  then (
    Log.erroR "SECURITY_VIOLATION" ~data:f ;
    Exception.internal "FILE SECURITY VIOLATION" )
  else f


let file_exists ~root f : bool =
  let f = check_filename ~root ~mode:`Check f in
  Sys.file_exists f = `Yes


let mkdir ~root dir : unit =
  let dir = check_filename ~root ~mode:`Dir dir in
  Unix.mkdir_p dir


let lsdir ~root dir : string list =
  let dir = check_filename ~root ~mode:`Dir dir in
  Sys.ls_dir dir


let rm ~root file : unit =
  let file = check_filename ~root ~mode:`Write file in
  Core_extended.Shell.rm ~f:() file


let readfile ~root f : string =
  let f = check_filename ~root ~mode:`Read f in
  let ic = Caml.open_in f in
  try
    let n = Caml.in_channel_length ic in
    let s = Bytes.create n in
    Caml.really_input ic s 0 n ;
    Caml.close_in ic ;
    Caml.Bytes.to_string s
  with e ->
    Caml.close_in_noerr ic ;
    raise e


let readfile_lwt ~root f : string Lwt.t =
  let f = check_filename ~root ~mode:`Read f in
  Lwt_io.with_file ~mode:Lwt_io.input f Lwt_io.read


let writefile ~root (f : string) (str : string) : unit =
  let f = check_filename ~root ~mode:`Write f in
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  Unix.with_file ~perm:0o600 ~mode:flags f ~f:(fun desc ->
      ignore (Unix.write desc ~buf:(Bytes.of_string str)) )


(* ------------------- *)
(* json *)
(* ------------------- *)

let readjsonfile
    ~root
    ?(stringconv : string -> string = ident)
    ~(conv : Yojson.Safe.t -> ('a, string) result)
    (filename : string) : 'a =
  filename
  |> readfile ~root
  |> stringconv
  |> Yojson.Safe.from_string
  |> conv
  |> Result.ok_or_failwith


let maybereadjsonfile
    ~root
    ?(stringconv : string -> string = ident)
    ~(conv : Yojson.Safe.t -> ('a, string) result)
    (filename : string) : 'a option =
  if file_exists ~root filename
  then Some (readjsonfile ~root ~stringconv ~conv filename)
  else None


(* ------------------- *)
(* spawning *)
(* ------------------- *)

let init () =
  (* Spawn creates lots of child processes. When they finish, the OS
   * asks the dark executable what to do. This tells it to ignore them
   * in such a way that the OS will clean them up. (I thought this was
   * the default, but this appears to fix the problem). *)
  Signal.ignore Signal.chld
