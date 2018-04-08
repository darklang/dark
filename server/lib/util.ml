open Core
open Lwt.Infix

let readfile f : string =
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

let readfile_lwt ?(default="") f : string Lwt.t =
  Lwt_io.with_file ~mode:Lwt_io.input f (Lwt_io.read ?count:None) >|= function
  | "" -> default
  | s -> s

let writefile (f: string) (str: string) : unit =
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  let file = Unix.openfile f ~mode:flags ~perm:0o640 in
  let _ = Unix.write file ~buf:(Bytes.of_string str) in
  Unix.close file



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

