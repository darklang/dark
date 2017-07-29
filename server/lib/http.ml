(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core
module C = Curl

let get url headers : string =
  (* C.global_init C.CURL_GLOBAL_ALL; *)
  let result = ref "" in
  let fn str : int =
    result := str;
    print_endline ("Have result: " ^ str);
    String.length str
  in
  let errorbuf = ref "" in
  let c = C.init () in
  C.set_url c "https://reddit.com";
  C.set_errorbuffer c errorbuf;
  C.set_followlocation c true;
  C.set_failonerror c true;
  C.set_writefunction c fn;
  C.perform c;
  let code = C.get_responsecode c in
  print_endline ("code is: " ^ (string_of_int code));
  C.cleanup c;
  print_endline ("error is:" ^ !errorbuf);
  "test"

(* TODO *)
let post url headers body = get url headers
