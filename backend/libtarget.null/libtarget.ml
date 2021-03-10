(* This is an implementation of libtarget that does nothing. We need this to
 * link libocaml without re2, which doesn't like being compiled into a
 * shared library. The functions here are only used for the execution engine in
 * libexecution, which libocaml doesn't use. libocaml only uses
 * the types for serialization *)

let sha (input : string) ~f : string = ""

let sha_bytes (input : Bytes.t) ~f : string = ""

let digest256 (input : string) : string = ""

let digest384 (input : string) : string = ""

let digest384_bytes (input : Bytes.t) : string = ""

let base64url_bytes (bytes : Bytes.t) : string = ""

let regexp_replace ~(pattern : string) ~(replacement : string) (str : string) :
    string =
  ""


exception Invalid_B64 of string

let valid_rfc4648_b64_or_exn (str : string) = ""

(* Raises Not_found when passed a string with an out-of-alphabet character *)
let bytes_from_base64url (b64 : string) : Bytes.t = Bytes.empty

let string_split ~sep s : string list = []
