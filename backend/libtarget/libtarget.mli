(* Returns base64 encoding of the sha384 of the string *)
val digest384 : string -> string

(* Returns base64 encoding of the sha384 of the bytes *)
val digest384_bytes : Bytes.t -> string

val valid_rfc4648_b64_or_exn : string -> string

(* Returns base64url encoding of the bytes using the uri-safe alphabet from RFC 4648 section 5 *)
val base64url_bytes : Bytes.t -> string

(* Decodes the provided base64 string, getting bytes from it.
 * Note: this depends on valid_rfc4648_b64_or_exn; if the alphabet is not
 * compliant with section 5, an exception will be raised *)
val bytes_from_base64url : string -> Bytes.t

(* Returns base64 encoding of the sha256 of the string *)
val digest256 : string -> string

val regexp_replace : pattern:string -> replacement:string -> string -> string

val string_split : sep:string -> string -> string list
