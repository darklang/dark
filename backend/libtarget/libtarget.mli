(* Returns base64 encoding of the sha384 of the string *)
val digest384 : string -> string

(* Returns base64 encoding of the sha384 of the bytes *)
val digest384_bytes : Bytes.t -> string

(* Returns base64url encoding of the bytes using the uri-safe alphabet from RFC 4648 section 5 *)
val base64url_bytes : Bytes.t -> string

(* Decodes the provided base64 string, getting bytes from it.
   XXX(JULIAN): behavior when given a non-compliant string diverges too
   much between frontend and backend. This must be fixed before merging *)
val bytes_from_base64url : string -> Bytes.t

(* Returns base64 encoding of the sha256 of the string *)
val digest256 : string -> string

val regexp_replace : pattern:string -> replacement:string -> string -> string

val string_split : sep:string -> string -> string list
