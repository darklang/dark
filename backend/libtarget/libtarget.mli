(* Returns base64 encoding of the sha384 of the string *)
val digest384 : string -> string

(* Returns base64 encoding of the sha384 of the bytes *)
val digest384_bytes : Bytes.t -> string
(* Returns base64 encoding of the sha256 of the string *)
val digest256 : string -> string

val regexp_replace : pattern:string -> replacement:string -> string -> string

val string_split : sep:string -> string -> string list
