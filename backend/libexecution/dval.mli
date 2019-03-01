(* ------------------------- *)
(* Strings *)
(* ------------------------- *)

(* Given an OCaml string, turn it into a Dark string. Will return Nothing
 * if the OCaml string is not proper UTF-8. *)

val dstr_of_string : string -> Types.RuntimeT.dval option

(* Given an OCaml string, turn it into a Dark string. Will raise an exception
 * if the OCaml string is not proper UTF-8. *)

val dstr_of_string_exn : string -> Types.RuntimeT.dval

(* ------------------------- *)
(* Types *)
(* ------------------------- *)
val tipe_to_string : Types.tipe_ -> string

val tipe_of_string : Core_kernel.String.t -> Types.tipe_

val tipe_of : Types.RuntimeT.dval -> Types.tipe_

val tipename : Types.RuntimeT.dval -> string

val tipe_to_yojson : Types.RuntimeT.tipe -> Yojson.Safe.json

val tipe_of_yojson :
  Yojson.Safe.json -> (Types.RuntimeT.tipe, 'a) Core_kernel._result

val is_json_primitive : Types.RuntimeT.dval -> bool

(* ------------------------- *)
(* Representations *)
(* ------------------------- *)

(* This is a format used for roundtripping dvals internally. v0 has bugs due to
 * a legacy of trying to make one function useful for everything. *)
val to_internal_roundtrippable_v0 : Types.RuntimeT.dval -> string

(* This is a format used for roundtripping dvals internally. There are some
 * rare cases where it will parse incorrectly without error. Throws on Json
 * bugs. *)

val of_internal_roundtrippable_v0 : string -> Types.RuntimeT.dval

(* This is a format used for roundtripping dvals internally, while still being
 * queryable. v0 has bugs due to a legacy of trying to make one function useful
 * for everything. *)

val to_internal_queryable_v0 : Types.RuntimeT.dval -> string

(* This is a format used for roundtripping dvals internally, while still being
 * queryable. There are some rare cases where it will parse incorrectly without
 * error. Throws on Json bugs. *)
val of_internal_queryable_v0 : string -> Types.RuntimeT.dval

(* all uses:
  * for 3rd-party machine consumption, with schema
  * pretty print for humans
  * pretty print for live values
  * url query string
  * form encoding
  * Also:
  *   redacting?
  *   versioning
  *)

(* uses:
  * Libstd::toString
  * headers in httpclient
  * *)
val as_string : Types.RuntimeT.dval -> string

(* uses:
  * creating DErrors in AST.ml
  * error message in String::foreach, Int::sum
  * httpclient body encoding if not an obj
  * Libstd::toRepr
  *
  * Uses indentation
  * *)
val to_repr :
     ?pp:bool
  -> ?open_:string
  -> ?close_:string
  -> ?reprfn:(Types.RuntimeT.dval -> string)
  -> Types.RuntimeT.dval
  -> string

(* If someone returns a string or int, that's probably a web page. If
 * someone returns something else, show the structure so they can figure
 * out how to get it into a string.
 *
 * uses:
  * text/plain & text/html printing in user_page
  * *)
val to_human_repr : Types.RuntimeT.dval -> string

(* For putting into URLs as query params 
 * uses:
  * Libtwitter urls
  * dval_to_query
  * *)
val to_url_string_exn : Types.RuntimeT.dval -> string

(* json *)

(* The "unsafe" variations here are bad. They encode data ambiguously, and
 * though we mostly have the decoding right, it's brittle and unsafe.
 * Uses:
   * twitter
   * jsanalysis
   *)

val unsafe_dval_of_yojson :
  Yojson.Safe.json -> (Types.RuntimeT.dval, string) Core_kernel.result

(* Uses:
   * stored_function_result
   * stored_event
   * event queue
   * user_db
   *)
val unsafe_dval_of_json_string : string -> Types.RuntimeT.dval

(* Uses:
   * user_db
   * runtime errors
   * libhttpclient
   *)
val unsafe_dval_to_yojson :
  ?redact:bool -> Types.RuntimeT.dval -> Yojson.Safe.json

(* Uses:
   * user_page response 
   *)
val unsafe_dval_to_pretty_json_string :
  ?redact:bool -> Types.RuntimeT.dval -> string

(* dvalmap *)
(* Uses:
  * user_db
  * stored_function_arguments
  *)
val unsafe_dvalmap_to_yojson :
  ?redact:bool -> Types.RuntimeT.dval_map -> Yojson.Safe.json

(* Uses:
  * stored_function_arguments
  *)
val unsafe_dvalmap_of_yojson : Yojson.Safe.json -> Types.RuntimeT.dval_map

(* parsing *)
(* Uses:
  * JSON::read
  * parsed_request
  * parse_literal
  * libhttpclient
  *)
val parse_basic_json : string -> Types.RuntimeT.dval option

(* Parse our internal literal strings (eg AST Values) *)
val parse_literal : string -> Types.RuntimeT.dval option

(* queries *)
(* Uses:
  * from_form_encoding
  * parsed_request
  *)
val query_to_dval : (string * string list) list -> Types.RuntimeT.dval

(* Uses:
  * libhttpclient
   *)
val dval_to_query : Types.RuntimeT.dval -> (string * string list) list

(* forms *)
(* Uses:
  * libhttpclient
   *)
val to_form_encoding : Types.RuntimeT.dval -> string

(* Uses:
  * parsed_request
  * libhttpclient
   *)
val from_form_encoding : string -> Types.RuntimeT.dval

(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)

(* If a DCharacter, returns `Some char`, as a string (Dark characters are EGCs,
 * and can be longer than a byte. *)

val to_char : Types.RuntimeT.dval -> string option

(* If a DChar, returns a char. *)
val to_char_deprecated : Types.RuntimeT.dval -> char option

val to_int : Types.RuntimeT.dval -> int option

(* Converts a Dark String to an OCaml string. Raises an Exception if not a
 * string. *)
val to_string_exn : Types.RuntimeT.dval -> string

(* Converts an object to string pairs. Raises an exception if not an object. *)
val to_string_pairs_exn : Types.RuntimeT.dval -> (string * string) list

val to_dval_pairs_exn :
  Types.RuntimeT.dval -> (string * Types.RuntimeT.dval) list

(* Errors if the values in the list are not strings, or if any key is duplicated. *)
val to_dobj_exn : (string * Types.RuntimeT.dval) list -> Types.RuntimeT.dval

(* ------------------------- *)
(* ErrorRail Functions *)
(* ------------------------- *)
val is_errorrail : Types.RuntimeT.dval -> bool

val unwrap_from_errorrail : Types.RuntimeT.dval -> Types.RuntimeT.dval

(* ------------------------- *)
(* Object Functions *)
(* ------------------------- *)
val obj_merge :
  Types.RuntimeT.dval -> Types.RuntimeT.dval -> Types.RuntimeT.dval

val empty_dobj : Types.RuntimeT.dval

val exception_to_dval : Core_kernel.Exn.t -> Types.RuntimeT.dval

(* ------------------------- *)
(* Misc *)
(* ------------------------- *)
val hash : Types.RuntimeT.dval list -> string
