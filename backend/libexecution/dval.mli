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

val unsafe_tipe_to_yojson : Types.RuntimeT.tipe -> Yojson.Safe.t

val unsafe_tipe_of_yojson :
  Yojson.Safe.t -> (Types.RuntimeT.tipe, 'a) Core_kernel._result

(* ------------------------- *)
(* Representations *)
(* ------------------------- *)

(* This is a format used for roundtripping dvals internally. v0 has bugs due to
 * a legacy of trying to make one function useful for everything. Does not
 * redact. *)
val to_internal_roundtrippable_v0 : Types.RuntimeT.dval -> string

(* This is a format used for roundtripping dvals internally. There are some
 * rare cases where it will parse incorrectly without error. Throws on Json
 * bugs. *)
val of_internal_roundtrippable_v0 : string -> Types.RuntimeT.dval

val of_internal_roundtrippable_json_v0 :
  Yojson.Safe.t -> (Types.RuntimeT.dval, string) Core_kernel._result

(* This is a format used for roundtripping dvals internally, while still being
 * queryable using jsonb in our DB. v0 has bugs due to a legacy of trying to
 * make one function useful for everything. Also roundtrippable. Does not
 * redact. *)
val to_internal_queryable_v0 : Types.RuntimeT.dval -> string

(* This is a format used for roundtripping dvals internally, while still being
 * queryable using jsonb in our DB. There are some rare cases where it will
 * parse incorrectly without error. Throws on Json bugs. *)
val of_internal_queryable_v0 : string -> Types.RuntimeT.dval

(* This is a format used for roundtripping dvals internally, while still being
 * queryable using jsonb in our DB. This reduces some of the v0 bugs, but at
 * the cost of not supporting many typed that we'll want to put in it.  Also
 * roundtrippable. Does not redact. *)
val to_internal_queryable_v1 : Types.RuntimeT.dval -> string

(* This is a format used for roundtripping dvals internally, while still being
 * queryable using jsonb in our DB. There are some rare cases where it will
 * parse incorrectly without error. Throws on Json bugs. *)

val of_internal_queryable_v1 : string -> Types.RuntimeT.dval

(* When printing to grand-users (our users' users) using text/plain, print a
 * human-readable format. TODO: this should probably be part of the functions
 * generating the responses. Redacts passwords. *)
val to_enduser_readable_text_v0 : Types.RuntimeT.dval -> string

(* When printing to grand-users (our users' users) using text/html, attempt to
 * extract a html-like string. Redacts passwords. TODO: this should probably be
 * part of the functions generating the responses. *)
val to_enduser_readable_html_v0 : Types.RuntimeT.dval -> string

(* For printing something for the developer to read, as a live-value, error
 * message, etc. This will faithfully represent the code, textually. Redacts
 * passwords. Customers should not come to rely on this format. *)
val to_developer_repr_v0 : Types.RuntimeT.dval -> string

(* For passing to Dark functions that operate on JSON, such as the JWT fns.
 * This turns Option and Result into plain values, or null/error. String-like
 * values are rendered as string. Redacts passwords.  *)
val to_pretty_machine_yojson_v1 : Types.RuntimeT.dval -> Yojson.Safe.t

(* When sending json back to the user, or via a HTTP API, attempt to convert
 * everything into reasonable json, in the absence of a schema. This turns
 * Option and Result into plain values, or null/error. String-like values are
 * rendered as string. Redacts passwords.  *)
val to_pretty_machine_json_v1 : Types.RuntimeT.dval -> string

(* When receiving unknown json from the user, or via a HTTP API, attempt to
 * convert everything into reasonable types, in the absense of a schema.
 * This does type conversion, which it shouldn't and should be avoided for new code. *)
val of_unknown_json_v0 : string -> Types.RuntimeT.dval

(* When receiving unknown json from the user, or via a HTTP API, attempt to
 * convert everything into reasonable types, in the absense of a schema.  *)
val of_unknown_json_v1 : string -> Types.RuntimeT.dval

(* For debugging internally, redacts passwords. Never throws. *)
val show : Types.RuntimeT.dval -> string

(* JSON coming in from the user as part of a known API should have a type which
 * can act as a schema to reconstruct the data perfectly. Redacts passwords. *)
(* type schema  *)
(* val of_json_with_schema : schema: schema -> Yojson.Safe.t -> Types.RuntimeT.dval *)
(* val to_json_with_schema : schema: schema -> Types.RuntimeT.dval -> Yojson.Safe.t  *)

(* Parse our internal literal strings (eg AST Values) *)
val parse_literal : string -> Types.RuntimeT.dval option

(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)

(* queries *)
val query_to_dval : (string * string list) list -> Types.RuntimeT.dval

val dval_to_query : Types.RuntimeT.dval -> (string * string list) list

(* forms *)
val to_form_encoding : Types.RuntimeT.dval -> string

val of_form_encoding : string -> Types.RuntimeT.dval

(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)

(* If a DCharacter, returns `Some char`, as a string (Dark characters are EGCs,
 * and can be longer than a byte. *)
val to_char : Types.RuntimeT.dval -> string option

val to_int : Types.RuntimeT.dval -> Dint.t option

val dint : int -> Types.RuntimeT.dval

(* Converts a Dark String to an OCaml string. *)
val to_string_opt : Types.RuntimeT.dval -> string option

(* Converts a Dark String to an OCaml string. Raises an Exception if not a
 * string. *)
val to_string_exn : Types.RuntimeT.dval -> string

(* Converts an object to (string,string) pairs. Raises an exception if not an
 * object. *)
val to_string_pairs_exn : Types.RuntimeT.dval -> (string * string) list

(* Converts an object to (string,dval) pairs. Raises an exception if not an
 * object. *)
val to_dval_pairs_exn :
  Types.RuntimeT.dval -> (string * Types.RuntimeT.dval) list

(* For putting into URLs as query params  *)
val to_url_string_exn : Types.RuntimeT.dval -> string

(* Errors if the values in the list are not strings, or if any key is
 * duplicated. *)
val to_dobj_exn : (string * Types.RuntimeT.dval) list -> Types.RuntimeT.dval

val exception_to_dval : Core_kernel.Exn.t -> Types.RuntimeT.dval

(* ------------------------- *)
(* ErrorRail Functions *)
(* ------------------------- *)
val is_errorrail : Types.RuntimeT.dval -> bool

val unwrap_from_errorrail : Types.RuntimeT.dval -> Types.RuntimeT.dval

val to_list : Types.RuntimeT.dval list -> Types.RuntimeT.dval

val is_fake_marker_dval : Types.RuntimeT.dval -> bool

(* ------------------------- *)
(* Object Functions *)
(* ------------------------- *)
val obj_merge :
  Types.RuntimeT.dval -> Types.RuntimeT.dval -> Types.RuntimeT.dval

val empty_dobj : Types.RuntimeT.dval

(* ------------------------- *)
(* Misc *)
(* ------------------------- *)
val hash : Types.RuntimeT.dval list -> string
