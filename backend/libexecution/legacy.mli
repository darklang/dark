module PrettyResponseJsonV0 : sig
  (* Original function for sending json back to the user. This uses an object
    * format for dates. *)
  val to_pretty_response_json_v0 : Types.RuntimeT.dval -> string
end

module PrettyRequestJsonV0 : sig
  (* Original function for making JSON HTTP requests. This uses an odd (and
   * invalid) format for dates ("<Date: datestr>"), as well as DBs, Errros,
   * UUIDs and IDs,wraps characters in single quotes (invalid json), errors
   * on results, includes the string "Nothing/Just" for options *)
  val to_pretty_request_json_v0 : Types.RuntimeT.dval -> string
end
