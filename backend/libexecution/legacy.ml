open Core_kernel
open Types
open Types.RuntimeT

module PrettyResponseJsonV0 = struct
  (* At time of writing, this is the same as Dval.unsafe_dval_to_yojson. It's being copied to be certain this format doesn't change. *)
  let rec unsafe_dval_to_yojson ?(redact = true) (dv : dval) : Yojson.Safe.json
      =
    let tipe = dv |> Dval.tipe_of |> Dval.unsafe_tipe_to_yojson in
    let wrap_user_type value = `Assoc [("type", tipe); ("value", value)] in
    let wrap_constructed_type cons values =
      `Assoc [("type", tipe); ("constructor", cons); ("values", `List values)]
    in
    let wrap_user_str value = wrap_user_type (`String value) in
    match dv with
    (* basic types *)
    | DInt i ->
        `Int i
    | DFloat f ->
        `Float f
    | DBool b ->
        `Bool b
    | DNull ->
        `Null
    | DStr s ->
        Unicode_string.to_yojson s
    | DList l ->
        `List (List.map l (unsafe_dval_to_yojson ~redact))
    | DObj o ->
        o
        |> DvalMap.to_alist
        |> List.map ~f:(fun (k, v) -> (k, unsafe_dval_to_yojson ~redact v))
        |> fun x -> `Assoc x
    | DBlock _ | DIncomplete ->
        wrap_user_type `Null
    | DCharacter c ->
        wrap_user_str (Unicode_string.Character.to_string c)
    | DError msg ->
        wrap_user_str msg
    | DResp (h, hdv) ->
        wrap_user_type
          (`List [dhttp_to_yojson h; unsafe_dval_to_yojson ~redact hdv])
    | DDB dbname ->
        wrap_user_str dbname
    | DDate date ->
        wrap_user_str (Util.isostring_of_date date)
    | DPassword hashed ->
        if redact
        then wrap_user_type `Null
        else hashed |> Bytes.to_string |> B64.encode |> wrap_user_str
    | DUuid uuid ->
        wrap_user_str (Uuidm.to_string uuid)
    | DOption opt ->
      ( match opt with
      | OptNothing ->
          wrap_user_type `Null
      | OptJust dv ->
          wrap_user_type (unsafe_dval_to_yojson ~redact dv) )
    | DErrorRail dv ->
        wrap_user_type (unsafe_dval_to_yojson ~redact dv)
    | DResult res ->
      ( match res with
      | ResOk dv ->
          wrap_constructed_type
            (`String "Ok")
            [unsafe_dval_to_yojson ~redact dv]
      | ResError dv ->
          wrap_constructed_type
            (`String "Error")
            [unsafe_dval_to_yojson ~redact dv] )
    | DBytes bytes ->
        wrap_user_str (Bytes.to_string bytes)


  let to_pretty_response_json_v0 dval =
    unsafe_dval_to_yojson dval |> Yojson.Safe.pretty_to_string
end

module PrettyRequestJsonV0 = struct
  (* Returns the string within string-ish values, without adornment. *)
  let as_string (dv : dval) : string =
    match dv with
    | DInt i ->
        string_of_int i
    | DBool true ->
        "true"
    | DBool false ->
        "false"
    | DStr s ->
        Unicode_string.to_string s
    | DFloat f ->
        string_of_float f
    | DCharacter c ->
        Unicode_string.Character.to_string c
    | DNull ->
        "null"
    | DDate d ->
        Util.isostring_of_date d
    | DDB dbname ->
        dbname
    | DError msg ->
        msg
    | DUuid uuid ->
        Uuidm.to_string uuid
    | _ ->
        "<" ^ (dv |> Dval.tipename) ^ ">"


  let as_literal (dv : dval) : string =
    match dv with
    | DStr s ->
        "\"" ^ Unicode_string.to_string s ^ "\""
    | DCharacter c ->
        "'" ^ Unicode_string.Character.to_string c ^ "'"
    | _ ->
        as_string dv


  let is_primitive (dv : dval) : bool =
    match dv with
    | DInt _ | DFloat _ | DBool _ | DNull | DCharacter _ | DStr _ ->
        true
    | _ ->
        false


  let is_stringable (dv : dval) : bool =
    match dv with
    | DBlock _
    | DIncomplete
    | DError _
    | DDate _
    | DPassword _
    | DDB _
    | DUuid _ ->
        true
    | _ ->
        is_primitive dv


  (* A simple representation, showing primitives as their expected literal
   * syntax, and odd types get type info in a readable format. Compund
   * types are listed as their type only *)
  let to_simple_repr ?(open_ = "<") ?(close_ = ">") (dv : dval) : string =
    let wrap value = open_ ^ (dv |> Dval.tipename) ^ ": " ^ value ^ close_ in
    match dv with
    | dv when is_primitive dv ->
        as_literal dv
    | dv when is_stringable dv ->
        wrap (as_string dv)
    | _ ->
        open_ ^ (dv |> Dval.tipename) ^ close_


  let dhttp_to_formatted_string (d : dhttp) : string =
    match d with
    | Redirect url ->
        "302 " ^ url
    | Response (c, hs) ->
        let string_of_headers hs =
          hs
          |> List.map ~f:(fun (k, v) -> k ^ ": " ^ v)
          |> String.concat ~sep:","
          |> fun s -> "{ " ^ s ^ " }"
        in
        string_of_int c ^ " " ^ string_of_headers hs


  (* A full representation, building on to_simple_repr, but including
 * lists and objects. *)
  let rec to_pretty_request_json_v0 (dv : dval) : string =
    let pp = true in
    let open_ = "<" in
    let close_ = ">" in
    let reprfn = to_simple_repr ~open_ ~close_ in
    let rec to_repr_ (indent : int) (pp : bool) (dv : dval) : string =
      let nl = if pp then "\n" ^ String.make indent ' ' else " " in
      let inl = if pp then "\n" ^ String.make (indent + 2) ' ' else "" in
      let indent = indent + 2 in
      match dv with
      | dv when is_stringable dv ->
          reprfn dv
      | DResp (h, hdv) ->
          dhttp_to_formatted_string h ^ nl ^ to_repr_ indent pp hdv
      | DList l ->
          if List.is_empty l
          then "[]"
          else
            "[ "
            ^ inl
            ^ String.concat ~sep:", " (List.map ~f:(to_repr_ indent pp) l)
            ^ nl
            ^ "]"
      | DObj o ->
          if DvalMap.is_empty o
          then "{}"
          else
            let strs =
              DvalMap.fold o ~init:[] ~f:(fun ~key ~data l ->
                  (key ^ ": " ^ to_repr_ indent pp data) :: l )
            in
            "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
      | DOption OptNothing ->
          "Nothing"
      | DOption (OptJust dv) ->
          "Just " ^ to_repr_ indent pp dv
      | DErrorRail dv ->
          "ErrorRail: " ^ to_repr_ indent pp dv
      | _ ->
          failwith ("printing an unprintable value:" ^ to_simple_repr dv)
    in
    to_repr_ 0 pp dv
end
