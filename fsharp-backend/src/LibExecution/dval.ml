(* ------------------------- *)
(* Obj Functions *)
(* ------------------------- *)
let obj_merge (l : dval) (r : dval) : dval =
  match (l, r) with
  | DObj l, DObj r ->
      DObj (Util.merge_left l r)
  | DNull, DObj r ->
      DObj r
  | DObj l, DNull ->
      DObj l
  | _ ->
      Exception.code "was expecting objs"


let empty_dobj : dval = DObj DvalMap.empty

let of_unknown_json_v0 str =
  try str |> Yojson.Safe.from_string |> unsafe_dval_of_yojson_v0
  with e ->
    Exception.code ~actual:str ("Invalid json: " ^ Exception.to_string e)


let of_unknown_json_v1 str =
  let rec convert json =
    match json with
    | `Int i ->
        DInt (Dint.of_int i)
    | `Intlit i ->
        DInt (Dint.of_string_exn i)
    | `Float f ->
        DFloat f
    | `Bool b ->
        DBool b
    | `Null ->
        DNull
    | `String s ->
        dstr_of_string_exn s
    | `List l ->
        to_list (List.map ~f:convert l)
    | `Variant v ->
        Exception.internal "We dont use variants"
    | `Tuple v ->
        Exception.internal "We dont use tuples"
    | `Assoc alist ->
        DObj
          (List.fold_left
             alist
             ~f:(fun m (k, v) -> DvalMap.insert m ~key:k ~value:(convert v))
             ~init:DvalMap.empty)
  in
  str |> Yojson.Safe.from_string |> convert


let rec show dv =
  match dv with
  | DInt i ->
      Dint.to_string i
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
  | DUuid uuid ->
      Uuidm.to_string uuid
  | DDB dbname ->
      "<DB: " ^ dbname ^ ">"
  | DError (_, msg) ->
      "<Error: " ^ msg ^ ">"
  | DIncomplete SourceNone ->
      "<Incomplete>"
  | DIncomplete (SourceId (tlid, id)) ->
      Printf.sprintf "<Incomplete[%s,%s]>" (string_of_id tlid) (string_of_id id)
  | DBlock _ ->
      (* See docs/dblock-serialization.ml *)
      "<Block>"
  | DPassword _ ->
      (* redacting, do not unredact *)
      "<Password>"
  | DObj o ->
      to_nested_string ~reprfn:show dv
  | DList l ->
      to_nested_string ~reprfn:show dv
  | DErrorRail d ->
      (* We don't print error here, because the errorrail value will know
          * whether it's an error or not. *)
      "<ErrorRail: " ^ show d ^ ">"
  | DResp (dh, dv) ->
      dhttp_to_formatted_string dh ^ "\n" ^ show dv ^ ""
  | DResult (ResOk d) ->
      "Ok " ^ show d
  | DResult (ResError d) ->
      "Error " ^ show d
  | DOption (OptJust d) ->
      "Just " ^ show d
  | DOption OptNothing ->
      "Nothing"
  | DBytes bytes ->
      "<Bytes: length=" ^ string_of_int (RawBytes.length bytes) ^ ">"


let parse_literal (str : string) : dval option =
  let len = String.length str in
  (* Character *)
  if len > 2 && str.[0] = '\'' && str.[len - 1] = '\''
  then
    Some
      (DCharacter
         (Unicode_string.Character.unsafe_of_string
            (String.sub ~pos:1 ~len:(len - 2) str)))
    (* String *)
  else if len > 1 && str.[0] = '"' && str.[len - 1] = '"'
  then
    (* It might have \n characters in it (as well as probably other codes like
     * \r or some shit that we haven't taken into account), which need to be
     * converted manually to appropriate string chars. *)
    str
    |> String.sub ~pos:1 ~len:(len - 2)
    |> Util.string_replace "\\\"" "\""
    |> fun s -> Some (dstr_of_string_exn s)
  else if str = "null"
  then Some DNull
  else if str = "true"
  then Some (DBool true)
  else if str = "false"
  then Some (DBool false)
  else
    try Some (DInt (Dint.of_string_exn str))
    with _ ->
      ( match float_of_string_opt str with
      | Some v ->
          Some (DFloat v)
      | None ->
          None )


(* ------------------------- *)
(* Conversion Functions *)
(* ------------------------- *)
let to_char dv : string option =
  match dv with
  | DCharacter c ->
      Some (Unicode_string.Character.to_string c)
  | _ ->
      None


let to_int dv : Dint.t option = match dv with DInt i -> Some i | _ -> None

let to_float dv : Float.t option =
  match dv with DFloat f -> Some f | _ -> None


let dint (i : int) : dval = DInt (Dint.of_int i)

let to_dobj_exn (pairs : (string * dval) list) : dval =
  match DvalMap.from_list_unique pairs with
  | Ok ok ->
      DObj ok
  | Error err ->
      DError (SourceNone, err)


let to_string_opt dv : string option =
  match dv with DStr s -> Some (Unicode_string.to_string s) | _ -> None


let to_string_exn dv : string =
  match to_string_opt dv with
  | Some s ->
      s
  | None ->
      Exception.code "expecting str" ~actual:(to_developer_repr_v0 dv)


let to_dval_pairs_exn dv : (string * dval) list =
  match dv with
  | DObj obj ->
      DvalMap.to_list obj
  | _ ->
      Exception.code "expecting str" ~actual:(to_developer_repr_v0 dv)


let to_string_pairs_exn dv : (string * string) list =
  dv |> to_dval_pairs_exn |> List.map ~f:(fun (k, v) -> (k, to_string_exn v))


(* For putting into URLs as query params *)
let rec to_url_string_exn (dv : dval) : string =
  match dv with
  | DBlock _ ->
      (* See docs/dblock-serialization.ml *)
      "<" ^ (dv |> tipename) ^ ">"
  | DIncomplete _ | DPassword _ ->
      "<" ^ (dv |> tipename) ^ ">"
  | DInt i ->
      Dint.to_string i
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
  | DErrorRail d ->
      to_url_string_exn d
  | DError (_, msg) ->
      "error=" ^ msg
  | DUuid uuid ->
      Uuidm.to_string uuid
  | DResp (_, hdv) ->
      to_url_string_exn hdv
  | DList l ->
      "[ " ^ String.concat ~sep:", " (List.map ~f:to_url_string_exn l) ^ " ]"
  | DObj o ->
      let strs =
        DvalMap.foldl o ~init:[] ~f:(fun ~key ~value l ->
            (key ^ ": " ^ to_url_string_exn value) :: l)
      in
      "{ " ^ String.concat ~sep:", " strs ^ " }"
  | DOption OptNothing ->
      "none"
  | DOption (OptJust v) ->
      to_url_string_exn v
  | DResult (ResError v) ->
      "error=" ^ to_url_string_exn v
  | DResult (ResOk v) ->
      to_url_string_exn v
  | DBytes bytes ->
      bytes |> RawBytes.to_string |> B64.encode


(* ------------------------- *)
(* Forms and queries Functions *)
(* ------------------------- *)

let query_to_dval (query : (string * string list) list) : dval =
  query
  |> List.map ~f:(fun (key, vals) ->
         let dval =
           match vals with
           | [] ->
               DNull
           | [v] ->
               if v = "" then DNull else dstr_of_string_exn v
           | vals ->
               DList (List.map ~f:(fun x -> dstr_of_string_exn x) vals)
         in
         (key, dval))
  |> DvalMap.from_list
  |> DObj


let dval_to_query (dv : dval) : (string * string list) list =
  match dv with
  | DObj kvs ->
      kvs
      |> DvalMap.to_list
      |> List.map ~f:(fun (k, value) ->
             match value with
             | DNull ->
                 (k, [])
             | DList l ->
                 (k, List.map ~f:to_url_string_exn l)
             | _ ->
                 (k, [to_url_string_exn value]))
  | _ ->
      Exception.code "attempting to use non-object as query param"


let to_form_encoding (dv : dval) : string =
  dv |> dval_to_query |> Uri.encoded_of_query


let of_form_encoding (f : string) : dval =
  f |> Uri.query_of_encoded |> query_to_dval

(* ------------------------- *)
(* Hashes *)
(* ------------------------- *)
