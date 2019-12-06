(* https://github.com/ocaml-batteries-team/batteries-included/blob/ce92e34230b84c0b533fe8811d5e0d723eed2ab2/src/batPervasives.ml *)

let rec dump r =
  if Obj.is_int r
  then string_of_int (Obj.magic r : int)
  else
    (* Block. *)
    let rec get_fields acc = function
      | 0 ->
          acc
      | n ->
          let n = n - 1 in
          get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r
      then r = Obj.repr 0 (* [] *)
      else
        let s = Obj.size r
        and t = Obj.tag r in
        t = 0 && s = 2 && is_list (Obj.field r 1)
      (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r
      then []
      else
        let h = Obj.field r 0
        and t = get_list (Obj.field r 1) in
        h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible
       * in pure OCaml at the moment. *)
      "<" ^ name ^ ">"
    in
    let s = Obj.size r
    and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
    | _ when is_list r ->
        let fields = get_list r in
        "[" ^ String.concat "; " (List.map dump fields) ^ "]"
    | 0 ->
        let fields = get_fields [] s in
        "(" ^ String.concat ", " (List.map dump fields) ^ ")"
    | x when x = Obj.lazy_tag ->
        (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
         * clear if very large constructed values could have the same
         * tag. XXX *)
        opaque "lazy"
    | x when x = Obj.closure_tag ->
        opaque "closure"
    | x when x = Obj.object_tag ->
        let fields = get_fields [] s in
        let _clasz, id, slots =
          match fields with h :: h' :: t -> (h, h', t) | _ -> assert false
        in
        (* No information on decoding the class (first field).  So just print
         * out the ID and the slots. *)
        "Object #"
        ^ dump id
        ^ " ("
        ^ String.concat ", " (List.map dump slots)
        ^ ")"
    | x when x = Obj.infix_tag ->
        opaque "infix"
    | x when x = Obj.forward_tag ->
        opaque "forward"
    | x when x < Obj.no_scan_tag ->
        let fields = get_fields [] s in
        "Tag"
        ^ string_of_int t
        ^ " ("
        ^ String.concat ", " (List.map dump fields)
        ^ ")"
    | x when x = Obj.string_tag ->
        "\"" ^ String.escaped (Obj.magic r : string) ^ "\""
    | x when x = Obj.double_tag ->
        string_of_float (Obj.magic r : float)
    | x when x = Obj.abstract_tag ->
        opaque "abstract"
    | x when x = Obj.custom_tag ->
        opaque "custom"
    | x when x = (Obj.final_tag [@warning "-3"]) ->
        opaque "final"
    (* | x when x = Obj.double_array_tag -> *)
    (*   BatIO.to_string (BatArray.print BatFloat.print) (Obj.magic r : float array) *)
    | _ ->
        opaque (Printf.sprintf "unknown: tag %d size %d" t s)


let dump v = dump (Obj.repr v)
