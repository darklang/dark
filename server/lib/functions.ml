open Core
open Types
open Types.RuntimeT

module RT = Runtime

let dval_of_yojson = Dval.dval_of_yojson
let dval_to_yojson = Dval.dval_to_yojson
let equal_dval = Dval.equal_dval

(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
let param_to_string (param: param) : string =
  param.name
  ^ (if param.optional then "?" else "")
  ^ " : "
  ^ (Dval.tipe_to_string param.tipe)

let exe ?(ind=0) ~(ctx: context) (fnname: string) (fn: fn) (args: dval_map) : dval =
  let apply f arglist =
    match ctx with
    | Preview ->
      if fn.previewExecutionSafe
      then f arglist
      else DIncomplete
    | Real ->
      f arglist
  in
  match fn.func with
  | InProcess f ->
      let arglist = fn.parameters
                    |> List.map ~f:(fun (p: param) -> p.name)
                    |> List.map ~f:(DvalMap.find_exn args) in
      (try
         if List.exists ~f:(fun x ->
                              match x with
                              | DIncomplete -> true
                              | DError _ -> true
                              | _ -> false)
             arglist
         then DIncomplete
         else apply f arglist
       with
       | TypeError _ ->
           Log.erroR ~name:"execution" ~ind "exception caught" args
             ~f:Dval.dvalmap_to_string;
           let range = List.range 0 (List.length arglist) in
           let all = List.map3_exn range fn.parameters arglist ~f:(fun i p a -> (i,p,a)) in
           let invalid = List.filter_map all
                           ~f:(fun (i,p,a) -> if (Dval.tipe_of a <> p.tipe
                                                  && p.tipe <> TAny)
                               then Some (i,p,a)
                               else None) in
           (* let invalid_count = List.length invalid in *)
           match invalid with
           | [] -> Exception.internal "There was an type error in the arguments, but we had an error and can't find it"

           | (i,p,a) :: _ ->
              RT.raise_error
                ~actual:a
                ~expected:(Dval.tipe_to_string p.tipe)
                (fnname ^ " was called with the wrong type to parameter: " ^ p.name))

  | API f ->
      try
        f args
      with
      | TypeError args ->
          RT.raise_error (fnname ^ " is missing a parameter")
            ~expected:(fn.parameters
                       |> List.map ~f:param_to_string
                       |> String.concat ~sep:", ")
            ~actual:DIncomplete

