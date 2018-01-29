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
type execute_t = (dval_map -> dval)
type argument = AEdge of int
              | AConst of dval
              [@@deriving eq, show, yojson]

let blank_arg = AConst DIncomplete
module ArgMap = String.Map
type arg_map = argument ArgMap.t

type param = { name: string
             ; tipe: tipe
             ; block_args : string list
             ; optional : bool
             ; description : string
             } [@@deriving eq, show, yojson]

let param_to_string (param: param) : string =
  param.name
  ^ (if param.optional then "?" else "")
  ^ " : "
  ^ (Dval.tipe_to_string param.tipe)


type ccfunc = InProcess of (dval list -> dval)
            | API of (dval_map -> dval)

type fn = { name : string
          ; other_names : string list
          ; parameters : param list
          ; return_type : tipe
          ; description : string
          ; preview : (dval list -> int -> dval list) option
          ; func : ccfunc
          ; previewExecutionSafe : bool
          }

let exe ?(ind=0) ~(ctx: context) (fn: fn) (args: dval_map) : dval =
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
        apply f arglist
       with
       | TypeError _ ->
           Log.pP ~name:"execution" ~ind "exception caught" args
             ~f:Dval.dvalmap_to_string;
           let range = List.range 0 (List.length arglist) in
           let all = List.map3_exn range fn.parameters arglist ~f:(fun i p a -> (i,p,a)) in
           let invalid = List.filter_map all
                           ~f:(fun (i,p,a) -> if Dval.tipe_of a <> p.tipe
                                              && p.tipe <> TAny
                                              || p.tipe = TIncomplete
                                              || p.tipe = TError
                               then Some (i,p,a)
                               else None) in
           (* let invalid_count = List.length invalid in *)
           match invalid with
           | [] -> Exception.internal "There was an type error in the arguments, but we had an error and can't find it"

           | (i,p,DIncomplete) :: _ ->
             DIncomplete

           | (i,p,DError _) :: _ ->
             DIncomplete

           | (i,p,a) :: _ ->
              RT.raise_error
                ~actual:a
                ~expected:(Dval.tipe_to_string p.tipe)
                (fn.name ^ " was called with the wrong type to parameter: " ^ p.name))

  | API f ->
      try
        f args
      with
      | TypeError args ->
          RT.raise_error (fn.name ^ " is missing a parameter")
            ~expected:(fn.parameters
                       |> List.map ~f:param_to_string
                       |> String.concat ~sep:", ")
            ~actual:DIncomplete

