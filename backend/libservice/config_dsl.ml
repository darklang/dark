open Core

(* Parsers for env vars *)

let absolute_dir name : string =
  let dir = Sys.getenv_exn name in
  if not (Filename.is_absolute dir)
  then failwith ("FAIL: " ^ name ^ " is not absolute")
  else dir ^ "/"


let int name : int = Sys.getenv_exn name |> int_of_string

let bool name : bool =
  let v = Sys.getenv_exn name in
  match v with
  | "y" ->
      true
  | "n" ->
      false
  | _ ->
      failwith
        ( "Invalid env var value for "
        ^ name
        ^ "="
        ^ v
        ^ ". Allowed values are 'n' and 'y'." )


let lowercase name v =
  if v = String.lowercase v
  then v
  else failwith ("Env vars must be lowercased but " ^ name ^ "=" ^ v ^ " is not")


let string name : string = Sys.getenv_exn name |> lowercase name

let string_option name : string option =
  let v = string name in
  if v = "none" then None else Some v


let int_option name : int option =
  let v = string_option name in
  match v with None -> None | Some s -> Some (int_of_string s)


let string_choice name (options : string list) : string =
  let v = Sys.getenv_exn name |> lowercase name in
  if List.mem ~equal:( = ) options v
  then v
  else
    failwith
      ( "Envvar is not a valid option: '"
      ^ name
      ^ "' not in ["
      ^ String.concat ~sep:", " options
      ^ " is not" )


let password name : string = Sys.getenv_exn name
