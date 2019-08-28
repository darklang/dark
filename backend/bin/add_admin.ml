open Core_kernel
module Util = Libexecution.Util
module Password = Libbackend.Password

let usage () =
  Format.printf "Usage: %s [--prompt-for-password]\n" Sys.argv.(0) ;
  exit 1


let prompt str =
  print_string str ;
  Out_channel.flush Out_channel.stdout ;
  match In_channel.input_line In_channel.stdin with None -> "" | Some s -> s


let () =
  Random.self_init () ;
  (* parse args *)
  let prompt_for_password =
    match (Array.length Sys.argv, Array.to_list Sys.argv) with
    | 1, _ ->
        false
    | 2, [_; "--prompt-for-password"] ->
        true
    | _ ->
        usage ()
  in
  (* prompt command-line user for some fields... *)
  let username = prompt "Admin Username: " in
  let password =
    if prompt_for_password then prompt "Password: " else Util.random_string 16
  in
  let email = prompt "Email: " in
  let name = prompt "Name: " in
  let hashed = Password.from_plaintext password |> Password.to_bytes in
  (* print out the new entry for account.ml *)
  Format.printf
    "
  (* This admin's password is as follows: %s
     Insert everything after this into backend/libbackend/account.ml in `let upsert_admins` *)\n
  upsert_admin_exn
    { username = \"%s\"
    ; password = Password.from_hash \"%s\"
    ; email = \"%s\"
    ; name = \"%s\"};\n\n"
    password
    username
    hashed
    email
    name
