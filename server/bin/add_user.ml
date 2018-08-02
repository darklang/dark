open Core_kernel
module Util = Libexecution.Util
module Account = Libbackend.Account

let usage () =
  Format.printf "Usage: %s [--prompt-for-password]\n" Sys.argv.(0);
  exit 1 ;;

let prompt str =
  print_string str;
  Out_channel.flush Out_channel.stdout;
  match In_channel.input_line In_channel.stdin with
    None -> ""
  | Some s -> s

let () =
  Random.self_init ();

  (* parse args *)
  let prompt_for_password = match (Array.length Sys.argv,
                                   Array.to_list Sys.argv) with
      (1, _) -> false
    | (2, _ :: "--prompt-for-password" :: []) -> true
    | _ -> usage () in

  (* prompt command-line user for some fields... *)
  let username = prompt "Username: " in
  let password = if prompt_for_password
                 then prompt "Password: "
                 else Util.random_string 16
  in
  let email = prompt "Email: " in
  let name = prompt "Name: " in
  let hashed = Account.hash_password password in
  (* print out the new entry for account.ml *)
  Format.printf "
  (* This user's password is as follows: %s
     Insert everything after this into server/libbackend/account.ml *)\n
  upsert_account
    { username = \"%s\"
    ; password = \"%s\"
    ; email = \"%s\"
    ; name = \"%s\"};\n\n"
    password username hashed email name


