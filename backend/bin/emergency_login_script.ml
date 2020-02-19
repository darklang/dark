open Libbackend

let usage () : unit =
  Format.printf "Usage: %s username\n\n" Sys.argv.(0) ;
  exit 1


let session_key_for_username (username : string) : string =
  Auth.SessionSync.new_for_username username


let report_to_rollbar (username : string) : unit =
  let open Libexecution in
  let e =
    Exception.make_exception
      Exception.DarkInternal
      (Printf.sprintf "emergency_login_script used for username %s" username)
  in
  let bt = Exception.get_backtrace () in
  let fake_exn_id = "0" in
  Rollbar.report e bt (Other "emergency_login_script") fake_exn_id
  |> function
  | `Success ->
      ()
  | `Disabled ->
      Caml.print_endline "!!!!!! ROLLBAR IS DISABLED HERE !!!!!!"
  | `Failure ->
      Caml.print_endline "!!!!!! FAILED TO ROLLBAR !!!!!!"


let () =
  if Array.length Sys.argv <> 2
  then usage ()
  else (
    Nocrypto_entropy_unix.initialize () ;
    let username = Sys.argv.(1) in
    Caml.print_endline
      (Printf.sprintf "Generating a cookie for %s." Config.cookie_domain) ;
    let session_key = username |> session_key_for_username in
    report_to_rollbar username ;
    Caml.print_endline
      (Printf.sprintf
         "To log in, you'll need the cookie manager extension installed:
  https://chrome.google.com/webstore/detail/cookie-inspector/jgbbilmfbammlbbhmmgaagdkbkepnijn

To use it, open the browser console, go to the Cookies tab, right click the
table and select Add Cookie.
Name = __session ,
Value = %s , and
Domain = %s (note: initial dot is _important_)

then click Submit and you're ready to go."
         session_key
         Config.cookie_domain) ;
    () )
