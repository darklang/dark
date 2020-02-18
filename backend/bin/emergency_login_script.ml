open Libbackend

let usage () : unit =
  Format.printf "Usage: %s username\n\n" Sys.argv.(0) ;
  exit 1


let domain =
  Re2.replace_exn
    (Re2.create_exn ":8000")
    (Re2.replace_exn (Re2.create_exn "static") Config.static_host ~f:(fun _ ->
         ""))
    ~f:(fun _ -> "")


let session_key_for_username (username : string) : string =
  Libbackend.Auth.SessionSync.new_for_username username


let () =
  if Array.length Sys.argv <> 2
  then usage ()
  else (
    Nocrypto_entropy_unix.initialize () ;
    let username = Sys.argv.(1) in
    Caml.print_endline (Printf.sprintf "Generating a cookie for %s." domain) ;
    let session_key = username |> session_key_for_username in
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
         domain) ;
    () )
