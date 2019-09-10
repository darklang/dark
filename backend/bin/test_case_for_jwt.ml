open Core_kernel
open Libbackend

let usage () : unit =
  Format.printf
    "Usage: %s\n\n  Use DARK_CONFIG_DB_DBNAME=prodclone to check prodclone."
    Sys.argv.(0) ;
  exit 1


let () =
  let test_der_b64 =
    "MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgU208KCg/doqiSzsVF5sknVtYSgt8/3oiYGbvryIRrzShRANCAAQfrvDWizEnWAzB2Hx2r/NyvIBO6KGBDL7wkZoKnz4Sm4+1P1dhD9fVEhbsdoq9RKEf8dvzTOZMaC/iLqZFKSN6"
  in
  let output =
    Jwt.signed_jwt ~b64_der:test_der_b64 ~kid:"CapExedKid" ~iss:"CapExedTeam"
  in
  Caml.print_endline output ;
  ()
