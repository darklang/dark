open Core_kernel
open Libexecution
open Libcommon

let encode ~(b64_secret : string) ~(kid : string) ~(iss : string) : string =
  let command = "/home/dark/bin/jwt" in
  let args =
    [| "encode"
     ; "-A"
     ; "ES256"
     ; "--kid"
     ; kid
     ; "--iss"
     ; iss
     ; "--secret-b64"
     ; b64_secret |> String.substr_replace_all ~pattern:"\n" ~with_:"" |]
  in
  let command = command ^ " " ^ String.concat_array args ~sep:" " in
  let in_channel, outchan, errchan = Unix.open_process_full command [||] in
  let lines = ref [] in
  let ctr = ref 0 in
  ( try
      while true do
        ctr := !ctr + 1 ;
        (* This is silly, but ... for lack of a timeout ... *)
        if !ctr > 1000 then Exception.internal "Failed to encode a jwt" else () ;
        let line = In_channel.input_line in_channel in
        match line with
        | Some line ->
            lines := line :: !lines ;
            (* This is ... gross? I'm not sure how _else_ to get EOF, though ...
             * and 
             * TODO: it also does _zero_ about failed exit statuses, which is
             * kinda important
             * *)
            raise End_of_file
        | None ->
            ()
      done
    with End_of_file ->
      ( try
          Unix.close_process_full (in_channel, outchan, errchan) |> ignore
          (* Workaround for "no child processes" *)
        with Unix.Unix_error _ -> Log.erroR "Unix error in jwt encode" ) ) ;
  String.concat !lines ~sep:"\n"
