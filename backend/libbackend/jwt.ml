open Core_kernel

let signed_jwt ~(b64_der : string) ~(kid : string) ~(iss : string) : string =
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
     ; b64_der |> String.substr_replace_all ~pattern:"\n" ~with_:"" |]
  in
  let command = command ^ " " ^ String.concat_array args ~sep:" " in
  let in_channel, outchan, errchan = Unix.open_process_full command [||] in
  let lines = ref [] in
  ( try
      while true do
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
      Unix.close_process_full (in_channel, outchan, errchan) |> ignore ) ;
  String.concat !lines ~sep:"\n"
