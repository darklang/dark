open Core_kernel
open Libexecution
open Libcommon
open Nocrypto

(* Here's how JWT with RS256, and this library, work:

   Users provide a private key, some headers, and a payload.

   We add fields "type" "JWT" and "alg" "RS256" to the header.

   We create the body by base64-encoding the header and the payload,
   and joining them with a period:

   body = (b64encode header) ^ "." ^ (b64encode payload)

   We use the private key to sign the body:

   signature = sign body

   Then we join the body with the signature with a period.

   token = body ^ "." ^ signature

   We verify by splitting the parts, checking the signature against the body,
   and then de-base-64-ing the body and parsing the JSON.

   https://jwt.io/ is helpful for validating this!
 *)

(* Returns a base64 encoded JWT built from the incoming payload and signed with
   the RS256 algorithm using the provided key. *)
let encode_rs256
    ~(key : Rsa.priv)
    ~(extra_headers : (string * Yojson.Safe.t) list)
    ~(payload : Yojson.Safe.t) : string =
  let header =
    `Assoc (extra_headers @ [("alg", `String "RS256"); ("type", `String "JWT")])
    |> Yojson.Safe.to_string
    |> B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false
  in
  let payload =
    payload
    |> Yojson.Safe.to_string
    |> B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false
  in
  let body = header ^ "." ^ payload in
  let signature =
    body
    |> Cstruct.of_string
    |> (fun x -> `Message x)
    |> Rsa.PKCS1.sign ~hash:`SHA256 ~key
    |> Cstruct.to_string
    |> B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false
  in
  body ^ "." ^ signature


(* Verifies the signature of the given JWT string with the provided key, then
   then base64 decodes the remainder of the token. On success, returns a tuple
   of (header, payload) where both values are unparsed JSON strings. *)
let decode_rs256 ~(key : Rsa.pub) ~(token : string) : (string * string) option
    =
  match String.split ~on:'.' token with
  | [header; payload; signature] ->
    (* do the minimum of parsing and decoding before verifying signature.
          c.f. "cryptographic doom principle". *)
    ( match B64.decode_opt ~alphabet:B64.uri_safe_alphabet signature with
    | None ->
        None
    | Some signature ->
        if header ^ "." ^ payload
           |> Cstruct.of_string
           |> (fun x -> `Message x)
           |> Rsa.PKCS1.verify
                ~hashp:(( = ) `SHA256)
                ~key
                ~signature:(Cstruct.of_string signature)
        then
          match
            ( B64.decode_opt ~alphabet:B64.uri_safe_alphabet header
            , B64.decode_opt ~alphabet:B64.uri_safe_alphabet payload )
          with
          | Some header, Some payload ->
              Some (header, payload)
          | _ ->
              None
        else None )
  | _ ->
      None


(* TODO *)
let encode_es256
    ~(pkey : string)
    ~(extra_headers : (string * Yojson.Safe.t) list)
    ~(payload : (string * Yojson.Safe.t) list) : (string, string) Result.t =
  let headers_to_args (headers : (string * Yojson.Safe.t) list) : string list =
    match Map.of_alist (module String) extra_headers with
    | `Duplicate_key _ ->
        []
    | `Ok headers_map ->
      ( match Map.find headers_map "kid" with
      | None ->
          []
      | Some (`String kid) ->
          ["--kid"; kid]
      | Some (`Int kid) ->
          ["--kid"; string_of_int kid]
      | Some _ ->
          [] )
  in
  let payload_to_args (payload : (string * Yojson.Safe.t) list) : string list =
    List.concat_map payload ~f:(fun (k, v) ->
        match v with
        | `String v ->
            ["-P"; k ^ "=" ^ v]
        | `Int v ->
            ["-P"; k ^ "=" ^ string_of_int v]
        | _ ->
            [] )
  in
  let pkey64 = String.substr_replace_all ~pattern:"\n" ~with_:"" pkey in
  let args =
    ["encode"]
    @ headers_to_args extra_headers
    @ payload_to_args payload
    @ ["-A"; "ES256"; "--secret-b64"; pkey64]
  in
  let command = "/home/dark/bin/jwt " ^ String.concat args ~sep:" " in
  Log.infO command ;
  let inchan = Unix.open_process_in command in
  let lines = ref [] in
  let ctr = ref 0 in
  ( try
      while true do
        ctr := !ctr + 1 ;
        (* This is silly, but ... for lack of a timeout ... *)
        if !ctr > 5000 then Exception.internal "Failed to encode a jwt" else () ;
        let line = In_channel.input_line inchan in
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
          Unix.close_process_in inchan |> ignore
          (* Workaround for "no child processes" *)
        with Unix.Unix_error (_, b, c) ->
          Log.erroR "Unix error in jwt encode" ~params:[("b", b); ("c", c)] )
  ) ;
  Ok (String.concat !lines ~sep:"\n")
