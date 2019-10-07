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


let decode_rs256_v1 ~(key : Rsa.pub) ~(token : string) :
    (string * string, string) Result.t =
  match String.split ~on:'.' token with
  | [header; payload; signature] ->
    (* do the minimum of parsing and decoding before verifying signature.
        c.f. "cryptographic doom principle". *)
    ( match B64.decode_opt ~alphabet:B64.uri_safe_alphabet signature with
    | None ->
        Error "Unable to base64-decode signature"
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
              Ok (header, payload)
          | Some header, None ->
              Error "Unable to base64-decode header"
          | _ ->
              Error "Unable to base64-decode payload"
        else Error "Unable to verify signature" )
  | _ ->
      Error "Invalid token format"


(* Returns a base64 encoded JWT built from the incoming payload and signed with
 * the ES256 algorithm using the provided key. *)
let encode_es256
    ~(pkey : string)
    ~(extra_headers : (string * Yojson.Safe.t) list)
    ~(payload : (string * Yojson.Safe.t) list) : (string, string) Result.t =
  let disallowed_extra_headers =
    extra_headers
    |> List.filter ~f:(fun (k, _) -> List.mem ["iss"] k ~equal:( = ))
    |> List.map ~f:(fun (k, _) -> k)
  in
  (* I bet Result.combine_errors would be cleaner here *)
  if not (List.is_empty disallowed_extra_headers)
  then
    Error
      ( "You provided one or more invalid headers (possibly they belong in the payload?): "
      ^ String.concat disallowed_extra_headers ~sep:"," )
  else
    (* the jwt cmd line program only allows setting the --kid header,
   * so we only extract that right now, even though there are theoretically
   * other valid headers in a JWT *)
    let headers_to_args (headers : (string * Yojson.Safe.t) list) : string list
        =
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
    let payload_to_args (payload : (string * Yojson.Safe.t) list) : string list
        =
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
    (* !!!! FIXME !!!! this does no escaping and allows malicious actors full RCE *)
    let args =
      ["encode"]
      @ headers_to_args extra_headers
      @ payload_to_args payload
      @ ["-A"; "ES256"; "--secret-b64"; pkey64]
    in
    let command =
      "timeout 1 /home/dark/bin/jwt " ^ String.concat args ~sep:" "
    in
    let inchan = Unix.open_process_in command in
    let buf = Buffer.create 256 in
    ( try
        while true do
          Buffer.add_channel buf inchan 64
        done
      with End_of_file -> () ) ;
    let trim buf = buf |> Buffer.contents |> String.rstrip in
    try
      let status = Unix.close_process_in inchan in
      match status with
      | Unix.WEXITED 0 ->
          Ok (trim buf)
      | Unix.WEXITED n ->
          Error (Printf.sprintf "failed to encode JWT (exit=%d)" n)
      | Unix.WSTOPPED n | Unix.WSIGNALED n ->
          Error (Printf.sprintf "failed to encode JWT (signal=%d)" n)
    with Unix.Unix_error (Unix.ECHILD, b, c) ->
      (* the child is already gone, which is probably fine, given we have something in buffer *)
      ( match Buffer.length buf with
      | 0 ->
          Error "failed to encode JWT (unknown)"
      | _ ->
          Ok (trim buf) )
