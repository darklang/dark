open Core_kernel
open Libexecution
open Runtime
open Lib
open Types.RuntimeT
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

let sign_and_encode
    ~(key : Rsa.priv)
    ~(extra_headers : (string * Yojson.Safe.t) list)
    ~(payload : Yojson.Safe.t) : string =
  let header =
    `Assoc ([("alg", `String "RS256"); ("type", `String "JWT")] @ extra_headers)
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


let verify_and_extract_v0 ~(key : Rsa.pub) ~(token : string) :
    (string * string) option =
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


let verify_and_extract_v1 ~(key : Rsa.pub) ~(token : string) :
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


let handle_error (fn : unit -> dval) =
  try DResult (ResOk (fn ())) with Invalid_argument msg ->
    let msg =
      if msg = "No RSA keys"
      then "Invalid private key: not an RSA key"
      else msg
    in
    DResult (ResError (Dval.dstr_of_string_exn msg))


let fns =
  [ { pns = ["JWT::signAndEncode"]
    ; ins = []
    ; p = [par "pemPrivKey" TStr; par "payload" TAny]
    ; r = TStr
    ; d =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm. Takes an unecnrypted RSA private key in PEM format."
    ; f =
        InProcess
          (function
          | _, [DStr key; payload] ->
              let (`RSA key) =
                key
                |> Unicode_string.to_string
                |> Cstruct.of_string
                |> X509.Encoding.Pem.Private_key.of_pem_cstruct1
              in
              let payload = Dval.to_pretty_machine_yojson_v1 payload in
              sign_and_encode ~key ~extra_headers:[] ~payload
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["JWT::signAndEncodeWithHeaders"]
    ; ins = []
    ; p = [par "pemPrivKey" TStr; par "headers" TObj; par "payload" TAny]
    ; r = TStr
    ; d =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm, with an extra header map. Takes an unecnrypted RSA private key in PEM format."
    ; f =
        InProcess
          (function
          | _, [DStr key; DObj headers; payload] ->
              let (`RSA key) =
                key
                |> Unicode_string.to_string
                |> Cstruct.of_string
                |> X509.Encoding.Pem.Private_key.of_pem_cstruct1
              in
              let json_hdrs =
                DObj headers
                |> Dval.to_pretty_machine_yojson_v1
                |> Yojson.Safe.Util.to_assoc
              in
              let payload = Dval.to_pretty_machine_yojson_v1 payload in
              sign_and_encode ~key ~extra_headers:json_hdrs ~payload
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["JWT::signAndEncode_v1"]
    ; ins = []
    ; p = [par "pemPrivKey" TStr; par "payload" TAny]
    ; r = TResult
    ; d =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm. Takes an unecnrypted RSA private key in PEM format."
    ; f =
        InProcess
          (function
          | _, [DStr key; payload] ->
              handle_error (fun () ->
                  let (`RSA key) =
                    key
                    |> Unicode_string.to_string
                    |> Cstruct.of_string
                    |> X509.Encoding.Pem.Private_key.of_pem_cstruct1
                  in
                  let payload = Dval.to_pretty_machine_yojson_v1 payload in
                  sign_and_encode ~key ~extra_headers:[] ~payload
                  |> Dval.dstr_of_string_exn )
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["JWT::signAndEncodeWithHeaders_v1"]
    ; ins = []
    ; p = [par "pemPrivKey" TStr; par "headers" TObj; par "payload" TAny]
    ; r = TResult
    ; d =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm, with an extra header map. Takes an unecnrypted RSA private key in PEM format."
    ; f =
        InProcess
          (function
          | _, [DStr key; DObj headers; payload] ->
              handle_error (fun () ->
                  let (`RSA key) =
                    key
                    |> Unicode_string.to_string
                    |> Cstruct.of_string
                    |> X509.Encoding.Pem.Private_key.of_pem_cstruct1
                  in
                  let json_hdrs =
                    DObj headers
                    |> Dval.to_pretty_machine_yojson_v1
                    |> Yojson.Safe.Util.to_assoc
                  in
                  let payload = Dval.to_pretty_machine_yojson_v1 payload in
                  sign_and_encode ~key ~extra_headers:json_hdrs ~payload
                  |> Dval.dstr_of_string_exn )
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["JWT::verifyAndExtract"]
    ; ins = []
    ; p = [par "pemPubKey" TStr; par "token" TStr]
    ; r = TOption
    ; d =
        "Verify and extra the payload and headers from an rfc751J9 JSON Web Token that uses the RS256 algorithm. Takes an unencrypted RSA public key in PEM format."
    ; f =
        InProcess
          (function
          | _, [DStr key; DStr token] ->
            ( match
                key
                |> Unicode_string.to_string
                |> Cstruct.of_string
                |> X509.Encoding.Pem.Public_key.of_pem_cstruct1
              with
            | `EC_pub _ ->
                DOption OptNothing
            | `RSA key ->
              ( match
                  verify_and_extract_v0
                    ~key
                    ~token:(Unicode_string.to_string token)
                with
              | Some (headers, payload) ->
                  [ ("header", Dval.of_unknown_json_v1 headers)
                  ; ("payload", Dval.of_unknown_json_v1 payload) ]
                  |> Prelude.StrDict.from_list_exn
                  |> DObj
                  |> OptJust
                  |> DOption
              | None ->
                  DOption OptNothing ) )
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["JWT::verifyAndExtract_v1"]
    ; ins = []
    ; p = [par "pemPubKey" TStr; par "token" TStr]
    ; r = TResult
    ; d =
        "Verify and extra the payload and headers from an rfc751J9 JSON Web Token that uses the RS256 algorithm. Takes an unencrypted RSA public key in PEM format."
    ; f =
        InProcess
          (function
          | _, [DStr key; DStr token] ->
            ( try
                match
                  key
                  |> Unicode_string.to_string
                  |> Cstruct.of_string
                  |> X509.Encoding.Pem.Public_key.of_pem_cstruct1
                with
                | `EC_pub _ ->
                    DOption OptNothing
                | `RSA key ->
                  ( match
                      verify_and_extract_v1
                        ~key
                        ~token:(Unicode_string.to_string token)
                    with
                  | Ok (headers, payload) ->
                      [ ("header", Dval.of_unknown_json_v1 headers)
                      ; ("payload", Dval.of_unknown_json_v1 payload) ]
                      |> Prelude.StrDict.from_list_exn
                      |> DObj
                      |> ResOk
                      |> DResult
                  | Error msg ->
                      DResult (ResError (Dval.dstr_of_string_exn msg)) )
              with Invalid_argument msg ->
                let msg =
                  if msg = "No public keys" then "Invalid public key" else msg
                in
                DResult (ResError (Dval.dstr_of_string_exn msg)) )
          | args ->
              fail args)
    ; ps = false
    ; dep = false } ]
