module LibBackend.StdLib.LibJwt

open FSharpPlus
open System.Security.Cryptography

open LibExecution.RuntimeTypes
open Prelude

module DvalRepr = LibExecution.DvalRepr
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"
let varErr = TVariable "err"

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

let signAndEncode (key : RSA) (extraHeaders : DvalMap) (payload : string) : string =
  let header =
    extraHeaders
    |> Map.add "alg" (DStr "RS256")
    |> Map.add "typ" (DStr "JWT")
    |> DObj
    |> DvalRepr.toPrettyMachineJsonStringV1
    |> toBytes
    |> base64Encode
    |> base64ToUrlEncoded

  let payload = payload |> toBytes |> base64Encode |> base64ToUrlEncoded
  let body = header + "." + payload
  let sha256 = SHA256.Create()

  let RSAFormatter = RSAPKCS1SignatureFormatter key
  RSAFormatter.SetHashAlgorithm "SHA256"

  let signature =
    RSAFormatter.CreateSignature(body |> toBytes |> sha256.ComputeHash)
    |> base64Encode
    |> base64ToUrlEncoded

  body + "." + signature

let verifyAndExtractV0 (key : RSA) (token : string) : (string * string) option =

  match Seq.toList (String.split [| "." |] token) with
  | [ header; payload; signature ] ->
      // do the minimum of parsing and decoding before verifying signature.
      // c.f. "cryptographic doom principle".
      try
        let sha256 = SHA256.Create()
        let hash = (header + "." + payload) |> toBytes |> sha256.ComputeHash

        let RSADeformatter = RSAPKCS1SignatureDeformatter key
        RSADeformatter.SetHashAlgorithm "SHA256"

        let signature = signature |> base64FromUrlEncoded |> base64DecodeOpt

        match signature with
        | None -> None
        | Some signature ->
            let header = header |> base64FromUrlEncoded |> base64DecodeOpt
            let payload = payload |> base64FromUrlEncoded |> base64DecodeOpt

            if RSADeformatter.VerifySignature(hash, signature) then
              match (header, payload) with
              | Some header, Some payload ->
                  Some(header |> ofBytes, payload |> ofBytes)
              | _ -> None
            else
              None

      with e -> Errors.throw (e.ToString())
  | _ -> None

let verifyAndExtractV1
  (key : RSA)
  (token : string)
  : Result<string * string, string> =

  match Seq.toList (String.split [| "." |] token) with
  | [ header; payload; signature ] ->
      //do the minimum of parsing and decoding before verifying signature.
      //c.f. "cryptographic doom principle".
      try
        let sha256 = SHA256.Create()
        let hash = (header + "." + payload) |> toBytes |> sha256.ComputeHash

        let RSADeformatter = RSAPKCS1SignatureDeformatter key
        RSADeformatter.SetHashAlgorithm "SHA256"

        let signature = signature |> base64FromUrlEncoded |> base64DecodeOpt

        match signature with
        | None -> Error "Unable to base64-decode signature"
        | Some signature ->
            let header = header |> base64FromUrlEncoded |> base64DecodeOpt
            let payload = payload |> base64FromUrlEncoded |> base64DecodeOpt

            if RSADeformatter.VerifySignature(hash, signature) then
              match (header, payload) with
              | Some header, Some payload ->
                  Ok(header |> ofBytes, payload |> ofBytes)
              | Some _, None -> Error "Unable to base64-decode header"
              | _ -> Error "Unable to base64-decode payload"
            else
              Error "Unable to verify signature"

      with e -> Error e.Message
  | _ -> Error "Invalid token format"

let fns : List<BuiltInFn> =
  [ { name = fn "JWT" "signAndEncode" 0
      parameters = [ Param.make "pemPrivKey" TStr ""; Param.make "payload" varA "" ]
      returnType = TStr
      description =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm. Takes an unencrypted RSA private key in PEM format."
      fn =
        (function
        | _, [ DStr key; payload ] ->
            let rsa = RSA.Create()
            rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))

            let payload = DvalRepr.toPrettyMachineJsonStringV1 payload

            signAndEncode rsa Map.empty payload |> DStr |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "JWT" "signAndEncode" 1) }
    { name = fn "JWT" "signAndEncodeWithHeaders" 0
      parameters =
        [ Param.make "pemPrivKey" TStr ""
          Param.make "headers" (TDict varB) ""
          Param.make "payload" varA "" ]
      returnType = TStr
      description =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm, with an extra header map. Takes an unencrypted RSA private key in PEM format."
      fn =
        (function
        | _, [ DStr key; DObj headers; payload ] ->
            let rsa = RSA.Create()
            rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))

            let payload = DvalRepr.toPrettyMachineJsonStringV1 payload

            signAndEncode rsa headers payload |> DStr |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "JWT" "signAndEncodeWithHeaders" 1) }
    { name = fn "JWT" "signAndEncode" 1
      parameters = [ Param.make "pemPrivKey" TStr ""; Param.make "payload" varA "" ]
      returnType = TResult(varB, varErr)
      description =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm. Takes an unencrypted RSA private key in PEM format."
      fn =
        (function
        | _, [ DStr key; payload ] ->
            try
              let rsa = RSA.Create()
              rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))

              let payload = DvalRepr.toPrettyMachineJsonStringV1 payload

              signAndEncode rsa Map.empty payload |> DStr |> Ok |> DResult |> Value
            with e -> Value(DResult(Error(DStr e.Message)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "JWT" "signAndEncodeWithHeaders" 1
      parameters =
        [ Param.make "pemPrivKey" TStr ""
          Param.make "headers" (TDict varA) ""
          Param.make "payload" varA "" ]
      returnType = TResult(varB, varErr)
      description =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm, with an extra header map. Takes an unecnrypted RSA private key in PEM format."
      fn =
        (function
        | _, [ DStr key; DObj headers; payload ] ->
            let rsa = RSA.Create()
            rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))

            let payload = DvalRepr.toPrettyMachineJsonStringV1 payload

            signAndEncode rsa headers payload |> DStr |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "JWT" "verifyAndExtract" 0
      parameters = [ Param.make "pemPubKey" TStr ""; Param.make "token" TStr "" ]
      returnType = TOption varA
      description =
        "Verify and extract the payload and headers from an rfc751J9 JSON Web Token that uses the RS256 algorithm. Takes an unencrypted RSA public key in PEM format."
      fn =
        (function
        | _, [ DStr key; DStr token ] ->
            try
              let rsa = RSA.Create()
              rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))

              match verifyAndExtractV0 rsa token with
              | Some (headers, payload) ->
                  [ ("header", DvalRepr.ofUnknownJsonV1 headers)
                    ("payload", DvalRepr.ofUnknownJsonV1 payload) ]
                  |> Map.ofList
                  |> DObj
                  |> Some
                  |> DOption
                  |> Value
              | None -> Value(DOption None)
            with _ ->
              Errors.throw
                "No supported key formats were found. Check that the input represents the contents of a PEM-encoded key file, not the path to such a file."
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "JWT" "verifyAndExtract" 1) }
    { name = fn "JWT" "verifyAndExtract" 1
      parameters = [ Param.make "pemPubKey" TStr ""; Param.make "token" TStr "" ]
      returnType = TResult(varA, varErr)
      description =
        "Verify and extract the payload and headers from an rfc751J9 JSON Web Token that uses the RS256 algorithm. Takes an unencrypted RSA public key in PEM format."
      fn =
        (function
        | _, [ DStr key; DStr token ] ->
            try
              let rsa = RSA.Create()
              rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))

              match verifyAndExtractV1 rsa token with
              | Ok (headers, payload) ->
                  [ ("header", DvalRepr.ofUnknownJsonV1 headers)
                    ("payload", DvalRepr.ofUnknownJsonV1 payload) ]
                  |> Map.ofList
                  |> DObj
                  |> Ok
                  |> DResult
                  |> Value
              | Error msg -> Value(DResult(Error(DStr msg)))
            with _ -> Value(DResult(Error(DStr "Invalid public key")))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
