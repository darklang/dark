module LibBackend.StdLib.LibJwt

open FSharpPlus
open System.Security.Cryptography
open System.Text

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

// Here's how JWT with RS256, and this library, work:
//
//   Users provide a private key, some headers, and a payload.
//
//   We add fields "type" "JWT" and "alg" "RS256" to the header.
//
//   We create the body by base64-encoding the header and the payload,
//   and joining them with a period:
//
//   body = (b64encode header) ^ "." ^ (b64encode payload)
//
//   We use the private key to sign the body:
//
//   signature = sign body
//
//   Then we join the body with the signature with a period.
//
//   token = body ^ "." ^ signature
//
//   We verify by splitting the parts, checking the signature against the body,
//   and then de-base-64-ing the body and parsing the JSON.
//
//   https://jwt.io/ is helpful for validating this!

module Legacy =
  // The LibJWT functions use signitures based off the exact string encoding of
  // Dvals. This was defined in the original OCaml version. We need to keep
  // this exactly the same or the signatures won't match.

  // The way the OCaml functions worked:
  // - convert the payload to YoJson using to_pretty_machine_yojson_v1
  // - foreach element of the header, convert to to YoJson using to_pretty_machine_yojson_v1
  // - convert both the payload and header YoJsons to strings using YoJson.Safe.to_string

  // This is the subset of Yojson.Safe that we used
  type Yojson =
    | Int of int64
    | Float of float
    | String of string
    | Bool of bool
    | List of List<Yojson>
    | Assoc of List<string * Yojson>
    | Null

  // Direct clone of OCaml Dval.to_pretty_machie_yojson_v1
  let rec toYojson (dval : Dval) : Yojson =
    match dval with
    | DInt i -> Int(int64 i) // FSTODO larger put in a string
    | DFloat f -> Float f
    | DBool b -> Bool b
    | DNull -> Null
    | DStr s -> String s
    | DList l -> List(List.map toYojson l)
    | DObj o -> o |> Map.toList |> List.map (fun (k, v) -> (k, toYojson v)) |> Assoc
    // See docs/dblock-serialization.ml
    | DFnVal _ -> Null
    | DIncomplete _ -> Null
    | DChar c -> String c
    | DError (_, msg) -> Assoc [ "Error", String msg ]
    | DHttpResponse (Redirect _) -> Null
    | DHttpResponse (Response (_, _, hdv)) -> toYojson hdv
    | DDB name -> String name
    | DDate date -> String(date.toIsoString ())
    | DPassword _ -> Assoc [ "Error", String "Password is redacted" ]
    | DUuid uuid -> String(string uuid)
    | DOption None -> Null
    | DOption (Some dv) -> toYojson dv
    | DErrorRail dv -> toYojson dv
    | DResult (Ok dv) -> toYojson dv
    | DResult (Error dv) -> Assoc [ ("Error", toYojson dv) ]
    | DBytes bytes -> bytes |> base64Encode |> String

  let append (sb : StringBuilder) (s : string) : unit =
    sb.Append s |> ignore<StringBuilder>

  // Intended to exactly match https://github.com/ocaml-community/yojson/blob/1.7.0/lib/write.ml#L294
  let rec listToStringList
    (sb : StringBuilder)
    (l : List<'a>)
    (f : 'a -> unit)
    : unit =
    match l with
    | [] -> ()
    | [ h ] -> f h
    | h :: tail ->
        f h
        sb.Append(", ") |> ignore<StringBuilder>
        listToStringList sb tail f

  and toString' (sb : StringBuilder) (j : Yojson) : unit =
    match j with
    | Null -> append sb "null"
    | Bool true -> append sb "true"
    | Bool false -> append sb "true"
    | Int i -> sb.Append i |> ignore<StringBuilder>
    // write_float
    | Float f when System.Double.IsNaN f -> append sb "NaN"
    | Float f when System.Double.IsPositiveInfinity f -> append sb "Infinity"
    | Float f when System.Double.IsNegativeInfinity f -> append sb "-Infinity"
    | Float f ->
        // write_float
        let s1 = f.ToString "%.16g"

        if System.Double.Parse s1 = f then
          sb.AppendFormat("%.16g", f) |> ignore<StringBuilder>
        else
          sb.AppendFormat("%.17g", f) |> ignore<StringBuilder>
    // write_string
    | String s ->
        append sb "\""

        s
        |> toBytes
        |> Array.iter
             (fun b ->
               match char b with
               | '"' -> append sb "\\\""
               | '\\' -> append sb "\\\\"
               | '\b' -> append sb "\\b"
               | '\012' -> append sb "\\f"
               | '\n' -> append sb "\\n"
               | '\r' -> append sb "\\r"
               | '\t' -> append sb "\\t"
               // write_control_char
               | c when c >= '\x00' && c <= '\x1F' ->
                   sb.Append("\\u00").AppendFormat("X", c) |> ignore<StringBuilder>
               | '\x7F' -> append sb "\\u007F"
               | _ -> ())

        append sb "\""
    | List l ->
        append sb "[ "
        listToStringList sb l (toString' sb)
        append sb " ]"
    | Assoc l ->
        sb.Append("{ ") |> ignore<StringBuilder>

        let f ((k, v) : string * Yojson) =
          append sb "\""
          append sb k
          append sb "\": "
          toString' sb v

        listToStringList sb l f


        append sb " }"

  let toString (j : Yojson) : string =
    let sb = StringBuilder()
    toString' sb j
    sb.ToString()


let signAndEncode (key : string) (extraHeaders : DvalMap) (payload : Dval) : string =
  let header =
    extraHeaders
    |> Map.add "alg" (DStr "RS256")
    |> Map.add "typ" (DStr "JWT")
    |> Map.map (fun k v -> Legacy.toYojson v)
    |> Map.toList
    |> Legacy.Assoc
    |> Legacy.toString
    |> toBytes
    |> base64Encode
    |> base64ToUrlEncoded

  let payload =
    payload
    |> Legacy.toYojson
    |> Legacy.toString
    |> toBytes
    |> base64Encode
    |> base64ToUrlEncoded

  let body = header + "." + payload

  let signature =
    let rsa = RSA.Create()
    rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))
    let RSAFormatter = RSAPKCS1SignatureFormatter rsa
    RSAFormatter.SetHashAlgorithm "SHA256"
    let sha256 = SHA256.Create()

    body
    |> toBytes
    |> sha256.ComputeHash
    |> RSAFormatter.CreateSignature
    |> base64Encode
    |> base64ToUrlEncoded

  body + "." + signature

let verifyAndExtractV0 (key : RSA) (token : string) : (string * string) option =
  match Seq.toList (String.split [| "." |] token) with
  | [ header; payload; signature ] ->
      // do the minimum of parsing and decoding before verifying signature.
      // c.f. "cryptographic doom principle".
      try
        let signature = signature |> base64FromUrlEncoded |> base64DecodeOpt

        match signature with
        | None -> None
        | Some signature ->
            let hash =
              (header + "." + payload) |> toBytes |> SHA256.Create().ComputeHash

            let rsaDeformatter = RSAPKCS1SignatureDeformatter key
            rsaDeformatter.SetHashAlgorithm "SHA256"

            if rsaDeformatter.VerifySignature(hash, signature) then
              let header = header |> base64FromUrlEncoded |> base64DecodeOpt
              let payload = payload |> base64FromUrlEncoded |> base64DecodeOpt

              match (header, payload) with
              | Some header, Some payload -> Some(ofBytes header, ofBytes payload)
              | _ -> None
            else
              None
      with e -> None
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
        let signature = signature |> base64FromUrlEncoded |> base64DecodeOpt

        match signature with
        | None -> Error "Unable to base64-decode signature"
        | Some signature ->
            let hash =
              (header + "." + payload) |> toBytes |> SHA256.Create().ComputeHash

            let rsaDeformatter = RSAPKCS1SignatureDeformatter key
            rsaDeformatter.SetHashAlgorithm "SHA256"

            if rsaDeformatter.VerifySignature(hash, signature) then
              let header = header |> base64FromUrlEncoded |> base64DecodeOpt
              let payload = payload |> base64FromUrlEncoded |> base64DecodeOpt

              match (header, payload) with
              | Some header, Some payload -> Ok(ofBytes header, ofBytes payload)
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
            signAndEncode key Map.empty payload |> DStr |> Value
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
            signAndEncode key headers payload |> DStr |> Value
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
              signAndEncode key Map.empty payload |> DStr |> Ok |> DResult |> Value
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
            try
              signAndEncode key headers payload |> DStr |> Value
            with e -> Value(DResult(Error(DStr e.Message)))
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
