/// <summary>
/// StdLib functions to encode, verify, and extract details from JWTs
/// </summary>
/// <remarks>
/// Here's how JWT with RS256, and this library, work:
///
///   Users provide a private key, some headers, and a payload.
///
///   We add fields "type" "JWT" and "alg" "RS256" to the header.
///
///   We create the body by base64-encoding the header and the payload,
///   and joining them with a period:
///
///   body = (b64encode header) ^ "." ^ (b64encode payload)
///
///   We use the private key to sign the body:
///
///   signature = sign body
///
///   Then we join the body with the signature with a period.
///
///   token = body ^ "." ^ signature
///
///   We verify by splitting the parts, checking the signature against the body,
///   and then de-base-64-ing the body and parsing the JSON.
///
///   https://jwt.io/ is helpful for validating this!
/// </remarks>
module BackendOnlyStdLib.LibJwt

open System.Security.Cryptography

open LibExecution.RuntimeTypes
open Prelude
open LibExecution.VendoredTablecloth

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"
let varErr = TVariable "err"

/// Verifies the signature of, and extracts data from, a JWT
///
/// Deprecated in favor of equivalent v1 function
let private verifyAndExtractV0
  (key : RSA)
  (token : string)
  : (string * string) option =
  match Seq.toList (String.split "." token) with
  | [ header; payload; signature ] ->
    // do the minimum of parsing and decoding before verifying signature.
    // c.f. "cryptographic doom principle".
    try
      let signature = signature |> Base64.fromUrlEncoded |> Base64.decodeOpt

      match signature with
      | None -> None
      | Some signature ->
        let hash =
          (header + "." + payload) |> UTF8.toBytes |> SHA256.Create().ComputeHash

        let rsaDeformatter = RSAPKCS1SignatureDeformatter key
        rsaDeformatter.SetHashAlgorithm "SHA256"

        if rsaDeformatter.VerifySignature(hash, signature) then
          let header =
            header
            |> Base64.fromUrlEncoded
            |> Base64.decodeOpt
            |> Option.bind UTF8.ofBytesOpt
          let payload =
            payload
            |> Base64.fromUrlEncoded
            |> Base64.decodeOpt
            |> Option.bind UTF8.ofBytesOpt
          Option.map2 Tuple2.make header payload
        else
          None
    with
    | _e -> None
  | _ -> None

/// Verifies the signature of, and extracts data from, a JWT
let private verifyAndExtractV1
  (key : RSA)
  (token : string)
  : Result<string * string, string> =

  match String.split "." token with
  | [ header; payload; signature ] ->
    //do the minimum of parsing and decoding before verifying signature.
    //c.f. "cryptographic doom principle".
    try
      let signature = signature |> Base64.fromUrlEncoded |> Base64.decodeOpt

      match signature with
      | None -> Error "Unable to base64-decode signature"
      | Some signature ->
        let hash =
          (header + "." + payload) |> UTF8.toBytes |> SHA256.Create().ComputeHash

        let rsaDeformatter = RSAPKCS1SignatureDeformatter key
        rsaDeformatter.SetHashAlgorithm "SHA256"

        if rsaDeformatter.VerifySignature(hash, signature) then
          let header =
            header
            |> Base64.fromUrlEncoded
            |> Base64.decodeOpt
            |> Option.bind UTF8.ofBytesOpt
          let payload =
            payload
            |> Base64.fromUrlEncoded
            |> Base64.decodeOpt
            |> Option.bind UTF8.ofBytesOpt

          match (header, payload) with
          | Some header, Some payload -> Ok(header, payload)
          | Some _, None -> Error "Unable to base64-decode header"
          | _ -> Error "Unable to base64-decode payload"
        else
          Error "Unable to verify signature"

    with
    | e -> Error e.Message
  | _ -> Error "Invalid token format"

/// <summary>
/// Bespoke serialization to match OCaml backend's JWTs
///
/// This is a thing because it need to be exactly the same as it was, or we
/// won't be able to extract the old values, and that would be bad.
/// </summary>
/// <remarks>
/// CLEANUP: make new version with a roundtrippable non-Newtonsoft format
///
/// The LibJWT functions use signatures based off the exact string encoding of
/// Dvals. This was defined in the original OCaml version. We need to keep this
/// exactly the same or the signatures won't match.
//// </remarks>
module Serialization =
  // We are adding bytes to match the old OCaml implementation. Don't use strings
  // or characters as those are different sizes: OCaml strings were literally
  // just byte arrays.
  // A SCG.List is a growing vector (unlike an F# List, which is a linked
  // list). This should have not-awful performance
  type private Vector = System.Collections.Generic.List<byte>

  let private append (v : Vector) (s : string) : unit =
    let bytes = UTF8.toBytes s
    v.Capacity <- v.Capacity + bytes.Length // avoid resizing multiple times
    Array.iter (fun b -> v.Add b) bytes

  let private appendString (v : Vector) (s : string) : unit =
    s
    |> UTF8.toBytes
    |> Array.iter (fun b ->
      match b with
      // " - quote
      | 0x22uy -> append v "\\\""
      // \ - backslash
      | 0x5cuy -> append v "\\\\"
      // \b - backspace
      | 0x08uy -> append v "\\b"
      // \f - form feed
      | 0x0cuy -> append v "\\f"
      // \n - new line
      | 0x0auy -> append v "\\n"
      // \r  - carriage return
      | 0x0duy -> append v "\\r"
      // \t - tab
      | 0x09uy -> append v "\\t"
      // write_control_char
      | b when b >= 0uy && b <= 0x1fuy ->
        append v "\\u"
        append v (b.ToString("x4"))
      | 0x7fuy -> append v "\\u007f"
      | b -> v.Add b)

  let rec private listToStringList
    (v : Vector)
    (l : List<'a>)
    (f : 'a -> unit)
    : unit =
    match l with
    | [] -> ()
    | [ h ] -> f h
    | h :: tail ->
      f h
      append v ","
      listToStringList v tail f

  let rec private toString' (v : Vector) (dval : Dval) : unit =
    let handleString s =
      append v "\""
      appendString v s
      append v "\""

    let handleAssoc l =
      append v "{"

      let f ((k, j) : string * Dval) =
        append v "\""
        appendString v k
        append v "\":"
        toString' v j

      listToStringList v l f

      append v "}"

    match dval with
    // null
    | DNull
    | DOption None
    | DFnVal _ // See docs/dblock-serialization.md
    | DHttpResponse (Redirect _)
    | DIncomplete _ -> append v "null"

    // ints
    | DInt i -> append v (string i)

    // floats
    | DFloat f when System.Double.IsNaN f -> append v "NaN"
    | DFloat f when System.Double.IsPositiveInfinity f -> append v "Infinity"
    | DFloat f when System.Double.IsNegativeInfinity f -> append v "-Infinity"
    | DFloat f ->
      let s =
        // based  on yojson code
        let s = sprintf "%.16g" f
        if System.Double.Parse s = f then s else (sprintf "%.17g" f)

      append v s
      let mutable needsZero = true

      String.toArray s
      |> Array.iter (fun d ->
        if d >= '0' && d <= '9' || d = '-' then () else needsZero <- false)

      if needsZero then append v ".0"

    // bools
    | DBool true -> append v "true"
    | DBool false -> append v "false"

    // lists
    | DList l ->
      append v "["
      listToStringList v l (toString' v)
      append v "]"

    // strings
    | DStr s -> handleString s
    | DChar c -> handleString c
    | DDB name -> handleString name
    | DUuid uuid -> string uuid |> handleString
    | DDate date -> DDateTime.toIsoString date |> handleString
    | DBytes bytes -> bytes |> Base64.defaultEncodeToString |> handleString

    // objects
    | DObj o -> o |> Map.toList |> List.map (fun (k, v) -> (k, v)) |> handleAssoc
    | DError _ -> handleAssoc [ "Error", DNull ]
    | DPassword _ -> handleAssoc [ "Error", DStr "Password is redacted" ]
    | DResult (Error dv) -> handleAssoc [ ("Error", dv) ]

    // wrapper types
    | DHttpResponse (Response (_, _, hdv)) -> toString' v hdv
    | DOption (Some dv) -> toString' v dv
    | DErrorRail dv -> toString' v dv
    | DResult (Ok dv) -> toString' v dv

  // SERIALIZER_DEF Custom LibJwt.serialize
  let serialize (j : Dval) : string =
    let v = Vector 10
    toString' v j
    v.ToArray() |> UTF8.ofBytesUnsafe

  open Newtonsoft.Json
  open Newtonsoft.Json.Linq

  // SERIALIZER_DEF Newtonsoft/Custom LibJwt.deserialize
  /// Parses header or payload of a JWT, transforming results into a Dval
  let deserialize (str : string) : Result<Dval, string> =
    /// Parses header or payload of a JWT
    let parseJson (s : string) : JToken =
      let reader = new JsonTextReader(new System.IO.StringReader(s))
      let jls = JsonLoadSettings()
      jls.CommentHandling <- CommentHandling.Load // Load them so we can error later
      jls.DuplicatePropertyNameHandling <- DuplicatePropertyNameHandling.Error
      jls.CommentHandling <- CommentHandling.Ignore

      reader.DateParseHandling <- DateParseHandling.None
      JToken.ReadFrom(reader)

    // We cannot change this to use System.Text.Json because STJ does not
    // allow "raw infinity"
    // Q: why can't we use JsonNumberHandling.AllowNamedFloatingPointLiterals?
    let rec convert (j : JToken) =
      match j.Type with
      | JTokenType.Integer -> DInt(j.Value<int64>())
      | JTokenType.Float -> DFloat(j.Value<float>())
      | JTokenType.Boolean -> DBool(j.Value<bool>())
      | JTokenType.Null -> DNull
      | JTokenType.String -> DStr(j.Value<string>())
      | JTokenType.Array ->
        j.Values<JToken>() |> Seq.toList |> List.map convert |> Dval.list
      | JTokenType.Object ->
        j.Values()
        |> seq
        |> Seq.toList
        |> List.map (fun (jp : JProperty) -> (jp.Name, jp.Value))
        |> List.fold Map.empty (fun m (k, v) -> Map.add k (convert v) m)
        |> DObj

      // Json.NET does a bunch of magic based on the contents of various types.
      // For example, it has tokens for Dates, constructors, etc. We've
      // disabled all those so we fail if we see any other JTokenType.
      | _ -> Exception.raiseInternal "Invalid type in json" [ "json", string j ]

    try
      str |> parseJson |> convert |> Ok
    with
    | :? JsonReaderException as e ->
      let msg = if str = "" then "JSON string was empty" else e.Message
      Error msg

module STJSerialization =
  // TODO add property tests to check comparisons of these serializers
  // TODO make these match the ones under Serialization
  // TODO use these, make them the default one, and purge the old one
  let serialize = Json.Vanilla.serialize
  let deserialize = Json.Vanilla.deserialize

// SERIALIZER_USAGE Custom LibJwt.SignAndEncode
/// Forms signed JWT given payload, extra header, and key
let signAndEncode (key : string) (extraHeaders : DvalMap) (payload : Dval) : string =
  let headerPartNew =
    extraHeaders
    |> Map.add "alg" (DStr "RS256")
    |> Map.add "type" (DStr "JWT")
    |> DObj
    |> Serialization.serialize
    |> UTF8.toBytes
    |> Base64.urlEncodeToString

  let payloadPart =
    payload |> Serialization.serialize |> UTF8.toBytes |> Base64.urlEncodeToString

  let body = headerPartNew + "." + payloadPart

  let signaturePart =
    let rsa = RSA.Create()
    rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))
    let RSAFormatter = RSAPKCS1SignatureFormatter rsa
    RSAFormatter.SetHashAlgorithm "SHA256"
    let sha256 = SHA256.Create()

    body
    |> UTF8.toBytes
    |> sha256.ComputeHash
    |> RSAFormatter.CreateSignature
    |> Base64.urlEncodeToString

  body + "." + signaturePart

let fns : List<BuiltInFn> =
  [ { name = fn "JWT" "signAndEncode" 0
      parameters = [ Param.make "pemPrivKey" TStr ""; Param.make "payload" varA "" ]
      returnType = TStr
      description =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm. Takes an unencrypted RSA private key in PEM format."
      fn =
        (function
        | _, [ DStr key; payload ] ->
          signAndEncode key Map.empty payload |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = ImpurePreviewable
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
          signAndEncode key headers payload |> DStr |> Ply
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
            signAndEncode key Map.empty payload |> DStr |> Ok |> DResult |> Ply
          with
          | e -> Ply(DResult(Error(DStr e.Message)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = ImpurePreviewable
      deprecated = NotDeprecated }

    { name = fn "JWT" "signAndEncodeWithHeaders" 1
      parameters =
        [ Param.make "pemPrivKey" TStr ""
          Param.make "headers" (TDict varA) ""
          Param.make "payload" varA "" ]
      returnType = TResult(varB, varErr)
      description =
        "Sign and encode an rfc751J9 JSON Web Token, using the RS256 algorithm, with an extra header map. Takes an unencrypted RSA private key in PEM format."
      fn =
        (function
        | _, [ DStr key; DObj headers; payload ] ->
          try
            signAndEncode key headers payload |> DStr |> Ok |> DResult |> Ply
          with
          | e -> Ply(DResult(Error(DStr e.Message)))
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
          let result =
            try
              let rsa = RSA.Create()
              rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))
              verifyAndExtractV0 rsa token
            with
            | _ ->
              Exception.raiseCode
                "No supported key formats were found. Check that the input represents the contents of a PEM-encoded key file, not the path to such a file."

          match result with
          | Some (headers, payload) ->
            let unwrap = Exception.unwrapResultCode
            [ ("header", Serialization.deserialize headers |> unwrap)
              ("payload", Serialization.deserialize payload |> unwrap) ]
            |> Map.ofList
            |> DObj
            |> Some
            |> DOption
            |> Ply
          | None -> Ply(DOption None)
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
          let result =
            try
              let rsa = RSA.Create()
              rsa.ImportFromPem(System.ReadOnlySpan(key.ToCharArray()))
              verifyAndExtractV1 rsa token
            with
            | _ -> Error "Invalid public key"

          match result with
          | Ok (headers, payload) ->
            let unwrap = Exception.unwrapResultCode
            [ ("header", Serialization.deserialize headers |> unwrap)
              ("payload", Serialization.deserialize payload |> unwrap) ]
            |> Map.ofList
            |> DObj
            |> Ok
            |> DResult
            |> Ply
          | Error msg -> Ply(DResult(Error(DStr msg)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
