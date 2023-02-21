/// StdLib functions to encode, verify, and extract details from JWTs
module BackendOnlyStdLib.LibJwt

open System.Security.Cryptography

open LibExecution.RuntimeTypes
open Prelude
open LibExecution.VendoredTablecloth

module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

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


// SERIALIZER_DEF LibJwt.LegacySerializer
// Plan: deprecate existing JWT fns; put these types and serializer fn in a
// corner to remain untouched. New JWT functions should use a new
// roundtrippable thing that is based on a type definition and is also separate
// from the rest of the world (even if code is identical) for safety.
module private LegacySerializer =
  // The LibJWT functions use signatures based off the exact string encoding of
  // Dvals. This was defined in the original OCaml version. We need to keep
  // this exactly the same or the signatures won't match.

  // The way the OCaml functions worked:
  // - convert the payload to YoJson using to_pretty_machine_yojson_v1
  // - foreach element of the header, convert to to YoJson using to_pretty_machine_yojson_v1
  // - convert both the payload and header YoJsons to strings using YoJson.Safe.to_string, version 1.7.0

  // This is the subset of Yojson.Safe that we used
  type Yojson =
    | Int of int64
    | Float of float
    | String of string
    | Bool of bool
    | List of List<Yojson>
    | Assoc of List<string * Yojson>
    | Null

  // Direct clone of OCaml Dval.to_pretty_machine_yojson_v1
  let rec toYojson (dval : Dval) : Yojson =
    match dval with
    | DInt i -> Int i
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
    | DError _ -> Assoc [ "Error", Null ]
    | DHttpResponse (Redirect _) -> Null
    | DHttpResponse (Response (_, _, hdv)) -> toYojson hdv
    | DDB name -> String name
    | DDate date -> String(DDateTime.toIsoString date)
    | DPassword _ -> Assoc [ "Error", String "Password is redacted" ]
    | DUuid uuid -> String(string uuid)
    | DOption None -> Null
    | DOption (Some dv) -> toYojson dv
    | DErrorRail dv -> toYojson dv
    | DResult (Ok dv) -> toYojson dv
    | DResult (Error dv) -> Assoc [ ("Error", toYojson dv) ]
    | DBytes bytes -> bytes |> Base64.defaultEncodeToString |> String
    | DTuple (first, second, theRest) ->
      // CLEANUP this doesn't roundtrip. It _could_ with this:
      // Assoc [
      //   "type", String "tuple"
      //   "first", toYojson first
      //   "second", toYojson second
      //   "theRest", List(List.map toYojson theRest)
      // ]
      // and some relevant code on the parsing side of libjwt.
      // Make LibJwt roundtrip tuples well with the next version.
      List([ toYojson first; toYojson second ] @ List.map toYojson theRest)

  // We are adding bytes to match the old OCaml implementation. Don't use strings
  // or characters as those are different sizes: OCaml strings were literally
  // just byte arrays.
  // A SCG.List is a growing vector (unlike an F# List, which is a linked
  // list). This should have not-awful performance
  type Vector = System.Collections.Generic.List<byte>

  let append (v : Vector) (s : string) : unit =
    let bytes = UTF8.toBytes s
    v.Capacity <- v.Capacity + bytes.Length // avoid resizing multiple times
    Array.iter (fun b -> v.Add b) bytes

  let rec listToStringList (v : Vector) (l : List<'a>) (f : 'a -> unit) : unit =
    match l with
    | [] -> ()
    | [ h ] -> f h
    | h :: tail ->
      f h
      append v ","
      listToStringList v tail f

  and appendString (v : Vector) (s : string) : unit =
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

  and toString' (v : Vector) (j : Yojson) : unit =
    match j with
    | Null -> append v "null"
    | Bool true -> append v "true"
    | Bool false -> append v "false"
    | Int i -> append v (string i)
    // write_float
    | Float f when System.Double.IsNaN f -> append v "NaN"
    | Float f when System.Double.IsPositiveInfinity f -> append v "Infinity"
    | Float f when System.Double.IsNegativeInfinity f -> append v "-Infinity"
    | Float f ->
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

    // write_string
    | String s ->
      append v "\""
      appendString v s
      append v "\""
    | List l ->
      append v "["
      listToStringList v l (toString' v)
      append v "]"
    | Assoc l ->
      append v "{"

      let f ((k, j) : string * Yojson) =
        append v "\""
        appendString v k
        append v "\":"
        toString' v j

      listToStringList v l f

      append v "}"

  let toString (j : Yojson) : string =
    let v = Vector 10
    toString' v j
    v.ToArray() |> UTF8.ofBytesUnsafe

/// Forms signed JWT given payload, extra header, and key
let signAndEncode (key : string) (extraHeaders : DvalMap) (payload : Dval) : string =
  let header =
    extraHeaders
    |> Map.add "alg" (DStr "RS256")
    |> Map.add "type" (DStr "JWT")
    |> Map.mapWithIndex (fun _ v -> LegacySerializer.toYojson v)
    |> Map.toList
    |> LegacySerializer.Assoc
    |> LegacySerializer.toString
    |> UTF8.toBytes
    |> Base64.urlEncodeToString

  let payload =
    payload
    |> LegacySerializer.toYojson
    |> LegacySerializer.toString
    |> UTF8.toBytes
    |> Base64.urlEncodeToString

  let body = header + "." + payload

  let signature =
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

  body + "." + signature

/// Verifies the signature of, and extracts data from, a JWT
let verifyAndExtractV1
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

// This is its own thing because it need to be exactly the same as it was, or we
// won't be able to extract the old values, and that would be bad.
// CLEANUP: make new version with a roundtrippable format

open Newtonsoft.Json
open Newtonsoft.Json.Linq

/// Parses header or payload of a JWT
let parseJson (s : string) : JToken =
  let reader = new JsonTextReader(new System.IO.StringReader(s))
  let jls = JsonLoadSettings()
  jls.CommentHandling <- CommentHandling.Load // Load them so we can error later
  jls.DuplicatePropertyNameHandling <- DuplicatePropertyNameHandling.Error
  jls.CommentHandling <- CommentHandling.Ignore

  reader.DateParseHandling <- DateParseHandling.None
  JToken.ReadFrom(reader)

/// Parses header or payload of a JWT, transforming results into a Dval
let ofJson (str : string) : Result<Dval, string> =
  // We cannot change this to use System.Text.Json because STJ does not
  // allow "raw infinity"
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
    // For example, it has tokens for Dates, constructors, etc. We've tried to
    // disable all those so we fail if we see them. However, we might need to
    // just convert some of these into strings.
    | JTokenType.None
    | JTokenType.Undefined
    | JTokenType.Constructor
    | JTokenType.Property
    | JTokenType.Guid
    | JTokenType.Raw
    | JTokenType.Bytes
    | JTokenType.TimeSpan
    | JTokenType.Uri
    | JTokenType.Comment
    | JTokenType.Date
    | _ -> Exception.raiseInternal "Invalid type in json" [ "json", string j ]

  try
    str |> parseJson |> convert |> Ok
  with
  | :? JsonReaderException as e ->
    let msg = if str = "" then "JSON string was empty" else e.Message
    Error msg

let fns : List<BuiltInFn> =
  [ { name = fn "JWT" "signAndEncode" 1
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


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
            [ ("header", ofJson headers |> unwrap)
              ("payload", ofJson payload |> unwrap) ]
            |> Map.ofList
            |> DObj
            |> Ok
            |> DResult
            |> Ply
          | Error msg -> Ply(DResult(Error(DStr msg)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
