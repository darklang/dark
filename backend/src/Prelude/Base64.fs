module Base64

// Base64 comes in various flavors, typically URLsafe (has '-' and '_' with no
// padding) or regular (has + and / and has '=' padding at the end)

type Base64UrlEncoded = Base64UrlEncoded of string
type Base64DefaultEncoded = Base64DefaultEncoded of string

type T =
  | UrlEncoded of Base64UrlEncoded
  | DefaultEncoded of Base64DefaultEncoded

  override this.ToString() =
    match this with
    | UrlEncoded(Base64UrlEncoded s) -> s
    | DefaultEncoded(Base64DefaultEncoded s) -> s

/// Encodes to base64 strings (using '+' and '/'), with padding. The result is not url-safe.
let defaultEncodeToString (input : byte array) : string =
  System.Convert.ToBase64String input

// Convert to base64 string
let encode (input : byte array) : T =
  input |> defaultEncodeToString |> Base64DefaultEncoded |> DefaultEncoded

// type-safe wrapper for an already-encoded urlEncoded string
let fromUrlEncoded (string : string) : T = string |> Base64UrlEncoded |> UrlEncoded

// type-safe wrapper for an already-encoded defaultEncoded string
let fromDefaultEncoded (string : string) : T =
  string |> Base64DefaultEncoded |> DefaultEncoded

// If we don't know how it's encoded, covert to urlEncoded as we can be certain then.
let fromEncoded (string : string) : T =
  string.Replace('+', '-').Replace('/', '_').Replace("=", "")
  |> Base64UrlEncoded
  |> UrlEncoded

let asUrlEncodedString (b64 : T) : string =
  match b64 with
  | UrlEncoded(Base64UrlEncoded s) -> s
  | DefaultEncoded(Base64DefaultEncoded s) ->
    s.Replace('+', '-').Replace('/', '_').Replace("=", "")

let asDefaultEncodedString (b64 : T) : string =
  match b64 with
  | DefaultEncoded(Base64DefaultEncoded s) -> s
  | UrlEncoded(Base64UrlEncoded s) ->
    let initial = s.Replace('-', '+').Replace('_', '/')
    let length = initial.Length

    if length % 4 = 2 then $"{initial}=="
    else if length % 4 = 3 then $"{initial}="
    else initial

/// Encodes to url-safe base64 strings (using '-' and '_'), with no padding
let urlEncodeToString (input : byte array) : string =
  input |> encode |> asUrlEncodedString

// Takes an already-encoded base64 string, and decodes it to bytes
let decode (b64 : T) : byte[] =
  b64 |> asDefaultEncodedString |> System.Convert.FromBase64String

let decodeFromString (input : string) : byte array = input |> fromEncoded |> decode

let decodeOpt (b64 : T) : byte[] option =
  try
    b64 |> decode |> Some
  with _ ->
    None
