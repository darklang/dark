/// The default System.Text.Encoding.UTF8 replaces invalid bytes on error. We
/// sometimes want this, but often we want to assert that the bytes we can are valid
/// and throw if they aren't.
module UTF8

// This encoding throws errors on invalid bytes
let utf8EncodingWithExceptions = System.Text.UTF8Encoding(false, true)

// Throws if the bytes are not valid UTF8
let ofBytesUnsafe (input : byte array) : string =
  utf8EncodingWithExceptions.GetString input

// I'm assuming if it makes it into a UTF8 string it must be valid? That might be
// an incorrect assumption.
let toBytes (input : string) : byte array = utf8EncodingWithExceptions.GetBytes input

let toBytesOpt (input : string) : byte array option =
  try
    Some(toBytes input)
  with e ->
    None

let ofBytesOpt (input : byte array) : string option =
  try
    Some(ofBytesUnsafe input)
  with e ->
    None


// Use this to ignore errors
let ofBytesWithReplacement (input : byte array) : string =
  System.Text.Encoding.UTF8.GetString input
