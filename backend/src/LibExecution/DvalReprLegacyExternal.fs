/// Ways of converting Dvals to/from strings, with external use allowed
module LibExecution.DvalReprLegacyExternal

// The remaining functions in this file are used by the HttpClient or the Http
// framework, and cannot be changed in their existing usages. The plan to
// remove them is to replace both the Http framework with a middleware based
// design, with some of that middleware being a typed json serializer. The
// typed json serializer would be a standard library function, not something
// that's an inate part of the framework/client in the way that these functions
// are.

// Printing Dvals is more complicated than you'd expect. Different situations
// have different constraints, such as develop-focused representation showing
// explicitly what the value is, vs an API-based representation which does
// something clever with Option/Result types. There is also versioning, as not
// all changes are going to be backward compatible.

// Note: we inline a lot of code which could be reused. This is deliberate: it
// allows us reason more easily about what changes are going to be safe. In
// general, we should avoid general purpose or reusable functions in this file.

// Since it's hard to know where this file is used from, do not throw exceptions in
// here.

open Prelude
open VendoredTablecloth

open RuntimeTypes

open System.Text.Json

let jsonWriterOptions : JsonWriterOptions =
  let mutable options = new JsonWriterOptions()
  options.Indented <- true
  options.SkipValidation <- true
  let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
  options.Encoder <- encoder
  options

let writePrettyJson (f : Utf8JsonWriter -> unit) : string =
  let stream = new System.IO.MemoryStream()
  let w = new Utf8JsonWriter(stream, jsonWriterOptions)
  f w
  w.Flush()
  UTF8.ofBytesUnsafe (stream.ToArray())

let jsonDocumentOptions : JsonDocumentOptions =
  let mutable options = new JsonDocumentOptions()
  options.CommentHandling <- JsonCommentHandling.Skip
  options.MaxDepth <- System.Int32.MaxValue // infinite
  options

let parseJson (s : string) : JsonDocument =
  JsonDocument.Parse(s, jsonDocumentOptions)


type Utf8JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEndObject()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEndArray()




// -------------------------
// Runtime Types
// -------------------------

// SERIALIZER_DEF DvalReprLegacyExternal.toEnduserReadableTextV0
// Plan: We'd like to deprecate this in favor of an improved version only
// usable/used by StdLib functions in various http clients and middlewares.
/// When printing to grand-users (our users' users) using text/plain, print a
/// human-readable format. Redacts passwords.
let toEnduserReadableTextV0 (dval : Dval) : string =

  let rec nestedreprfn dv =
    // If nesting inside an object or a list, wrap strings in quotes
    match dv with
    | DStr _
    | DUuid _
    | DChar _ -> "\"" + reprfn dv + "\""
    | _ -> reprfn dv

  and toNestedString (dv : Dval) : string =
    let rec inner (indent : int) (dv : Dval) : string =
      let nl = "\n" + String.replicate indent " "
      let inl = "\n" + String.replicate (indent + 2) " "
      let indent = indent + 2
      let recurse = inner indent

      match dv with
      | DTuple (first, second, rest) ->
        let l = [ first; second ] @ rest
        "(" + inl + String.concat ", " (List.map recurse l) + nl + ")"
      | DList l ->
        if l = [] then
          "[]"
        else
          "[" + inl + String.concat ", " (List.map recurse l) + nl + "]"
      | DObj o ->
        if o = Map.empty then
          "{}"
        else
          let strs =
            Map.fold [] (fun l key value -> (key + ": " + recurse value) :: l) o

          "{" + inl + String.concat ("," + inl) strs + nl + "}"
      | _ -> nestedreprfn dv

    inner 0 dv

  and reprfn dv =
    match dv with
    | DInt i -> string i
    | DBool true -> "true"
    | DBool false -> "false"
    | DStr s -> s
    | DFloat f ->
      // See DvalRepr.Tests for edge cases.
      if System.Double.IsPositiveInfinity f then
        "Infinity"
      else if System.Double.IsNegativeInfinity f then
        "-Infinity"
      else if System.Double.IsNaN f then
        "NaN"
      else
        let result = sprintf "%.12g" f
        if result.Contains "." then result else $"{result}.0"
    | DChar c -> c
    | DNull -> "null"
    | DDate d -> DDateTime.toIsoString d
    | DUuid uuid -> string uuid
    | DDB dbname -> $"<DB: {dbname}>"
    | DError _ -> "Error"
    | DIncomplete _ -> "<Incomplete>"
    | DFnVal _ ->
      // See docs/dblock-serialization.ml
      "<Block>"
    | DPassword _ ->
      // redacting, do not unredact
      "<Password>"
    | DObj _
    | DList _
    | DTuple _ -> toNestedString dv
    | DErrorRail d ->
      // We don't print error here, because the errorrail value will know
      // whether it's an error or not.
      reprfn d
    | DHttpResponse (Redirect url) -> $"302 {url}\n" + nestedreprfn DNull
    | DHttpResponse (Response (code, headers, body)) ->
      let headerString =
        headers
        |> List.map (fun (k, v) -> k + ": " + v)
        |> String.concat ","
        |> fun s -> "{ " + s + " }"

      $"{code} {headerString}" + "\n" + nestedreprfn body
    | DResult (Ok d) -> reprfn d
    | DResult (Error d) -> "Error: " + reprfn d
    | DOption (Some d) -> reprfn d
    | DOption None -> "Nothing"
    | DBytes bytes -> System.Text.Encoding.UTF8.GetString bytes

  reprfn dval
