module LibExecution.Legacy

open Prelude
open VendoredTablecloth
open RuntimeTypes

open System.Text.Json

let writePrettyJson (f : Utf8JsonWriter -> unit) : string =
  let stream = new System.IO.MemoryStream()
  let mutable options = new JsonWriterOptions()
  options.Indented <- true
  options.SkipValidation <- true
  let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
  options.Encoder <- encoder
  let w = new Utf8JsonWriter(stream, options)
  f w
  w.Flush()
  UTF8.ofBytesUnsafe (stream.ToArray())


type Utf8JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEndObject()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEndObject()
  member this.writeOCamlFloatValue(f : float) =
    if System.Double.IsPositiveInfinity f then
      this.WriteRawValue("Infinity", true)
    else if System.Double.IsNegativeInfinity f then
      this.WriteRawValue("-Infinity", true)
    else if System.Double.IsNaN f then
      this.WriteRawValue("NaN", true)
    else if f = 0.0 && System.Double.IsNegative f then
      // check for -0.0
      this.WriteRawValue("-0.0", true)
    else if System.Math.Abs(f % 1.0) <= System.Double.Epsilon then
      // Check for int-valued floats. By default, STJ prints them with no decimal
      let asString = string f
      if asString.Contains('.') || asString.Contains('E') then // Don't add .0 at the end of 4.2E18
        this.WriteRawValue(String.toLowercase asString, true)
      else
        this.WriteRawValue($"{asString}.0", true)
    else
      let v = f |> string |> String.toLowercase
      this.WriteRawValue(v)
  member this.writeFullInt64Value(i : int64) = this.WriteRawValue(string i)



module PrettyResponseJsonV0 =

  // At time of writing, this is the same as Dval.unsafe_dval_to_yojson. It's being copied to be certain this format doesn't change.
  let rec unsafeDvalToJsonValue
    (w : Utf8JsonWriter)
    (redact : bool)
    (dv : Dval)
    : unit =
    let writeDval = unsafeDvalToJsonValue w redact

    let wrapStringValue (typ : string) (str : string) =
      w.writeObject (fun () ->
        w.WriteString("type", typ)
        w.WriteString("value", str))

    let wrapNullValue (typ : string) =
      w.writeObject (fun () ->
        w.WriteString("type", typ)
        w.WriteNull("value"))

    let wrapNestedDvalValue (typ : string) (dv : Dval) =
      w.writeObject (fun () ->
        w.WriteString("type", typ)
        w.WritePropertyName "value"
        writeDval dv)

    match dv with
    // basic types
    | DInt i -> w.writeFullInt64Value i
    | DFloat f -> w.writeOCamlFloatValue f
    | DBool b -> w.WriteBooleanValue b
    | DNull -> w.WriteNullValue()
    | DStr s -> w.WriteStringValue s
    | DList l -> w.writeArray (fun () -> List.iter writeDval l)
    | DObj o ->
      w.writeObject (fun () ->
        Map.iter
          (fun k v ->
            w.WritePropertyName k
            writeDval v)
          o)
    | DFnVal _ ->
      // See docs/dblock-serialization.md
      wrapNullValue "block"
    | DIncomplete _ -> wrapNullValue "incomplete"
    | DChar c -> wrapStringValue "character" c
    | DError (_, msg) ->
      // Only used internally, so this is safe to save here
      wrapStringValue "error" msg
    | DHttpResponse (h) ->
      w.writeObject (fun () ->
        w.WriteString("type", "response")

        w.WritePropertyName "value"
        w.writeArray (fun () ->
          match h with
          | Redirect str ->
            w.writeArray (fun () ->
              w.WriteStringValue "Redirect"
              w.WriteStringValue str)

            writeDval DNull
          | Response (code, headers, hdv) ->
            w.writeArray (fun () ->
              w.WriteStringValue "Response"
              w.writeFullInt64Value (code)

              w.writeArray (fun () ->
                List.iter
                  (fun (k : string, v : string) ->
                    w.writeArray (fun () ->
                      w.WriteStringValue k
                      w.WriteStringValue v))
                  headers))

            writeDval hdv))
    | DDB dbname -> wrapStringValue "datastore" dbname
    | DDate date -> wrapStringValue "date" (date.toIsoString ())
    | DPassword (Password hashed) ->
      if redact then
        wrapNullValue "password"
      else
        hashed |> Base64.defaultEncodeToString |> wrapStringValue "password"
    | DUuid uuid -> wrapStringValue "uuid" (string uuid)
    | DOption opt ->
      (match opt with
       | None -> wrapNullValue "option"
       | Some ndv -> wrapNestedDvalValue "option" ndv)
    | DErrorRail erdv -> wrapNestedDvalValue "errorrail" erdv
    | DResult res ->
      (match res with
       | Ok rdv ->
         w.writeObject (fun () ->
           w.WriteString("type", "result")
           w.WriteString("constructor", "Ok")
           w.WritePropertyName "values"
           w.writeArray (fun () -> writeDval rdv))
       | Error rdv ->
         w.writeObject (fun () ->
           w.WriteString("type", "result")
           w.WriteString("constructor", "Error")
           w.WritePropertyName "values"
           w.writeArray (fun () -> writeDval rdv)))
    | DBytes bytes ->
      // Note that the OCaml version uses the non-url-safe b64 encoding here
      bytes |> System.Convert.ToBase64String |> wrapStringValue "bytes"



  let toPrettyResponseJsonV0 (dval : Dval) : string =
    writePrettyJson (fun w -> unsafeDvalToJsonValue w false dval)
