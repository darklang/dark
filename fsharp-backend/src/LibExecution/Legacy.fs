module LibExecution.Legacy

open Prelude
open VendoredTablecloth
open RuntimeTypes

open Newtonsoft.Json
open Newtonsoft.Json.Linq

let writePrettyJson (f : JsonWriter -> unit) : string =
  let stream = new System.IO.StringWriter()
  let w = new JsonTextWriter(stream)
  // Match yojson
  w.FloatFormatHandling <- FloatFormatHandling.Symbol
  w.Formatting <- Formatting.Indented
  f w
  stream.ToString()

type JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEnd()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEnd()

module PrettyResponseJsonV0 =

  (* At time of writing, this is the same as Dval.unsafe_dval_to_yojson. It's being copied to be certain this format doesn't change. *)
  let rec unsafeDvalToJsonValue (w : JsonWriter) (redact : bool) (dv : Dval) : unit =
    let writeDval = unsafeDvalToJsonValue w redact

    let wrapStringValue (typ : string) (str : string) =
      w.writeObject (fun () ->
        w.WritePropertyName "type"
        w.WriteValue(typ)
        w.WritePropertyName "value"
        w.WriteValue(str))

    let wrapNullValue (typ : string) =
      w.writeObject (fun () ->
        w.WritePropertyName "type"
        w.WriteValue(typ)
        w.WritePropertyName "value"
        w.WriteNull())

    let wrapNestedDvalValue (typ : string) (dv : Dval) =
      w.writeObject (fun () ->
        w.WritePropertyName "type"
        w.WriteValue(typ)
        w.WritePropertyName "value"
        writeDval dv)

    match dv with
    (* basic types *)
    | DInt i -> w.WriteValue(int i)
    | DFloat f -> w.WriteValue f
    | DBool b -> w.WriteValue b
    | DNull -> w.WriteNull()
    | DStr s -> w.WriteValue s
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
    | DError _ -> wrapStringValue "error" "error"
    | DHttpResponse (h) ->
      w.writeObject (fun () ->
        w.WritePropertyName "type"
        w.WriteValue "response"
        w.WritePropertyName "value"

        w.writeArray (fun () ->
          match h with
          | Redirect str ->
            w.writeArray (fun () ->
              w.WriteValue "Redirect"
              w.WriteValue str)

            writeDval DNull
          | Response (code, headers, hdv) ->
            w.writeArray (fun () ->
              w.WriteValue "Response"
              w.WriteValue code

              w.writeArray (fun () ->
                List.iter
                  (fun (k : string, v : string) ->
                    w.writeArray (fun () ->
                      w.WriteValue k
                      w.WriteValue v))
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
           w.WritePropertyName "type"
           w.WriteValue("result")
           w.WritePropertyName "constructor"
           w.WriteValue("Ok")
           w.WritePropertyName "values"
           w.writeArray (fun () -> writeDval rdv))
       | Error rdv ->
         w.writeObject (fun () ->
           w.WritePropertyName "type"
           w.WriteValue("result")
           w.WritePropertyName "constructor"
           w.WriteValue("Error")
           w.WritePropertyName "values"
           w.writeArray (fun () -> writeDval rdv)))
    | DBytes bytes ->
      bytes |> System.Text.Encoding.ASCII.GetString |> wrapStringValue "bytes"

  let toPrettyResponseJsonV0 (dval : Dval) : string =
    writePrettyJson (fun w -> unsafeDvalToJsonValue w false dval)
