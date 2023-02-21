module LibExecutionStdLib.LibObject

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

open Newtonsoft.Json
open Newtonsoft.Json.Linq


module Errors = LibExecution.Errors
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"

// Note: this is used outside of this fn!
module PrettyResponseJsonV0 =

  // At time of writing, this is the same as Dval.unsafe_dval_to_yojson. It's being
  // copied to be certain this format doesn't change.
  let writePrettyJson (f : JsonWriter -> unit) : string =
    let stream = new System.IO.StringWriter()
    let w = new JsonTextWriter(stream)
    // Match yojson
    w.FloatFormatHandling <- FloatFormatHandling.Symbol
    w.Formatting <- Formatting.Indented
    f w
    string stream

  type JsonWriter with

    member this.writeObject(f : unit -> unit) =
      this.WriteStartObject()
      f ()
      this.WriteEnd()

    member this.writeArray(f : unit -> unit) =
      this.WriteStartArray()
      f ()
      this.WriteEnd()

  let rec unsafeDvalToJsonValueV0 (w : JsonWriter) (dv : Dval) : unit =
    let writeDval = unsafeDvalToJsonValueV0 w

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
    // basic types
    | DInt i -> w.WriteValue i
    | DFloat f -> w.WriteValue f
    | DBool b -> w.WriteValue b
    | DNull -> w.WriteNull()
    | DStr s -> w.WriteValue s
    | DList l -> w.writeArray (fun () -> List.iter writeDval l)
    | DTuple (first, second, theRest) ->
      w.writeArray (fun () -> List.iter writeDval ([ first; second ] @ theRest))
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
    | DDate date -> wrapStringValue "date" (DDateTime.toIsoString date)
    | DPassword _ -> wrapNullValue "password"
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
    | DBytes bytes -> bytes |> UTF8.ofBytesWithReplacement |> wrapStringValue "bytes"


  let toPrettyResponseJsonV0 (dval : Dval) : string =
    writePrettyJson (fun w -> unsafeDvalToJsonValueV0 w dval)
