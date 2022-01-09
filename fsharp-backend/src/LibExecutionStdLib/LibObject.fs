module LibExecutionStdLib.LibObject

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

open Newtonsoft.Json
open Newtonsoft.Json.Linq


module Errors = LibExecution.Errors
module DvalReprExternal = LibExecution.DvalReprExternal

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

module PrettyResponseJsonV0 =

  // At time of writing, this is the same as Dval.unsafe_dval_to_yojson. It's being copied to be certain this format doesn't change.
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

  let rec unsafeDvalToJsonValueV0
    (w : JsonWriter)
    (redact : bool)
    (dv : Dval)
    : unit =
    let writeDval = unsafeDvalToJsonValueV0 w redact

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
      // Note that the OCaml version uses the non-url-safe b64 encoding here
      bytes |> System.Convert.ToBase64String |> wrapStringValue "bytes"



  let toPrettyResponseJsonV0 (dval : Dval) : string =
    debuG "calling the dval" dval
    writePrettyJson (fun w ->
      debuG "the dval" dval
      unsafeDvalToJsonValueV0 w false dval)


let fns : List<BuiltInFn> =
  [ { name = fn "Object" "empty" 0
      parameters = []
      returnType = TDict varA
      description = "Return an empty object"
      fn =
        (function
        | _, [] -> Ply(DObj(Map.empty))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "empty" 0) }
    { name = fn "Object" "merge" 0
      parameters =
        [ Param.make "left" (TDict varA) ""; Param.make "right" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Return a combined object with both objects' keys and values. If the same key exists in both `left` and `right`, then use the value from `right`"
      fn =
        (function
        | _, [ DObj l; DObj r ] -> Ply(DObj(Map.mergeFavoringRight l r))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "merge" 0) }
    { name = fn "Object" "toJSON" 0
      parameters = [ Param.make "obj" (TDict varA) "" ]
      returnType = TStr
      description = "Dumps `obj` to a JSON string"
      fn =
        (function
        | _, [ DObj o ] ->
          DObj o |> PrettyResponseJsonV0.toPrettyResponseJsonV0 |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Object" "toJSON" 1) }
    { name = fn "Object" "toJSON" 1
      parameters = [ Param.make "obj" (TDict varA) "" ]
      returnType = TStr
      description = "Dumps `obj` to a JSON string"
      fn =
        (function
        | _, [ DObj o ] ->
          DObj o |> DvalReprExternal.toPrettyMachineJsonStringV1 |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "toJSON" 0) } ]
