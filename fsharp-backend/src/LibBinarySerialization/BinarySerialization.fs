/// Conversion to/from Dark values to binary formats
module LibBinarySerialization.BinarySerialization


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth
open Prelude.Tablecloth

module PT = LibExecution.ProgramTypes
module ST = SerializedTypes
module ST2PT = SerializedTypesToProgramTypes
module PT2ST = ProgramTypesToSerializedTypes

open MessagePack
open MessagePack.Resolvers
open MessagePack.FSharp

// Serializers sometimes throw at runtime if the setup is not right. We do not
// currently know of a way to statically ensure these run. As a result, we don't
// expose the generic serialization functions, only functions for specific types that
// are tested (that is, they have unit tests!) and are known to work.

let resolver =
  Resolvers.CompositeResolver.Create(
    FSharpResolver.Instance,
    StandardResolver.Instance
  )

let optionsWithoutZip = MessagePackSerializerOptions.Standard.WithResolver(resolver)

let optionsWithZip =
  MessagePack
    .MessagePackSerializerOptions
    .Standard
    .WithResolver(resolver)
    .WithCompression(MessagePack.MessagePackCompression.Lz4BlockArray)

let wrapSerializationException (tlid : tlid) (f : unit -> 'a) : 'a =
  try
    f ()
  with
  | e ->
    Exception.callExceptionCallback e
    raise (InternalException("error deserializing toplevel", [ "tlid", tlid ], e))

let serializeExpr (tlid : tlid) (e : PT.Expr) : byte [] =
  wrapSerializationException tlid (fun () ->
    let serializableValue = PT2ST.Expr.toST e
    MessagePack.MessagePackSerializer.Serialize(serializableValue, optionsWithoutZip))

let deserializeExpr (tlid : tlid) (data : byte []) : PT.Expr =
  wrapSerializationException tlid (fun () ->
    MessagePack.MessagePackSerializer.Deserialize<ST.Expr>(data, optionsWithoutZip)
    |> ST2PT.Expr.toPT)

let serializeToplevel (tl : PT.Toplevel.T) : byte [] =
  wrapSerializationException (PT.Toplevel.toTLID tl) (fun () ->
    let v = PT2ST.Toplevel.toST tl
    MessagePack.MessagePackSerializer.Serialize(v, optionsWithoutZip))

let deserializeToplevel (tlid : tlid) (data : byte []) : PT.Toplevel.T =
  wrapSerializationException tlid (fun () ->
    MessagePack.MessagePackSerializer.Deserialize(data, optionsWithoutZip)
    |> ST2PT.Toplevel.toPT)


let serializeOplist (tlid : tlid) (oplist : PT.Oplist) : byte [] =
  wrapSerializationException tlid (fun () ->
    let v = List.map PT2ST.Op.toST oplist
    MessagePack.MessagePackSerializer.Serialize(v, optionsWithZip))


let deserializeOplist (tlid : tlid) (data : byte []) : PT.Oplist =
  wrapSerializationException tlid (fun () ->
    MessagePack.MessagePackSerializer.Deserialize(data, optionsWithZip)
    |> List.filterMap ST2PT.Op.toPT)

module Test =
  let serializeOplistToJson (tlid : tlid) (oplist : PT.Oplist) : string =
    wrapSerializationException tlid (fun () ->
      let v = List.map PT2ST.Op.toST oplist
      MessagePack.MessagePackSerializer.SerializeToJson(v, optionsWithZip))
