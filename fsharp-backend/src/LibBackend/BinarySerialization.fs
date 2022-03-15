/// Conversion to/from Dark values to binary formats
module LibBackend.BinarySerialization


open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth
open Prelude.Tablecloth

module PT = LibExecution.ProgramTypes

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

let serializeToplevel (tl : PT.Toplevel.T) : byte [] =
  MessagePack.MessagePackSerializer.Serialize(tl, optionsWithoutZip)

let deserializeToplevel (data : byte []) : PT.Toplevel.T =
  MessagePack.MessagePackSerializer.Deserialize(data, optionsWithoutZip)

let serializeOplist (oplist : PT.Oplist) : byte [] =
  MessagePack.MessagePackSerializer.Serialize(oplist, optionsWithZip)

let serializeOplistToJson (oplist : PT.Oplist) : string =
  MessagePack.MessagePackSerializer.SerializeToJson(oplist, optionsWithZip)

let deserializeOplist (data : byte []) : PT.Oplist =
  MessagePack.MessagePackSerializer.Deserialize(data, optionsWithZip)
