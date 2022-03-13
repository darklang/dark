module LibBackend.BinarySerialization

// Conversion to/from Dark values to binary formats

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

let internalSerialize
  (options : MessagePackSerializerOptions)
  (data : 'a)
  : byte [] =
  MessagePack.MessagePackSerializer.Serialize<'a>(data, options)

let internalDeserialize<'a>
  (options : MessagePackSerializerOptions)
  (bytes : byte [])
  : 'a =
  MessagePack.MessagePackSerializer.Deserialize<'a>(bytes, options)

let serializeToplevel (tl : PT.Toplevel.T) : byte [] =
  internalSerialize optionsWithoutZip tl

let deserializeToplevel (data : byte []) : PT.Toplevel.T =
  internalDeserialize optionsWithoutZip data

let serializeOplist (oplist : PT.Oplist) : byte [] =
  internalSerialize optionsWithZip oplist

let deserializeOplist (data : byte []) : PT.Oplist =
  internalDeserialize optionsWithZip data
