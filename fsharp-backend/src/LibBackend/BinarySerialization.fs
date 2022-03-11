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

let internalSerialize (data : 'a) : byte [] =
  MessagePackSerializer.Serialize<'a> data

let internalDeserialize<'a> (bytes : byte []) : 'a =
  MessagePackSerializer.Deserialize<'a> bytes

let serializeToplevel (tl : PT.Toplevel) : byte [] = internalSerialize tl
let deserializeToplevel (data : byte []) : PT.Toplevel = internalDeserialize data

let serializeOplist (oplist : PT.Oplist) : byte [] = internalSerialize oplist

let deserializeOplist (data : byte []) : PT.Oplist = internalDeserialize data



let init () : unit =
  let resolver =
    Resolvers.CompositeResolver.Create(
      FSharpResolver.Instance,
      StandardResolver.Instance,
      NativeGuidResolver.Instance,
      ContractlessStandardResolver.Instance
    )
  let options = MessagePackSerializerOptions.Standard.WithResolver(resolver)
  MessagePackSerializer.DefaultOptions <- options
