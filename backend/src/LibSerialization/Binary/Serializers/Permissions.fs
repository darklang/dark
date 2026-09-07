/// Versioned binary format for trusted policy files.
module LibSerialization.Binary.Serializers.Permissions

open System.IO
open Prelude

module P = LibExecution.Permissions
module E = LibExecution.Effects

open LibSerialization.Binary.Serializers.Common

let private writeScope
  (writeValue : BinaryWriter -> 'a -> unit)
  (writer : BinaryWriter)
  (scope : P.Scope<'a>)
  =
  match scope with
  | P.Scope.All -> writer.Write 0uy
  | P.Scope.Only value ->
    writer.Write 1uy
    writeValue writer value

let private readScope
  (readValue : BinaryReader -> 'a)
  (reader : BinaryReader)
  : P.Scope<'a> =
  match reader.ReadByte() with
  | 0uy -> P.Scope.All
  | 1uy -> P.Scope.Only(readValue reader)
  | tag -> raiseFormatError $"Invalid permission scope tag: {tag}"

let private writeStringScope (writer : BinaryWriter) (scope : P.Scope<string>) =
  writeScope (fun w value -> String.write w value) writer scope

let private readStringScope (reader : BinaryReader) : P.Scope<string> =
  readScope (fun r -> String.read r) reader

let private writeIntScope (writer : BinaryWriter) (scope : P.Scope<int>) =
  writeScope (fun w value -> Int32.writeInt32 w value) writer scope

let private readIntScope (reader : BinaryReader) : P.Scope<int> =
  readScope (fun r -> Int32.readInt32 r) reader

/// Effects cross the wire by name, like every other serialized effect set
/// (`Serializers.Effects`), so adding an effect needs no tag table here.
let private writeEffect (writer : BinaryWriter) (effect : E.Effect) =
  String.write writer (E.name effect)

let private readEffect (reader : BinaryReader) : E.Effect =
  let name = String.read reader
  match E.fromName name with
  | Some effect -> effect
  | None -> raiseFormatError $"Unknown effect name: {name}"

let private writeHost (writer : BinaryWriter) (host : P.HostRule) =
  match host with
  | P.HostRule.Any -> writer.Write 0uy
  | P.HostRule.Exact value ->
    writer.Write 1uy
    String.write writer value
  | P.HostRule.SubdomainsOf value ->
    writer.Write 2uy
    String.write writer value

let private readHost (reader : BinaryReader) : P.HostRule =
  match reader.ReadByte() with
  | 0uy -> P.HostRule.Any
  | 1uy -> P.HostRule.Exact(String.read reader)
  | 2uy -> P.HostRule.SubdomainsOf(String.read reader)
  | tag -> raiseFormatError $"Invalid host rule tag: {tag}"

let private writeAccess (writer : BinaryWriter) (access : P.AccessKind) =
  writer.Write(
    match access with
    | P.AccessKind.Read -> 0uy
    | P.AccessKind.Write -> 1uy
  )

let private readAccess (reader : BinaryReader) : P.AccessKind =
  match reader.ReadByte() with
  | 0uy -> P.AccessKind.Read
  | 1uy -> P.AccessKind.Write
  | tag -> raiseFormatError $"Invalid access tag: {tag}"

let private writeHttpRule (writer : BinaryWriter) (rule : P.HttpRule) =
  writeStringScope writer rule.method
  writeStringScope writer rule.scheme
  writeHost writer rule.host
  writeIntScope writer rule.port
  writeStringScope writer rule.pathPrefix
  writeStringScope writer rule.query

let private readHttpRule (reader : BinaryReader) : P.HttpRule =
  { method = readStringScope reader
    scheme = readStringScope reader
    host = readHost reader
    port = readIntScope reader
    pathPrefix = readStringScope reader
    query = readStringScope reader }

let private writeArgs (writer : BinaryWriter) args =
  match args with
  | P.Scope.All -> writer.Write 0uy
  | P.Scope.Only args ->
    writer.Write 1uy
    List.write writer String.write args

let private readArgs (reader : BinaryReader) : P.Scope<List<string>> =
  match reader.ReadByte() with
  | 0uy -> P.Scope.All
  | 1uy -> P.Scope.Only(List.read reader String.read)
  | tag -> raiseFormatError $"Invalid process argument rule tag: {tag}"

let private writeRule (writer : BinaryWriter) rule =
  match rule with
  | P.Rule.All -> writer.Write 0uy
  | P.Rule.Effect effect ->
    writer.Write 1uy
    writeEffect writer effect
  | P.Rule.Http http ->
    writer.Write 2uy
    writeHttpRule writer http
  | P.Rule.HttpServer ports ->
    writer.Write 3uy
    writeIntScope writer ports
  | P.Rule.File(access, roots) ->
    writer.Write 4uy
    writeAccess writer access
    writeStringScope writer roots
  | P.Rule.Env(access, names) ->
    writer.Write 5uy
    writeAccess writer access
    writeStringScope writer names
  | P.Rule.Db(access, names) ->
    writer.Write 6uy
    writeAccess writer access
    writeStringScope writer names
  | P.Rule.Process processRule ->
    writer.Write 7uy
    writeStringScope writer processRule.executable
    writeArgs writer processRule.args

let private readRule (reader : BinaryReader) : P.Rule =
  match reader.ReadByte() with
  | 0uy -> P.Rule.All
  | 1uy -> P.Rule.Effect(readEffect reader)
  | 2uy -> P.Rule.Http(readHttpRule reader)
  | 3uy -> P.Rule.HttpServer(readIntScope reader)
  | 4uy -> P.Rule.File(readAccess reader, readStringScope reader)
  | 5uy -> P.Rule.Env(readAccess reader, readStringScope reader)
  | 6uy -> P.Rule.Db(readAccess reader, readStringScope reader)
  | 7uy ->
    P.Rule.Process { executable = readStringScope reader; args = readArgs reader }
  | tag -> raiseFormatError $"Invalid permission rule tag: {tag}"

/// One policy. Versioning belongs to the enclosing file format
/// (`PolicyStore.formatVersion`); a policy has no version of its own.
let write (writer : BinaryWriter) (policy : P.Policy) =
  let allow, deny = P.Policy.rules policy
  List.write writer writeRule allow
  List.write writer writeRule deny

let read (reader : BinaryReader) : P.Policy =
  let allow = List.read reader readRule
  let deny = List.read reader readRule
  P.Policy.create allow deny
