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

/// The effects a policy file may still name from before they were retired. A rule naming one
/// is dropped on read rather than failing the whole file: every policy written while they
/// existed carried them in its defaults, and nothing can request them any more.
let private retiredEffectNames : Set<string> = set [ "db-read"; "db-write" ]

let private readEffect (reader : BinaryReader) : Option<E.Effect> =
  let name = String.read reader
  match E.fromName name with
  | Some effect -> Some effect
  | None when Set.contains name retiredEffectNames -> None
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
  // 6 was Db, retired with user datastores; a stored one is dropped on read
  | P.Rule.Process processRule ->
    writer.Write 7uy
    writeStringScope writer processRule.executable
    writeArgs writer processRule.args

/// `None` for a rule about something retired (see `retiredEffectNames`), which the caller drops.
let private readRule (reader : BinaryReader) : Option<P.Rule> =
  match reader.ReadByte() with
  | 0uy -> Some P.Rule.All
  | 1uy -> readEffect reader |> Option.map P.Rule.Effect
  | 2uy -> Some(P.Rule.Http(readHttpRule reader))
  | 3uy -> Some(P.Rule.HttpServer(readIntScope reader))
  | 4uy -> Some(P.Rule.File(readAccess reader, readStringScope reader))
  | 5uy -> Some(P.Rule.Env(readAccess reader, readStringScope reader))
  | 6uy ->
    // A datastore rule: read it to keep the stream aligned, then drop it.
    let _access = readAccess reader
    let _names = readStringScope reader
    None
  | 7uy ->
    Some(
      P.Rule.Process { executable = readStringScope reader; args = readArgs reader }
    )
  | tag -> raiseFormatError $"Invalid permission rule tag: {tag}"

/// One policy. Versioning belongs to the enclosing file format
/// (`PolicyStore.formatVersion`); a policy has no version of its own.
let write (writer : BinaryWriter) (policy : P.Policy) =
  let allow, deny = P.Policy.rules policy
  List.write writer writeRule allow
  List.write writer writeRule deny

let read (reader : BinaryReader) : P.Policy =
  let allow = List.read reader readRule |> List.choose (fun rule -> rule)
  let deny = List.read reader readRule |> List.choose (fun rule -> rule)
  P.Policy.create allow deny

/// Private host transport, paired with the worker binary from the same build.
let writeExecutionAccess (writer : BinaryWriter) (access : P.Access) : unit =
  List.write
    writer
    (fun writer (layer, policy) ->
      match layer with
      | P.Layer.Instance -> writer.Write 0uy
      | P.Layer.Run -> writer.Write 1uy
      | P.Layer.Package id ->
        writer.Write 2uy
        String.write writer id
      | P.Layer.Function id ->
        writer.Write 3uy
        String.write writer id
      | P.Layer.Value name ->
        writer.Write 4uy
        String.write writer name
      write writer policy)
    (P.Access.layers access)

let readExecutionAccess (reader : BinaryReader) : P.Access =
  List.read reader (fun reader ->
    let layer =
      match reader.ReadByte() with
      | 0uy -> P.Layer.Instance
      | 1uy -> P.Layer.Run
      | 2uy -> P.Layer.Package(String.read reader)
      | 3uy -> P.Layer.Function(String.read reader)
      | 4uy -> P.Layer.Value(String.read reader)
      | tag -> raiseFormatError $"Invalid permission layer tag: {tag}"
    layer, read reader)
  |> P.Access.ofLayers
