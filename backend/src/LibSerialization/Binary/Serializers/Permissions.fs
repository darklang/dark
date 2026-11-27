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

/// Effects cross the wire by name, so adding an effect needs no tag table here.
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

let private writeHttpPath (writer : BinaryWriter) path =
  match path with
  | P.HttpPath.All -> writer.Write 0uy
  | P.HttpPath.Exact path ->
    writer.Write 1uy
    String.write writer path
  | P.HttpPath.Subtree path ->
    writer.Write 2uy
    String.write writer path

let private readHttpPath (reader : BinaryReader) =
  match reader.ReadByte() with
  | 0uy -> P.HttpPath.All
  | 1uy -> P.HttpPath.Exact(String.read reader)
  | 2uy -> P.HttpPath.Subtree(String.read reader)
  | tag -> raiseFormatError $"Invalid HTTP path tag: {tag}"

let private writeHttpRule (writer : BinaryWriter) (rule : P.HttpRule) =
  writeStringScope writer rule.method
  writeStringScope writer rule.scheme
  writeHost writer rule.host
  writeIntScope writer rule.port
  writeHttpPath writer rule.path
  writeStringScope writer rule.query

let private readHttpRule (reader : BinaryReader) : P.HttpRule =
  { method = readStringScope reader
    scheme = readStringScope reader
    host = readHost reader
    port = readIntScope reader
    path = readHttpPath reader
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
  | P.Rule.HttpServer(addresses, ports) ->
    writer.Write 3uy
    writeStringScope writer addresses
    writeIntScope writer ports
  | P.Rule.File(access, roots) ->
    writer.Write 4uy
    writeAccess writer access
    writeStringScope writer roots
  | P.Rule.Env(access, names) ->
    writer.Write 5uy
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
  | 3uy -> P.Rule.HttpServer(readStringScope reader, readIntScope reader)
  | 4uy -> P.Rule.File(readAccess reader, readStringScope reader)
  | 5uy -> P.Rule.Env(readAccess reader, readStringScope reader)
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

/// A policy as store format 9 wrote it, for the one-time upgrade. Returns the policy as it is
/// written today, and a line for each rule that could not be carried over.
///
/// Nothing here may widen access: an allow rule becomes the same rule or a narrower one, or is
/// dropped; a deny rule becomes the same rule or a broader one.
let readFormat9 (reader : BinaryReader) : P.Policy * List<string> =
  let readPath () : P.HttpPath =
    match readStringScope reader with
    | P.Scope.All -> P.HttpPath.All
    | P.Scope.Only prefix -> P.HttpPath.Subtree prefix
  // Format 9 had no address, and Dark only ever served on this one.
  let serverAddress (allowing : bool) : P.Scope<string> =
    if allowing then P.Scope.Only "127.0.0.1" else P.Scope.All
  let readRule (allowing : bool) : Result<P.Rule, string> =
    match reader.ReadByte() with
    | 0uy -> Ok P.Rule.All
    | 1uy ->
      let name = String.read reader
      match E.fromName name with
      | Some effect -> Ok(P.Rule.Effect effect)
      | None -> Error $"{name} (Dark no longer has it)"
    | 2uy ->
      let method = readStringScope reader
      let scheme = readStringScope reader
      let host = readHost reader
      let port = readIntScope reader
      let path = readPath ()
      let query = readStringScope reader
      Ok(
        P.Rule.Http
          { method = method
            scheme = scheme
            host = host
            port = port
            path = path
            query = query }
      )
    | 3uy -> Ok(P.Rule.HttpServer(serverAddress allowing, readIntScope reader))
    | 4uy ->
      let access = readAccess reader
      Ok(P.Rule.File(access, readStringScope reader))
    | 5uy ->
      let access = readAccess reader
      Ok(P.Rule.Env(access, readStringScope reader))
    | 6uy ->
      let access = readAccess reader
      let names = readStringScope reader
      let verb =
        match access with
        | P.AccessKind.Read -> "read"
        | P.AccessKind.Write -> "write"
      let which =
        match names with
        | P.Scope.All -> "*"
        | P.Scope.Only name -> name
      Error $"db {verb} {which} (Dark no longer has databases)"
    | 7uy ->
      let executable = readStringScope reader
      Ok(P.Rule.Process { executable = executable; args = readArgs reader })
    | tag -> raiseFormatError $"Invalid permission rule tag: {tag}"
  let allow = List.read reader (fun _ -> readRule true)
  let deny = List.read reader (fun _ -> readRule false)
  let kept = List.choose Result.toOption
  let dropped (label : string) rules =
    rules
    |> List.choose (function
      | Error what -> Some $"{label} {what}"
      | Ok _ -> None)
  P.Policy.create (kept allow) (kept deny),
  dropped "allow" allow @ dropped "deny" deny

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
      | 4uy -> P.Layer.Value(String.read reader)
      | tag -> raiseFormatError $"Invalid permission layer tag: {tag}"
    layer, read reader)
  |> P.Access.ofLayers
