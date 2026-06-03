/// Binary serializer for the on-disk capability grant.
///
/// TODO: when `Capabilities` moves to the language level (see `LibExecution.Capabilities`), this should
/// move under the matching `PT`/`RT` folder and adopt its versioning conventions.
module LibSerialization.Binary.Serializers.Capabilities

open System.IO
open Prelude

module Cap = LibExecution.Capabilities

open LibSerialization.Binary.BaseFormat
open LibSerialization.Binary.Serializers.Common

// A Scope is a tag (Any vs Only) + the allow-list when Only.
let private writeScopeStr (w : BinaryWriter) (s : Cap.Scope<string>) : unit =
  match s with
  | Cap.Any -> Bool.write w true
  | Cap.Only xs ->
    Bool.write w false
    List.write w String.write (Set.toList xs)

let private readScopeStr (r : BinaryReader) : Cap.Scope<string> =
  if Bool.read r then Cap.Any else Cap.Only(List.read r String.read |> Set.ofList)

let private writeScopeInt (w : BinaryWriter) (s : Cap.Scope<int64>) : unit =
  match s with
  | Cap.Any -> Bool.write w true
  | Cap.Only xs ->
    Bool.write w false
    List.write w Int64.writeInt64 (Set.toList xs)

let private readScopeInt (r : BinaryReader) : Cap.Scope<int64> =
  if Bool.read r then
    Cap.Any
  else
    Cap.Only(List.read r Int64.readInt64 |> Set.ofList)

let private writeRW (w : BinaryWriter) (rw : Cap.RW<string>) : unit =
  writeScopeStr w rw.read
  writeScopeStr w rw.write

let private readRW (r : BinaryReader) : Cap.RW<string> =
  let read = readScopeStr r
  let write = readScopeStr r
  { read = read; write = write }

let private writeHost (w : BinaryWriter) (h : Cap.HostMatch) : unit =
  match h with
  | Cap.AnyHost -> w.Write(0uy)
  | Cap.ExactHost s ->
    w.Write(1uy)
    String.write w s
  | Cap.Subdomain d ->
    w.Write(2uy)
    String.write w d

let private readHost (r : BinaryReader) : Cap.HostMatch =
  match r.ReadByte() with
  | 0uy -> Cap.AnyHost
  | 1uy -> Cap.ExactHost(String.read r)
  | 2uy -> Cap.Subdomain(String.read r)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid HostMatch tag: {b}"))

let private writeUrl (w : BinaryWriter) (u : Cap.UrlScope) : unit =
  writeScopeStr w u.schemes
  List.write w writeHost u.hosts
  writeScopeInt w u.ports
  writeScopeStr w u.paths

let private readUrl (r : BinaryReader) : Cap.UrlScope =
  let schemes = readScopeStr r
  let hosts = List.read r readHost
  let ports = readScopeInt r
  let paths = readScopeStr r
  { schemes = schemes; hosts = hosts; ports = ports; paths = paths }

let private writeHttpRule (w : BinaryWriter) (rule : Cap.HttpRule) : unit =
  writeScopeStr w rule.methods
  writeUrl w rule.url

let private readHttpRule (r : BinaryReader) : Cap.HttpRule =
  let methods = readScopeStr r
  let url = readUrl r
  { methods = methods; url = url }

let private writeExecRule (w : BinaryWriter) (rule : Cap.ExecRule) : unit =
  writeScopeStr w rule.programs
  writeScopeStr w rule.args

let private readExecRule (r : BinaryReader) : Cap.ExecRule =
  let programs = readScopeStr r
  let args = readScopeStr r
  { programs = programs; args = args }

let write (w : BinaryWriter) (c : Cap.Capabilities) : unit =
  List.write w writeHttpRule c.httpClient
  writeScopeInt w c.httpServer
  writeRW w c.file
  writeRW w c.env
  writeRW w c.db
  List.write w writeExecRule c.exec
  Bool.write w c.stdout
  Bool.write w c.stdin
  Bool.write w c.random
  Bool.write w c.clock
  Bool.write w c.llm

let read (r : BinaryReader) : Cap.Capabilities =
  let httpClient = List.read r readHttpRule
  let httpServer = readScopeInt r
  let file = readRW r
  let env = readRW r
  let db = readRW r
  let exec = List.read r readExecRule
  let stdout = Bool.read r
  let stdin = Bool.read r
  let random = Bool.read r
  let clock = Bool.read r
  let llm = Bool.read r
  { httpClient = httpClient
    httpServer = httpServer
    file = file
    env = env
    db = db
    exec = exec
    stdout = stdout
    stdin = stdin
    random = random
    clock = clock
    llm = llm }
