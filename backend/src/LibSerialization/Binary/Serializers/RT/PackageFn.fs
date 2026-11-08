module LibSerialization.Binary.Serializers.RT.PackageFn

open System
open System.IO
open Prelude

open LibExecution.RuntimeTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.RT.Common


module Parameter =
  let write (w : BinaryWriter) (p : PackageFn.Parameter) =
    String.write w p.name
    TypeReference.write w p.typ

  let read (r : BinaryReader) : PackageFn.Parameter =
    let name = String.read r
    let typ = TypeReference.read r
    { name = name; typ = typ }


/// The symbol table, on its own.
///
/// Written to its own column rather than into the instruction blob, because running code never
/// needs it and reading code always does. A store that has the instructions and not the symbols
/// is a store that can run but not show, which is exactly the right thing to degrade to.
module DebugSymbols =
  let private writeTable
    (w : BinaryWriter)
    (table : Map<int, struct (id * Register)>)
    =
    let entries = Map.toList table
    Varint.write w entries.Length
    entries
    |> List.iter (fun (idx, struct (exprId, reg)) ->
      Varint.write w idx
      UInt64.writeUInt64 w exprId
      Varint.write w reg)

  let rec private readEntries
    (r : BinaryReader)
    (remaining : int)
    (table : Map<int, struct (id * Register)>)
    : Map<int, struct (id * Register)> =
    if remaining = 0 then
      table
    else
      let idx = Varint.read r
      let exprId = UInt64.readUInt64 r
      let reg = Varint.read r
      readEntries r (remaining - 1) (Map.add idx (struct (exprId, reg)) table)

  let private readTable (r : BinaryReader) : Map<int, struct (id * Register)> =
    readEntries r (Varint.read r) Map.empty

  let write (w : BinaryWriter) (symbols : DebugSymbols) =
    writeTable w symbols.exprAt
    let lambdas = Map.toList symbols.lambdas
    Varint.write w lambdas.Length
    lambdas
    |> List.iter (fun (exprId, table) ->
      UInt64.writeUInt64 w exprId
      writeTable w table)

  let rec private readLambdas
    (r : BinaryReader)
    (remaining : int)
    (lambdas : Map<id, Map<int, struct (id * Register)>>)
    : Map<id, Map<int, struct (id * Register)>> =
    if remaining = 0 then
      lambdas
    else
      let exprId = UInt64.readUInt64 r
      readLambdas r (remaining - 1) (Map.add exprId (readTable r) lambdas)

  let read (r : BinaryReader) : DebugSymbols =
    let exprAt = readTable r
    { exprAt = exprAt; lambdas = readLambdas r (Varint.read r) Map.empty }


let write (w : BinaryWriter) (fn : PackageFn.PackageFn) =
  Hash.write w fn.hash
  List.write w String.write fn.typeParams
  NEList.write Parameter.write w fn.parameters
  TypeReference.write w fn.returnType
  Instructions.write w fn.body
  Option.write
    w
    LibSerialization.Binary.Serializers.Effects.write
    fn.permissionCeiling
  List.write w TypeReference.Bound.write fn.bounds

let read (version : uint32) (r : BinaryReader) : PackageFn.PackageFn =
  let hash = Hash.read r
  let typeParams = List.read r String.read
  let parameters = NEList.read Parameter.read r
  let returnType = TypeReference.read r
  let body = Instructions.read version r
  let permissionCeiling =
    Option.read r LibSerialization.Binary.Serializers.Effects.read
  let bounds = TypeReference.Bound.readList version r
  { hash = hash
    typeParams = typeParams
    parameters = parameters
    returnType = returnType
    body = body
    // Not in this blob. The caller attaches them from their own column when something is
    // reading rather than running.
    symbols = DebugSymbols.emptyLazy
    permissionCeiling = permissionCeiling
    bounds = bounds }
