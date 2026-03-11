module LibSerialization.Binary.Serializers.PT.BranchOp

open System
open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common


let write (w : BinaryWriter) (op : BranchOp) : unit =
  match op with
  | BranchOp.CreateBranch(branchId, name, parentBranchId, baseCommitHash) ->
    w.Write(0uy)
    Guid.write w branchId
    String.write w name
    Option.write w Guid.write parentBranchId
    Option.write w Hash.write baseCommitHash
  | BranchOp.CreateCommit(commitHash, message, branchId, opHashes) ->
    w.Write(1uy)
    Hash.write w commitHash
    String.write w message
    Guid.write w branchId
    List.write w Hash.write opHashes
  | BranchOp.RebaseBranch(branchId, newBaseCommitHash) ->
    w.Write(2uy)
    Guid.write w branchId
    Hash.write w newBaseCommitHash
  | BranchOp.MergeBranch(branchId, intoBranchId) ->
    w.Write(3uy)
    Guid.write w branchId
    Guid.write w intoBranchId
  | BranchOp.ArchiveBranch branchId ->
    w.Write(4uy)
    Guid.write w branchId

let read (r : BinaryReader) : BranchOp =
  match r.ReadByte() with
  | 0uy ->
    let branchId = Guid.read r
    let name = String.read r
    let parentBranchId = Option.read r (fun r -> Guid.read r : BranchId)
    let baseCommitHash = Option.read r Hash.read
    BranchOp.CreateBranch(branchId, name, parentBranchId, baseCommitHash)
  | 1uy ->
    let commitHash = Hash.read r
    let message = String.read r
    let branchId = Guid.read r
    let opHashes = List.read r Hash.read
    BranchOp.CreateCommit(commitHash, message, branchId, opHashes)
  | 2uy ->
    let branchId = Guid.read r
    let newBaseCommitHash = Hash.read r
    BranchOp.RebaseBranch(branchId, newBaseCommitHash)
  | 3uy ->
    let branchId = Guid.read r
    let intoBranchId = Guid.read r
    BranchOp.MergeBranch(branchId, intoBranchId)
  | 4uy ->
    let branchId = Guid.read r
    BranchOp.ArchiveBranch branchId
  | b -> raiseFormatError $"Invalid BranchOp tag: {b}"


let serialize (id : string) (op : BranchOp) : byte array =
  use memoryStream = new MemoryStream()
  use binaryWriter = new BinaryWriter(memoryStream)
  String.write binaryWriter id
  write binaryWriter op
  memoryStream.ToArray()

let deserialize (id : string) (bytes : byte array) : BranchOp =
  use memoryStream = new MemoryStream(bytes)
  use binaryReader = new BinaryReader(memoryStream)
  let readId = String.read binaryReader
  if readId <> id then
    raiseFormatError $"BranchOp id mismatch: expected {id}, got {readId}"
  read binaryReader
