module LibSerialization.Binary.Serializers.PT.Toplevel

open System.IO
open Prelude

open LibExecution.ProgramTypes

open LibSerialization.Binary.Serializers.Common
open LibSerialization.Binary.Serializers.PT.Common


module DB =
  let write (w : BinaryWriter) (db : DB.T) : unit =
    w.Write db.tlid
    String.write w db.name
    w.Write db.version
    TypeReference.write w db.typ

  let read (r : BinaryReader) : DB.T =
    let tlid = r.ReadUInt64()
    let name = String.read r
    let version = r.ReadInt32()
    let typ = TypeReference.read r
    { tlid = tlid; name = name; version = version; typ = typ }


// Tag byte 0 = DB. Old blobs with tag 1 (TLHandler) will fail to read,
// matching the kill-and-fill cutover called out in the migrations
// rewrite design.
let write (w : BinaryWriter) (db : DB.T) : unit =
  w.Write 0uy
  DB.write w db

let read (r : BinaryReader) : DB.T =
  match r.ReadByte() with
  | 0uy -> DB.read r
  | b -> raiseFormatError $"Invalid Toplevel tag: {b}"
