/// Toplevel serialization for custom binary format
module LibBinarySerialization.Binary.Serializers.Toplevel

open System.IO
open Prelude
open LibBinarySerialization.Binary.BinaryFormat
open LibBinarySerialization.Binary.Primitives
open LibBinarySerialization.SerializedTypes
open TypeReference
open Expr

module Handler =
  module CronInterval =
    let write (writer : BinaryWriter) (interval : Handler.CronInterval) : unit =
      match interval with
      | Handler.EveryDay -> writer.Write(0uy)
      | Handler.EveryWeek -> writer.Write(1uy)
      | Handler.EveryFortnight -> writer.Write(2uy)
      | Handler.EveryHour -> writer.Write(3uy)
      | Handler.Every12Hours -> writer.Write(4uy)
      | Handler.EveryMinute -> writer.Write(5uy)

    let read (reader : BinaryReader) : Handler.CronInterval =
      match reader.ReadByte() with
      | 0uy -> Handler.EveryDay
      | 1uy -> Handler.EveryWeek
      | 2uy -> Handler.EveryFortnight
      | 3uy -> Handler.EveryHour
      | 4uy -> Handler.Every12Hours
      | 5uy -> Handler.EveryMinute
      | b ->
        raise (BinaryFormatException(CorruptedData $"Invalid CronInterval tag: {b}"))

  module Spec =
    let write (writer : BinaryWriter) (spec : Handler.Spec) : unit =
      match spec with
      | Handler.Worker name ->
        writer.Write(0uy)
        Writer.writeString writer name
      | Handler.Cron(name, interval) ->
        writer.Write(1uy)
        Writer.writeString writer name
        CronInterval.write writer interval
      | Handler.REPL name ->
        writer.Write(2uy)
        Writer.writeString writer name
      | Handler.HTTP(route, method) ->
        writer.Write(3uy)
        Writer.writeString writer route
        Writer.writeString writer method

    let read (reader : BinaryReader) : Handler.Spec =
      match reader.ReadByte() with
      | 0uy -> Handler.Worker(Reader.readString reader)
      | 1uy ->
        let name = Reader.readString reader
        let interval = CronInterval.read reader
        Handler.Cron(name, interval)
      | 2uy -> Handler.REPL(Reader.readString reader)
      | 3uy ->
        let route = Reader.readString reader
        let method = Reader.readString reader
        Handler.HTTP(route, method)
      | b ->
        raise (BinaryFormatException(CorruptedData $"Invalid Handler.Spec tag: {b}"))

  let write (writer : BinaryWriter) (handler : Handler.T) : unit =
    writer.Write(handler.tlid)
    writeExpr writer handler.ast
    Spec.write writer handler.spec

  let read (reader : BinaryReader) : Handler.T =
    let tlid = reader.ReadUInt64()
    let ast = readExpr reader
    let spec = Spec.read reader
    { tlid = tlid; ast = ast; spec = spec }

module DB =
  let write (writer : BinaryWriter) (db : DB.T) : unit =
    writer.Write(db.tlid)
    Writer.writeString writer db.name
    writer.Write(db.version)
    writeTypeReference writer db.typ

  let read (reader : BinaryReader) : DB.T =
    let tlid = reader.ReadUInt64()
    let name = Reader.readString reader
    let version = reader.ReadInt32()
    let typ = readTypeReference reader
    { tlid = tlid; name = name; version = version; typ = typ }

let write (writer : BinaryWriter) (toplevel : Toplevel.T) : unit =
  match toplevel with
  | Toplevel.TLDB db ->
    writer.Write(0uy)
    DB.write writer db
  | Toplevel.TLHandler handler ->
    writer.Write(1uy)
    Handler.write writer handler

let read (reader : BinaryReader) : Toplevel.T =
  match reader.ReadByte() with
  | 0uy -> Toplevel.TLDB(DB.read reader)
  | 1uy -> Toplevel.TLHandler(Handler.read reader)
  | b -> raise (BinaryFormatException(CorruptedData $"Invalid Toplevel tag: {b}"))
